#include <stdio.h>
#include <stdlib.h>

#include "compiler.h"
#include "../common.h"
#include "../scanner/scanner.h"

// Must come _after_ common.h include
#ifdef DEBUG_PRINT_CODE
#include "../debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)();

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

Parser parser;
Chunk* compilingChunk;

/**
 * @return a pointer to the chunk currently being compiled.
 */
static Chunk* currentChunk() {
    return compilingChunk;
}

/**
 * Prints an error message along with relevant source information.
 * @param token The token which generated the error
 * @param message The error message to report
 */
static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;  // Suppress any further errors if we're panicking
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

/**
 * If the parser encounters syntax error in the next token, this function reports it
 * @param message The error message to report
 */
static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

/**
 * Reports a syntax error in the token we have just consumed
 * @param message The error message to report
 */
static void error(const char* message) {
    errorAt(&parser.previous, message);
}

/**
 * Steps through the token stream, saving the current token and advancing the pointer one token at a time
 */
static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

/**
 * Consumes a token and checks that it's type matches that expected
 * @param type The type we expect
 * @param message The error message to report if the types don't match.
 */
static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

/**
 * Append a single byte to memory
 * @param byte The byte value to store
 */
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

/**
 * emits a pair of bytes into the byte stream
 * @param byte1 The first byte to write
 * @param byte2 The second byte to write
 */
static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

/**
 * Emits a return opcode into the byte stream
 */
static void emitReturn() {
    emitByte(OP_RETURN);
}

/**
 * Adds a constant to memory and returns the address at which it is stored
 * @param value The value to add to memory
 * @return The address/index of that constant
 */
static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

/**
 * Compiles a constant opcode for a constant value
 * @param value The value to bind to the opcode.
 */
static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

/**
 * Closes out compilation and shuts down the compiler
 */
static void endCompiler() {
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

// Some forward declarations to handle C's archaic compiler
static void expression();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

/**
 * Compile binary operations (+ - / *, etc)
 */
static void binary() {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_PLUS:            emitByte(OP_ADD); break;
        case TOKEN_MINUS:           emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:            emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:           emitByte(OP_DIVIDE); break;
        default:
            return; // Unreachable
    }
}

/**
 * Compile parenthesised expressions
 */
static void grouping() {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/**
 * Compiles number literals
 */
static void number() {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(value);
}

/**
 * Compiles unary operations
 */
static void unary() {
    TokenType operatorType = parser.previous.type;

    // Compile the operand
    parsePrecedence(PREC_UNARY);

    // emit the operator instruction
    switch (operatorType) {
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default: return; // Unreachable
    }
}

/**
 * Table of function pointers for compiling specific tokens to opcodes
 */
ParseRule rules[] = {
        [TOKEN_LEFT_PAREN]      = {grouping,    NULL,       PREC_NONE},
        [TOKEN_RIGHT_PAREN]     = {NULL,        NULL,       PREC_NONE},
        [TOKEN_LEFT_BRACE]      = {NULL,        NULL,       PREC_NONE},
        [TOKEN_RIGHT_BRACE]     = {NULL,        NULL,       PREC_NONE},
        [TOKEN_COMMA]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_DOT]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_MINUS]           = {unary,       binary,     PREC_TERM},
        [TOKEN_PLUS]            = {NULL,        binary,     PREC_TERM},
        [TOKEN_SEMICOLON]       = {NULL,        NULL,       PREC_NONE},
        [TOKEN_SLASH]           = {NULL,        binary,     PREC_FACTOR},
        [TOKEN_STAR]            = {NULL,        binary,     PREC_FACTOR},
        [TOKEN_BANG]            = {NULL,        NULL,       PREC_NONE},
        [TOKEN_BANG_EQUAL]      = {NULL,        NULL,       PREC_NONE},
        [TOKEN_EQUAL]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_EQUAL_EQUAL]     = {NULL,        NULL,       PREC_NONE},
        [TOKEN_GREATER]         = {NULL,        NULL,       PREC_NONE},
        [TOKEN_GREATER_EQUAL]   = {NULL,        NULL,       PREC_NONE},
        [TOKEN_LESS]            = {NULL,        NULL,       PREC_NONE},
        [TOKEN_LESS_EQUAL]      = {NULL,        NULL,       PREC_NONE},
        [TOKEN_IDENTIFIER]      = {NULL,        NULL,       PREC_NONE},
        [TOKEN_STRING]          = {NULL,        NULL,       PREC_NONE},
        [TOKEN_NUMBER]          = {number,      NULL,       PREC_NONE},
        [TOKEN_AND]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_CLASS]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_ELSE]            = {NULL,        NULL,       PREC_NONE},
        [TOKEN_FALSE]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_FOR]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_FUN]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_IF]              = {NULL,        NULL,       PREC_NONE},
        [TOKEN_NIL]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_OR]              = {NULL,        NULL,       PREC_NONE},
        [TOKEN_PRINT]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_RETURN]          = {NULL,        NULL,       PREC_NONE},
        [TOKEN_SUPER]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_THIS]            = {NULL,        NULL,       PREC_NONE},
        [TOKEN_TRUE]            = {NULL,        NULL,       PREC_NONE},
        [TOKEN_VAR]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_WHILE]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_ERROR]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_EOF]             = {NULL,        NULL,       PREC_NONE},
};

/**
 * Compiles a lox expression and emits the requisite bytecode
 */
static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

/**
 * Parse tokens at the supplied precedence level
 * @param precedence The precedence level at which we're operating
 */
static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    prefixRule();

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule();
    }
}

/**
 * Get the parsing rule from the virtual function table for parsing (see later in this file)
 * @param type the Token for the opcode we are compiling
 * @return The entry in the rules table corresponding to the opcode
 */
static ParseRule* getRule(TokenType type) {
    return &rules[type];
}



/**
 * The main workhorse of the compiler.  Compiles the source code into bytecode and stuffs it into the memory chunk
 * @param source Pointer to the source code
 * @param chunk Pointer to the VM memory
 * @return true if compilation succeeded, false otherwise
 */
bool compile(const char* source, Chunk* chunk) {
    initScanner(source);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();
    expression();
    consume(TOKEN_EOF, "Expect end of expression.");
    endCompiler();
    return !parser.hadError;
}