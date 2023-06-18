#include <stdio.h>
#include <stdlib.h>

#include "compiler.h"
#include "common.h"
#include "scanner.h"

// Must come _after_ common.h include
#ifdef DEBUG_PRINT_CODE
#include "debug.h"
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

typedef void (*ParseFn)(bool canAssign);

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
 * Returns true if the current token matches the required type.  False otherwise
 * @param type The type to check
 * @return true if type matches, false otherwise
 */
static bool check(TokenType type) {
    return parser.current.type == type;
}

/**
 * If the current token matches the required type, consume it and return true.  Otherwise, do nothing
 * and return false
 * @param type the type of the token we want to match
 * @return true if current token matches type, false otherwise
 */
static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
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
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

/**
 * Takes a token and adds it's lexeme to the global contants table
 * @param name The token to use
 * @return pointer to the generated constant entry.
 */
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

/**
 * Parses out a variable name from the source token stream
 * @param errorMessage
 * @return
 */
static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);
    return identifierConstant(&parser.previous);
}

/**
 * Emits the bytecode for defining a variable
 * @param global pointer to the global constant table containing the variable
 */
static void defineVariable(uint8_t global) {
    emitBytes(OP_DEFINE_GLOBAL, global);
}

/**
 * Compile binary operations (+ - / *, etc)
 */
static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_PLUS:            emitByte(OP_ADD); break;
        case TOKEN_MINUS:           emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:            emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:           emitByte(OP_DIVIDE); break;
        case TOKEN_BANG_EQUAL:      emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:     emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:         emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL:   emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:            emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:      emitBytes(OP_GREATER, OP_NOT); break;
        default:
            return; // Unreachable
    }
}

/**
 * Compile literals for inbuilt data types
 */
static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        default: return;
    }
}

/**
 * Compile parenthesised expressions
 */
static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/**
 * Compiles number literals
 */
static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

/**
 * Compiles string literals
 */
static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t arg = identifierConstant(&name);
    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_GLOBAL, arg);
    } else {
        emitBytes(OP_GET_GLOBAL, arg);
    }
}

/**
 * Compiles variables
 */
static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

/**
 * Compiles unary operations
 */
static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand
    parsePrecedence(PREC_UNARY);

    // emit the operator instruction
    switch (operatorType) {
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        case TOKEN_BANG:  emitByte(OP_NOT); break;
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
        [TOKEN_BANG]            = {unary,       NULL,       PREC_NONE},
        [TOKEN_BANG_EQUAL]      = {NULL,        binary,     PREC_EQUALITY},
        [TOKEN_EQUAL]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_EQUAL_EQUAL]     = {NULL,        binary,     PREC_EQUALITY},
        [TOKEN_GREATER]         = {NULL,        binary,     PREC_COMPARISON},
        [TOKEN_GREATER_EQUAL]   = {NULL,        binary,     PREC_COMPARISON},
        [TOKEN_LESS]            = {NULL,        binary,     PREC_COMPARISON},
        [TOKEN_LESS_EQUAL]      = {NULL,        binary,     PREC_COMPARISON},
        [TOKEN_IDENTIFIER]      = {variable,    NULL,       PREC_NONE},
        [TOKEN_STRING]          = {string,      NULL,       PREC_NONE},
        [TOKEN_NUMBER]          = {number,      NULL,       PREC_NONE},
        [TOKEN_AND]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_CLASS]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_ELSE]            = {NULL,        NULL,       PREC_NONE},
        [TOKEN_FALSE]           = {literal,     NULL,       PREC_NONE},
        [TOKEN_FOR]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_FUN]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_IF]              = {NULL,        NULL,       PREC_NONE},
        [TOKEN_NIL]             = {literal,     NULL,       PREC_NONE},
        [TOKEN_OR]              = {NULL,        NULL,       PREC_NONE},
        [TOKEN_PRINT]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_RETURN]          = {NULL,        NULL,       PREC_NONE},
        [TOKEN_SUPER]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_THIS]            = {NULL,        NULL,       PREC_NONE},
        [TOKEN_TRUE]            = {literal,     NULL,       PREC_NONE},
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
 * Compiles a variable declaration and emits the requisite bytecode
 */
static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

/**
 * Compiles an expression statement and emits the requisite bytecode
 */
static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

/**
 * Compiles a print statement and emits the requisite bytecode
 */
static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

/**
 * If the parser panics due to an error in the user's program, we attempt to resynchronize and continue
 */
static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS: // Fall through is intentional
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            default:; // Do nothing
        }

        advance();
    }
}

/**
 * Compile a lox declaration and emit the requisite bytecode
 */
static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

/**
 * Compile a lox statement and emit the requisite bytecode
 */
static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else {
        expressionStatement();
    }
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

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
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

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();
    return !parser.hadError;
}