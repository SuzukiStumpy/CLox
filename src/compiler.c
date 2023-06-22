#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

typedef struct {
    Token name;
    int depth;
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
    TYPE_METHOD,
    TYPE_INITIALIZER,
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
} ClassCompiler;

Parser parser;
Compiler* current = NULL;
ClassCompiler* currentClass = NULL;

/**
 * Return a pointer to the current chunk being compiled
 * @return Pointer to the current chunk
 */
static Chunk* currentChunk() {
    return &current->function->chunk;
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
 * emits a loop start operation
 * @param loopStart The offset at which the jump needs to return to
 */
static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

/**
 * Emits a Jump instruction
 * @param instruction the instruction opcode to emit
 * @return pointer to the instruction so we can backpatch the actual jump later
 */
static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

/**
 * Emits a return opcode into the byte stream
 */
static void emitReturn() {
    if (current->type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }

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
 * Completes a compiled jump instruction by actually inserting the address to jump to
 * @param offset The address in memory that we need to patch with the current IP
 */
static void patchJump(int offset) {
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (jump >> 8) &0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

/**
 * Initialises the compiler structure which keeps track of local variables and lexical scope
 * @param compiler The compiler structure to initialise
 * @param type The type of script we're compiling [whether main body or function call]
 */
static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start, parser.previous.length);
    }

    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;

    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

/**
 * Closes out compilation and shuts down the compiler
 */
static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;

#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
    }
#endif

    current = current->enclosing;
    return function;
}

/**
 * Begins a new lexical scope
 */
static void beginScope() {
    current->scopeDepth++;
}

/**
 * Ends a lexical scope
 */
static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
            current->locals[current->localCount - 1].depth > current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        current->localCount--;
    }
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
 * Checks two tokens for name equality
 * @param a The first token to check
 * @param b The second token to check
 * @return true if a == b, false otherwise
 */
static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

/**
 * Resolves a local variable from the stack
 * @param compiler Pointer to the locals lookup array
 * @param name Variable to look up
 * @return The offset to the resolved variable
 */
static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

/**
 * Generates an upvalue and adds it to the current compiler scope
 * @param compiler The current scope to add the upvalue to
 * @param index Index to the value
 * @param isLocal Flag indicating whether it is a global or local variable
 * @return Index of the generated upvalue on the stack
 */
static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

/**
 * Resolves an upvalue (captured variable by a closure)
 * @param compiler Pointer to the current compiler structure
 * @param name name of the variable to capture
 * @return index to the variable's current value on the stack
 */
static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

/**
 * Adds local variables to the locals structure
 * @param name The variable to add
 */
static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}

/**
 * Declaration of local variables
 */
static void declareVariable() {
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;

    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

/**
 * Parses out a variable name from the source token stream
 * @param errorMessage
 * @return
 */
static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

/**
 * Marks a local variable as ready for assignment
 */
static void markInitialized() {
    if (current->scopeDepth == 0) return;

    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

/**
 * Emits the bytecode for defining a variable
 * @param global pointer to the global constant table containing the variable
 */
static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

/**
 * Compiles a function's arguments and returns the number of arguments supplied
 * @return The number of arguments to the function
 */
static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();

            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

/**
 * Emits the bytecode for a logical and
 * @param canAssign
 */
static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
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
 * Compile a function call
 */
static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

/**
 * Compile support for the dot operator in class field assignments
 * @param canAssign used to determine whether we are setting a property or getting one
 */
static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
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
 * Compile logical or statement
 * @param canAssign
 */
static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

/**
 * Compiles string literals
 */
static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);

    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, arg);
    } else {
        emitBytes(getOp, arg);
    }
}

/**
 * Compiles variables
 */
static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

/**
 * Compiler support for this reference in classes
 * @param canAssign
 */
static void this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    variable(false);
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
        [TOKEN_LEFT_PAREN]      = {grouping,    call,       PREC_CALL},
        [TOKEN_RIGHT_PAREN]     = {NULL,        NULL,       PREC_NONE},
        [TOKEN_LEFT_BRACE]      = {NULL,        NULL,       PREC_NONE},
        [TOKEN_RIGHT_BRACE]     = {NULL,        NULL,       PREC_NONE},
        [TOKEN_COMMA]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_DOT]             = {NULL,        dot,        PREC_CALL},
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
        [TOKEN_AND]             = {NULL,        and_,       PREC_AND},
        [TOKEN_CLASS]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_ELSE]            = {NULL,        NULL,       PREC_NONE},
        [TOKEN_FALSE]           = {literal,     NULL,       PREC_NONE},
        [TOKEN_FOR]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_FUN]             = {NULL,        NULL,       PREC_NONE},
        [TOKEN_IF]              = {NULL,        NULL,       PREC_NONE},
        [TOKEN_NIL]             = {literal,     NULL,       PREC_NONE},
        [TOKEN_OR]              = {NULL,        or_,        PREC_OR},
        [TOKEN_PRINT]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_RETURN]          = {NULL,        NULL,       PREC_NONE},
        [TOKEN_SUPER]           = {NULL,        NULL,       PREC_NONE},
        [TOKEN_THIS]            = {this_,       NULL,       PREC_NONE},
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
 * Compiles a block of statements
 */
static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

/**
 * Compile a function block
 * @param type type of the function to build [top level or user function]
 */
static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");

    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));
    //emitBytes(OP_CONSTANT, makeConstant(OBJ_VAL(function)));
    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

/**
 * Compile a method into a class definition
 */
static void method() {
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = identifierConstant(&parser.previous);

    FunctionType type = TYPE_METHOD;
    if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }
    function(type);
    emitBytes(OP_METHOD, constant);
}

/**
 * Compile a class
 */
static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;
    uint8_t nameConstant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    ClassCompiler classCompiler;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    namedVariable(className, false);

    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OP_POP);

    currentClass = currentClass->enclosing;
}

/**
 * Compile a function declaration
 */
static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
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
 * Compiles a for loop and emits the requisite bytecode
 */
static void forStatement() {
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    if (match(TOKEN_SEMICOLON)) {
        // No initialiser
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;

    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
    }

    if (!match(TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement();
    emitLoop(loopStart);

    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP);
    }

    endScope();
}

/**
 * Compiles an if statement
 */
static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    statement();

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP);

    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
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
 * Compiles a return statement and emits the requisite bytecode
 */
static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer.");
        }

        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

/**
 * Compiles a while statement and emits the requisite bytecode
 */
static void whileStatement() {
    int loopStart = currentChunk()->count;

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
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
    if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else if (match(TOKEN_CLASS)) {
        classDeclaration();
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
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
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
 * @return Pointer to the current chunk/function
 */
ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}

/**
 * Part of the GC implementation.  Marks any heap allocated objects that are reachable during the compilation
 * phase.
 */
void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}