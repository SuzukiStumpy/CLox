#include <stdio.h>
#include <stdarg.h>

#include "common.h"
#include "debug.h"
#include "compiler/compiler.h"
#include "vm.h"

VM vm;

/**
 * Initialises the stack within the VM
 */
static void resetStack() {
    vm.stackTop = vm.stack;
}

/**
 * Reports a runtime error and resets the VM
 * @param format The format of the message to print
 * @param ... The arguments to the format string
 */
static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    size_t instruction = vm.ip - vm.chunk->code - 1;
    int line = vm.chunk->lines[instruction];
    fprintf(stderr, "{line %d} in script\n", line);
    resetStack();
}

/**
 * Initialises the Virtual Machine
 */
void initVM() {
    resetStack();
}

/**
 * Tears down the Virtual Machine
 */
void freeVM() {

}

/**
 * Push a value onto the VM's stack
 * @param value The value to push
 */
void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

/**
 * Pops the topmost value off the VM's stack
 * @return The value popped.
 */
Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

/**
 * Returns the item currently 'distance' positions from the top of the stack (without removing it)
 * @param distance How far down the stack to look
 * @return The value at the position stackTop - (distance+1)
 */
static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
}

/**
 * Determines the truthiness / falsiness of a value.
 * @param value the value to test
 * @return true if the value is falsey, false otherwise.
 */
static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

/**
 * Main workhorse for the VM.  Executes the code stored within it.
 * @return The status code returned as part of execution.
 */
static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define BINARY_OP(valueType, op)                              \
    do {                                                      \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {     \
            runtimeError("Operands must be numbers.");        \
            return INTERPRET_RUNTIME_ERROR;                   \
        }                                                     \
        double b = AS_NUMBER(pop());                          \
        double a = AS_NUMBER(pop());                          \
        push(valueType(a op b));                              \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_ADD:      BINARY_OP(NUMBER_VAL, +); break;
            case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
            case OP_NEGATE:
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(NUMBER_VAL(-AS_NUMBER(pop())));
                break;
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_NIL: push(NIL_VAL); break;
            case OP_FALSE: push(BOOL_VAL(false)); break;
            case OP_TRUE: push(BOOL_VAL(true)); break;
            case OP_NOT: push(BOOL_VAL(isFalsey(pop()))); break;
            case OP_RETURN:
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            case OP_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GREATER:    BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:       BINARY_OP(BOOL_VAL, <); break;
        }
    }
#undef BINARY_OP
#undef READ_CONSTANT
#undef READ_BYTE
}


/**
 * Interprets code stored in the chunk
 * @param chunk The memory chunk to execute
 * @return The status result of execution
 */
InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);

    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;

    InterpretResult result = run();

    freeChunk(&chunk);
    return result;
}
