#include <stdio.h>

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
 * Main workhorse for the VM.  Executes the code stored within it.
 * @return The status code returned as part of execution.
 */
static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define BINARY_OP(op)     \
    do {                  \
        double b = pop(); \
        double a = pop(); \
        push(a op b);     \
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
            case OP_ADD:      BINARY_OP(+); break;
            case OP_SUBTRACT: BINARY_OP(-); break;
            case OP_MULTIPLY: BINARY_OP(*); break;
            case OP_DIVIDE:   BINARY_OP(/); break;
            case OP_NEGATE:   push(-pop()); break;
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_RETURN:
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
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
    compile(source);
    return INTERPRET_OK;
}
