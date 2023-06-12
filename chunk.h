#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H

#include "common.h"
#include "value.h"

// Definition of the opcodes supported by the interpreter
typedef enum {
    OP_CONSTANT,
    OP_RETURN,
} OpCode;

// Memory for code storage within the VM
typedef struct {
    int count;            // Number of bytes in use
    int capacity;         // Number of bytes allocated
    uint8_t* code;        // Storage (bytes) for code
    int* lines;           // Storage for source code line numbers
    ValueArray constants; // Storage for constant values
} Chunk;


void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif //CLOX_CHUNK_H
