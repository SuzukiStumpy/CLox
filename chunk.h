#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H

#include "common.h"

// Definition of the opcodes supported by the interpreter
typedef enum {
    OP_RETURN,
} OpCode;

// Memory for code storage within the VM
typedef struct {
    int count;          // Number of bytes in use
    int capacity;       // Number of bytes allocated
    uint8_t* code;      // Storage (bytes) for code
} Chunk;


void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte);

#endif //CLOX_CHUNK_H
