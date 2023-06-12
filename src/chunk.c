#include <stdlib.h>
#include "chunk.h"
#include "memory.h"

/**
 * Initialise a previously allocated chunk object
 * @param chunk the allocated region of memory to initialise.
 */
void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->constants);
}

/**
 * Deallocate a block of memory that has previously been allocated
 * @param chunk The chunk to deallocate
 */
void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

/**
 * Append a byte into an allocated array.  Extend the array if necessary
 * @param chunk The array to write to
 * @param byte The data to write
 * @param line the source code line number for this particular byte
 */
void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

/**
 * Adds a new constant into the memory pool for a chunk
 * @param chunk The chunk to add the constant to
 * @param value The constant value to add
 * @return offset to the newly added constant
 */
int addConstant(Chunk* chunk, Value value) {
    writeValueArray(&chunk->constants, value);
    return chunk->constants.count - 1;
}