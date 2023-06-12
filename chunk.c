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
}

/**
 * Deallocate a block of memory that has previously been allocated
 * @param chunk The chunk to deallocate
 */
void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    initChunk(chunk);
}

/**
 * Append a byte into an allocated array.  Extend the array if necessary
 * @param chunk The array to write to
 * @param byte The data to write
 */
void writeChunk(Chunk* chunk, uint8_t byte) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->count++;
}