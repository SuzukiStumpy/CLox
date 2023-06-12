#include <stdlib.h>
#include "memory.h"

/**
 * Performs all memory allocation for the CLOX VM.
 * @param pointer The block of memory on which to operate
 * @param oldSize The original size of the memory block
 * @param newSize The target size of the memory block
 */
void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    // If newSize is zero, then deallocate memory
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if (result == NULL) exit(1);    // Abort if allocation failed
    return result;
}