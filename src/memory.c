#include <stdlib.h>

#include "memory.h"
#include "vm.h"

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

/**
 * Frees up memory allocated to heap objects
 * @param object The object to free up.
 */
static void freeObject(Obj* object) {
    switch (object->type) {
        case OBJ_STRING: {
            ObjString* string = (ObjString*)object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }
        case OBJ_NATIVE: {
            FREE(ObjNative, object);
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_UPVALUE: {
            FREE(ObjUpvalue, object);
            break;
        }
    }
}

/**
 * Walks the list of objects in memory and frees them
 */
void freeObjects() {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }
}