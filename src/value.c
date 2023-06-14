#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "value.h"
#include "object.h"

/**
 * Initialise a structure for storing constant values
 * @param array The structure to initialise
 */
void initValueArray(ValueArray* array) {
    array->count = 0;
    array->capacity = 0;
    array->values = NULL;
}

/**
 * Write a value into a ValueArray structure
 * @param array The array to write to
 * @param value The value to write
 */
void writeValueArray(ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

/**
 * Releases memory assigned to a ValueArray and leaves the structure in a sensible state.
 * @param array The array structure to reset.
 */
void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

/**
 * Prints a value to the screen
 * @param value The value to print
 */
void printValue(Value value) {
    switch (value.type) {
        case VAL_BOOL:      printf(AS_BOOL(value) ? "true" : "false"); break;
        case VAL_NIL:       printf("nil"); break;
        case VAL_NUMBER:    printf("%g", AS_NUMBER(value)); break;
        case VAL_OBJ:       printObject(value); break;
    }
}

/**
 * Compares any two Values (of any type) and returns true if they are equal
 * @param a the first Value
 * @param b the second Value
 * @return true if a and b are equal/equivalent, false otherwise.
 */
bool valuesEqual(Value a, Value b) {
    if (a.type != b.type) return false;
    switch (a.type) {
        case VAL_BOOL:      return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL:       return true;
        case VAL_NUMBER:    return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_OBJ:       return AS_OBJ(a) == AS_OBJ(b);
        default:            return false; // Unreachable
    }
}