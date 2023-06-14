#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

/**
 * Actually allocates space for an object on the heap and returns a pointer to the Object
 * @param size size of the object
 * @param type type of the object
 * @return Pointer to the allocated object.
 */
static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*) reallocate(NULL, 0, size);
    object->type = type;
    object->next = vm.objects;
    vm.objects = object;

    return  object;
}

/**
 * Allocates memory on the heap for a string of _chars_ length, stuffs the details into an ObjString structure
 * and returns it
 * @param chars The string to allocate
 * @param length The length of the string
 * @return An ObjString structure pointing to the allocated string
 */
static ObjString* allocateString(char* chars, int length) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    return string;
}

/**
 * Takes a raw C string and wraps it in a Lox object
 * @param chars The string to wrap
 * @param length The length of the string
 * @return Pointer to the wrapped string
 */
ObjString* takeString(char* chars, int length) {
    return allocateString(chars, length);
}

/**
 * Copies the supplied character array to a new location in memory and returns a pointer to the new string
 * @param chars The array of characters to copy
 * @param length The number of characters to copy
 * @return Pointer to the new copy of the string
 */
ObjString* copyString(const char* chars, int length) {
    char* heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length);
}

/**
 * Helper function.  Prints the value of an Object to the console
 * @param value The Object to print
 */
void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
    }
}
