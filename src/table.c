#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

/**
 * Initialises a new hash table
 * @param table The table structure to initialise.
 */
void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

/**
 * Releases the memory occupied by an existing hash table and resets internal state for the base object
 * @param table The table to erase.
 */
void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

/**
 * Looks up an entry in a hash table given the key to find
 * @param entries The entries in the hash table
 * @param capacity The size of the hash table
 * @param key The key to find
 * @return The entry with the corresponding key if it exists, NULL otherwise.
 */
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    //uint32_t index = key->hash % capacity;
    uint32_t index = key->hash & (capacity - 1);  // Optimisation for speed
    Entry* tombstone = NULL;

    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                // Empty entry
                return tombstone != NULL ? tombstone : entry;
            } else {
                // We found a tombstone
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) {
            // We found the key
            return entry;
        }

        //index = (index + 1) % capacity;
        index = (index - 1) & (capacity - 1);
    }
}

/**
 * Looks up a value from a hash table.  Sets the value parameter to the retrieved value if found and returns true.
 * If the value is not found, returns false
 * @param table The table to perform the lookup within
 * @param key The key to retrieve
 * @param value The value found in the table (if it exists)
 * @return true if the value was found, false otherwise.
 */
bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    *value = entry->value;
    return true;
}

/**
 * Allocates a fresh hash table with _capacity_ entries and initialises these to nil, then copies in
 * entries from the existing hash table (if any) before releasing the old memory
 * @param table The table to allocate
 * @param capacity The size of the table required
 */
static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    table->count = 0;

    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL) continue;
        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);

    table->entries = entries;
    table->capacity = capacity;
}

/**
 * Adds (or overwrites) a value in a hash table
 * @param table The table to add to
 * @param key The key to use in the table
 * @param value The value to write
 * @return true if the new entry was successfully added to the table, false otherwise.
 */
bool tableSet(Table* table, ObjString* key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

/**
 * Deletes an entry from a hash table
 * @param table The table to work with
 * @param key The key for the entry to delete
 * @return True if the entry was deleted, false otherwise
 */
bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) return false;

    // Find the entry
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    // Place a tombstone in the entry
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

/**
 * Copies entries from one table and inserts them into another
 * @param from The table to copy entries from
 * @param to The table to copy entries to
 */
void tableAddAll(Table* from, Table* to) {
    for (int i = 0; i < from->capacity; i++) {
        Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

/**
 * Looks up a string from the provided hash table and returns a reference to it.  Allows us to handle the
 * interning of strings within the VM
 * @param table The hash table to look up from
 * @param chars The sequence of characters to find
 * @param length The length of the sequence of characters
 * @param hash The hash code of the string
 * @return Reference to the string
 */
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->count == 0) return NULL;

    //uint32_t index = hash % table->capacity;
    uint32_t index = hash & (table->capacity - 1);

    for (;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            // Stop if we find an empty non-tombstone entry.
            if (IS_NIL(entry->value)) return NULL;
        } else if (entry->key->length == length &&
                    entry->key->hash == hash &&
                    memcmp(entry->key->chars, chars, length) == 0) {
            // We found it
            return entry->key;
        }

        //index = (index + 1) % table->capacity;
        index = (index + 1) & (table->capacity - 1);
    }
}

/**
 * Removes obsolete/unreachable entries from the given table
 * @param table The table to traverse
 */
void tableRemoveWhite(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key != NULL && !entry->key->obj.isMarked) {
            tableDelete(table, entry->key);
        }
    }
}

/**
 * Marks entries in the given table as reachable by the GC
 * @param table The table to mark
 */
void markTable(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        markObject((Obj*)entry->key);
        markValue(entry->value);
    }
}