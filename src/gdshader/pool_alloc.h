
#ifndef POOLALLOC_H
#define POOLALLOC_H

#include <vector>
#include <map>
#include <memory>

namespace gdshader_lsp {

/**
 * Pools operate on chunks and buckets. These pool allocator are O(1) which makes them super efficient for fast lookup, allocation and dealltocation especially with some fine grained like a borrower-concept.
 * We could use an Aren alloc too, but this thing is i guess not fast enough / too simple.
 */

/**
 * @brief Chunks only need to store a reference to the next chunk object, as their size is predefined and we dont really care. 
 * When a chunk is free, the 'next' contains the address of the next chunk in the list. When it's allocated, this space is used by the user.
 * 
 * @note Chunks are grouped in 'Blocks'; however these blocks are not direct data structs but more of an abstract concept.
 */
struct Chunk {
    Chunk *next;
};


class Pool {

public:

    Pool(unsigned int chunkSize, unsigned int chunksPerBlock);
    ~Pool();

    void* allocate();
    void deallocate(void* ptr);

private:
    unsigned int chunkSize;
    unsigned int chunksPerBlock;

    Chunk* freeList = nullptr;

    std::vector<void*> allocatedBlocks;

    void allocateBlock();

};

class PoolAllocator {

public:

    static PoolAllocator& getInstance() {
        static PoolAllocator instance;
        return instance;
    }

    void* allocate(unsigned int size);
    void deallocate(void *ptr, unsigned int size);

private:

    PoolAllocator() {};
    std::map<unsigned int, std::unique_ptr<Pool>> pools;

public:

    PoolAllocator(PoolAllocator const&) = delete;
    void operator=(PoolAllocator const&) = delete;

};

}

#endif