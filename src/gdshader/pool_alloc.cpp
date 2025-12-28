
#include "gdshader/pool_alloc.h"

using namespace gdshader_lsp;

Pool::Pool(unsigned int _chunkSize, unsigned int _chunks) : chunkSize(_chunkSize), chunksPerBlock(_chunks), freeList(nullptr) {

    allocateBlock();

}

Pool::~Pool() {
    for (void* block : allocatedBlocks) {
        free(block);
    }
}

void* Pool::allocate() {

    if (freeList == nullptr) {
        allocateBlock();
    }

    void* allocatedChunk = freeList;
    freeList = freeList->next;

    return allocatedChunk;

}

void Pool::deallocate(void* ptr) {
    Chunk* chunkToDeallocate = static_cast<Chunk*>(ptr);

    chunkToDeallocate->next = freeList;
    freeList = chunkToDeallocate;
}

void Pool::allocateBlock() {
        
    unsigned int blockSize = chunksPerBlock * chunkSize;
    
    // Use calloc to zero-initialize memory, which can be useful for debugging.
    Chunk* blockBegin = static_cast<Chunk*>(malloc(blockSize));
    if (blockBegin == nullptr) {
        return;
    }
    
    allocatedBlocks.push_back(blockBegin);
    
    // Chain the new chunks and add them to the free list.
    Chunk* chunk = blockBegin;
    for (unsigned int i = 0; i < chunksPerBlock - 1; ++i) {
        chunk->next = reinterpret_cast<Chunk*>(reinterpret_cast<char*>(chunk) + chunkSize);
        chunk = chunk->next;
    }
    
    // The last chunk's next pointer points to the old free list head.
    chunk->next = freeList;
    freeList = blockBegin; // The new block becomes the head of the free list.
}

/**
 * @brief Allocates a pool of chunks of defined size.
 */
void* PoolAllocator::allocate(unsigned int size) {

    // We are out of chunks
    // Allocate a new one

    // No existing pools
    if(pools.find(size) == pools.end()) {
        pools[size] = std::make_unique<Pool>(size, 1024); 
    }

    return pools[size]->allocate();

}

/**
 * @brief Simply puts the chunk at the fron of the chunk list.
 */

void PoolAllocator::deallocate(void *ptr, unsigned int size) {

    if(pools.find(size) != pools.end()) {
        pools[size]->deallocate(ptr);
    }
}