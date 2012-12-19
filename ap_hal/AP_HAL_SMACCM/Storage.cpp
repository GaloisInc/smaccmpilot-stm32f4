
#include <string.h>
#include "Storage.h"

using namespace SMACCM;

SMACCMStorage::SMACCMStorage()
{}

void SMACCMStorage::init(void*)
{}

uint8_t SMACCMStorage::read_byte(uint16_t loc){
    return 0;
}

uint16_t SMACCMStorage::read_word(uint16_t loc){
    return 0;
}

uint32_t SMACCMStorage::read_dword(uint16_t loc){
    return 0;
}

void SMACCMStorage::read_block(void* dst, uint16_t src, size_t n) {
    memset(dst, 0, n);
}

void SMACCMStorage::write_byte(uint16_t loc, uint8_t value)
{}

void SMACCMStorage::write_word(uint16_t loc, uint16_t value)
{}

void SMACCMStorage::write_dword(uint16_t loc, uint32_t value)
{}

void SMACCMStorage::write_block(uint16_t loc, void* src, size_t n)
{}

