#pragma once
#include"stdint.h"

typedef struct{
    uint8_t id;
    uint16_t cyclinders;
    uint16_t sectors;
    uint16_t heads;
} DISK;

bool DISK_Initialize(DISK* disk, uint8_t driverNumber);
bool DISK_ReadSectors(DISK* disk, uint32_t lba, uint8_t sectors, void far* dataOut);