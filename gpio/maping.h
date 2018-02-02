
#include <stdint.h>

#define BCM2837_PERI_BASE   0x3F000000
#define GPIO_BASE          (BCM2837_PERI_BASE + 0x200000)
#define PAGE_SIZE          (4*1024)

extern uint32_t *base;
extern uint32_t size;

int map_pheriferal ();
