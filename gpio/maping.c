#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/types.h>

#include "maping.h"

int mem_fd;
void *map;

uint32_t base = 0;
uint32_t size = 0x40960;

int map_pheriferal ()
{
  if ((mem_fd = open ("/dev/gpiomem", O_RDWR | O_SYNC) ) < 0) {
    return -1;
  }

  map = mmap
    ((void *)GPIO_BASE,
    size,
    PROT_READ|PROT_WRITE,
    MAP_SHARED,
    mem_fd,
    base);

  if (map == MAP_FAILED) {
    return -2;
  }

  return 0;
}
