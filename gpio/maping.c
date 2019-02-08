#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "maping.h"

int mem_fd;
void *gpio;
void *all;

uint32_t base = 0;

int map_gpio ()
{
  if ((mem_fd = open ("/dev/gpiomem", O_RDWR | O_SYNC) ) < 0) {
    return -1;
  }

  gpio = mmap
    ((void *)GPIO_BASE,
    (uint32_t) 0x256,
    PROT_READ|PROT_WRITE,
    MAP_SHARED,
    mem_fd,
    base);

  if (mem_fd >= 0)
      close(mem_fd);

  if (gpio == MAP_FAILED) {
    return -2;
  }

  return 0;
}

void *map_all ()
{
  if ((mem_fd = open ("/dev/mem", O_RDWR | O_SYNC) ) < 0) {
    return NULL;
  }

  all = mmap
    (NULL,
    (size_t) 0x01000000,
    (PROT_READ|PROT_WRITE),
    MAP_SHARED,
    mem_fd,
    (off_t) BCM2837_PERI_BASE);

  if (mem_fd >= 0)
      close(mem_fd);

  if (all == MAP_FAILED) {
    return NULL;
  }

  return all;
}
