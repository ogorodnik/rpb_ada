#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/types.h>

#include "maping.h"

int mem_fd;
void *map;

uint32_t *base = (uint32_t *)BCM2837_PERI_BASE;
uint32_t size = 0x01000000;

int map_pheriferal ()
{
  FILE *fp;

  if ((fp = fopen("/proc/device-tree/soc/ranges", "rb"))) {
    unsigned char buf[4];
    fseek (fp, 4, SEEK_SET);
    if (fread(buf, 1, sizeof(buf), fp) == sizeof(buf))
      base = (uint32_t*)(buf[0] << 24 | buf[1] << 16 | buf[2] << 8 | buf[3] << 0);
    fseek (fp, 8, SEEK_SET);
    if (fread(buf, 1, sizeof(buf), fp) == sizeof(buf))
      size = (buf[0] << 24 | buf[1] << 16 | buf[2] << 8 | buf[3] << 0);
    fclose(fp);
  }

  if ((mem_fd = open ("/dev/gpiomem", O_RDWR | O_SYNC) ) < 0) {
    return -1;
  }

  base = 0;
  map = mmap
    ((void *)GPIO_BASE,
    size,
    PROT_READ|PROT_WRITE,
    MAP_SHARED,
    mem_fd,
    (uint32_t)base);

  if (map == MAP_FAILED) {
    return -2;
  }

  return 0;
}
