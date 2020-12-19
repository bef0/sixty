#include <stdlib.h>
#include <sys/mman.h>

char* init_stack() {
  size_t size = 16 * 1024 * 1024; // 16 MB
  char* address = mmap(0, size, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  if (address == MAP_FAILED) {
    exit(EXIT_FAILURE);
  }
  return address + size - sizeof(size_t);
}
