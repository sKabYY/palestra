#include <stdlib.h>
#include <stdio.h>

#ifdef __APPLE__ /* MacOS */
#define SCHEME_ENTRY scheme_entry
#else
#define SCHEME_ENTRY _scheme_entry
#endif

extern long SCHEME_ENTRY(void *stack_addr, void *heap_addr);

void
print(long x) {
  printf("%ld\n", x);
}

int
main(int argc, char *argv[]) {
  if (argc != 1) {
    fprintf(stderr, "usage: %s\n", argv[0]);
    exit(1);
  }

  void *stack_addr = malloc(1024 * 8);
  void *heap_addr = malloc(1024 * 8);

  print(scheme_entry(stack_addr, heap_addr));

  free(stack_addr);
  free(heap_addr);

  return 0;
}
