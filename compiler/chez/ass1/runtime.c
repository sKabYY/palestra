#include <stdlib.h>
#include <stdio.h>

#ifdef __APPLE__ /* MacOS */
#define SCHEME_ENTRY scheme_entry
#else
#define SCHEME_ENTRY _scheme_entry
#endif

extern long scheme_entry(void);

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

  print(scheme_entry());
  return 0;
}
