//
// syscalls.c -- Low level memory and I/O functions for newlib 
//
//  Nov, 2012
//  <andy@payne.org>
//

#include <sys/stat.h>
#include <stddef.h>
#include "uart.h"
char (*rx)(void) = NULL;
void (*tx)(char) = NULL;

void syscalls_set_io(char (*r)(void), void (*t)(char))
{
  rx = r;
  tx = t;
}

int _close(int file) { return -1; }
int _isatty(int file) { return 1; }
int _open(const char *name, int flags, int mode) { return -1; }

int _fstat(int file, struct stat *st) 
{
  st->st_mode = S_IFCHR;                  // Character device
  return 0;
}

int _write(int file, char *p, int len)
{
  int i;
  switch(file)
  {
  case 1:
  case 2:
    if (tx == NULL)
    {
      return len;
    }
    for (i = 0; i < len; ++i) tx(p[i]);
    return len;
  default:       return -1;
  }
}

int _read(int file, char *p, int len)
{
  int i;
  for (i = 0; i < len; ++i)
  {
    p[i] = rx();
  }
  return len;
}

// ------------------------------------------------------------------------------------
// _sbrk(len) -- Allocate space on the heap
// TODO(henrik) Does this work?
extern unsigned int __heap_start[];
static char *heap_end = (char *) __heap_start;

char *_sbrk(int incr)
{
  heap_end += incr;                   // TODO:  check for collisions with the stack
  return (heap_end - incr);
}

// Signal handler (fault)
int _kill(int pid, int sig)
{
  while(1);
  return -1;                          // Never gets here
}

off_t _lseek(int fd, off_t offset, int whence) { return 0; }
