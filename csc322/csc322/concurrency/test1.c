#include <stdio.h>
#include <unistd.h>

int main() 
{
  int x = 0;

  while (fork() != 0)
  {
    printf("x=%d\n", ++x);
  }
}
