#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char *byte_pointer;

void show_bytes(byte_pointer a, int len) 
{
	int i;
	for(i=0; i<len; i++) 
	{
		printf("%.2x ", a[i]);
	}
	printf("\n");
}

int main()
{
	unsigned int x = 2;
	show_bytes((unsigned char *) &x, sizeof(unsigned int));
	return 0;
}
