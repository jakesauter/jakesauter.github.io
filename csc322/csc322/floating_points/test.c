#include <stdio.h>
#include <stdlib.h>

void test()
{
	printf("test\n");
}

int main()
{
	unsigned x = 4;
	x = x >> 1;
	//~(~0<<k)<<j;// all 1s push k zeros, compliment push j 0s
	test();
	printf("%u\n", x);
	return 0;
}
