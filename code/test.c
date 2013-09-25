#include <stdio.h>

#define MAXLEN 32

int main(int argc, const char *argv[])
{
	struct xyz{
		float x;
		float y;
		float z;
	};

	struct velocity {
		struct xyz direction;
		float speed;	
	};

	struct obj {
		char name[MAXLEN];
		long mass;
		struct xyz pos;
		struct velocity v; 
	};
	return 0;
}
