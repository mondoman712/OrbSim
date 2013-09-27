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
		long double mass;
		struct xyz pos;
		struct velocity v; 
	};

	struct obj sun = {"Sun", 1.9891e30, 0, 0, 0, 0, 0, 0, 0};

	printf("%s\n", sun.name);
	printf("%g\n", sun.mass);
	printf("%d\n", sun.pos.x);

	return 0;
}
