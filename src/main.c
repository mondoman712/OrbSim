#include <stdio.h>
#include <math.h>

const int grav = 6.673e-11;

typedef enum 
	{ NOTHING, SUN, MERCURY,
	       	VENUS, EARTH, MARS,
	       	JUPITER, SATURN, URANUS,
	       	NEPTUNE }
	Obj;

typedef struct {
	long mass;
	Obj orb;
} Body;


/* Calculates the velocity of an object in circular orbit */
float cvelocity(long mass, long radius)
{
	return sqrt( (grav * mass) / radius);
}


int main(int argc, const char *argv[])
{
	Body sun = { 1.989e30, NOTHING };
	
	printf("%f\n", cvelocity(sun.mass, 1.496e8));

	return 0;
}
