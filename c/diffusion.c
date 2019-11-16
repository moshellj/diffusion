#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* TESTING RESULTS
 * Without partition:
 * At Rdivs = 10, equilibriated in 51.438 seconds simulated time, 3.095 seconds real time.
 * At Rdivs = 500, completes a single step in 17.125 seconds.
 * (Compare that to python's 2m52s and ~15 minutes respectively. C can run!)
 * With partition:
 * At Rdivs = 10, equilibriated in 259.294 seconds simulated, 20.489 seconds user time.
 */

//NEAT LANGUAGE THING: Macros! This one gives an easy way to treat a 1d array
//as a 3d array (which is simpler and should be more efficient with memory).
#define ROOM(X, Y, Z) room[(X)*Rdivs*Rdivs + (Y)*Rdivs + (Z)]

const double Lroom = 5.0;	//Length of room edge in meters
const int Rdivs = 25;		//Number of divisions in room
const double Urms = 250.0;
const double D = 0.175;
const double Rdist = Lroom/(double)Rdivs;
const double Tstep = Lroom/Urms/(double)Rdivs;
const double Dterm = D * Tstep /Rdist /Rdist;
const int volelCount = Rdivs*Rdivs*Rdivs;

int partitionOn = 1;
const int Px = Rdivs/2;
const int Py = (3*Rdivs)/4;

int inbounds(int dx, int dy, int dz);

//Determines the ratio of the least concentrated volel to the greatest.
//Room can't be treated as 1d here because inbounds requires coordinates.
double minMaxRatio(double* room){
	double min = INFINITY;//INFINITY is a macro defined by gcc
	double max = 0.0;
	for(int x = 0; x < Rdivs; ++x){
		for(int y = 0; y < Rdivs; ++y){
			for(int z = 0; z < Rdivs; ++z){
				if(!partitionOn || inbounds(x, y, z)){
					if(ROOM(x, y, z) > max){
						max = ROOM(x, y, z);
					}
					if(ROOM(x, y, z) < min){
						min = ROOM(x, y, z);
					}
				}
			}
		}
	}
	return min/max;
}

int inbounds(int dx, int dy, int dz){
	if( dx >= Rdivs || dx < 0 ) return 0;
    if( dy >= Rdivs || dy < 0 ) return 0;
    if( dz >= Rdivs || dz < 0 ) return 0;
	//partition
	if(partitionOn && dx == Px && dy <= Py) return 0;
	return 1;
}

//Performs diffusion between two volels.
//room = room
//x, y, z = coordinates of originating volel.
//dx, dy, dz = offset of the destination volel from the origin.
//These are transformed into the destination coordinates.
//	(function verifies to see if it's a valid location first.)
void diffuse(double* room, int x, int y, int z, int dx, int dy, int dz){
	//get new coordinates
	dx = x+dx;
	dy = y+dy;
	dz = z+dz;
	//bounds checking
	if(inbounds(x, y, z) + inbounds(dx, dy, dz) < 2){
		return;
	}
	//diffusion
	double change = (ROOM(x, y, z) - ROOM(dx, dy, dz))*Dterm;
	//printf("C= %f\n", change);
	ROOM(x, y, z) = ROOM(x, y, z) - change;
	ROOM(dx, dy, dz) = ROOM(dx, dy, dz) + change;
	return;
}

int main(void){
	double* room = calloc(volelCount, sizeof(double));
	ROOM(0, 0, 0) = 1e21;
	double Ttotal = 0.0;
	while(minMaxRatio(room) < 0.99){
		//for each volel in the room
		for(int x = 0; x < Rdivs; ++x){
		for(int y = 0; y < Rdivs; ++y){
		for(int z = 0; z < Rdivs; ++z){
			//diffuse to each surrounding volel
			diffuse(room, x, y, z, -1,  0,  0);
			diffuse(room, x, y, z, +1,  0,  0);
			diffuse(room, x, y, z,  0, -1,  0);
			diffuse(room, x, y, z,  0, +1,  0);
			diffuse(room, x, y, z,  0,  0, -1);
			diffuse(room, x, y, z,  0,  0, +1);
		}
		}
		}
		Ttotal += Tstep;
		//printf("%.5f\t%.8f\n", Ttotal, minMaxRatio(room));
	}
	printf("%.5f \t%.8f\n", Ttotal, minMaxRatio(room));
	free(room);
	return 0;
}
