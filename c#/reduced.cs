using System;
using System.Collections.Generic;
using System.Linq;

/* TESTING RESULTS:
 * Without partition:
 * At room.divs = 10, equilibriated in 51.438 seconds simulated time, 1m0.482s real time.
 * At room.divs = 500, completed 1 step in 4m48s, which included a very long pause before and
 * after the simulation loop, likely due to memory management?
 */

class Diffusion{
	static void Main(string[] args){
		// initialize
		// NEAT LANGUAGE THING: Named arguments! Helps other people understand your code.
		Room room = new Room(divs: 10, length: 5.0, partitionOn: false);
		Gas gas = new Gas(Urms: 250.0, D: 0.175);
		double Tstep = room.length / gas.rms / (double)(room.divs);
		double Dterm = gas.D * Tstep / room.cellDist / room.cellDist;
		
		//room.cell[0, 0, 0] = 1e21;
		// Alternate initial conditions:
		for(int x = 0; x < room.divs; ++x){
        for(int y = 0; y < room.divs; ++y){
        for(int z = 0; z < room.divs; ++z){
			room.cell[x, y, z] = 1e20;
		}
		}
		}
		room.cell[0, 0, 0] = 1.5e20;
		
		double Ttotal = 0.0;
		Coord here;
		double concentrationRatio = minMaxRatio(room);
		
		while(concentrationRatio < 0.99){
			double minConc = 1e100;
			double maxConc = 0.0;
			for(int x = 0; x < room.divs; ++x){
			for(int y = 0; y < room.divs; ++y){
			for(int z = 0; z < room.divs; ++z){//for each cell
				here = new Coord(x, y, z);
				if(room.inBounds(here)){
					List<Coord> nbrs = room.neighbors(here);
					foreach(Coord nbr in nbrs){//neigbors are already in bounds
						double change = (room.get(here) - room.get(nbr)) * Dterm;
						room.set(here, room.get(here) - change);
						room.set(nbr,  room.get(nbr)  + change);
					}
					if(room.get(here) > maxConc) maxConc = room.get(here);
					if(room.get(here) < minConc) minConc = room.get(here);
				}
			}
			}
			}
			Ttotal += Tstep;
			concentrationRatio = minConc/maxConc;
			Console.WriteLine("{0:0.000000} \t{1:0.00000000} \t{2:0.00000000}", Ttotal, minMaxRatio(room), concentrationRatio);
		}
		Console.WriteLine("{0:0.000000} \t{1:0.00000000}", Ttotal, minMaxRatio(room));
	}
	//Calculates the concentration ratio
	//This used to be really cool, but bounds checking means it can't be.
	static double minMaxRatio(Room room){
		double min = 1e100;
		double max = 0.0;
		for(int x = 0; x < room.divs; ++x){
		for(int y = 0; y < room.divs; ++y){
		for(int z = 0; z < room.divs; ++z){
			if(room.inBounds(new Coord(x, y, z))){
				if(room.cell[x, y, z] > max) max = room.cell[x, y, z];
				if(room.cell[x, y, z] < min) min = room.cell[x, y, z];
			}
		}
		}
		}
		return min/max;
	}
}

//a class for the gas properties
class Gas{
	public double rms;
	public double D;
	public Gas(double Urms, double D){
		rms = Urms;
		this.D = D;
	}
}

/* Since C# is an object-oriented language, we're going to use an object for the room.
 */
class Room{
	public int divs{get;}
	public double length;
	public double cellDist;
	public double[ , , ] cell;
	bool partitionOn;
	int Px;
	int Py;
	
	public Room(int divs, double length, bool partitionOn = false){
		this.divs = divs;
		this.length = length;
		cellDist = length/(double)this.divs;
		this.partitionOn = partitionOn;
		Px = divs/2;
		Py = 3*divs/4;
		cell = new double[this.divs, this.divs, this.divs];
	}
	
	// Getter for coordinates.
	public double get(Coord here){
		return cell[here.x, here.y, here.z];
	}
	//setter for coordinates.
	public void set(Coord loc, double val){
		cell[loc.x, loc.y, loc.z] = val;
	}
	
	// Returns a list of other cells that can be diffused to.
	public List<Coord> neighbors(Coord here){
		List<Coord> p = new List<Coord>();
		List<Coord> give = new List<Coord>();
		//get new coordinates
		for(int i = 1; i <= 6; ++i){
			p.Add(new Coord(here));
		}
		p[0].x += -1;
		p[1].x += 1;
		p[2].y += -1;
		p[3].y += 1;
		p[4].z += -1;
		p[5].z += 1;
		//check if valid
		foreach(Coord dest in p){
			if(inBounds(dest)){
				give.Add(dest);
			}
		}
		return give;
	}
	
	//tests if a coordinate is inside the room.
	public bool inBounds(Coord here){
		if( here.x < 0 || here.x >= divs || here.y < 0 || here.y >= divs || here.z < 0 || here.z >= divs ) return false;
		if( partitionOn && here.x == Px && here.y <= Py) return false;
		return true;
	}
}

/* Another class for XYZ coordinates.
 */
class Coord{
	public int x;
	public int y;
	public int z;
	
	//basic constructor
	public Coord(int x, int y, int z){
		this.x = x;
		this.y = y;
		this.z = z;
	}
	
	//copy constructor
	public Coord(Coord other){
		this.x = other.x;
		this.y = other.y;
		this.z = other.z;
	}
	
	public bool equals(Coord other){
		return other.x == this.x && other.y == this.y && other.z == this.z;
	}
}
