using System;
using System.Collections.Generic;

class Diffusion{
	static void Main(string[] args){
		// This number here  vv  is the number of divisions in the room.
		Room room = new Room(10, 5.0);
		
	}
}

/* Since C# is an object-oriented language, we're going to use an object for the room.
 */
class Room{
	int divs{get;}
	double length;
	double cellDist;
	public double[ , , ] cell;
	
	public Room(int divs, double length){
		this.divs = divs;
		this.length = length;
		cellDist = length/(double)this.divs;
		cell = new double[this.divs, this.divs, this.divs];
	}
	
	// Getter for coordinates.
	public double get(Coord here){
		return cell[here.x, here.y, here.z];
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
		if( here.x < 0 || here.x >= divs ) return false;
		if( here.y < 0 || here.y >= divs ) return false;
		if( here.z < 0 || here.z >= divs ) return false;
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
