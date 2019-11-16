package main

/*
TESTING RESULTS:
Without partition:
at Rdivs = 10, equilibriates in 51.438s simulated time, 2.142s real time. (faster that c!)
at Rdivs = 500, completed 1 step in 11.147s real time.
*/

import "fmt"
//import "math"

const Rdivs int = 10
const Lroom float64 = 5.0
const Urms float64 = 250.0
const D float64 = 0.175
const Rdist float64 = Lroom/float64(Rdivs)
const Tstep float64 = Rdist/Urms
const Dterm float64 = D*Tstep/Rdist/Rdist

const partitionOn bool = true
const Px int = Rdivs/2
const Py int = (3*Rdivs)/4

func main(){
	//for efficiency, room must be a slice and not an array
	//this is the easiest way, apparently
	
	var room [][][]float64
	room = make([][][]float64, Rdivs)
	for i := range room {
		room[i] = make([][]float64, Rdivs)
		for j := range room[i] {
			room[i][j] = make([]float64, Rdivs)
			for k := range room[i][j] {
				room[i][j][k] = 0.0
			}
		}
	}
	
	room[0][0][0] = 1e21
	Ttotal := 0.0
	ratio := 0.0
	for ratio < 0.99 {
		for x := 0; x < Rdivs; x++ {
		for y := 0; y < Rdivs; y++ {
		for z := 0; z < Rdivs; z++ {
			diffuse(room, x, y, z, x-1, y, z)
			diffuse(room, x, y, z, x+1, y, z)
			diffuse(room, x, y, z, x, y-1, z)
			diffuse(room, x, y, z, x, y+1, z)
			diffuse(room, x, y, z, x, y, z-1)
			diffuse(room, x, y, z, x, y, z+1)
		}
		}
		}
		Ttotal = Ttotal + Tstep
		ratio = minMaxRatio(room)
		//fmt.Printf("%.6f \t%.8f\n", Ttotal, ratio)
	}
	fmt.Printf("%.6f \t%.8f\n", Ttotal, ratio)
}

//diffuses between 2 cells
//nx,ny,nz must be set beforehand
func diffuse(room [][][]float64, x int, y int, z int, nx int, ny int, nz int){
	if(inbounds(nx, ny, nz) && inbounds(x, y, z)){
		var change float64 = (room[x][y][z] - room[nx][ny][nz])*Dterm
		room[x][y][z] = room[x][y][z] - change
		room[nx][ny][nz] = room[nx][ny][nz] + change
	}
	return
}

//checks if a function is in bounds
//no partition yet
func inbounds(x int, y int, z int) bool {
	if (x < 0 || x >= Rdivs || y < 0 || y >= Rdivs || z < 0 || z >= Rdivs) {
		return false
	}
	if (partitionOn && x == Px && y <= Py){
		return false
	}
	return true
}

func minMaxRatio(room [][][]float64) float64 {
	min := 1.0e100
	max := 0.0
	// BAD LANGUAGE NON-FEATURE: Go has a range keyword, but it doesn't take integers.
	// So we have to use old-school for-loops.
	for x := 0; x < Rdivs; x++ {
		for y := 0; y < Rdivs; y++ {
			for z := 0; z < Rdivs; z++ {
				if(inbounds(x, y, z)){
				if room[x][y][z] > max {
					max = room[x][y][z]
				}
				if room[x][y][z] < min {
					min = room[x][y][z]
				}
				}
			}
		}
	}
	//fmt.Printf("MM: %.15f\t%.15f\n", min, max)
	return (min/max)
}
