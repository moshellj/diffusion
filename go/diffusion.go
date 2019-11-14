package main

import "fmt"
//import "math"

const Rdivs int = 10
const Lroom float64 = 5.0
const Urms float64 = 250.0
const D float64 = 0.175
const Rdist float64 = Lroom/float64(Rdivs)
const Tstep float64 = Rdist/Urms
const Dterm float64 = D*Tstep/Rdist/Rdist

func main(){
	//for efficiency, room must be a slice and not an array
	//this is the easiest way, apparently
	var room [][][]float64
	room = make([][][]float64, Rdivs)
	for i := range room {
		room[i] = make([][]float64, Rdivs)
		for j := range room[i] {
			room[i][j] = make([]float64, Rdivs)
		}
	}
	
	for x := 0; x < Rdivs; x++ {
		for y := 0; y < Rdivs; y++ {
			for z := 0; z < Rdivs; z++ {
				
			}
		}
	}
}

//checks if a function is in bounds
//no partition yet
func inbounds(x int, y int, z int) bool {
	if (x < 0 || x >= Rdivs) {
		return false
	}
	if (y < 0 || y >= Rdivs) {
		return false
	}
	if (z < 0 || z >= Rdivs) {
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
				if room[x][y][z] > max {
					max = room[x][y][z]
				}
				if room[x][y][z] < min {
					min = room[x][y][z]
				}
			}
		}
	}
	return (min/max)
}
