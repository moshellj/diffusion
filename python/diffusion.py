#!/usr/bin/env python

#x = right, y = down, z = back

## constants
Lroom = 5.0 #m
Rdivs = 10
R = Lroom/Rdivs #m
Urms = 0.250 #m/s
D = 0.175 
Tstep = Lroom/Urms/Rdivs
#room
room = [[[0.0 for i in range(Rdivs)] for j in range(Rdivs)] for k in range(Rdivs)]

## functions

# Given a volume element, returns a list of other volume elements that can be diffused to.
# volels are known by their xyz indexes in a list.
def canDiffuse(here):
	results = []
	if here[0] !=0:
		results.append( [here[0]-1,here[1],here[2]] )
	if here[0] != Rdivs-1:
		results.append( [here[0]+1,here[1],here[2]] )
	if here[1] !=0:
		results.append( [here[0],here[1]-1,here[2]] )
	if here[1] != Rdivs-1:
		results.append( [here[0],here[1]+1,here[2]] )
	if here[2] !=0:
		results.append( [here[0],here[1],here[2]-1] )
	if here[2] != Rdivs-1:
		results.append( [here[0],here[1],here[2]+1] )
	return results

# returns the ratio of the maximum conc cell to the min conc cell
def minMaxRatio():
	#NEAT LANGUAGE THING: List comprehension! 3 smart for-loops on one line to flatten a 3d list.
	room2d = [z for x in room for y in x for z in y]
	return min(room2d)/max(room2d)

#testing script
def testSuite():
	for x in range(Rdivs):
		for y in range(Rdivs):
			print( "(" + str(x) + ", " + str(y) + ") can diffuse to: " + str(canDiffuse([x, y, 0])))
	
### MAIN
