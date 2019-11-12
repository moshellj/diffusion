#!/usr/bin/julia

### CONSTANTS

const Rdivs = 10
const Lroom = 5.0
const Urms = 250.0
const D = 0.175
const Rdist = Lroom/Rdivs
const Tstep = Lroom/Urms/Rdivs
Dterm = D*Tstep/Rdist/Rdist

room = zeros(Float64, Rdivs, Rdivs, Rdivs)

### FUNCTIONS
# Functions must be defined before their first use. As far as I can tell, this is undocumented.

function minMaxRatio(room)
	#NEAT LANGUAGE THING: Julia supports maximum over multidimensional arrays.
	#No need for fancy tricks like some other languages. 
	return minimum(room)/maximum(room)
end

#tests features
function testSuite()
	miniroom = zeros(Float64, 3, 3, 3)
	miniroom[1, 1, 1] = 5.0
	miniroom[3, 3, 3] = -5.0
	println(minMaxRatio(miniroom))
end

### MAIN

testSuite()
