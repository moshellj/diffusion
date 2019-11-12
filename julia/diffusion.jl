#!/usr/bin/julia

using Printf

### TESTING RESULTS
# Without partition:
# at Rdivs = 10, equilibriated in 51.438s simulated, 51.091s real time

### CONSTANTS

const Rdivs = 10
const Lroom = 5.0
const Urms = 250.0
const D = 0.175
const Rdist = Lroom/Rdivs
const Tstep = Lroom/Urms/Rdivs
const Dterm = D*Tstep/Rdist/Rdist

room = zeros(Float64, Rdivs, Rdivs, Rdivs)

#offset array for finding neighbors
const offsets = [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]

### FUNCTIONS
# NOT NEAT LANGUAGE THING:
# Functions must be defined before their first use. As far as I can tell, this is undocumented.

function minMaxRatio(room)
	#NEAT LANGUAGE THING: Julia supports maximum over multidimensional arrays.
	#No need for fancy tricks like some other languages. 
	return minimum(room)/maximum(room)
end

#checks if coordinates are in bounds
#coords are known by their xyz tuples
#NEAT LANGUAGE THING: if 'here' is an integer tuple (x, y, z) and
#inbounds(x, y, z) is a function then inbounds(here...) works
function inbounds(x, y, z)
	if x < 1 || x > Rdivs
		return false
	elseif y < 1 || y > Rdivs
		return false
	elseif z < 1 || z > Rdivs
		return false
	end
	return true
end

#performs diffusion between two cells if both are inbounds
function diffuse(x, y, z, a, b, c)
	a += x
	b += y
	c += z
	#check bounds
	if ! inbounds(a, b, c)
		return
	elseif ! inbounds(x, y, z)
		return
	end
	change = (room[x, y, z] - room[a, b, c])*Dterm
	room[x, y, z] -= change
	room[a, b, c] += change
	return
end

### MAIN

room[1, 1, 1] = 1e21
Ttotal = 0.0
while minMaxRatio(room) < 0.99
	#julia is column-major. this doesn't matter because diffusion jumps between rows & cols constantly
	#also, NEAT LANGUAGE THING: range constructors and anything that shortens for loops are nice.
	# Just look at this. This is the platonic ideal of a main function.
	global Ttotal
	for x = 1:Rdivs
		for y = 1:Rdivs
			for z = 1:Rdivs
				for nbr in offsets
					diffuse(x, y, z, nbr...)
				end
			end
		end
	end
	Ttotal += Tstep
	@printf("%9.6f \t%f\n", Ttotal, minMaxRatio(room))
end
