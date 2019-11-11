#!/usr/bin/julia

### CONSTANTS

const Rdivs = 10
const Lroom = 5.0
const Urms = 250.0
const D = 0.175
const Rdist = Lroom/Rdivs
const Tstep = Lroom/Urms/Rdivs
Dterm = D*Tstep/Rdist/Rdist

cube = zeros(Float64, Rdivs, Rdivs, Rdivs)

### MAIN


### FUNCTIONS
function minMaxRatio(cube)
	return 
end
