#!/usr/bin/Rscript

## TESTING RESULTS:
# Without partition:
# Equilibriated in 51.438s simulated, 24m52s user time.
# With partition: with Rdivs = 10, equilibriated in 259.294s simulated time, 136m 46s user time.
# with Rdivs <- 500, it works. Estimated time just to iterate over the room once is 42 minutes.

#constants
Rdivs <- 20L
Lroom <- 5.0
Urms <- 250.0
D <- 0.175
Rdist <- Lroom/Rdivs
Tstep <- Lroom/Urms/Rdivs
Dterm <- D*Tstep/Rdist/Rdist

partitionOn <- FALSE
Px <- Rdivs %/% 2 + 1
Py <- (3*Rdivs) %/% 4 + 1

offsets <- t(array(c(c(-1,0,0),c(1,0,0),c(0,-1,0),c(0,1,0),c(0,0,-1),c(0,0,1)), c(3,6)))

room <- array(rep(0, Rdivs*Rdivs*Rdivs), c(Rdivs, Rdivs, Rdivs))

# min & max work over 3d arrays, but since we need to exclude the partition, we can't use them.
minMaxRatio <- function(room){
	minval = 1.0e100
	maxval = 0.0
	for(x in 1:Rdivs){
	for(y in 1:Rdivs){
	for(z in 1:Rdivs){
		if(inbounds(c(x,y,z))){
			val = getCon(room, c(x,y,z))
			if(val > maxval){
				maxval = val
			}
			if(val < minval){
				minval = val
			}
		}
	}
	}	
	}
	return(minval/maxval)
}

#getter for vector
getCon <- function(room, here){
	return(room[here[1],here[2],here[3]])
}
#setter for vector
setCon <- function(room, here, val){
	room[here[1],here[2],here[3]] <<- room[here[1],here[2],here[3]] + val
}

#checks if a coordinate is inbounds.
#(R is 1-indexed!)
inbounds <- function(here){
	for(n in here){
		if(n < 1 || n > Rdivs){
			return(FALSE)
		}
	}
	#PARTITION
	if(partitionOn && here[1] == Px && here[2] <= Py){
		return(FALSE)
	}
	return(TRUE)
}

#performs diffusion.
#room = room. here and offset are 3-vectors
diffuse <- function(room, here, offset){
	nbr = c(here[1]+offset[1],here[2]+offset[2],here[3]+offset[3])
	if(inbounds(nbr) && inbounds(here)){
		change <- (getCon(room, here) - getCon(room, nbr))*Dterm
		setCon(room, here, -1*change)
		setCon(room, nbr, change)
	}
}

### MAIN
room[1, 1, 1] <- 1e21
Ttotal <- 0.0
els <- 1

while(minMaxRatio(room) < 0.99){
	for(x in 1:Rdivs){
	for(y in 1:Rdivs){
	for(z in 1:Rdivs){
		for(ro in 1:6){
			nbr = offsets[ro,]# The comma is necessary. Indexing into 2d elements is an element, not a row.
			diffuse(room, c(x, y, z), nbr)
		}
	}
	}
	}
	Ttotal <<- Ttotal + Tstep
	#print(c(Ttotal, minMaxRatio(room)))
	quit()
}

print(Ttotal)
print(minMaxRatio(room))
