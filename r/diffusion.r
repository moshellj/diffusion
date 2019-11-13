#!/usr/bin/Rscript

#constants
Rdivs <- 10L
Lroom <- 5.0
Urms <- 250.0
D <- 0.175
Rdist <- Lroom/Rdivs
Tstep <- Lroom/Urms/Rdivs
Dterm <- D*Tstep/Rdist/Rdist
offsets <- c(c(-1,0,0),c(1,0,0),c(0,-1,0),c(0,1,0),c(0,0,-1),c(0,0,1))

room <- array(rep(0e1, Rdivs*Rdivs*Rdivs), c(Rdivs, Rdivs, Rdivs))

# min & max work over 3d arrays, so that's nice.
minMaxRatio <- function(room){
	return(min(room)/max(room))
}

#getter for vector
getCon <- function(room, here){
	return(room[here[1],here[2],here[3]])
}
#setter for vector
getCon <- function(room, here, val){
	room[here[1],here[2],here[3]] <- room[here[1],here[2],here[3]] + val
}

#checks if a coordinate is inbounds.
#(R is 1-indexed!)
inbounds <- function(here){
	for(n in here){
		if(n < 1 | n > Rdivs){
			return(FALSE)
		}
	}
	return(TRUE)
}

#performs diffusion.
#room = room. here and offset are 3-vectors
diffuse <- function(room, here, offset){
	nbr = c(here[1]+offset[1],here[2]+offset[2],here[3]+offset[3])
	if(inbounds(here) & inbounds(nbr)){
		change <- (getCon(room, here) - getCon(room, nbr))*Dterm
		setCon(room, here, -1*change)
		setCon(room, nbr, change)
	}
}

### MAIN
room[1, 1, 1] <- 1e21
Ttotal <- 0.0

print(room)

while(minMaxRatio(room) < 0.99){
	for(x in 1:Rdivs){
	for(y in 1:Rdivs){
	for(z in 1:Rdivs){
		for(nbr in offsets){
			diffuse(room, c(x, y, z), nbr)
		}
	}
	}
	}
}
