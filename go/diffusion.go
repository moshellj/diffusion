package main
import "fmt"

const Rdivs int = 10
const Lroom float64 = 5.0
const Urms float64 = 250.0
const D float64 = 0.175
const Rdist float64 = Lroom/float64(Rdivs)
const Tstep float64 = Rdist/Urms
const Dterm float64 = D*Tstep/Rdist/Rdist

func main(){
	
}
