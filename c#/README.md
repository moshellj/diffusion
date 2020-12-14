To compile on linux: mcs diffusion.cs, or csc diffusion.cs, depending on your version of mono

To run on linux: mono diffusion.cs

To change settings, the divs parameter in the constructor for room sets the subdivisions in each dimension. Likewise, set partitionOn in the constructor to enable or disable the partition.

### Reduced version

Per Anil Narra's request, I have provided a demonstration showing that it is not possible to combine the loops in the main function and in the minMaxRatio function.

The files `diffusion.cs` and `reduced.cs` are identical, except for the condition in the main while loop. `diffusion.cs` uses the minMaxRatio function to calculate if the room has reached equilibrium. `reduced.cs` calculates the minimum and maximum concentration in the same loop where the diffusion calculations are done, uses those to calculate concentrationRatio, and uses that to determine if the room has reached equilibrium.

The starting condition has been modified so that it takes much less time for the simulation to reach equilibrium. Every cell has been set to a concentration of 1e20, and the cell at (0, 0, 0) has been set to 1.5e20. The partition has been disabled.

After each step of the simulation, the program prints out the number of simulated seconds that have passed, the concentration ratio according to minMaxRatio, and the concentration ratio according to concentrationRatio.

## Results

Running either program with any initial conditions shows that the two methods of calculating the concentration ratio almost always give different results.

For the given initial conditions, after 2.968 simulated seconds, the truncated value of minMaxRatio is 0.9900, and the truncated value of concentrationRatio is 0.9899. Consequently, diffusion.cs stops after 2.968 seconds, but reduced.cs does not stop until one step later, at 2.970 seconds. Part of the assignment was that our programs had to give identical results to a reference program. Since diffusion.cs is known to be correct, the change made in reduced.cs makes the program incorrect.

The minimum and maximum concentration cannot be calculated in the same loop in which the diffusion calculations are done, because the concentration in each cell also changes when its neighbors are being simulated. The program needs to wait until the cells are finished updating before it can calculate the minimum and maximum concentration, which is what minMaxRatio does.
