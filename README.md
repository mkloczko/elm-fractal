# Introduction

A tool for visualizing different attractors using different numerical methods.
Use the provided controls to modify different parameters and see their effect on the visualisation. 

You can follow the link [here](https://student.agh.edu.pl/~mkloczko/fractal/) to see it in action.

The project requires WebGL and is best viewed in Chrome. 

Tested with both Chrome and Firefox.

# Compiling

The project is build using ThreeJS and Elm. The Elm side of the project is handled by make.sh script.

# Usage: 

Click on the canvas to rotate the camera around the attractor. Use shift to move the height.

The inpux boxes can be modified using up and down arrows.


## Controls
Function - choose between Lorenz and Rosslers attractors.
Method   - change the derivatation methods - Euler or 4th order Runge Kutta.

Simulation:
ix       - iterations
dt       - time step - controls the trade off between precision and furthering the simulation
d0       - starting time - only relevant for RK4 method.

Parameters:
Parameters for given functions. Change them to see the difference in generated attractors.

Starting point:
The starting point for simulation. Has to be different from x = 0, y = 0, z = 0.