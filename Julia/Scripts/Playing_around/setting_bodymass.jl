#=
Creating a basic food web and learning how to change thw bodymass distributions
=#

#=
Cheat sheet:

BioenergeticResponse(fw_h, h = h)
LogisticGrowth(fw, K = k)
FoodWeb(nichemodel, S; C, Z = z)
ProducerCompetiton(fw_comp; αii = 1.0, αij = alpha_ij)
=#

## Packages
using DataFrames, Plots, Random, Distributions, DifferentialEquations
using EcologicalNetworksDynamics

# simulate a simple network 
# Make a network
S = 10
C = 0.2
z = 10

Random.seed!(12325)
fw = FoodWeb(nichemodel, S, C=C, Z=z)
fw.M
fw.A

# set parameters 
params = ModelParameters(fw)

# Set initial biomasses
Random.seed!(123)
B0 = rand(S)

# Simulate
sim = simulate(params, B0)

# Visualize
No_M = plot(sim, label = ["Producer" "Consumer" "Top consumer";])
title!("No M")

## Change body sizes
# 1) make the network
S = 10
C = 0.2
z = 10
Random.seed!(123)
M = rand(1:S, S)

Random.seed!(12325)
fw_M = FoodWeb(nichemodel, S, C=C, tol_C = 1, Z=z)
fw_M.M
fw_M.A

# 2) set parameters
params_M = ModelParameters(fw_M)

# 3) set initial biomass of population
Random.seed!(123)
B0 = rand(S)

# 4) simulate dynamics
sim_M = simulate(params, B0)

# Visualize
plot_M = plot(sim_M, label = ["Producer" "Consumer" "Top consumer";])
title!("M")

# make minor edit to check github is working