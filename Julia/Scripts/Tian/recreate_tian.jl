## Recreate Tians paper using their body sizes

## Packages
# Install
# import Pkg 
# Pkg.activate(".")
# Pkg.add("Plots")
# Pkg.add("Statistics")
# Pkg.add("Random")
# Pkg.add("Distributions")
# Pkg.add("StatsBase")
# Pkg.add("StatsPlots")
# Pkg.add("DifferentialEquations")
# Pkg.add("CSV")
# Pkg.add("DataFrames")
# Pkg.add("DelimitedFiles")
# Pkg.add("XLSX")


# Packages
using DataFrames, Plots, Random, Distributions, XLSX, StatsPlots
using DifferentialEquations, EcologicalNetworksDynamics
using LinearAlgebra
include("cascade_model.jl")

####### Experiemnts

# import my body size data
tian_data = DataFrame(XLSX.readtable("Raw_data/Tian_data.xlsx", "tian_data"))
# seperate into two vectors for phytoplankton and zoo phytoplankton
tian_biomass_P = Float64.(tian_data[occursin.("P", tian_data.code), "mass.mg"])
tian_biomass_Z = Float64.(tian_data[occursin.("Z", tian_data.code), "mass.mg"])

## Experiment 1
# Recreate Fig 8 with values K = 2 and K = 4

# 1) Make the structural network
# Define variables
S = length(tian_biomass_P)+length(tian_biomass_Z)
C = 0.1

# set the seed
Random.seed!(12325)

# use cascade model to make the network 
fw_fig8 = cascade_model_tian(C; mprod = tian_biomass_P, minvert = tian_biomass_Z)

# 2) Define model parameters
#initial biomass
B0_fig8 = fill(0.05, S)

# logistic growth with K = 2
LG_fig8=LogisticGrowth(fw_fig8, K = 2, a = (diag = 1.0, offdiag = 1.0))

# set functional response
fr_fig8 = BioenergeticResponse(fw_fig8, h = 1, c = 0.1)

# set model parameters
params_fig8 = ModelParameters(fw_fig8, 
    producer_growth = LG_fig8, 
    functional_response = fr_fig8)

# 3) Simulate
# normal solver
sim_fig8 = simulate(params_fig8, B0_fig8, extinction_threshold = 1e-10)

# RK4
alg = RK4()
sim_fig8_RK4 = simulate(params_fig8, B0_fig8, alg = alg, extinction_threshold = 1e-10)

# 4) Plot
# normal solver
plot(sim_fig8, legend = false)
# just phytoplankton
biomasses = reduce(hcat, sim_fig8.u)'
producers(fw_fig8)
fig8_default = plot(sim_fig8.t, biomasses[:, producers(fw_fig8)], xlim = (0,20), legend = false)
png("fig8_defaut")

# RK4 solver
plot(sim_fig8_RK4, legend = false)
# just phytoplankton
biomasses = reduce(hcat, sim_fig8_RK4.u)'
producers(fw_fig8)
fig8_RK4 = plot(sim_fig8_RK4.t, biomasses[:, producers(fw_fig8)], xlim = (0,20), legend = false)
png("fig8_RK4")


## Experiemnt 1: varyig K 
# 1) define variables
S = length(tian_biomass_P)+length(tian_biomass_Z)
C = 0.1
K_levels = [2,4,6,8,10,12,14,16,18,20]

# set seed
Random.seed!(12325)

# Make network
fw_tian_K = cascade_model_tian(C; mprod = tian_biomass_P, minvert = tian_biomass_Z)

# Set collecting dataframe
df_collect_tian_K = DataFrame(K = [], FinalRichness = [], FinalBiomass = [], ComunityStability = [], PopulationStability = [])

for k in K_levels
    println("***> This is iteration with K = $k\n")

    # Specify K
    LogisticGrowth(fw_tian_K, K = k)

    # Define parameters
    B0 = rand(S)
    params_K = ModelParameters(fw_tian_K)

    # Simulate
    sim_tian_K = simulate(params_K, B0)

    # collect info and push to dataframe
    fin_rich = richness(sim_tian_K)
    fin_bio = biomass(sim_tian_K).total
    com_stab = coefficient_of_variation(sim_tian_K).community
    pop_stab = coefficient_of_variation(sim_tian_K).species_mean

    push!(df_collect_tian_K, [k, fin_rich, fin_bio, com_stab, pop_stab])
end

K_plot_1 = @df df_collect_tian_K plot(:K, [:ComunityStability],
    ylabel = "Comunity stabilty", 
    xlabel = "Carrying capacity",
    seriestype = [:scatter, :line],
    legend = false)

K_plot_2 = @df df_collect_tian_K plot(:K, [:PopulationStability],
    ylabel = "Population stability", 
    xlabel = "Carrying capacity",
    seriestype = [:scatter, :line],
    legend = false)
    
K_plot_3 = @df df_collect_tian_K plot(:K, [:FinalBiomass],
    ylabel = "Biomass", 
    xlabel = "Carrying capacity",
    seriestype = [:scatter, :line],
    legend = false)

K_plot_4 = @df df_collect_tian_K plot(:K, [:FinalRichness],
    ylabel = "Richness", 
    xlabel = "Carrying capacity",
    seriestype = [:scatter, :line],
    legend = false)

K_plots = plot(K_plot_1, K_plot_2, K_plot_3, K_plot_4, layout=(4,1), legend = false, size=(1000, 1000))
png("K_plots")

df_collect_tian_K