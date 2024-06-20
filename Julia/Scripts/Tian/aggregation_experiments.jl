# 4/06/2024
# Simulating systems at diferent levels of aggregation using Tians data 

# Packages
using DataFrames, Plots, Random, Distributions, XLSX, StatsPlots, CSV
using DifferentialEquations, EcologicalNetworksDynamics
using LinearAlgebra
include("cascade_model.jl")

# import Data
tian_genus = CSV.read("R/Data_outputs/Aggregation/tian_genus.csv", DataFrame)
tian_family = CSV.read("R/Data_outputs/Aggregation/tian_family.csv", DataFrame)
tian_order = CSV.read("R/Data_outputs/Aggregation/tian_order.csv", DataFrame)
tian_class = CSV.read("R/Data_outputs/Aggregation/tian_class.csv", DataFrame)
tian_phylum = CSV.read("R/Data_outputs/Aggregation/tian_phylum.csv", DataFrame)
tian_kingdom = CSV.read("R/Data_outputs/Aggregation/tian_kingdom.csv", DataFrame)
tian_group = CSV.read("R/Data_outputs/Aggregation/tian_group.csv", DataFrame)

# select phyto and zoo from each
# genus
tian_genus_p = Float64.(tian_genus[occursin.("phytoplankton", tian_genus.group), "mean.mass.ug"])
tian_genus_z = Float64.(tian_genus[occursin.("zooplankton", tian_genus.group), "mean.mass.ug"])
# family 
tian_family_p = Float64.(tian_family[occursin.("phytoplankton", tian_family.group), "mean.mass.ug"])
tian_family_z = Float64.(tian_family[occursin.("zooplankton", tian_family.group), "mean.mass.ug"])
# order 
tian_order_p = Float64.(tian_order[occursin.("phytoplankton", tian_order.group), "mean.mass.ug"])
tian_order_z = Float64.(tian_order[occursin.("zooplankton", tian_order.group), "mean.mass.ug"])
# class 
tian_class_p = Float64.(tian_class[occursin.("phytoplankton", tian_class.group), "mean.mass.ug"])
tian_class_z = Float64.(tian_class[occursin.("zooplankton", tian_class.group), "mean.mass.ug"])
# phylum 
tian_phylum_p = Float64.(tian_phylum[occursin.("phytoplankton", tian_phylum.group), "mean.mass.ug"])
tian_phylum_z = Float64.(tian_phylum[occursin.("zooplankton", tian_phylum.group), "mean.mass.ug"])
# kingdom 
tian_kingdom_p = Float64.(tian_kingdom[occursin.("phytoplankton", tian_kingdom.group), "mean.mass.ug"])
tian_kingdom_z = Float64.(tian_kingdom[occursin.("zooplankton", tian_kingdom.group), "mean.mass.ug"])
# group 
tian_group_p = Float64.(tian_group[occursin.("phytoplankton", tian_group.group), "mean.mass.ug"])
tian_group_z = Float64.(tian_group[occursin.("zooplankton", tian_group.group), "mean.mass.ug"])

 
# 1) Make the structural network
# loop through the different levels of aggregation
S_levels = [length(tian_genus_p)+length(tian_genus_z), length(tian_family_p)+length(tian_family_z), length(tian_order_p)+length(tian_order_z)]
mprod = [tian_genus_p, tian_family_p, tian_order_p]
minvert = [tian_genus_z, tian_family_z, tian_order_z]

# set number of replicates
reps = 5

# set the seed
Random.seed!(12325)

# Make networks
begin
    # list to store networks
    global networks = []

    # while loop
    for n in 1:reps
        for i in 1:length(S_levels)
            # make network 
            A = cascade_model_tian(0.15; mprod = mprod[i], minvert = minvert[i])
            # add the network to the set
            push!(networks, A)
        end
    end
end

networks[1]

#  2) simulate

# make collecting dataframe
df_collect_tian_agg = DataFrame(Network = [], aggregation_level = [], S = [], FinalRichness = [], FinalBiomass = [], FinalStability = [])
df_collect_tian_agg
# make an array of the aggregation levels repeated the number of reps to add into data frame
agg_level = repeat(["Genus", "Family", "Order"], reps)

for i in 1:length(networks) # for each network in the network array
    # 1) Define network and size
    A0 = networks[i]
    S = size(networks[i].A)[1] #the number of colums in the matrix in the ith network

    # 2) Define parameters
    # logistic growth with K = 2
    LG = LogisticGrowth(A0, K = 2, a = (diag = 1.0, offdiag = 1.0))

    # functional response
    fr = BioenergeticResponse(A0, h = 1)

    # set model parameters
    params = ModelParameters(A0, producer_growth = LG, functional_response = fr)

    # 3) Define initial biomass of populations
    B0 = fill(0.05, size(S)) # make an array the legnth of the first 

    # 4) Simulate
    sim = simulate(params, B0, extinction_threshold = 1e-10)

    # 5) Calculate metrics
    fin_rich = richness(sim)
    fin_bio = biomass(sim).total
    stab = community_cv(sim)

    # 6) Add metrics to dataframe
    push!(df_collect_tian_agg, [i, agg_level[i], S, fin_rich, fin_bio, stab]) 
end

df_collect_tian_agg

# save dataframe as csv
CSV.write("Julia/Data_outputs/tian_agg", df_collect_tian_agg)

# Plot
p_agg = @df df_collect_tian_agg plot(:S, [:FinalStability], group = :S,
    ylabel = "Stability",
    xlabel = "S",
    seriestype = [:scatter, :line],
    legend = false)