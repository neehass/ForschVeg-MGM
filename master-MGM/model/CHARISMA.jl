#Macrophyte Growth Model (MGM)
#
#Anne Lewerentz <anne.lewerentz@uni-wuerzburg.de>
#(c) 2021-2022, licensed under the terms of the MIT license
#
#Contains all functions to run this depth-explicit macrophytes growth model.

#Set dir to home_dir of file
cd(dirname(@__DIR__))
pwd()

#load packages
using Pkg
# Pkg.add("HCubature") 
# Pkg.add("DataFrames")
# Pkg.add("CSV")
# Pkg.add("Distributions")

using
    HCubature, #for Integration
    DelimitedFiles, # for function writedlm, used to write output files
    Dates, #to create output folder
    Distributions, Random #for killWithProbability
using CSV
using DataFrames

# Include functions
include("structs.jl")
include("defaults.jl")
include("input.jl")
include("functions.jl")
include("run_simulation.jl")
include("output.jl")

# --- Get Settings for selection of lakes, species & depth -------------------------
# GeneralSettings = parseconfigGeneral("./input/general.config.txt")

GeneralSettings = parseconfigGeneral("./input/sel.general.config.txt") # 3 slected
GeneralSettings = parseconfigGeneral("./input/chiem.general.config.txt") # chiemsee only

depths = parse.(Float64, GeneralSettings["depths"])

# --- Create output folder name ---------------------------------------------------------
#folder = string(Dates.format(now(), "yyyy_m_d_HH_MM"))
folder = GeneralSettings["modelrun"][1]  

# --- Simulation Loop ----------------------------------------------------------------------
# Single Threaded Loop for model run for selected lakes, species and depths
l = 1
s = 1

for l in 1:length(GeneralSettings["lakes"])

    println(GeneralSettings["lakes"][l])

    for s in 1:length(GeneralSettings["species"])

        println(GeneralSettings["species"][s])

        #Get settings
        settings = getsettings(GeneralSettings["lakes"][l], GeneralSettings["species"][s])
        push!(settings, "years" => parse.(Int64,GeneralSettings["years"])[1]) #add "years" from GeneralSettings
        push!(settings, "yearsoutput" => parse.(Int64,GeneralSettings["yearsoutput"])[1]) #add "years" from GeneralSettings
        push!(settings, "modelrun" => GeneralSettings["modelrun"][1]) #add "modelrun" from GeneralSettings
        push!(settings, "tempProfile" =>to_bool(GeneralSettings["tempProfile"][1])) # add "tempProfile" true or false
        push!(settings, "k" =>  parse.(Int64,GeneralSettings["k"][1])) # seepness factor for temp profile
        push!(settings, "HypoFrac_dir" => GeneralSettings["HypoFrac_dir"][1]) # add HypoFrac_dir
        push!(settings, "MetaFrac_dir" => GeneralSettings["MetaFrac_dir"][1])

        if !(settings["tempProfile"] isa Bool)
            error("tempProfile must be a Boolean (true or false)")
        elseif settings["tempProfile"] == true
            println("Using temperature profile for simulation")
        else
            println("Using epilimnion temperature (surface temp) for simulation")
        end

        dynamicData = Dict{Int16, DayData}()

        # Get climate for default variables . !Gives just one year as environment is not yet changing between years
        environment = simulateEnvironment(settings, dynamicData, settings["HypoFrac_dir"], settings["MetaFrac_dir"])
        # tempprofile: tempEpi, tempHypo, metaDepth, irradiance, waterlevel, lightAttenuation

        # Get macrophytes in multiple depths
        result = simulateMultipleDepth_parallel(depths,settings, dynamicData, settings["tempProfile"]) #Biomass, Number, indWeight, Height,
        #  depths = LevelOfGrid
        # Save results as .csv files in new folder;
        writeOutput(settings, depths, environment, result, GeneralSettings, folder)

    end
end

"""
# Multi Threaded Loop for model run for selected lakes, species and depths
write_lock = ReentrantLock()

Threads.@threads for l in 1:length(GeneralSettings["lakes"])
    println(GeneralSettings["lakes"][l])

    for s in 1:length(GeneralSettings["species"])

        println(GeneralSettings["species"][s])

        #Get settings
        settings = getsettings(GeneralSettings["lakes"][l], GeneralSettings["species"][s])
        push!(settings, "years" => parse.(Int64,GeneralSettings["years"])[1]) #add "years" from GeneralSettings
        push!(settings, "yearsoutput" => parse.(Int64,GeneralSettings["yearsoutput"])[1]) #add "years" from GeneralSettings
        push!(settings, "modelrun" => GeneralSettings["modelrun"][1]) #add "modelrun" from GeneralSettings

        dynamicData = Dict{Int16, DayData}()

        # Get climate for default variables . !Gives just one year as environment is not yet changing between years
        # also used to Initialize dynamicData
        environment = simulateEnvironment(settings, dynamicData)
        # Output: temp, irradiance, waterlevel, lightAttenuation

        # Get macrophytes in multiple depths
        result = simulateMultipleDepth_parallel(depths,settings, dynamicData) #Biomass, Number, indWeight, Height,

        lock(write_lock)
        try
            # Save results as .csv files in new folder;
            writeOutput(settings, depths, environment, result, GeneralSettings, folder)
        finally
            unlock(write_lock)
        end
    end
end

println("Done with MultiThreaded Lake Loop")
"""
