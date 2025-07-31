# -------------------------------------------------------------------------------------------------
# Test Functions for MGM Model
# simualtionEnviroment()
# -------------------------------------------------------------------------------------------------
# 1) load settings
# 2) load settings for testing the environment simulation
# 3) test the environment simulation
# ------------------------------------------------------------------------------------------------- 

# ---- set up ------------------------------------
pwd()
dir = "C:\\Users\\maiim\\Documents\\25-25SS\\Forschungsprojekt_Vegetationskunde\\scripte-data-MGM\\master-MGM"
cd(dir)

# load functions and packages
include(joinpath(dir,"model/input.jl"))
include(joinpath(dir,"model/structs.jl"))
include(joinpath(dir,"model/functions.jl"))
include(joinpath(dir,"model/run_simulation.jl"))
include(joinpath(dir,"model/defaults.jl"))
import Pkg

using Plots
using DataFrames
# -------------------------------------------------------------------------------------------------
# ---- 1) load settings 
GeneralSettings = parseconfigGeneral("./input/general.config.txt")

depths = parse.(Float64, GeneralSettings["depths"])[1] #da nur 1 depth # parse = Converts each string in array to Float64: ["10.0", "20.0"] → [10.0, 20.0]
nyears = parse.(Int64, GeneralSettings["years"])
nlakes = length(GeneralSettings["lakes"]) #VE
nspecies = length(GeneralSettings["species"]) #VE

# ---- 2) load settings for testing the environment simulation -------------------------
l = 6 # lake = chiemsee
s = 1 #species 1
settings = getsettings(GeneralSettings["lakes"][l], GeneralSettings["species"][s])
push!(settings, "years" => parse.(Int64,GeneralSettings["years"])[1]) #add "years" from GeneralSettings
push!(settings, "yearsoutput" => parse.(Int64,GeneralSettings["yearsoutput"])[1]) #add "years" from GeneralSettings
push!(settings, "modelrun" => GeneralSettings["modelrun"][1]) #add "modelrun" from GeneralSettings

# to get insights into the settings
# df = DataFrame(Key = collect(keys(settings)), Value = collect(values(settings)))

lak_nam = settings["Name"]

# ---- 3) test the environment simulation -----------------------------
dynamicData = Dict{Int16, DayData}()
environment = simulateEnvironment(settings, dynamicData)

sim_tempEpi = environment[1]
sim_tempHypo = environment[2]
sim_mesoDepth = environment[3]

# ---- 4) plot: check the results -----------------------------------
plot(1:365, sim_tempEpi, 
    label = "Epi", title = lak_nam * " - Epi & Hypo Temperature", xlabel = "Day of the year", ylabel = "Temperature [°C]", legend = :topright)
plot!(1:365, sim_tempHypo, label = "Hypo", legend = :topright)
savefig("./plots/1st_Temperature_Epi_Hypo.png")


plot(1:365, sim_mesoDepth, 
    label = "mesoDepth", title = lak_nam * " - Mesolimnion Depth", xlabel = "Day of the year", ylabel = "Depth [m]", legend = :topright)
savefig("./plots/1st_Temperature_Epi_Hypo.png")

