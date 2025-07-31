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
using Pkg
using CSV
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

# test parameters
push!(settings, "Areakm2" => (76.76)) #add "Areakm2" from GeneralSettings
push!(settings, "lakeDepth" => -60) #add "depths" from GeneralSettings

# area group  "very.small", "small", "medium", "large", "very.large"
push!(settings, "AreaGroup" => getAreaGroup(settings["Areakm2"]))

# if parameters are added: 
# push!(settings, "Areakm2" => parse.(Float64, GeneralSettings["Areakm2"])) #add "Areakm2" from GeneralSettings
# push!(settings, "depth" => parse.(Float64, GeneralSettings["depth"])) #add "depth" from GeneralSettings

# to get insights into the settings
# df = DataFrame(Key = collect(keys(settings)), Value = collect(values(settings)))

lak_nam = settings["Name"]
lak_group =  settings["AreaGroup"]

# ---- 3) test the environment simulation -----------------------------
HypoFrac_dir = "./input/lakeFractionParameters/HypoTemp_fraction.config.txt"
MesoFrac_dir = "./input/lakeFractionParameters/MesoDepth_fraction.config.txt"

dynamicData = Dict{Int16, DayData}()
environment = simulateEnvironment(settings, dynamicData, HypoFrac_dir, MesoFrac_dir)

sim_tempEpi = environment[1]
sim_tempHypo = environment[2]
sim_mesoDepth = environment[3] 

# ---- 4) plot: check the results -----------------------------------
plot(1:365, sim_tempEpi, 
    label = "Epi", title = lak_nam * " - Epi & Hypo Temperature (" * lak_group * ")", xlabel = "Day of the year", ylabel = "Temperature [°C]", legend = :topright)
plot!(1:365, sim_tempHypo, label = "Hypo", legend = :topright)
savefig("./plots/1st_Temperature_Epi_Hypo.png")

plot(1:365, sim_mesoDepth, 
    label = "mesoDepth", title = lak_nam * " - Epi & Hypo Temperature (" * lak_group * ")", xlabel = "Day of the year", ylabel = "Depth [m]", legend = :topright)
savefig("./plots/1st_Temperature_Epi_Hypo.png")







