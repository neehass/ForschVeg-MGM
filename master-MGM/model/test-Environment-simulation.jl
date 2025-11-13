# -------------------------------------------------------------------------------------------------
# Test Functions for MGM Model
# simualtionEnviroment()
# -------------------------------------------------------------------------------------------------
# sel.general.config: Lakes: chiemsee (large) = 1, AbtsdorfSee (very.small) = 2, Eibsee (medium) = 3
# general.config: Lakes: chiemsee (large) = 6, AbtsdorfSee (very.small) = 1, Eibsee (medium) = 7
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
GeneralSettings = parseconfigGeneral("./input/sel.general.config.txt")

depths = parse.(Float64, GeneralSettings["depths"])[1] #da nur 1 depth # parse = Converts each string in array to Float64: ["10.0", "20.0"] → [10.0, 20.0]
nyears = parse.(Int64, GeneralSettings["years"])
nlakes = length(GeneralSettings["lakes"]) #VE
nspecies = length(GeneralSettings["species"]) #VE

# ---- 2) load settings for testing the environment simulation -------------------------
l = 6 # lake = chiemsee in general.config
l = 1 # lake = chiemsee in sel.general.config 
s = 1 #species 1
settings = getsettings(GeneralSettings["lakes"][l], GeneralSettings["species"][s])
push!(settings, "years" => parse.(Int64,GeneralSettings["years"])[1]) #add "years" from GeneralSettings
push!(settings, "yearsoutput" => parse.(Int64,GeneralSettings["yearsoutput"])[1]) #add "years" from GeneralSettings
push!(settings, "modelrun" => GeneralSettings["modelrun"][1]) #add "modelrun" from GeneralSettings

# Show the result
println("Settings:")
for (key, value) in  settings #defaultSettingsLake()
    println("  $key => $value")
end

# to get insights into the settings
settings["Areakm2"]
settings["AreaGroup"]
keys(settings)
df = DataFrame(Key = collect(), Value = collect(values(settings)))

lak_nam = settings["Name"]
lak_group =  settings["AreaGroup"]

# ---- 3) test the environment simulation -----------------------------
HypoFrac_dir = "./input/lakeFractionParameters/HypoTemp_fraction.config.txt"
MetaFrac_dir = "./input/lakeFractionParameters/MetaDepth_fraction.config.txt"

dynamicData = Dict{Int16, DayData}()
env = simulateEnvironment(settings, dynamicData, HypoFrac_dir, MetaFrac_dir)

keys(dynamicData)

keys(dynamicData) # keys = days
dump(dynamicData[5]) # insight of day 5
dynamicData[5].tempEpi

sim_tempEpi = [dynamicData[d].tempEpi for d in sort(collect(keys(dynamicData)))] # 356 days
sim_tempHypo = [dynamicData[d].tempHypo for d in sort(collect(keys(dynamicData)))]
sim_metaDepth = [dynamicData[d].metaDepth for d in sort(collect(keys(dynamicData)))]

# ---- 4) plot: check the results -----------------------------------
plot(1:365, sim_tempEpi, 
    label = "Epi", title = lak_nam * " - Epi & Hypo Temperature (" * lak_group * ")", xlabel = "Day of the year", ylabel = "Temperature [°C]", legend = :topright)
plot!(1:365, sim_tempHypo, label = "Hypo", legend = :topright)
savefig("./plots/1st_Temperature_Epi_Hypo.png")

plot(1:365, sim_metaDepth, 
    label = "metaDepth", title = lak_nam * " - Epi & Hypo Temperature (" * lak_group * ")", xlabel = "Day of the year", ylabel = "Depth [m]", legend = :topright)
savefig("./plots/1st_Temperature_Epi_Hypo.png")

# ---- 5) test getTemperatureProfile_depth() -----------------------------
day = 150
depth = -5 # depth where temp is needed
tempEpi = dynamicData[day].tempEpi
tempHypo = dynamicData[day].tempHypo
metaDepth = dynamicData[day].metaDepth
getTemperatureProfile_depth(depth, tempEpi, tempHypo, metaDepth, k=5)

# ---- 6) plot profile at distinct days -----------------------------
depths_profile = collect(0:-5:settings["lakeDepth"]) # start:step:stop
days_profile = [50, 150, 250, 350] # days to plot
for day in days_profile
    temp_profile = [getTemperatureProfile_depth(depth, dynamicData[day].tempEpi, dynamicData[day].tempHypo, dynamicData[day].metaDepth, k=5) for depth in depths_profile]
    plot(temp_profile, depths_profile, 
        label = "Day " * string(day), 
        title = lak_nam * " - Temperature Profile at distinct days (" * lak_group * ")", 
        xlabel = "Temperature [°C]", 
        ylabel = "Depth [m]", 
        legend = :topright, ylim = (settings["lakeDepth"],0))
    
    savefig("./plots/1st_Temperature_Profile_Day_" * string(day) * ".png")
end





