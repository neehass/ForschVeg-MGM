#Macrophyte Growth Model (MGM)
#
#Anne Lewerentz <anne.lewerentz@uni-wuerzburg.de>
#(c) 2021-2022, licensed under the terms of the MIT license
#
#Contains Structs for more efficiently storing dynamic data

"""
Structs for more efficiently storing dynamic data
"""

Base.@kwdef mutable struct DayData
        # getSurfaceIrradianceDay(day, settings::Dict{String, Any})
        surfaceIrradiance::Union{Missing, Float64} = missing

        # getSurfaceIrradianceHour(day, hour, settings::Dict{String, Any}) #times in hour after sunset
        surfaceIrradianceHour::Dict{Int8, Float64} = Dict{Int8, Float64}()

        # getTemperature_Epi(day, settings::Dict{String, Any})
        tempEpi::Union{Missing, Float64} = missing
         
        # getTemperature_Hypo_mean(day, settings::Dict{String, Any})
        tempHypo::Union{Missing, Float64} = missing

        # getMesolimnion_Depth_mean(day, settings::Dict{String, Any})
        mesoDepth::Union{Missing, Float64} = missing

        # getDaylength(day, settings::Dict{String, Any})
        daylength::Union{Missing, Float64} = missing

        # getWaterlevel(day, settings::Dict{String, Any})
        waterlevel::Union{Missing, Float64} = missing

        # getLightAttenuation(day, settings::Dict{String, Any})
        lightAttenuation::Union{Missing, Float64} = missing
end
