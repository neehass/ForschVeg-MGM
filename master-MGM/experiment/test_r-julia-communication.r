# Libraries
library(JuliaCall)

options(JULIA_HOME = "/opt/julia-1.12.1/bin/")
options(JULIA_HOME = "/usr/local/bin/")

julia_setup(installJulia=FALSE)


# Hi Neele: 

# ... in der Fehlermeldung, die nur kurz kommt steht unter anderem, "unable to load dependent library /opt/julia-1.12.1/bin/.-/lib/julia/libjulia-internal.so.1."


# Hast du es mit einer anderen Version von julia probiert? zB die stable Version 1.10.10? ... ich scheitere gerade daran sie zu installieren. https://github.com/JuliaLang/juliaup


# Und noch eine Fehlermeldung: ... keine Ahnung bei was die gerade entstanden ist ...
# LoadError("/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.5/JuliaCall/julia/setup.jl", 16, 
# LoadError("/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.5/JuliaCall/julia/display/RmdJulia.jl", 
# 6, ErrorException("could not load symbol \"SET_SYMVALUE\":\n/usr/lib/R/lib/libR.so: undefined symbol: SET_SYMVALUE"))) 
# Error in .julia$cmd(paste0(Rhomeset, "Base.include(Main,\"", system.file("julia/setup.jl",  : 
#  Error happens when you try to execute command ENV["R_HOME"] = "/usr/lib/R";Base.include(Main,
# "/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.5/JuliaCall/julia/setup.jl") in Julia.

# Hilf mir auch nicht weiter, aber klingt danach, als ob komische Sachen in Pfaden passieren? 

# ... Leider hab ich es bis jetzt auch nicht geschafft und muss jetzt los. 
# Glaube eine ältere Julia Version neu zu installieren wäre evt noch eine Möglichkeit.

# Liebe Grüße!