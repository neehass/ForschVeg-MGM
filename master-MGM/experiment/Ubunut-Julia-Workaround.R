# Ubunut-Julia-Workaround

.libPaths(c("/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.2", "/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.5")) # packages Path


Sys.setenv(LD_LIBRARY_PATH = "/opt/julia-1.12.1/lib:$LD_LIBRARY_PATH")
system("/opt/julia-1.12.1/bin/julia --version")

library(JuliaCall)

Sys.setenv(JULIA_HOME = "/opt/julia-1.12.1/bin")
julia_setup(installJulia = FALSE)
julia_command("2 + 2")


library(JuliaCall)
# Define the helper function
julia_exec <- function(code) {
  julia_cmd <- "/opt/julia-1.12.1/bin/julia"
  out <- system(paste(julia_cmd, "-e", shQuote(code)))
  return(out)
}

# Now call it
julia_exec("println(2 + 2)")

