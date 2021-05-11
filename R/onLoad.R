.onLoad <- function(libname, pkgname) {
  
  options(server = "www.findn.cn:5000")
  options( protocol = "http")
  options( path = "/api")
  
  options(renv.consent = TRUE)
  options(runShiny = "runscript.R")
}
