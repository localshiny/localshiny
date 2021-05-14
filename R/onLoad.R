.onLoad <- function(libname, pkgname) {
  
  options( server = "www.localshiny.org")
  options( protocol = "https")
  options( path = "/api")
  
  options(renv.consent = TRUE)
  options(runShiny = "runscript.R")
}
