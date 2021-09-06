.onLoad <- function(libname, pkgname) {
  
  options( protocol = "https")
  options( server = "www.localshiny.org")
  options( path = "/api")
  options( lockfile ="renv.lock")
  options( renv.consent = TRUE)
  options( runShiny = "runscript.R")
  
}
