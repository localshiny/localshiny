setServer <- function() {
  url    <- paste( getOption("protocol"), 
                   "://", 
                   getOption("server"), 
                   getOption("path"), sep="")
  info   <- list(name = getOption("server"), url = url)

  info
}

