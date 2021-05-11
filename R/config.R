configuration <- function(username, congFile){
    
    configInfo    <- as.data.frame(read.dcf(congFile))
    
    configAuth    <- as.character(configInfo["username"][[1]])
    configSession <- as.character(configInfo["session"][[1]])
    configserver  <- as.character(configInfo["server"][[1]])

    if(!identical(configAuth, username)){
        stop("Your account ' ", username, "' has not been logged in! 
             Please use 'loginAccount()' function to log in your account ")
    }
    
    if(is.null(configSession)){
        stop("Your account '", username, "' has not started a session!")
    }
    
    list( username = configAuth,
          session  = configSession,
          server   = configserver)

}

authConfigDir <- function(subDir) {
  
  # first check whether the main rsconnect directory exists
  config_dir <- ConfigDir(subDir =subDir, create = FALSE)
  if(!dir.exists(config_dir)){
    config_dir <- ConfigDir(subDir =subDir, create = TRUE)
  }
  
  # return the directory
  config_dir
}

ConfigDir <- function(subDir = NULL, create = TRUE) {

  # get the home directory from the operating system (in case
  # the user has redefined the meaning of ~) but fault back
  # to ~ if there is no HOME variable defined
  homeDir <- Sys.getenv("HOME", unset="~")

  # determine application config dir (platform specific)
  sysName <- Sys.info()[['sysname']]
  if (identical(sysName, "Windows"))
    configDir <- Sys.getenv("APPDATA")
  else if (identical(sysName, "Darwin"))
    configDir <- file.path(homeDir, "Library/Application Support")
  else
    configDir <- Sys.getenv("XDG_CONFIG_HOME", file.path(homeDir, ".config"))

  # append the application name and optional subdir
  configDir <- file.path(configDir, "R", "localShiny")
  if (!is.null(subDir))
    configDir <- file.path(configDir, subDir)

  # normalize path
  configDir <- normalizePath(configDir, mustWork=FALSE)

  # ensure that it exists
  if (!file.exists(configDir) && create)
    dir.create(configDir, recursive=TRUE)

  # return it
  configDir
}




