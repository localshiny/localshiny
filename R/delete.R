## ------------------------------------------------------------------
##    DELETE
## ------------------------------------------------------------------
deleteApp <- function(username, appname) {

  if (!isStringParam(username))
    stop(stringParamErrorMessage("username"))  

  if (!isStringParam(appname))
    stop(stringParamErrorMessage("appname"))  
  
  # get the path to the config file
  configDir   <- authConfigDir(username)
  configFile  <- file.path(configDir , "config.dcf")

  # read login information for username
  if(!file.exists(configFile)){
    stop("Unable to determine the login information for the account named '", username, "'!
          Please use loginAccount() function to log in first!")
  }
  configAuthInfo <- configuration(username, configFile)
  
  # create connect client
  serverInfo <- setServer()     
  client     <- connectClient(serverInfo$url)
  
  # delete app 
  headers   <- list('Cookie'=configAuthInfo$session)
  deleteRequest  <- client$deleteApp(appname, headers)

  message(paste("***: ", jsonlite::fromJSON(deleteRequest$content)$description, sep=""))

  invisible(deleteRequest)
}






