login <- function(username, token) {

  if (!isStringParam(username))
    stop(stringParamErrorMessage("username"))

  if (!isStringParam(token))
    stop(stringParamErrorMessage("token"))
  
  # create connect client
  serverInfo <- setServer()     
  client     <- connectClient(serverInfo$url)

  # log in
  token     <- paste(username, token, sep="-")
  headers   <- list(token=token)
  loginResponse  <- client$authLogin(headers)
  
  message(paste("***: ", loginResponse$content$description, sep=""))
  
  # write configuration file
  if(loginResponse$content$result==1){
    # get the path to the config file
    configDir   <- authConfigDir(username)
    configFile  <- file.path(configDir , "config.dcf")
    
    # write the user info and session code
    write.dcf(list(username  = username,
                  token     = token,
                  status    = loginResponse$status,
                  session   = loginResponse$session,
                  server    = serverInfo$name,
                  url       = serverInfo$url),
              configFile,
              width = 100)
  }

  invisible(loginResponse)
}

logout <- function(username) {

  if (!isStringParam(username))
    stop(stringParamErrorMessage("username"))
  
  # get the path to the config file
  configDir   <- authConfigDir(username)
  configFile  <- file.path(configDir , "config.dcf")
  
  # read login information for username
  if(!file.exists(configFile)){
    stop("Unable to determine the login information for the account named '", username, "'!
          Please use loginAccount() function to log in first!")
  }else{
    configAuthInfo <- configuration(username, configFile)
    on.exit(file.remove(configFile))
  }
  
  # create connect client
  serverInfo <- setServer()     
  authInfo   <- list(username = configAuthInfo$username)
  client     <- connectClient(serverInfo$url)
  
  # log out
  headers    <- list('Cookie'=configAuthInfo$session)
  logoutResponse  <- client$authLogout(headers)
  message(paste("***: ", logoutResponse$content$description, sep=""))
  
  invisible(logoutResponse)
}



