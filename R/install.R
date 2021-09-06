## ------------------------------------------------------------------
##    INSTALL
## ------------------------------------------------------------------
installApp <- function(appID, appPath=getwd()){
  
  if (!isStringParam(appID))
    stop(stringParamErrorMessage("appID"))   

  # set the directory
  appDir  <- dirAppInstall(appPath, appID)
  oldWD   <- getwd()
  on.exit(setwd(oldWD), add = TRUE)
  setwd(appDir)   

  # create connect client
  serverInfo  <- setServer()     
  client      <- connectClient(serverInfo$url)  
  
  #step1:  get APP information
  appInfoList <- infoAppInstall(appID, client)
  
  #step2: download zip file 
  appZipFile  <- fileAppInstall(appID, client)
  
  #step3: install application
  installAppLock(appZipFile)

}

installAppLock <- function(zipFile){
  
  # unzip app files under the current directory
  tryCatch(zip::unzip(zipFile, files = NULL, overwrite = TRUE, junkpaths =TRUE, exdir = "."),
    error=function(e){ print(e) } )
  
  lockfile <- getOption("lockfile")
  ## install dependencies wirten in the 'renv.lock' files
  if(!file.exists(lockfile)){
    stop("***: Lockfile 'renv.lock' does not exist. Need the lockfile to deplpy dependence packages!")
  }
  
  pkgLockfile  <- names(jsonlite::fromJSON(lockfile)$Packages)
  if("V8" %in% pkgLockfile){ Sys.setenv(DOWNLOAD_STATIC_LIBV8=1) }
  
  tryCatch(renv::restore(lockfile = lockfile, prompt = FALSE),
    error=function(e){ print(e) } )
  
  ## check omitted packages and reinstall them
  statusApp <- statusAppInstall(lockfile)
  reLoop    <- 1

  if(!statusApp && reLoop <=2){

    tryCatch(renv::restore(lockfile = lockfile, prompt = FALSE),
      error=function(e){ print(e)  } )
    
    statusApp <- statusAppInstall(lockfile)
    reLoop <- reLoop+1
  }
  
  ## report result
  if(statusApp){
    message("***: Done!")
  }else{
    message("***: Failed to install the application.")
  }
  
}

statusAppInstall <- function(lockfile){
  
  pkgInstalled <- as.data.frame(installed.packages())$Package
  pkgLockfile  <- names(jsonlite::fromJSON(lockfile)$Packages)

  # check which packages have not installed but recorded in the lockfile
  pkgOmits    <- pkgLockfile[!pkgLockfile %in% pkgInstalled]

  if(length(pkgOmits) >0){
    message("***: Start to reinstall packages omitted: \n")
    message(paste(pkgOmits, collapse = ","))
    return(FALSE)
  }else{
    return(TRUE)
  }
}

dirAppInstall  <- function(appPath, appID){

  appDir <- file.path(appPath, appID)

  # normalize path
  appDir <- normalizePath(appDir, mustWork=FALSE)

  # ensure that it exists
  if (!dir.exists(appDir))
    dir.create(appDir, recursive=TRUE)
  
  appDir
}

infoAppInstall <- function(appID, client){
  
  message("getting app infomation ...\n")
  # get the file containing app information
  infoFile <-client$infoApp(appID)
  on.exit(unlink(infoFile))

  infoList    <- jsonlite::fromJSON(infoFile)

  ## check result
  if(!infoList$result){
    stop(paste("***:", infoList$description, sep=" "))
  }
  
  if(is.null(infoList$arch)||!identical(infoList$arch, "script")){
    stop("***: The application you specified is not archived as a script file!")
  }
  message("Done! \n")
  infoList
}

fileAppInstall <- function(appID, client){

  message("getting app files ...")
  ## download app installation files.
  zipFile <-client$downloadApp(appID)

  message(paste("saved as '", zipFile, "'. \n", sep=""))
  message("Done! \n")
  zipFile
}
