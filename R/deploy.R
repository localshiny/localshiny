## ------------------------------------------------------------------
##    DEPLOY
## ------------------------------------------------------------------
deployApp <- function(username,
                      project = getwd(),
                      upload =TRUE,
                      version = NULL,
                      name = NULL,
                      description = NULL,
                      os   = NULL) {
  if (!isStringParam(username))
    stop(stringParamErrorMessage("username"))  
  
  if (!isStringParam(project))
    stop(stringParamErrorMessage("project"))
  
  if(isDocumentPath(project)){
    deployDoc(username, 
              project, 
              upload =upload,
              version = version, 
              name = name, 
              description=description, 
              os=os )
  }else{
    deployDir(username,
              project,
              project, 
              upload =upload,
              version = version, 
              name = name, 
              description=description, 
              os=os,
              script = getOption("runShiny"))
  }
  
}


deployDoc <- function(username, 
                      document, 
                      upload=TRUE,
                      version = NULL,
                      name = NULL,
                      description = NULL,
                      os   = NULL){
  
  appdir <- tempfile(pattern="localshiny")
  dir.create(appdir)
  on.exit(unlink(appdir, recursive=TRUE))
  file.copy(document, appdir)  ## failed
  
  name <- if(!is.null(name)) name else gsub(".R", "", basename(document))
  
  tryCatch({
    deployDir(username,
            appdir,
            document,
            upload=upload,
            version = version, 
            name = name, 
            description=description, 
            os=os,
            script = document) 
  },error = function(e, ...) {
        stop(e)
    })
  
}


deployDir <- function(username,
                      appdir,
                      project,
                      upload=TRUE,
                      version = NULL,
                      name = NULL,
                      description = NULL,
                      os   = NULL,
                      script = getOption("runShiny")){
  if(isDocumentPath(project)){
    projectDir=dirname(project)
  }
  else
  {
    projectDir=project
  }
  # normalize project path and ensure it contains files
  appdir <- normalizePath(appdir,winslash ="/", mustWork = FALSE)
  projectDir <- normalizePath(projectDir,winslash = "/", mustWork = FALSE)
  if (!file.exists(appdir)) {
    stop(appdir, "contains no files")
  }   
  
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
  
  assignBrowseURL=c("library('utils');","assignInNamespace('browseURL',browseURL,ns='utils');")
  browseCmd="browseURL<-function (url,broser=getOption('browser'),encodeIfNeeded=FALSE){}"
  dirRunCmd=c(assignBrowseURL,"library(shiny);","runApp();",browseCmd)
  docRunCmd=c(assignBrowseURL,browseCmd)
  
  # if we snapshot a shiny application, write a script to run it
  if(identical(script, getOption("runShiny"))){
    script <- file.path(".", getOption("runShiny"))
    writeLines(dirRunCmd, paste(appdir,script,sep=''))

  }
  else{
    appfile=list.files(project,pattern="*.R",full.names = T)
    script <- file.path(paste(appdir,basename(project),sep="/"))
    docRunCmd=c(docRunCmd,readLines(script))
    writeLines(docRunCmd, script)
  }

  # zip up the project 
  appFile <- lockAppDeploy(appdir)
  if(projectDir!=appdir)
  {
      file.copy(paste(appdir,"renv.lock",sep='/'),paste(projectDir,"renv.lock",sep='/'))
  }
  #on.exit(unlink(appFile))
  
  
  # get app information
  
  
  # upload app .tar file
  message("uploading the R Shiny application files ...\n")
  headers   <- list('Cookie'=configAuthInfo$session)
  if (upload==TRUE)
  {
    appInfo    <- infoAppDeploy(project, script, name, description, version, os)
    uploadRequest  <- client$uploadApp(appInfo, appFile, headers)
    message(paste("***: ", uploadRequest$content$description, sep=""))
    invisible(uploadRequest)
  }
  else{
    appInfo    <- infoXmlDeploy(appdir, script,username, name, description, version, os)
    getXmlRequest  <- client$getXml(appInfo,headers)
    message(paste("***: ", getXmlRequest$headers$'content-disposition', sep=""))
    invisible(getXmlRequest)
    unlink(paste(paste(appdir,basename(project),sep='/'),'.zip',sep=''))
    appID <- strsplit(getXmlRequest$headers$'content-disposition','filename=')[[1]][2]
    xmlFile=paste(appdir,appID,sep="/")
    xml2::write_xml(getXmlRequest$bodies,file=xmlFile)
    zipFile=paste(paste(projectDir,strsplit(appID,'.xml')[[1]][1],sep='/'),".zip",sep = "")
    
    rFileList=list.files(appdir,pattern="*.R",full.names = T)
    lockFileList=list.files(appdir,pattern="*.lock",full.names = T)
    fileList=c(rFileList,lockFileList,xmlFile)
    zipr(zipfile = zipFile,files =fileList,root=projectDir)
    unlink(xmlFile)
  }
  unlink(paste(project,"renv.lock",sep="/"))
  
}

lockAppDeploy <- function(appDir){
  
  oldWD <- getwd()
  on.exit(setwd(oldWD), add = TRUE)
  setwd(appDir)  
  message("extracting environment settings of the project...\n")
  #capture the state of a project's R package dependencies and create a lockfile, "renv.lock".
  #The lockfile can be used to later restore these project's dependencies as required.
  
  renv::settings$package.dependency.fields(c("Imports", "Depends", "LinkingTo"))
  renv::snapshot(project = ".", prompt = FALSE)
  
  # remove renv setting file
  if(dir.exists("renv")){
    unlink("renv", recursive=TRUE)
  }  
  
  # check snapshot results
  if(!file.exists("renv.lock")){
    stop("This project has not yet been snapshotted. Lockfile does not exist!")
  }
  
  # archive all files under the current directory
  message("compressing the R Shiny application files ...\n")
  appZipFile <- paste(basename(appDir), "zip", sep=".")
  zip::zipr(appZipFile, ".")
  
  normalizePath(appZipFile)
}

infoAppDeploy <- function(appDir, appRunScript, appName = NULL, appDesc=NULL, appVersion=NULL, appOs = NULL){
  
  pakname <- if(!is.null(appName)) appName else basename(appDir)
  version <- if(!is.null(appVersion)) appVersion else "1.0"
  pakdesc <- if(!is.null(appDesc)) appDesc 
              else paste( "This is an application named '", pakname, "' !",sep = "")
  os      <- if(!is.null(appOs)) appOs  
              else  paste(Sys.info()['sysname'], Sys.info()['release'], sep='')
  rversion <- paste(R.Version()['major'], R.Version()['minor'], sep='.')       
  
  list( pakname = pakname,
        version = version, 
        pakdesc = pakdesc, 
        os      = os,
        rversion = rversion,
        runcmd  = basename(appRunScript) )
}

infoXmlDeploy <- function(appDir, appRunScript,authorname,appName = NULL, appDesc=NULL, appVersion=NULL, appOs = NULL){
  info=infoAppDeploy(appDir, appRunScript, appName, appDesc, appVersion, appOs)
  list( pakname = info[1],
        authorname=authorname,
        version = info[2], 
        pakdesc = info[3], 
        os      = info[4],
        rversion = info[5],
        runcmd  = info[6] )
}

