connectClient <- function(service){
  service <- parseHttpUrl(service)
  # #headers     = toJSON(as.data.frame(authInfo)))
  list(
    authLogin = function(headers){

      POST(service, 
           "login", 
           headers = headers)
          
    },

    authLogout = function(headers){
      POST(service, 
           "logout", 
           headers = headers)
    },

    uploadApp = function(appInfo, appZipFile, headers){   
      POST( service, 
            "upload", 
            headers     = headers,
            content     = toJSON(as.data.frame(appInfo)), 
            contentFile = appZipFile,
            contentType = "multipart/form-data")
    },

    deleteApp = function(appName, headers){   
      POST( service, 
            paste("delete", appName, sep="/"),
            headers     = headers)
    },
    
    downloadApp = function(appID){
      GET( service, "file", appID)
    },

    infoApp = function(appID){
      GET( service, "info", appID)
    }

  ) 
}

## ------------------------------------------------------------------
##    POST
## ------------------------------------------------------------------
POST <- function(service,
                 path,
                 headers = list(),
                 content = NULL,
                 contentFile = NULL,
                 contentType = NULL) {
               
  # check if the request needs a body
  if (is.null(contentFile)) {
      # no file, don't include a body with the request
      httpPost(service, path, 
               headers     = headers,
               content     = content, 
               contentType = contentType)
  } else {
      # include the request's data in the body
      #Content-Type: multipart/form-data
      httpPostFile(service, path, 
                   headers = headers,
                   content = content, 
                   contentFile = contentFile, 
                   contentType = contentType)
  }
}

httpPost <- function(service,
                        path,
                        headers     = list(),
                        content     = NULL,
                        contentType = NULL ) {
                      
  ## used to log in or log out                        
  # prepend the service path
  urlPath <- paste(service$path, path, sep="/")  

  # perform request
  httpPostRCurl(service$protocol,
       service$host,
       service$port,
       urlPath,
       headers = headers,
       content = content,
       contentType = contentType)
}

httpPostFile <- function(service,
                            path,
                            headers     = list(),
                            content     = NULL,
                            contentFile = NULL,
                            contentType = NULL) {
  ## used to upload files    
  # prepend the service path
  urlPath <- paste(service$path, path, sep="/")  
  
  if (is.null(contentFile))
    stop("You must specify application files")

  # perform request
  httpPostRCurl(service$protocol,
       service$host,
       service$port,
       urlPath,
       headers,
       content = content,
       contentFile = contentFile,
       contentType = contentType)
  
}

## ------------------------------------------------------------------
##    GET
## ------------------------------------------------------------------
GET <- function(service, path, appID){
  
  outFile <- NULL
  if(identical(path, "file")){
    outFile <- paste("app_", appID, ".zip", sep="")  
  }else if(identical(path, "info")){
    outFile <- paste("app_", appID, ".info", sep="")  
  }
  
  path <- paste("package",path, appID, sep="/")  
  
  httpGet(service, 
          path,
          outFile)  

}

httpGet <- function(service,
                    path,
                    outFile ) {
                     
  # prepend the service path
  urlPath <- paste(service$path, path, sep="/")  
  
  # perform request
  httpGetRCurl( service$protocol,
                service$host,
                service$port,
                urlPath,
                outFile)
}