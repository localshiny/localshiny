## ------------------------------------------------------------------
##    GET
## ------------------------------------------------------------------
httpGetRCurl <- function( protocol,
                          host,
                          port,
                          urlPath,
                          outFile) {

  # add prefix to port if necessary
  if (!is.null(port) && nzchar(port))
    port <- paste(":", port, sep="")
  
  # build url
  url <- paste(protocol, "://", host, port, urlPath, sep="")
  if(!url.exists(url)){
    stop("***: The appID you specified dose not exist! 
               Fail to obtain the APP information or APP installation files")
  }
  
  # download files
  f = RCurl::CFILE(outFile, mode="wb")
  RCurl::curlPerform(url=url, writedata = f@ref)
  RCurl::close(f)   
  
  normalizePath(outFile)

}

## ------------------------------------------------------------------
##    POST
## ------------------------------------------------------------------
httpPostRCurl <- function(protocol,
                     host,
                     port,
                     urlPath,
                     headers = list(),
                     content = NULL,
                     contentFile = NULL,
                     contentType = NULL) {
                       
  # HTTP transport using the curl command-line utility. Useful on systems that have a working curl
  
  if (!is.null(content) && is.null(contentType))
    stop("You must specify a contentType for the specified content")
  
  # add prefix to port if necessary
  if (!is.null(port) && nzchar(port))
    port <- paste(":", port, sep="")
  
  # build url
  url <- paste(protocol, "://", host, port, urlPath, sep="")
  
  # establish options
  options        <- RCurl::curlOptions(url)

  headerGatherer <- RCurl::basicHeaderGatherer()
  textGatherer   <- RCurl::basicTextGatherer(.mapUnicode = FALSE)
  options$headerfunction <- headerGatherer$update  
  options$writefunction  <- textGatherer$update  
  
  # make the request
  time <- system.time(gcFirst = FALSE, tryCatch({
    if (!is.null(contentFile)) {
      # upload .zip file
      headers$'Content-Type' <- contentType
      options$httpheader     <- headers
      RCurl::postForm(url,
         .opts  = options,
         "file" = fileUpload(filename = contentFile),
         "info" = content)

    } else{
        # log in or log out
        options$customrequest  <- "POST"
        RCurl::curlPerform(url=url, 
                          .opts = options, 
                          httpheader=unlist(headers))
      }
  },
    error = function(e, ...) {
        stop(e)
    }))
  
  # get list of HTTP response headers 
  responHeaders  <- headerGatherer$value()

  # get HTTP response bodies
  responBodies  <- textGatherer$value()

  # read HTTP response
  req = list(protocol  = protocol,
              host     = host,
              port     = port,
              path     = urlPath)
  
  readHttpResponse(req, responHeaders, responBodies)

}

readHttpResponse <- function(req, headers, bodies) {

  names(headers) <- tolower(names(headers))
  status      <- parseHttpStatus(headers)
  contentType <- parseHttpContentType(headers)
  sessionCode <- parseHttpSession(headers)

  list( req     = req,
        status  = status,
        session = sessionCode,
        content = bodies,
        contentType = contentType) 
}

parseHttpStatus <- function(headers){

  # deduce status.
  status <- 200
  statuses <- headers[names(headers) == "status"]  
  if (length(statuses) > 0) {
    # we found a numeric status header
    status <- as.integer(statuses[[1]])
  }
  
  status
}

parseHttpContentType <- function(headers){

  # presume a plain text response unless specified otherwise
  contentType <- if ("content-type" %in% names(headers)) {
    headers[["content-type"]]
  } else {
    "text/plain"
  }

  contentType
}

parseHttpSession <- function(headers){
  
  # Parse cookies from header; 
  ######bear in mind that there may be multiple headers
  cookieHeaders <- headers[names(headers) == "set-cookie"]
  
  # extract session code in the set-cookies
  splitCookies <- strsplit(cookieHeaders,";")
  session      <- splitCookies$`set-cookie`[1]
  
  if (is.null(session))
    return(-1)
  else
    return(as.character(session))

}

parseHttpUrl <- function(urlText) {
  
  matches <- regexec("(http|https)://([^:/#?]+)(?::(\\d+))?(.*)", urlText)
  components <- regmatches(urlText, matches)[[1]]
  if (length(components) == 0)
    stop("Invalid url: ", urlText)
  
  url <- list()
  url$protocol <- components[[2]]
  url$host <- components[[3]]
  url$port <- components[[4]]
  url$path <- components[[5]]
  url
}





