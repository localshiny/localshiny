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
  #if(!url.exists(url)){
  #  #eMsg <- paste(paste("URL '", url, "' does not exist \n"), "The appID you specified dose not exist! \n Fail to obtain the APP information or APP installation files \n")
  #  eMsg <- paste("URL '", url, "' does not exist \n")
  #  stop(eMsg)
  #}
  
  f <- httr::GET(url)
  bin <- httr::content(f, "raw")
  writeBin(bin, outFile)
   
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

  # make the request
  time <- system.time(gcFirst = FALSE, tryCatch({
    if (!is.null(contentFile)) {
      # upload .zip file
      resp <- httr::POST(url, add_headers(.headers = c('Cookie'=headers$Cookie, 'Content-Type'=contentType)), 
                              body=list('info'=content, 'file'=upload_file(contentFile)))
    } else{
      # log in or log out
      resp <- httr::POST(url, httr::add_headers(.headers = unlist(headers)))
    }
  },
    error = function(e, ...) {
        stop(e)
    }))
  
  # get list of HTTP response headers 
  responHeaders  <- httr::headers(resp)

  # get HTTP response bodies
  responBodies  <- httr::content(resp)
  
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
  splitCookies <- strsplit(cookieHeaders$`set-cookie`,";")
  session      <- splitCookies[[1]][1]
  
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





