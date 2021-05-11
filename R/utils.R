toJSON <- function(x) {
  xJson <- jsonlite::toJSON(x)
  if(!is.character(xJson))
    xJson <- as.character(xJson)
  xJson <- substr(xJson, 2, nchar(xJson)-1)
  
  xJson
}

isStringParam <- function(param) {
  is.character(param) && (length(param) == 1)
}


isDocumentPath <- function(path) {
  # whether the given path points to an individual piece of content
  ext <- tolower(tools::file_ext(path))
  !is.null(ext) && ext != ""
}