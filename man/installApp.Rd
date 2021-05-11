\name{installApp}
\alias{installApp}
\title{Install applications}
\usage{
installApp(appID, appPath=getwd())
}
\arguments{
\item{appID}{ID of an application. You can copy the appID from LocalShiny website.}

\item{appPath}{path under which the application files will be downloaded.
Defaults to current working directory.}
}
\description{
Install the application. This function depends on the function 'renv::restore()' to install all dependencies. 
}
\examples{
    # copy the appID from the LocalShiny website.
    appID <- "idSVehNS"
    installApp(appID)
}
