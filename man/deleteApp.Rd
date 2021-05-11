\name{deleteApp}
\alias{deleteApp}
\title{Delete an Application deployed on the LocalShiny web}
\usage{
deleteApp(username, appID)
}
\arguments{
\item{username}{Name of account}

\item{appID}{the ID of application. You can copy the appID from the LocalShiny web.}

}
\description{
Delete an application in users' own repository. 
}
\examples{
    # username : python
    # appID : "JbmxDXhe"; copied from the LocalShiny web.
    deleteApp("python", "JbmxDXhe")
}