\name{deployApp}
\alias{deployApp}
\title{Deploy an Application on LocalShiny web}
\usage{
deployApp(
    username,
    command = "",
    project = getwd(),
    version = NULL,
    name = NULL,
    description = NULL,
    os   = NULL) 
}
\arguments{
\item{username}{Name of account}

\item{command}{R file that needs to be specified to run the application.}

\item{project}{Directory containing application. 
Defaults to current working directory.}

\item{version}{Application version}

\item{name}{Name of application (names must be unique within an account).
Defaults to the base name of the specified \code{project}.}

\item{description}{A brief description of the application.}

\item{os}{Operation system that the application can be used. 
Defaults to the current system in your machine.}

}
\description{
Deploy an application, which can be an R package or R documents, to a server. 
This function will extract all dependencies information based on 'renv::sapshot()'
and write a 'renv.lock' file under the project directory.
}
\examples{
    deployApp("python", project="app1")
}
\seealso{
\code{\link[=updateApp]{updateApp()}}
}



