\name{login}
\alias{login}
\title{Login an account}

\usage{
login(username, token) 
}

\arguments{
\item{username}{Name of account that is registered on the server}
\item{token}{User token for the account. Users can copy the token on the LocalShiny web.}
}

\description{
Configure a LocalShiny account in the current system. 
}

\examples{
# copy the token from the website "localshiny.org".
mytoken <- "MTYxNjgyNjY2My42Mz"
login("python", token = mytoken)
}

\seealso{
\code{\link[=logout]{logout}}
}