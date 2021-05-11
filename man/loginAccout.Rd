\name{loginAccount}
\alias{loginAccount}
\title{Set LocalShiny Account Info}
\usage{
loginAccount(username, token) 
}
\arguments{
\item{username}{Name of account that is registered on the server}
\item{token}{User token for the account. Users can copy the token on the LocalShiny website.}
}
\description{
Configure a LocalShiny account in the current system. 
}
\examples{
    # copy the token from the website "www.findn.com:5000".
    token <- "MTYxNjgyNjY2My42MzY4NzQyLTg5NGY5ODUwZDIzZGI5OGIwNjVmNzJkZjY0MDcyZjgxOGU4MjM2MjktbmF0dHQ="
    loginAccount("python", token = token)
}
\seealso{
\code{\link[=logoutAccount]{logoutAccount()}}
}