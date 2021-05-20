\name{logout}
\alias{logout}
\title{Log out an account}

\usage{
logoutAccount(username)
}

\arguments{
\item{username}{Name of account}
}

\description{
Log out the account. This function will remove the configuration files created by \code{\link[=login]{login()}}.
Note: before you log out your account, you need to log in first.
}

\examples{
logout("python")
}

\seealso{
\code{\link[=login]{login()}}
}