\name{logoutAccount}
\alias{logoutAccount}
\title{Log out account}
\usage{
logoutAccount(username)
}
\arguments{
\item{username}{Name of account}
}
\description{
Log out the account. This function will remove the configuration files created by \code{\link[=loginAccount]{loginAccount()}}.
Note: before you log out your account, you need to log in first.
}
\examples{
    logoutAccount("python")
}
\seealso{
\code{\link[=loginAccount]{loginAccount()}}
}