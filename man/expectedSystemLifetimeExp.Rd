\name{expectedSystemLifetimeExp}
\alias{expectedSystemLifetimeExp}
\alias{expectedNetworkLifetimeExp}
\alias{expectedSignatureLifetimeExp}
\title{Compute the expected lifetime of a given system}
\description{
Computes the expected lifetime of a system/network specified by its signature or graph structure when the components have Exponential lifetime distribution with specified rate.  Useful for ordering systems/networks by expected lifetime.
}
\usage{
expectedSystemLifetimeExp(sys, rate=1)
expectedNetworkLifetimeExp(sys, rate=1)
expectedSignatureLifetimeExp(s, rate=1)
}
\arguments{
  \item{sys}{
    a \code{system} object representing the system whose types are to be set.  This should have been created by a call to \code{\link{createSystem}}.
}
  \item{s}{
    the signature vector of the system/network whose expected lifetime is to be computed.
}
  \item{rate}{
  	the rate parameter of the Exponential distribution.
  }
}
\details{
The system or network is specified by means of a \code{\link{system}} object, whereby each end of the system is denoted by nodes named \code{s} and \code{t} which are taken to be perfectly reliable.  It is easy to construct the appropriate reliability block diagram representation using the function \code{\link{createSystem}}.  Note that each physically distinct component should be separately numbered when constructing this object.

Alternatively, the signature may be provided instead (the other functions simply use the graph object to compute the signature).
}
\value{
  All the functions return a single scalar value which is the expected lifetime.
}
\references{
Samaniego, F. J. (2007), \emph{System Signatures and Their Applications in Engineering Reliability}, Springer.
}
\author{
Louis J.M. Aslett \email{louis.aslett@durham.ac.uk} (\url{https://www.louisaslett.com/})
}
\note{
Please feel free to email \email{louis.aslett@durham.ac.uk} with any queries or if you encounter errors when running this function.
}

\seealso{
\code{\link{computeSystemSignature}}
}
\examples{
# Find the expected lifetime of two component series system
expectedSystemLifetimeExp(createSystem(s -- 1 -- 2 -- t))

# Find the expected lifetime of two component series system using it's signature
# directly
expectedSignatureLifetimeExp(c(1,0))

# Find the expected lifetime of two component parallel system
expectedSystemLifetimeExp(createSystem(s -- 1:2 -- t))

# Find the expected lifetime of two component parallel system using it's
# signature directly
expectedSignatureLifetimeExp(c(0,1))
}
\keyword{ system }
\keyword{ expected lifetime }
\keyword{ signature }% __ONLY ONE__ keyword per line
