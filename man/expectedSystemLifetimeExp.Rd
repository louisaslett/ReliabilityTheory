\name{expectedSystemLifetimeExp}
\alias{expectedSystemLifetimeExp}
\alias{expectedNetworkLifetimeExp}
\alias{expectedSignatureLifetimeExp}
\title{Compute the expected lifetime of a given system}
\description{
Computes the expected lifetime of a system/network specified by its signature or graph structure when the components have Exponential lifetime distribution with specified rate.  Useful for ordering systems/networks by expected lifetime.
}
\usage{
expectedSystemLifetimeExp(g, rate=1)
expectedNetworkLifetimeExp(g, rate=1)
expectedSignatureLifetimeExp(s, rate=1)
}
\arguments{
  \item{g}{
    an \code{\link{igraph}} object representing the system or network whose expected lifetime is to be computed.  There should be two terminal 'dummy' nodes to represent either end of the structure which must be labelled "s" and "t".  They are assumed perfectly reliable.  See details and examples.
}
  \item{s}{
    the signature vector of the system/network whose expected lifetime is to be computed.
}
  \item{rate}{
  	the rate parameter of the Exponential distribution.
  }
}
\details{
The system or network can be specified by means of an \code{\link{igraph}} object, whereby each end of the system is denoted by nodes names "s" and "t" which are taken to be perfectly reliable.  It is easy to construct the appropriate graph representation using the function \code{\link{graph.formula}}.

Alternatively, the signature may be provided instead (the other functions simply use the graph object to compute the signature).
}
\value{
  All the functions return a single scalar value which is the expected lifetime.
}
\references{
Samaniego, F. J. (2007), \emph{System Signatures and Their Applications in Engineering Reliability}, Springer.
}
\author{
Louis J.M. Aslett \email{aslett@stats.ox.ac.uk} (\url{http://www.louisaslett.com/})
}
\note{
Please feel free to email \email{aslett@stats.ox.ac.uk} with any queries or if you encounter errors when running this function.
}

\seealso{
\code{\link{computeSystemSignature}}
}
\examples{
# Find the expected lifetime of two component series system
expectedSystemLifetimeExp(graph.formula(s -- 1 -- 2 -- t))

# Find the expected lifetime of two component series system using it's signature directly
expectedSignatureLifetimeExp(c(1,0))

# Find the expected lifetime of two component parallel system
expectedSystemLifetimeExp(graph.formula(s -- 1:2 -- t))

# Find the expected lifetime of two component parallel system using it's signature directly
expectedSignatureLifetimeExp(c(0,1))
}
\keyword{ system }
\keyword{ expected lifetime }
\keyword{ signature }% __ONLY ONE__ keyword per line
