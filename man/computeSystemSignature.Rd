\name{computeSystemSignature}
\alias{computeSystemSignature}
\alias{computeNetworkSignature}
\title{Compute the signature of a system}
\description{
The system signature (Samaniego, 2007) is an alternative to the structure function as a starting point for a structural reliability analysis.  This automatically computes the signature of the specified system or network.  Here, system implies components are unreliable whereas network implies links are unreliable.
}
\usage{
computeSystemSignature(sys, cutsets=NULL, frac=FALSE)
computeNetworkSignature(sys, cutsets=NULL, frac=FALSE)
}
\arguments{
  \item{sys}{
    a \code{system} object representing the system whose types are to be set.  This should have been created by a call to \code{\link{createSystem}}.
}
  \item{cutsets}{
    if the cut-sets of the system or network are already known they may be passed in as a list of numeric vectors.  This can save time because cut-set computation is the slowest part of the algorithm.  Leaving as NULL causes the function to find the cut sets itself.
}
  \item{frac}{
    if TRUE then the function prints out signature elements as fractions rather than returning a decimal signature vector.
}
}
\details{
The signature of a system is the probability vector \eqn{\mathbf{s}=(s_1, \dots, s_n)}{s=(s_1, \dots, s_n)} with elements:
\deqn{s_i = P(T = T_{i:n})}{s_i = P(T=T_i:n)}
where \eqn{T} is the failure time of the system and \eqn{T_{i:n}}{T_i:n} is the ith order statistic of the n component failure times.  Likewise the network signature is the same but where components are reliable and it is links which fail.  See Samaniego (2007) for details.

The system or network is specified by means of a \code{\link{system}} object, whereby each end of the system is denoted by nodes named \code{s} and \code{t} which are taken to be perfectly reliable.  It is easy to construct the appropriate reliability block diagram representation using the function \code{\link{createSystem}}.  Note that each physically distinct component should be separately numbered when constructing this object.
}
\value{
  \code{computeSystemSignature} returns a numeric probability vector which is the system/network signature.
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
\code{\link{computeSystemSurvivalSignature}}
}
\examples{
# Find the signature of two component series system (which is just s=(1, 0))
computeSystemSignature(createSystem(s -- 1 -- 2 -- t))

# Find the signature of two component parallel system (which is just s=(0, 1))
computeSystemSignature(createSystem(s -- 1:2 -- t))

# Find the signature of the five component 'bridge' system (which
# is s=(0, 0.2, 0.6, 0.2, 0))
computeSystemSignature(createSystem(s -- 1 -- 2 -- t, s -- 3 -- 4 -- t, 1:2 -- 5 -- 3:4))
}
\keyword{ system }
\keyword{ signature }% __ONLY ONE__ keyword per line
