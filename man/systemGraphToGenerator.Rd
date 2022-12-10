\name{systemGraphToGenerator}
\alias{systemGraphToGenerator}
\title{Construct a Continuous-time Markov Chain Generator}
\description{
This function enables easy construction of an absorbing continuous-time Markov chain generator matrix representation for a system when components are treated as having Exponential failure and repair times.
}
\usage{
systemGraphToGenerator(g, failRate, repairRate)
}
\arguments{
  \item{g}{
    an \code{\link{igraph}} object representing the system or network whose generator matrix representation is to be computed.  There should be two terminal 'dummy' nodes to represent either end of the structure which must be labelled "s" and "t".  They are assumed perfectly reliable.  See details and examples.
}
  \item{failRate}{
    the rate parameter of the Exponentially distributed lifetime distribution of the components.
}
  \item{repairRate}{
    the rate parameter of the Exponentially distributed repair time distribution of the components.
}
}
\details{
When the system or network is specified by means of an \code{\link{igraph}} object, each end of the system must be denoted by nodes named "s" and "t" which are taken to be perfectly reliable.  It is easy to construct the appropriate graph representation using the function \code{\link{graph.formula}}.

This function then creates the generator matrix for an absorbing continuous-time Markov chain representation of such a system where components are repairable.  All system states which in which the system is inoperative are collapsed into the absorbing state.

The returned values are in the format required by the \code{\link{phtMCMC2}}.%, \code{\link{maskedInferenceIIDPhaseType}}, and \code{\link{maskedInferenceEXCHPhaseType}} functions making their use even with big systems very easy.

Full details are in Aslett (2012).
}
\value{
  A list is returned with both a numeric generator matrix (in \code{$G} with the failure rate, \code{failRate}, and repair rate, \code{repairRate}) and a symbolic matrix (in \code{$structure$G}), along with a matrix of the constant multiples of generator entries (in \code{$structure$C}).
}
\references{
Aslett, L. J. M. (2012), \emph{MCMC for Inference on Phase-type and Masked System Lifetime Models}, PhD Thesis, Trinity College Dublin.
}
\author{
Louis J.M. Aslett \email{louis.aslett@durham.ac.uk} (\url{https://www.louisaslett.com/})
}
\note{
Please feel free to email \email{louis.aslett@durham.ac.uk} with any queries or if you encounter errors when running this function.
}

%\seealso{
%\code{\link{computeNetworkSignature}}
%}
\examples{
# Get the generator representing a repairable 5 component 'bridge' system with
# failure rate 1 and repair rate 365.
data(sccsO5)
G <- systemGraphToGenerator(sccsO5[[18]]$graph, 1, 365)
}
\keyword{ generator matrix }
\keyword{ system }
\keyword{ signature }% __ONLY ONE__ keyword per line
