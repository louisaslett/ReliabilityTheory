\name{maskedInferenceIIDExponential}
\alias{maskedInferenceIIDExponential}
\title{Inference for Masked iid System Lifetimes, Exponential Components}
\description{
Performs Bayesian inference via a signature based data augmentation MCMC scheme for masked system lifetime data for Exponentially distributed component lifetimes.  The underlying assumption is iid components and iid systems.
}
\usage{
maskedInferenceIIDExponential(t, signature, iter, priorShape, priorScale)
}
\arguments{
  \item{t}{
    a vector of masked system lifetimes.
  }
  \item{signature}{
    the signature vector of the system/network for which inference is performed.  It may be a list of signatures which results in topological inference on the system design being jointly performed over the collection of signatures provided.
  }
  \item{iter}{
    number of MCMC iterations to perform.
  }
  \item{priorShape}{
    the shape parameter of the Gamma prior of the Exponential rate.
  }
  \item{priorScale}{
    the scale parameter of the Gamma prior of the Exponential rate.
  }
}
\details{
This is a full implementation of the signature based data augmented MCMC scheme described in Aslett (2012) for iid systems with Exponential component lifetimes.

Thus, components are taken to have Exponential lifetimes and be arranged into some system.  However, only the failure time of the system is observed, not those of the components or indeed which components were failed at the system failure time.  By specifying a Gamma prior distribution on the component lifetime Exponential rate parameter via \code{priorShape} and \code{priorScale}, this function then produces MCMC samples from the posterior of the rate parameter.

Additionally, if one does not know the system design, then it is possible to pass a list of many system signatures in the \code{signature} argument, in which case the topology of the system is jointly inferred with the parameters.
}
\value{
  If a single signature vector is provided above, then a data frame with a single column of MCMC samples from the posterior of the rate parameter are returned.
  
  If a list of signature vectors is provided above, then a list is returned containing two items:
  \item{topology}{A vector of posterior samples from the discrete marginal posterior distribution of topologies provided in the signature list.}
  \item{parameters}{A data frame with a single column of MCMC samples from the posterior of the rate parameter.}
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

\seealso{
\code{\link{computeSystemSignature}}
}
\examples{
# Some masked system lifetime data for a system with Exponential component
# lifetime, rate=3.14
t <- c(0.2696, 0.3613, 0.0256, 0.1287, 0.2305, 0.1565, 0.2484, 0.7482, 
0.1748, 0.1805, 0.1985, 0.0799, 0.2843, 0.2392, 0.2151, 0.1177, 
0.1278, 0.4189, 0.4374, 0.0931, 0.2846, 0.0357, 0.1809, 0.2077, 
0.5211, 0.4935, 0.1464, 0.0297, 0.5429, 0.1294, 0.7089, 0.5534, 
0.1183, 0.2628, 0.0481, 0.0518, 0.0533, 0.3595, 0.0767, 0.2606, 
0.1005, 0.227, 0.01, 0.0947, 0.1248, 0.2288, 0.1422, 0.233, 0.1428, 
0.2043)

# Load the signatures of order 4 simply connected coherent systems -- the data
# above correspond to simulations from system number 3
data(sccsO4)

# Perform inference on the rate parameter:
\dontrun{samps <- maskedInferenceIIDExponential(t, sccsO4[[3]]$signature, 2000,
priorShape=9, priorScale=0.5)}

# Or perform inference on rate parameter and topology jointly, taking as candidate
# set all possible simply connected coherent systems of order 4:
\dontrun{samps <- maskedInferenceIIDExponential(t, sccsO4, 2000, priorShape=9,
priorScale=0.5)}
}
\keyword{ masked system lifetime model }
\keyword{ Exponential }
\keyword{ iid }
\keyword{ bayesian inference }
\keyword{ data augmentation }
\keyword{ signature }% __ONLY ONE__ keyword per line
