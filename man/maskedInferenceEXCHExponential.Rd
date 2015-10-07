\name{maskedInferenceEXCHExponential}
\alias{maskedInferenceEXCHExponential}
\title{Inference for Masked Exchangeable System Lifetimes, Exponential Components}
\description{
Performs Bayesian inference via a signature based data augmentation MCMC scheme for masked system lifetime data for Exponentially distributed component lifetimes.  The underlying assumption is exchangeability at the system level.
}
\usage{
maskedInferenceEXCHExponential(t, signature, iter, priorMu_Mu, priorSigma_Mu,
                               priorMu_Sigma, priorSigma_Sigma)
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
  \item{priorMu_Mu, priorSigma_Mu}{
    \eqn{\mu} and \eqn{\sigma} parameters for Log-Normal hyperprior on the mean of the exchangeable Gamma population distribution for the rate.
  }
  \item{priorMu_Sigma, priorSigma_Sigma}{
    \eqn{\mu} and \eqn{\sigma} parameters for Log-Normal hyperprior on the variance of the exchangeable Gamma population distribution for the rate.
  }
}
\details{
This is a full implementation of the signature based data augmented MCMC scheme described in Aslett (2012) for exchangeable systems with Exponential component lifetimes.

Thus, components are taken to have Exponential lifetimes where the rate of the components in any given system is a realisation from an exchangeable population distribution.  However, only the failure time of the system is observed, not those of the components or indeed which components were failed at the system failure time.  By specifying a Log-Normal hyper-prior on the exchangeable Gamma rate parameter population distribution, this function then produces MCMC samples from the posterior of the rate parameter and hyperparameters.

The model is as follows:
\deqn{Y \,|\, \lambda \sim \mbox{Exponential}(\lambda)}{Y | \lambda ~ Exp(\lambda)}
\deqn{\lambda \,|\, \nu, \zeta \sim \mbox{Gamma}(\mathrm{shape}=\nu, \mathrm{scale}=\zeta)}{\lambda | \nu, \zeta ~ Gamma(shape=\nu, scale=\zeta)}
\deqn{\mu \sim \mbox{Log-Normal}(\mu_\nu, \sigma_\nu)}{\mu ~ Log-Norm(\mu_\mu, \sigma_\mu)}
\deqn{\sigma^2 \sim \mbox{Log-Normal}(\mu_\zeta, \sigma_\zeta)}{\sigma^2 ~ Log-Norm(\mu_\sigma, \sigma_\sigma)}

Additionally, if one does not know the system design, then it is possible to pass a list of many system signatures in the \code{signature} argument, in which case the topology of the system is jointly inferred with the parameters.
}
\value{  
  If a single signature vector is provided above, then a list is returned containing two items:
  \item{parameters}{A list of data frames, each with a single column of MCMC samples from the posterior samples from the exhcangeable rate parameter for the given system.}
  \item{hyperparameters}{A data frame with a column of posterior MCMC samples for each of the Log-Normal hyperprior parameters.}

  If a list of signature vectors is provided above, then a list is returned containing three items -- the two items above, plus:
  \item{topology}{A vector of posterior samples from the discrete marginal posterior distribution of topologies provided in the signature list.}

}
\references{
Aslett, L. J. M. (2012), \emph{MCMC for Inference on Phase-type and Masked System Lifetime Models}, PhD Thesis, Trinity College Dublin.
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
# Some masked system lifetime data for an exchangeable collection of systems with
# Exponential component lifetime, rate drawn from the population distribution
# Gamma(shape=9, scale=0.5)
t <- c(0.2265, 0.0795, 0.1178, 0.2463, 0.1053, 0.0982, 0.0349, 0.0363, 
0.1546, 0.1357, 0.1239, 0.0354, 0.0124, 0.1003, 0.0827, 0.2446, 
0.1214, 0.1272, 0.5438, 0.2738, 0.0378, 0.2293, 0.1706, 0.0146, 
0.1506, 0.3665, 0.046, 0.1196, 0.2724, 0.2593, 0.0438, 0.1493, 
0.0697, 0.1774, 0.1157, 0.0996, 0.2815, 0.1411, 0.0921, 0.2088, 
0.1164, 0.149, 0.048, 0.1019, 0.2349, 0.2211, 0.0475, 0.0721, 
0.0371, 0.611, 0.1959, 0.0666, 0.0956, 0.1416, 0.2126, 0.0104, 
0.088, 0.0159, 0.078, 0.1747, 0.1921, 0.3558, 0.4956, 0.0436, 
0.2292, 0.1159, 0.1201, 0.1299, 0.043, 0.0584, 0.0347, 0.2084, 
0.1334, 0.1491, 0.0384, 0.0589, 0.2962, 0.1023, 0.0506, 0.0501, 
0.1859, 0.0714, 0.1424, 0.0027, 0.2812, 0.0318, 0.4147, 0.1088, 
0.2894, 0.0734, 0.1405, 0.0367, 0.0323, 0.517, 0.1034, 0.026, 
0.0485, 0.0512, 0.0116, 0.1629)

# Load the signatures of order 4 simply connected coherent systems -- the data
# above correspond to simulations from system number 3
data(sccsO4)

# Perform inference on the rate parameter:
\dontrun{samps <- maskedInferenceEXCHExponential(t, sccsO4[[3]]$signature, 
2000, priorMu_Mu=1, priorSigma_Mu=0.5, priorMu_Sigma=1, priorSigma_Sigma=0.7)}

# Or perform inference on rate parameter and topology jointly, taking as candidate
# set all possible simply connected coherent systems of order 4:
\dontrun{samps <- maskedInferenceEXCHExponential(t, sccsO4, 2000, priorMu_Mu=1,
priorSigma_Mu=0.5, priorMu_Sigma=1, priorSigma_Sigma=0.7)}
}
\keyword{ masked system lifetime model }
\keyword{ Exponential }
\keyword{ exchangeable }
\keyword{ bayesian inference }
\keyword{ data augmentation }
\keyword{ signature }% __ONLY ONE__ keyword per line
