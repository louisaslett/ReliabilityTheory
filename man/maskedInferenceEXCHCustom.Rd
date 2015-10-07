\name{maskedInferenceEXCHCustom}
\alias{maskedInferenceEXCHCustom}
\title{Inference for Masked Exchangeable System Lifetimes, Custom Distribution}
\description{
Performs Bayesian inference via a signature based data augmentation MCMC scheme for masked system lifetime data for any custom component lifetime distribution.  The underlying assumption is of exchangeability at the system level (iid components within each exchangeable system).
}
\usage{
maskedInferenceEXCHCustom(t, signature, cdfComp, pdfComp, rParmGivenData,
                          rCompGivenParm, startCompParm, startHypParm, iter, ...)
}
\arguments{
  \item{t}{
    a vector of masked system lifetimes.
  }
  \item{signature}{
    the signature vector of the system/network for which inference is performed.  It may be a list of signatures which results in topological inference on the system design being jointly performed over the collection of signatures provided.
  }
  \item{cdfComp}{
  	user-defined vectorised cumulative distribution function of component lifetime \eqn{F_Y()}{F(y)} with prototype:
  	\code{function(y, parametersm, ...)}
  }
  \item{pdfComp}{
  	user-defined vectorised probability distribution function of component lifetime \eqn{f_Y()}{f(y)} with prototype:
  	\code{function(y, parameters, ...)}
  }
  \item{rParmGivenData}{
  	user-defined function which should produce random draws from \eqn{f_{\Xi\,|\,Y}}{f(\Xi | Y)} with prototype:
  	
  	\code{function(y, ...)}
  	
  	This must return the new parameters as a 2 item list: the first item being the hyperprior parameters drawn in the same named vector format and order as \code{startHypPriorParm}; the second item being a list of component lifetime parameters drawn in the same named vector format and order as \code{startCompParm}.
  }
  \item{rCompGivenParm}{
  	user-defined function which should produce random draws from \eqn{f_{Y\,|\,\Psi}}{f(Y | \Psi)} with prototype:
  	
  	\code{function(parameters, t, censoring, ...)}
  	
  	where censoring is -1 for left censoring, 0 for exact observations and 1 for right censoring.
  }
  \item{startCompParm}{
    list consisting of a vectors of starting values per system (in the same order as \code{t}) of named parameters for the component lifetime distribution.  The order within each named vector should match the order expected for the \code{parameters} argument in the user defined functions above.
  }
  \item{startHypParm}{
    vector of starting values of named hyper-parameters for the hyperprior.
  }
  \item{iter}{
    number of MCMC iterations to perform.
  }
  \item{...}{
  	additional arguments which are passed through to the user-defined functions above.
  }
}
\details{
This is a low level implementation of the signature based data augmented MCMC scheme described in Aslett (2012) for exchangeable systems.  This function need only be used if the component lifetime distribution of interest has not already been implemented within this package.

The arguments of the function are the prerequisites described in Algorithm 6.2 of Aslett (2012).  The interested user is advised to inspect the source code of this package at the file \code{MaskedLifetimeInference_Exponential.R} for an example of its usage, which may be seen in the function \code{\link{maskedInferenceEXCHExponential}} defined there, together with the associated user-definied functions above it.
}
\value{
  If a single signature vector is provided above, then a data frame of MCMC samples with columns named the same as the \code{startParm} argument is returned.
  
  If a list of signature vectors is provided above, then a list is returned containing three items:
  \item{topology}{A vector of posterior samples from the discrete marginal posterior distribution of topologies provided in the signature list.}
  \item{parameters}{A list of data frames of MCMC samples with columns named the same as the \code{startCompParm} argument.}
  \item{hyperparameters}{A data frame of MCMC samples with columns named the same as the \code{startHypPriorParm} argument.}
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
# Please inspect the source of this package, file MaskedLifetimeInference_Exponential.R
# for example usage (see details section)
}
\keyword{ masked system lifetime model }
\keyword{ bayesian inference }
\keyword{ data augmentation }
\keyword{ signature }% __ONLY ONE__ keyword per line
