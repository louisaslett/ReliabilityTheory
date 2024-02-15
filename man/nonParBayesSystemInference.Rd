\name{nonParBayesSystemInference}
\alias{nonParBayesSystemInference}
\title{Non-parametric Bayesian posterior predictive system survival inference}
\description{
Computes a non-parametric Bayesian posterior predictive survival probability
given the survival signature of a system and test data on each of the
components as described in Aslett \emph{et al} (2015).
}
\usage{
nonParBayesSystemInference(at.times, survival.signature, test.data, alpha=1, beta=1)
}
\arguments{
  \item{at.times}{
    a vector of times at which the posterior predictive estimate of survival
    probability should be computed.
  }
  \item{survival.signature}{
    the survival signature matrix of the system/network for which inference is
    performed.  This should be in the same format as returned by
    \code{\link{computeSystemSurvivalSignature}}.
  }
  \item{test.data}{
    a list of vectors containing the component test data.  The elements of the
    list should be named identically to the component columns in the
    \code{survival.signature} argument.
  }
  \item{alpha, beta}{
    the Beta prior shape parameters.  Each must match in type and can be:
    \itemize{
      \item a single scalar for a fixed prior across time and component types;
      \item a vector of parameters of the same length as the \code{at.times}
        argument, which indicates the time-varying prior parameter at the
        corresponding time in \code{at.times}.  This is therefore time-varying,
        but indicates the same time-varying prior for all component types;
      \item a data frame where each column is named using the same names as for
        the \code{survival.signature} argument and each row corresponds to the
        time-varying prior parameter at the corresponding time in
        \code{at.times}.
    }
    By default the 'uninformative' prior with \code{alpha=1} and \code{beta=1}
    is used for all components at all times.
  }
}
\details{
This function implements the technique described in detail in Section 4 of Aslett \emph{et al} (2015).

In brief, at any fixed time \eqn{t}, the functioning of a single component of type \eqn{k} is Bernoulli(\eqn{p_t^k}) distributed for suitable \eqn{p_t^k}, irrespective of the lifetime distribution of the component.  Correspondingly, the distribution of the number of components still functioning at time \eqn{t} in a collection of \eqn{n_k} iid components of type \eqn{k} is Binomial(\eqn{n_k, p_t^k}).

Taking the priors \eqn{p_t^k \sim} Beta(\eqn{\alpha_t^k, \beta_t^k}), Aslett \emph{et al} (2015) show that this leads to a posterior predictive survival distribution with a nice closed form (see equations 9 and 10 in Section 4).
}
\value{
  A vector of the same length as the \code{at.times} argument, where each element is the posterior predictive probability of a new system surviving to the corresponding time in \code{at.times}.
}
\references{
Aslett, L. J. M., Coolen, F. P. A. and Wilson, S. P. (2015), \sQuote{Bayesian Inference for Reliability of Systems and Networks using the Survival Signature}, \emph{Risk Analysis} \bold{39}(9), 1640--1651.  \href{https://www.louisaslett.com/Papers/Aslett_Coolen_Wilson_2014.html}{Download paper}
}
\author{
Louis J. M. Aslett \email{louis.aslett@durham.ac.uk} (\url{https://www.louisaslett.com/})
}
\note{
Please feel free to email \email{louis.aslett@durham.ac.uk} with any queries or if you encounter errors when running this function.
}

\seealso{
\code{\link{computeSystemSurvivalSignature}}
}
\examples{
## Exactly reproduce the example in Section 4.1 of Aslett et al (2015), including Figure 5
# First specify the system layout, numbered as per Figure 4
sys <- createSystem(s -- 1 -- 2:4:5, 2 -- 3 -- t, 4:5 -- 6 -- t,
                    s -- 7 -- 8 -- t, s -- 9 -- 10 -- 11 -- t, 7 -- 10 -- 8,
                    types = list(T1 = c(1, 6, 11),
                                 T2 = c(2, 3, 9),
                                 T3 = c(4, 5, 10),
                                 T4 = c(7, 8)))

# Compute the survival signature table from Appendix
sig <- computeSystemSurvivalSignature(sys)

# Simulate the test data (same seed as used in the paper)
set.seed(233)
t1 <- rexp(100, rate=0.55)
t2 <- rweibull(100, scale=1.8, shape=2.2)
t3 <- rlnorm(100, 0.4, 0.9)
t4 <- rgamma(100, scale=0.9, shape=3.2)

# Compile into a list as required by this function
test.data <- list("T1"=t1, "T2"=t2, "T3"=t3, "T4"=t4)

# Create a vector of times at which to evaluate the posterior predictive
# survival probability and compute using this function
t <- seq(0, 5, length.out=300)
yS <- nonParBayesSystemInference(t, sig, test.data)

# Compute the survival curves for the individual components (just to match
# Figure 5)
y1 <- sapply(t, pexp, rate=0.55, lower.tail=FALSE)
y2 <- sapply(t, pweibull, scale=1.8, shape=2.2, lower.tail=FALSE)
y3 <- sapply(t, plnorm, meanlog=0.4, sdlog=0.9, lower.tail=FALSE)
y4 <- sapply(t, pgamma, scale=0.9, shape=3.2, lower.tail=FALSE)

# Plot
\donttest{
library(ggplot2)
p <- ggplot(data.frame(Time=rep(t,5), Probability=c(yS,y1,y2,y3,y4),
            Item=c(rep(c("System", "T1", "T2", "T3", "T4"), each=300))))
p <- p + geom_line(aes(x=Time, y=Probability, linetype=Item))
p <- p + xlab("Time") + ylab("Survival Probability")
p}
}
\keyword{ non parametric }
\keyword{ Bayesian inference }
\keyword{ survival signature }
\keyword{ test data }
