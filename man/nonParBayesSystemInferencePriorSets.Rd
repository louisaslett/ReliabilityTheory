\name{nonParBayesSystemInferencePriorSets}
\alias{nonParBayesSystemInferencePriorSets}
\title{Non-parametric Bayesian posterior predictive system survival inference using sets of priors}
\description{
Computes a non-parametric Bayesian posterior predictive survival probability
given the survival signature of a system, test data on each of the
components and a set of priors. This is the methodology described in Walter
\emph{et al} (2017), which extends the method in \code{\link{nonParBayesSystemInference}} (Aslett \emph{et al}, 2015) to allow modelling imperfect prior knowledge.
}
\usage{
nonParBayesSystemInferencePriorSets(at.times, survival.signature, test.data,
                                    nLower=2, nUpper=2, yLower=0.5, yUpper=0.5, cores=NA)
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
  \item{nLower, nUpper}{
    the reparameterised lower/upper prior parameter \eqn{n} for the Beta distribution, where
    \eqn{n = \alpha+\beta}.  Each must match in type and can be:
    \itemize{
      \item a single scalar for a fixed prior across time and component types;
      \item a vector of parameters of the same length as the \code{at.times}
        argument, which indicates the time-varying prior parameter at the
        corresponding time in \code{at.times}.  This is therefore time-varying,
        but indicates the same time-varying prior for all component types;
      \item or a data frame where each column is named using the same names as for
        the \code{survival.signature} argument and each row corresponds to the
        time-varying prior parameter at the corresponding time in
        \code{at.times}.
    }
    In all cases, \code{nUpper} but be elementwise greater than or equal to
    \code{nLower}.

    By default the 'uninformative' (but certain) prior with \code{nLower=2} and
    \code{nUpper=1} is used for all components at all times.
  }
  \item{yLower, yUpper}{
    the reparameterised lower/upper prior parameter \eqn{y} for the Beta distribution, where
    \eqn{y = \alpha/(\alpha+\beta)}.  Each must match in type and can be:
    \itemize{
      \item a single scalar for a fixed prior across time and component types;
      \item a vector of parameters of the same length as the \code{at.times}
        argument, which indicates the time-varying prior parameter at the
        corresponding time in \code{at.times}.  This is therefore time-varying,
        but indicates the same time-varying prior for all component types;
      \item or a data frame where each column is named using the same names as for
        the \code{survival.signature} argument and each row corresponds to the
        time-varying prior parameter at the corresponding time in
        \code{at.times}.
    }
    In all cases, \code{yUpper} but be elementwise greater than or equal to
    \code{yLower}.

    By default the 'uninformative' (but certain) prior with \code{yLower=0.5}
    and \code{yUpper=0.5} is used for all components at all times.
  }
  \item{cores}{
    a scalar indicating how many CPU cores on which to execute parallel parts of
    the algorithm (uses the \code{parallel} library internally).
  }
}
\details{
This function implements the technique described in Walter \emph{et al} (2017),
which extends the methodology of Aslett \emph{et al} (2015) to allow modelling
partial or imperfect prior knowledge on component failure distributions.

In brief Aslett \emph{et al} (2015) consider, at any fixed time \eqn{t}, the functioning of a single component of type \eqn{k} to be Bernoulli(\eqn{p_t^k}) distributed for suitable \eqn{p_t^k}, irrespective of the lifetime distribution of the component.  Correspondingly, the distribution of the number of components still functioning at time \eqn{t} in a collection of \eqn{n_k} iid components of type \eqn{k} is Binomial(\eqn{n_k, p_t^k}).

Taking the priors \eqn{p_t^k \sim} Beta(\eqn{\alpha_t^k, \beta_t^k}), Aslett \emph{et al} (2015) show that this leads to a posterior predictive survival distribution with a nice closed form (see equations 9 and 10 in Section 4).

Walter \emph{et al} (2017) use the standard reparameterisation (dropping sub/super-scripts for readability) \eqn{n = \alpha+\beta} and \eqn{y = \alpha/(\alpha+\beta)}. This allows a more natural interpretation, where \eqn{n} represents the prior strength (it represents a pseudo-count for number of failures informing the prior specification) and \eqn{y} represents the prior expectation for the probability a component functions.

In particular, Walter \emph{et al} (2017) then enable imprecise prior specification by allowing lower and upper bounds on \eqn{n} and \eqn{y}, which may optionally be time varying. This is then propagated to construct bounds on the posterior predictive distribution for the functioning of a new system containing components exchangeable with those provided in the testing set and used in a system with design specified by the survival signature provided.
}
\value{
  A list containing two slots, \code{lower} and \code{upper}, each of which is a vector of the same length as the \code{at.times} argument, where each element is the lower/upper posterior predictive probability of a new system surviving to the corresponding time in \code{at.times}.
}
\references{
Walter, G., Aslett, L. J. M. and Coolen, F. P. A. (2017), \sQuote{Bayesian nonparametric system reliability using sets of priors}, \emph{International Journal of Approximate Reasoning}, \bold{80}, 67--88.  \href{https://www.louisaslett.com/Papers/Walter_Aslett_Coolen_2017.html}{Download paper}

Aslett, L. J. M., Coolen, F. P. A. and Wilson, S. P. (2015), \sQuote{Bayesian Inference for Reliability of Systems and Networks using the Survival Signature}, \emph{Risk Analysis} \bold{39}(9), 1640--1651.  \href{https://www.louisaslett.com/Papers/Aslett_Coolen_Wilson_2014.html}{Download paper}
}
\author{
Louis J. M. Aslett \email{louis.aslett@durham.ac.uk} (\url{https://www.louisaslett.com/})
}
\note{
Please feel free to email \email{louis.aslett@durham.ac.uk} with any queries or if you encounter errors when running this function.
}

\seealso{
\code{\link{computeSystemSurvivalSignature}}, and also \code{\link{nonParBayesSystemInference}} which is the precise counterpart to this method.
}
\examples{
## Exactly reproduce the toy bridge system example in Section 7.2.1 of Walter et al (2017)

# Produces survival signature matrix for one component of type "name", for use
# in nonParBayesSystemInference()
oneCompSurvSign <- function(name){
  res <- data.frame(name=c(0,1), Probability=c(0,1))
  names(res)[1] <- name
  res
}

# Produces data frame with prior and posterior lower & upper component survival
# function for component of type "name" based on
# nonParBayesSystemInferencePriorSets() inputs for all components except
# survival signature; nLower, nUpper, yLower, yUpper must be data frames where
# each column corresponds to the component type, so there must be a match
oneCompPriorPostSet <- function(name, at.times, test.data, nLower, nUpper, yLower, yUpper){
  sig <- oneCompSurvSign(name)
  nodata <- list(name=NULL)
  names(nodata) <- name
  nL <- nLower[, match(name, names(nLower))]
  nU <- nUpper[, match(name, names(nUpper))]
  yL <- yLower[, match(name, names(yLower))]
  yU <- yUpper[, match(name, names(yUpper))]
  data <- test.data[match(name, names(test.data))]
  # NB limited to 2 cores on CRAN
  prio <- nonParBayesSystemInferencePriorSets(at.times, sig, nodata, nL, nU, yL, yU, cores = 2)
  post <- nonParBayesSystemInferencePriorSets(at.times, sig,   data, nL, nU, yL, yU, cores = 2)
  data.frame(Time=rep(at.times,2),
             Lower=c(prio$lower,post$lower),
             Upper=c(prio$upper,post$upper),
             Item=rep(c("Prior", "Posterior"), each=length(at.times)))
}

# ----------------------------------------------

# System
b3 <- createSystem(s -- 2:3 -- 4 -- 5:6 -- 1 -- t, 2 -- 5, 3 -- 6,
                   types = list(T1 = c(2,3,5,6), T2 = c(4), T3 = c(1)))

# Data
b3nulldata <- list("T1"=NULL, "T2"=NULL, "T3"=NULL)
b3testdata <- list("T1"=c(2.2, 2.4, 2.6, 2.8),
                   "T2"=c(3.2, 3.4, 3.6, 3.8),
                   "T3"=(1:4)/10+4) # T3 late failures
b3testdata <- list("T1"=c(2.2, 2.4, 2.6, 2.8),
                   "T2"=c(3.2, 3.4, 3.6, 3.8),
                   "T3"=(1:4)/10+0.5) # T3 early failures
b3testdata <- list("T1"=c(2.2, 2.4, 2.6, 2.8),
                   "T2"=c(3.2, 3.4, 3.6, 3.8),
                   "T3"=(1:4)-0.5) # T3 fitting failures
b3dat <- reshape2::melt(b3testdata); names(b3dat) <- c("x", "Part")
b3dat$Part <- ordered(b3dat$Part, levels=c("T1", "T2", "T3", "System"))

# Setup to run
b3sig <- computeSystemSurvivalSignature(b3)
b3t <- seq(0, 5, length.out=301)
b3nL <- data.frame(T1=rep(1,301), T2=rep(1,301), T3=rep(1,301))
b3nU <- data.frame(T1=rep(2,301), T2=rep(2,301), T3=rep(4,301))
b3yL <- data.frame(T1=rep(0.001, 301),
                   T2=rep(0.001, 301),
                   T3=c(rep(c(0.625,0.375,0.250,0.125,0.010), each=60), 0.01))
b3yU <- data.frame(T1=rep(0.999, 301),
                   T2=rep(0.999, 301),
                   T3=c(rep(c(0.999,0.875,0.500,0.375,0.250), each=60), 0.25))

b3T1 <- oneCompPriorPostSet("T1", b3t, b3testdata, b3nL, b3nU, b3yL, b3yU)
b3T2 <- oneCompPriorPostSet("T2", b3t, b3testdata, b3nL, b3nU, b3yL, b3yU)
b3T3 <- oneCompPriorPostSet("T3", b3t, b3testdata, b3nL, b3nU, b3yL, b3yU)

# Compute prior and posterior sets!!  NB limited to 2 cores on CRAN
b3prio <- nonParBayesSystemInferencePriorSets(b3t, b3sig, b3nulldata,
                                              b3nL, b3nU, b3yL, b3yU, cores = 2)
b3post <- nonParBayesSystemInferencePriorSets(b3t, b3sig, b3testdata,
                                              b3nL, b3nU, b3yL, b3yU, cores = 2)

b3df <- rbind(data.frame(b3T1, Part="T1"),
              data.frame(b3T2, Part="T2"),
              data.frame(b3T3, Part="T3"),
              data.frame(Time=rep(b3t,2),
                         Lower=c(b3prio$lower,b3post$lower),
                         Upper=c(b3prio$upper,b3post$upper),
                         Item=rep(c("Prior", "Posterior"), each=length(b3t)), Part="System"))
b3df$Item <- ordered(b3df$Item, levels=c("Prior", "Posterior"))
b3df$Part <- ordered(b3df$Part, levels=c("T1", "T2", "T3", "System"))

\donttest{
library("ggplot2")
library("xtable")

ggplot(b3df, aes(x=Time)) +
  scale_fill_manual(values = c("#b2df8a", "#1f78b4")) +
  scale_colour_manual(values = c("#b2df8a", "#1f78b4")) +
  geom_line(aes(y=Upper, group=Item, colour=Item)) +
  geom_line(aes(y=Lower, group=Item, colour=Item)) +
  geom_ribbon(aes(ymin=Lower, ymax=Upper, group=Item, colour=Item, fill=Item), alpha=0.5) +
  facet_wrap(~Part, nrow=2) +
  geom_rug(aes(x=x), data=b3dat) +
  xlab("Time") +
  ylab("Survival Probability") +
  theme_bw() +
  theme(legend.title = element_blank())

b3sigtable <- b3sig[b3sig$T3 == 1,]
b3sigtable$T1 <- as.factor(b3sigtable$T1)
b3sigtable$T2 <- as.factor(b3sigtable$T2)
b3sigtable$T3 <- as.factor(b3sigtable$T3)
xtable(b3sigtable)
}
}
\keyword{ non parametric }
\keyword{ Bayesian inference }
\keyword{ imprecise probability }
\keyword{ prior sets }
\keyword{ survival signature }
\keyword{ test data }
