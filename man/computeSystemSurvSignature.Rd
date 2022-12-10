\name{computeSystemSurvivalSignature}
\alias{computeSystemSurvivalSignature}
\title{Compute the survival signature of a system}
\description{
The system survival signature (Coolen and Coolen-Maturi, 2012) is a generalisation of the signature to systems with multiple component types.  This function automatically computes the survival signature of the specified system.  Here, system implies components (as opposed to links) are unreliable.
}
\usage{
computeSystemSurvivalSignature(graph, cutsets=NULL, frac=FALSE)
}
\arguments{
  \item{graph}{
    an \code{\link{igraph}} object representing the system whose survival signature is to be computed.  There should be two terminal 'dummy' nodes to represent either end of the structure which must be labelled "s" and "t" (assumed perfectly reliable) as well as a vertex attribute named \code{compType} identifying each component type.  See details and examples.
}
  \item{cutsets}{
    if the cut-sets of the system or network are already known they may be passed in as a list of numeric vectors.  This can save time because cut-set computation is the slowest part of the algorithm.  Leaving as NULL causes the function to find the cut sets itself.
}
  \item{frac}{
    if TRUE then the function prints out survival signature probabilities as fractions rather than decimals.
}
}
\details{
The survival signature of a system with \eqn{K} types of component is the functional \eqn{\Phi(l_1, \dots, l_K)} giving the probability that the system works given exactly \eqn{l_k} of the components of type \eqn{k} are working.  See Coolen and Coolen-Maturi (2012) for details.  Thus, the survival signature can be represented by a table with \eqn{K+1} columns, the first \eqn{K} being the number of each type of component which is working and the final column being the probability the system works.

The system or network is specified by means of an \code{\link{igraph}} object, whereby each end of the system is denoted by nodes names "s" and "t" which are taken to be perfectly reliable.  It is easy to construct the appropriate graph representation using the function \code{\link{graph.formula}}.  Note that each physically distinct component should be separately numbered when constructing this graph object.

Once the topology of the system has been defined, one must attach a vertex attribute named \code{compType} to each component to indicate the type of the component.  The Examples section below features the full computation of the survival signature for Figure 1 in Coolen and Coolen-Maturi (2012) and Figure 2 in Coolen \emph{et al} (2013) to make this clear.
}
\value{
  \code{computeSystemSurvivalSignature} returns a data frame with \eqn{K+1} columns.  The first \eqn{K} columns represent the function inputs, \eqn{l_1, \dots, l_K} and the final column is the probability that the system works given the corresponding numbers of each component which are working.
}
\references{
Coolen, F. P. A. and Coolen-Maturi, T. (2012), Generalizing the signature to systems with multiple types of components, \emph{in} 'Complex Systems and Dependability', Springer, pp. 115-130.

Coolen, F. P. A., Coolen-Maturi, T., Al-nefaiee, A. H. and Aboalkhair, A. M. (2013), \sQuote{Recent advances in system reliability using the survival signature}, \emph{Proceedings of Advances in Risk and Reliability Technology Symposium, Loughborough}.
}
\author{
Louis J. M. Aslett \email{louis.aslett@durham.ac.uk} (\url{https://www.louisaslett.com/})
}
\note{
Please feel free to email \email{louis.aslett@durham.ac.uk} with any queries or if you encounter errors when running this function.
}

\seealso{
\code{\link{computeSystemSignature}}
}
\examples{
## EXAMPLE 1
## Figure 1 in Coolen and Coolen-Maturi (2012)

# First, define the structure, ensuring that each physically separate component
# is separately numbered
fig1 <- graph.formula(s -- 1 -- 2:3 -- 4 -- 5:6 -- t, 2 -- 5, 3 -- 6)

# Second, create a vertex attribute specifying the type of each of those
# numbered component (leaving s,t with no type)
V(fig1)$compType <- NA # This just creates the attribute compType
V(fig1)$compType[match(c("1","2","5"), V(fig1)$name)] <- "Type 1"
V(fig1)$compType[match(c("3","4","6"), V(fig1)$name)] <- "Type 2"
V(fig1)$compType[match(c("s","t"), V(fig1)$name)] <- NA

# Third, compute the survival signature (getting fractions rather than decimals)
computeSystemSurvivalSignature(fig1, frac=TRUE)



## EXAMPLE 2
## Figure 3 in Coolen et al (2013)

# First, define the structure, ensuring that each physically separate component
# is separately numbered
fig3 <- graph.formula(s -- 1:4 -- 2:5 -- 3:6 -- t, s -- 7:8, 8 -- 9, 7:9 -- t)

# Second, create a vertex attribute specifying the type of each of those
# numbered component (leaving s,t with no type)
V(fig3)$compType <- NA # This just creates the attribute compType
V(fig3)$compType[match(c("1"), V(fig3)$name)] <- "Type 1"
V(fig3)$compType[match(c("2","3","4","7"), V(fig3)$name)] <- "Type 2"
V(fig3)$compType[match(c("5","6","8","9"), V(fig3)$name)] <- "Type 3"
V(fig3)$compType[match(c("s","t"), V(fig3)$name)] <- NA
  
# Third, compute the survival signature (getting fractions rather than decimals)
computeSystemSurvivalSignature(fig3, frac=TRUE)
}
\keyword{ system }
\keyword{ survival }
\keyword{ signature }% __ONLY ONE__ keyword per line
