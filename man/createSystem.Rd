\name{createSystem}
\alias{createSystem}
\title{
Create a system specification
}
\description{
Creates a system design specification based on passing a textual representation of design.
}
\usage{
createSystem(..., types = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
multiple expressions which together define an undirected graph representation of the reliability block diagram for the system design. There should be two terminal \sQuote{dummy} nodes to represent either end of the system structure, which must be labelled \code{s} and \code{t} (assumed perfectly reliable).  All reliability assessment is of the connectivity of these nodes.  See details and examples.
}
  \item{types}{
(optional) named list of vectors. The names correspond to component types, whilst each vector indicates which components are of that type. When it is not specified then all components are assumed to be of the same type. This can be updated later using the function \code{\link{setCompTypes}}.
}
}
\details{
This function enables specification of a system design by textual representation of the reliability block diagram, for use in many other functions in this package.  The method of representing the system is as for an undirected graph in the \pkg{igraph} package.

There should be two terminal \sQuote{dummy} nodes to represent either end of the system structure, which must be labelled \code{s} and \code{t} (assumed perfectly reliable). Dashes \code{--} are then used to connect numbered nodes together.  The full specification can be spread over multiple arguments.  Colon notation can denote an edge to multiple nodes, but is \emph{not} a range specifier (eg \code{1:5} means components 1 and 5, not components 1 to 5).  Following are some concrete examples:

\enumerate{
\item a series system of 3 components:

  \code{createSystem(s -- 1 -- 2 -- 3 -- t)}

\item a parallel system of 3 components:

  \code{createSystem(s -- 1 -- t, s -- 2 -- t, s -- 3 -- t)}

  Or, more succinctly:

  \code{createSystem(s -- 1:2:3 -- t)}

\item a classic \sQuote{bridge} system consisting of 5 components:

  \code{createSystem(s -- 1:2 -- 5 -- 3:4 -- t, 1 -- 3, 2 -- 4)}

  Exactly equivalently:

  \code{createSystem(s -- 1 -- 3 -- t, s -- 2 -- 4 -- t, 1:2 -- 5 -- 3:4)}
}
}
\value{
Returns a system of the design specified.

Internally, this is an \pkg{igraph} object, with some additional attributes relevant to system specification.
}
\author{
Louis J.M. Aslett \email{louis.aslett@durham.ac.uk} (\url{https://www.louisaslett.com/})
}
\seealso{
\code{\link{setCompTypes}} to specify component types after system creation, rather than in the same command.
}
\examples{
# Create a bridge system, with all components of the same type (or with type to
# be defined later)
bridge <- createSystem(s -- 1:2 -- 5 -- 3:4 -- t, 1 -- 3, 2 -- 4)

# Create a bridge system, with two types of component
bridge <- createSystem(s -- 1:2 -- 5 -- 3:4 -- t, 1 -- 3, 2 -- 4,
                       types = list(T1 = 1:4, T2 = 5))
}
