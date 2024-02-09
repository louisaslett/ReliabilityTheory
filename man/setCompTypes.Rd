\name{setCompTypes}
\alias{setCompTypes}
\title{
Set component types in a system
}
\description{
A system created with \code{\link{createSystem}} by default assumes components to be of the same type. This function enables assigning particular types (or modified from a previous types assignment) to each component after system creation.
}
\usage{
setCompTypes(sys, types)
}
\arguments{

\item{sys}{
a \code{system} object representing the system whose types are to be set.  This should have been created by a call to \code{\link{createSystem}}.
}

\item{types}{
a named list of vectors.  The names correspond to component types, whilst each vector indicates which components are of that type.
}
}
\details{
This function enables specifying (or modifying) the types of the components in a system. The types can be specified when the system is initially defined using \code{\link{createSystem}}, but if none are specified at that time then it is assumed all components are of the same type.

The \code{types} argument should be a named list of vectors, for example \code{list("a" = 1:3, "b" = c(4,6), "c" = 5)} would specify that component numbers 1 through 3 are of type a, 4 and 6 are type b, with the remaining component 5 as type c. The numbering should match numbering used when creating the system. The start and terminal nodes should not be given a type (as they are assumed perfectly reliable).
}
\value{
The system which was passed in the \code{sys} argument is returned with the component types updated.
}
\author{
Louis J.M. Aslett \email{louis.aslett@durham.ac.uk} (\url{https://www.louisaslett.com/})
}

\seealso{
\code{\link{createSystem}}
}
\examples{
## EXAMPLE 1
## Figure 1 in Coolen and Coolen-Maturi (2012)

# First, define the structure, ensuring that each physically separate component
# is separately numbered
fig1 <- createSystem(s -- 1 -- 2:3 -- 4 -- 5:6 -- t, 2 -- 5, 3 -- 6)

# Second, assign types to the components with this function
setCompTypes(fig1, list("Type 1" = c(1, 2, 5), "Type 2" = c(3, 4, 6)))

# Note that one can create the same system and avoid using setCompTypes by
# specifying the types in the initial call to createSystem if desired.
# The following code results in exactly the same system specification as fig1:
fig1b <- createSystem(s -- 1 -- 2:3 -- 4 -- 5:6 -- t, 2 -- 5, 3 -- 6,
                      types = list("Type 1" = c(1, 2, 5), "Type 2" = c(3, 4, 6)))
}
