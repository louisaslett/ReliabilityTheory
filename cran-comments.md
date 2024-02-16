## Response to review 1

Thank you for the thorough review of the package. I have carefully gone through and addressed each point, with only 1 item not address (reason given) and 1 item I cannot identify (explained below).

> Please omit the redundant "Tools for" at the start of your title and description.

I have removed these words from the title and reworded the description in response to this and the following issue.

> If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> authors (year, ISBN:...)
> or if those are not available: <[https:...]https:...>
> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")

Key references have been added to the description in the format requested.

> You write information messages to the console that cannot be easily suppressed.
> It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.
> Instead of print()/cat() rather use message()/warning() or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console. (except for print, summary, interactive functions)
> -> R/CoherentNetworksOfOrder.R; R/CoherentSystemsOfOrder.R; R/ComputeSignature.R; R/MaskedLifetimeInference.R; R/nonParBayesSystemInferencePriorSetsTEST.R

`cat()` has been changed to `message()` in R/CoherentNetworksOfOrder.R and R/CoherentSystemsOfOrder.R. I have also changed it in R/nonParBayesSystemInferencePriorSetsTEST.R although this file is only present for historical archiving and not exported in the namespace.

The only place I have not made the change is in R/ComputeSignature.R. This is because, per the documentation for this function, `cat()` is only used if `frac = TRUE`. The documentation states that (added star emphasis mine):

\item{frac}{if TRUE then the function **prints out** signature elements as fractions **rather than returning** a decimal signature vector}

I hope this is acceptable.

> Please ensure that you do not use more than 2 cores in your examples, vignettes, etc.
> -> nonParBayesSystemInferencePriorSets-R files

I have looked at this help file multiple times and cannot see where we use more than 1 core. There are multiple references to `cores = 1` (which I did because I was already aware of this rule). I have also monitored the output of `top` during a full R CMD CHECK and it does not exceed 100% of a single core. If the issue persists, please could you let me know where you see more than 1 core specified and I'm happy to fix?

## Comments (submission)

 * I am hoping to return this package to CRAN. It was archived due to a dependency {PhaseType} being removed from CRAN, but {PhaseType} is now back on CRAN.
 * I have moved institution since last updating this package, hence the maintainer email address has changed.
 * I have tried to go through as carefully as possible changes to CRAN policy since last submission leading to many changes, apologies if I have missed anything.

## R CMD check results

0 errors | 0 warnings | 1 note

The note relates to this being a new submission on CRAN due to prior archiving.
