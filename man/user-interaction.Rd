% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verbs.R
\name{user-interaction}
\alias{user-interaction}
\alias{give}
\alias{copy}
\alias{suggest}
\alias{do}
\alias{ask}
\title{User interaction}
\usage{
give(...)

copy(...)

suggest(...)

do(...)

ask(..., .demo = FALSE)
}
\arguments{
\item{...}{Character vectors supporting \strong{glue} strings and \strong{cli} inline
styles.}
}
\description{
These functions provide a way to interact with the user. Use
\itemize{
\item \code{rui::give()} to give the user a piece of code to be inserted elsewhere,
\item \code{rui::suggest()} to suggest the user a thing to do,
\item \code{rui::ask()} to ask the user a yes/no question.
Note \code{rui::give()} does not support \strong{cli} styles.
}
}
\seealso{
\code{\link[glue:glue]{glue::glue()}}, \code{\link[cli:inline-markup]{cli::inline-markup}}
}
