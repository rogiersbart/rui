% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/console.R, R/deprecated.R
\name{single-line-feedback}
\alias{single-line-feedback}
\alias{begin}
\alias{proceed}
\alias{clear}
\alias{succeed}
\alias{fail}
\alias{update}
\alias{end}
\title{Provide single-line feedback}
\usage{
begin(..., .envir = parent.frame())

proceed(..., .envir = parent.frame())

clear(.envir = parent.frame())

succeed(.envir = parent.frame())

fail(.envir = parent.frame())

update(...)

end(...)
}
\arguments{
\item{...}{Character vectors supporting \strong{glue} strings and \strong{cli} inline
styles.}
}
\description{
These functions provide a way to provide single-line feedback. This is
typically useful for longer tasks with different subtasks, for which there
is no important information that should persist in the console for the
user to refer back to (\emph{e.g.} downloads, optimization, running an external
code that doesn't output important information). Use
\itemize{
\item \code{rui::begin()} to begin a task,
\item \code{rui::proceed()} to proceed with another task,
\item for ending the single-line feedback, any of
\itemize{
\item \code{rui::succeed()} for succesful completion,
\item \code{rui::fail()} for unsuccesful completion, and
\item \code{rui::clear()} to remove the feedback line.
}
}
}
\seealso{
\code{\link[glue:glue]{glue::glue()}}, \code{\link[cli:inline-markup]{cli::inline-markup}}
}
