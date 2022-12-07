#' @rdname object-inspection
#' @param level Level of indentation, typically useful for (nested) lists.
#' Defaults to 1.
#' @export
extract <- function(...) {
  .Deprecated(new = "expose()", old = "extract()")
  expose(...)
}

#' @rdname object-inspection
#' @export
show <- function(...) {
  .Deprecated(new = "display()", old = "show()")
  display(...)
}

#' @rdname conditions
#' @export
stop <- function(...) {
  .Deprecated(new = "error()", old = "stop()")
  error(...)
}

#' @rdname user-interaction
#' @export
copy <- function(...) {
  .Deprecated(new = "give()", old = "copy()")
  clip(...)
}

#' @rdname user-interaction
#' @export
do <- function(...) {
  .Deprecated(new = "suggest()", old = "do()")
  suggest(...)
}

#' @rdname single-line-feedback
#' @export
update <- function(...) {
  .Deprecated(new = "proceed()", old = "update()")
  proceed(...)
}

#' @rdname single-line-feedback
#' @export
end <- function(...) {
  .Deprecated(new = "clear()", old = "end()")
  clear(...)
}

#' @rdname multi-line-feedback
#' @export
title <- function(...) {
  .Deprecated(new = "entitle()", old = "title()")
  entitle(...)
}
