# Standard message ----

#' Output standard text messages
#'
#' @param ... Character vectors supporting **glue** strings and **cli** inline
#' styles.
#' @seealso [glue::glue()], [`cli::inline-markup`]
#' @name standard-text
NULL

#' @rdname standard-text
#' @export
tell <- function(
  ...,
  .envir = parent.frame(),
  capture = FALSE
) {
  if (capture) {
    return(capture.output(tell(..., .envir = .envir), type = "message")[1])
  }
  cli::cli_text(paste(...), .envir = .envir)
}

# Multi-line feedback ----

#' Provide multi-line feedback
#'
#' These functions provide a way to provide multi-line feedback. This is
#' typically useful for any kind of small tasks you want to provide feedback
#' about, or longer tasks for which you need the feedback to persist in the
#' console (*e.g.* running external code that does provide essential
#' information (in such a case, these are useful in combination with
#' [processx::run()] and its `stdout_line_callback` argument)) Use
#' - `rui::entitle()` for naming sections,
#' - `rui::inform()` for providing information,
#' - `rui::approve()` for succesful completion of a task or a positive test,
#'   and
#' - `rui::disapprove()` for unsuccesful completion or a negative test.
#'
#' @param ... Character vectors supporting **glue** strings and **cli** inline
#' styles.
#' @seealso [glue::glue()], [`cli::inline-markup`]
#' @name multi-line-feedback
NULL

#' @rdname multi-line-feedback
#' @export
entitle <- function(...) {
  tell(cli::col_yellow("#"),
       ...,
       .envir = parent.frame())
}

#' @rdname multi-line-feedback
#' @export
title <- function(...) {
  .Deprecated(new = "entitle()", old = "title()")
  entitle(...)
}

#' @rdname multi-line-feedback
#' @export
inform <- function(...) {
  tell(cli::col_cyan("i"),
       ...,
       .envir = parent.frame())
}

#' @rdname multi-line-feedback
#' @export
approve <- function(...) {
  tell(cli::col_green("v"),
       ...,
       .envir = parent.frame())
}

#' @rdname multi-line-feedback
#' @export
disapprove <- function(...) {
  tell(cli::col_br_red("x"),
       ...,
       .envir = parent.frame())
}

# Single line feedback ----

#' Provide single-line feedback
#'
#' These functions provide a way to provide single-line feedback. This is
#' typically useful for longer tasks with different subtasks, for which there
#' is no important information that should persist in the console for the
#' user to refer back to (*e.g.* downloads, optimization, running an external
#' code that doesn't output important information). Use
#' - `rui::begin()` to begin a task,
#' - `rui::proceed()` to proceed with another task,
#' - for ending the single-line feedback, any of
#'   - `rui::succeed()` for succesful completion,
#'   - `rui::fail()` for unsuccesful completion, and
#'   - `rui::clear()` to remove the feedback line.
#'
#' @param ... Character vectors supporting **glue** strings and **cli** inline
#' styles.
#' @seealso [glue::glue()], [`cli::inline-markup`]
#' @name single-line-feedback
NULL

#' @rdname single-line-feedback
#' @export
begin <- function(..., .envir = parent.frame()) {
  if (interactive()) {
    cli::cli_progress_message(paste0("{cli::col_yellow('~')} ",
                                 paste(...), " ..."),
                    .envir = .envir)
  }
  assignInNamespace(
    "msg_done",
    paste0("{cli::col_green('v')} ", paste(...), " ... done"),
    "rui"
  )
  assignInNamespace(
    "msg_failed",
    paste0("{cli::col_br_red('x')} ",
           paste(...), " ... failed"),
    "rui"
  )
}
msg_done <- NULL
msg_failed <- NULL

#' @rdname single-line-feedback
#' @export
proceed <- function(..., .envir = parent.frame()) {
  clear(.envir)
  begin(..., .envir = .envir)
}

#' @rdname single-line-feedback
#' @export
update <- function(...) {
  .Deprecated(new = "proceed()", old = "update()")
  proceed(...)
}

#' @rdname single-line-feedback
#' @export
clear <- function(.envir = parent.frame()) {
  cli::cli_progress_done(.envir = .envir, result = "clear")
}

#' @rdname single-line-feedback
#' @export
end <- function(...) {
  .Deprecated(new = "clear()", old = "end()")
  clear(...)
}

#' @rdname single-line-feedback
#' @export
succeed <- function(.envir = parent.frame()) {
  clear(parent.frame())
  tell(msg_done, .envir = parent.frame())
}

#' @rdname single-line-feedback
#' @export
fail <- function(.envir = parent.frame()) {
  clear(parent.frame())
  tell(msg_failed, .envir = parent.frame())
}

# User interaction ----

#' User interaction
#'
#' These functions provide a way to interact with the user. Use
#' - `rui::give()` to give the user a piece of code to be inserted elsewhere,
#' - `rui::suggest()` to suggest the user a thing to do,
#' - `rui::ask()` to ask the user a yes/no question.
#' Note `rui::give()` does not support **cli** styles.
#'
#' @param ... Character vectors supporting **glue** strings and **cli** inline
#' styles.
#' @seealso [glue::glue()], [`cli::inline-markup`]
#' @name user-interaction
NULL

# NOTE this does not work with cli styling, but that's OK?
#' @rdname user-interaction
#' @export
give <- function(...) {
  usethis::ui_code_block(c(...))
}

#' @rdname user-interaction
#' @export
copy <- function(...) {
  .Deprecated(new = "give()", old = "copy()")
  clip(...)
}

#' @rdname user-interaction
#' @export
suggest <- function(...) {
  tell(cli::col_br_red('*'),
       ...,
       .envir = parent.frame())
}

#' @rdname user-interaction
#' @export
do <- function(...) {
  .Deprecated(new = "suggest()", old = "do()")
  suggest(...)
}

#' @rdname user-interaction
#' @export
ask <- function(..., .demo = FALSE) {
  # NOTE this is based on usethis::ui_yeah, but uses text symbols only for
  # consistent behaviour on all consoles/operating systems
  tell(cli::col_yellow('?'), ..., .envir = parent.frame())
  if (!interactive() & !.demo) {
    error("User input required, but session is not interactive.")
  }
  if (.demo) {
    tell("")
    tell("1: Yes please.")
    tell("2: No thanks.")
    tell("")
    return(invisible())
  }
  selection <- utils::menu(c("Yes please.", "No thanks."))
  if (selection == 0) error("A choice is required.")
  as.logical(2 - selection)
}

# Conditions ----

#' Conditions
#'
#' These functions provide a way for signaling conditions. `rui::warn()` and
#' `rui::error()` are drop-in replacements for [base::warning()] and
#' [base::stop()], which support **glue** strings and **cli** inline styles.
#' ANSI colours are lost however. For retaining colours, `rui::alert()`
#' can be used. We recommend doing so before issuing a warning or error.
#' @param ... Character vectors supporting **glue** strings and **cli** inline
#' styles.
#' @param warn Logical. Should a warning be issued with the same message.
#' @param error Logical. Should an error be issued with the same message.
#' @seealso [glue::glue()], [`cli::inline-markup`]
#' @name conditions
NULL

#' @rdname conditions
#' @export
alert <- function(..., warn = FALSE, error = FALSE) {
  tell(cli::col_br_red("!"),
       ...,
       .envir = parent.frame())
  if (warn) warn(...)
  if (error) error(...)
}

#' @rdname conditions
#' @export
warn <- function(..., .demo = FALSE) {
  msg <- tell(..., capture = TRUE)
  if (.demo) {
    message(paste0("Warning: ", msg))
    return(invisible())
  }
  usethis::ui_warn(msg)
}

#' @rdname conditions
#' @export
error <- function(..., .demo = FALSE) {
  msg <- tell(..., capture = TRUE)
  if (.demo) {
    message(paste0("Error: ", msg))
    return(invisible())
  }
  usethis::ui_stop(msg)
}

#' @rdname conditions
#' @export
stop <- function(...) {
  .Deprecated(new = "error()", old = "stop()")
  error(...)
}

# Object inspection ----

#' Object inspection
#'
#' These functions provide a means to inspect the internal structure of
#' objects, or design a UI to do so, which is consistent with the rest of the
#' {rui} functionality. `rui::expose()` can be used to print information on
#' different parts of an object (where the `.` in the prefix should be
#' familiar to users of the {magrittr} pipe, and the `$` is one of the
#' subsetting operators), whereas `rui::display()` would be more appropriate for
#' the atomic types. `rui::inspect()` attempts to use both these functions
#' together with `rui::entitle()` to process the output of a `utils::str()` call,
#' and print the structure consistent with the rest of the {rui} functions,
#' borrowing some style elements from `tibble:::print.tbl()` and
#' `tibble::glimpse()`.
#' @param ... Character vectors supporting **glue** strings and **cli** inline
#' styles.
#' @seealso [glue::glue()], [`cli::inline-markup`]
#' @name object-inspection
NULL

#' @rdname object-inspection
#' @param level Level of indentation, typically useful for (nested) lists.
#' Defaults to 1.
#' @export
expose <- function(..., level = 1) {
  prefix <- paste0(
    rep(c(cli::col_cyan(".$"), cli::col_yellow(".$")),
        ceiling(level / 2))[1:level],
    collapse = ""
  )
  tell(prefix, ..., .envir = parent.frame())
  invisible()
}

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
display <- function(...) {
  tell(cli::col_cyan("."),
       ...,
       .envir = parent.frame())
}

#' @rdname object-inspection
#' @export
show <- function(...) {
  .Deprecated(new = "display()", old = "show()")
  display(...)
}

#' @rdname object-inspection
#' @param object Object to print the structure of.
#' @param levels Number of levels to include. Defaults to 1.
#' @export
inspect <- function(object, levels = 1) {
  txt <- capture.output(str(object, give.attr = FALSE,
                            vec.len = 2,
                            max.level = levels + 1))
  # TODO condense this, in a separate function, by applying gsub to a nx2
  # character matrix?
  txt <- gsub(": num ", ": {.emph <dbl>} ", txt, fixed = TRUE)
  txt <- gsub("^ num ", "{.emph <dbl>} ", txt)
  txt <- gsub(": Named num ", ": {.emph <n.dbl>} ", txt, fixed = TRUE)
  txt <- gsub("^ Named num ", "{.emph <n.dbl>} ", txt)

  txt <- gsub("^ int ", "{.emph <int>} ", txt)
  txt <- gsub(": int ", ": {.emph <int>} ", txt, fixed = TRUE)
  txt <- gsub("^ Named int ", "{.emph <n.int>} ", txt)
  txt <- gsub(": Named int ", ": {.emph <n.int>} ", txt, fixed = TRUE)

  txt <- gsub("^ chr ", "{.emph <chr>} ", txt)
  txt <- gsub(": chr ", ": {.emph <chr>} ", txt, fixed = TRUE)
  txt <- gsub("^ Named chr ", "{.emph <n.chr>} ", txt)
  txt <- gsub(": Named chr ", ": {.emph <n.chr>} ", txt, fixed = TRUE)

  txt <- gsub(": logi ", ": {.emph <lgl>} ", txt, fixed = TRUE)
  txt <- gsub("^ logi ", "{.emph <lgl>} ", txt)
  txt <- gsub(": Named logi ", ": {.emph <n.lgl>} ", txt, fixed = TRUE)
  txt <- gsub("^ Named logi ", "{.emph <n.lgl>} ", txt)

  txt <- gsub(": raw ", ": {.emph <raw>} ", txt, fixed = TRUE)
  txt <- gsub("^ raw ", "{.emph <raw>} ", txt)
  txt <- gsub(": Named raw ", ": {.emph <n.raw>} ", txt, fixed = TRUE)
  txt <- gsub("^ Named raw ", "{.emph <n.raw>} ", txt)

  txt <- gsub("'difftime'", "{.emph <drtn>}", txt, fixed = TRUE)

  txt <- gsub(" '", " {.val '", txt)
  txt <- gsub("' ", "'} ", txt)
  txt <- gsub("':", "'} ", txt)
  txt <- gsub(":'", ": {.val '", txt)
  txt <- gsub("^'", "{.val '", txt)

  txt <- gsub(" ‘", " {.val '", txt)
  txt <- gsub("’ ", "'} ", txt)
  txt <- gsub("’:", "'} ", txt)
  txt <- gsub(":‘", ": {.val '", txt)
  txt <- gsub("^‘", "{.val '", txt)

  txt <- gsub(":Classes ", ": ", txt, fixed = TRUE)
  txt <- gsub(" and ", ", ", txt, fixed = TRUE)
  txt <- gsub(" obs. of  ", "x", txt, fixed = TRUE)
  txt <- gsub(" variables:", "", txt)
  txt <- gsub("^ Date\\[", "Date[", txt)
  txt <- gsub("Date[", "{.emph <date>} [", txt, fixed = TRUE)
  txt <- gsub("^ POSIX", "POSIX", txt)
  txt <- gsub("POSIXct[", "{.emph <dttm>} [", txt, fixed = TRUE)
  txt <- gsub("POSIXlt[", "{.emph <dttm>} [", txt, fixed = TRUE)
  txt <- gsub("Factor w/", "{.emph <fct>}", txt)
  txt <- gsub("Time-Series", "{.emph <ts>}", txt)
  txt <- ifelse(grepl("List of ", txt), paste0(txt, "]"), txt)
  txt <- gsub("List of ", " {.val 'list'} [1:", txt)
  txt <- ifelse(grepl("'data.frame'", txt),
                paste0(gsub("frame'} \t", "frame'} [", txt), "]"),
                txt)
  txt <- ifelse(grepl(" tibble ", txt, fixed = TRUE),
                gsub(" tibble ", " {.val 'tbl_df'} ", txt, fixed = TRUE),
                txt)
  txt <- gsub(" x ", "x", txt, fixed = TRUE)
  txt <- gsub(">}  ", ">} ", txt, fixed = TRUE)
  txt <- gsub(" (S3: tbl_df/tbl/data.frame)", "", txt, fixed = TRUE)

  if (is.null(dim(object))) {
    dimensions <- paste0("[1:", length(object), "]")
  } else {
    dimensions <- paste0("[", paste0(dim(object), collapse = "x"), "]")
  }
  first_line <- txt[1]
  txt <- txt[-1]
  if (is.vector(object) & is.atomic(object)) {
    entitle("vector of type {.val {typeof(object)[1]}} {dimensions}")
  } else {
    entitle("object of class {.val {class(object)[1]}}",
               "type {.val {typeof(object)[1]}} {dimensions}")
  }
  if (is.atomic(object)) {
    display(first_line)
    return(invisible())
  }
  txt_levels <- inspect_expose_level(txt)
  txt <- inspect_remove_level_prefix(txt)
  for (i in 1:length(txt)) {
    if (txt_levels[i] <= levels)
      expose(txt[i], level = txt_levels[i])
  }
  invisible()
}

inspect_expose_level <- function(txt) {
  dots <- substring(txt, 1, regexpr("$", txt, fixed = TRUE) - 1)
  dots <- gsub(" ", "", dots)
  nchar(dots) / 2 + 1
}
inspect_remove_level_prefix <- function(txt) {
  substring(txt,
            regexpr("$", txt, fixed = TRUE) + 2,
            nchar(txt))
}
