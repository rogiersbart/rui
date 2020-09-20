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
    .envir = parent.frame()
  ) {
    cli::cli_status(paste(...),
                    .envir = .envir,
                    .keep = TRUE)
    cli::cli_status_clear()
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
  #' - `rui::title()` for titling sections,
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
  title <- function(...) {
    tell(cli::col_yellow("#"),
         ...,
         .envir = parent.frame())
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
    tell(cli::col_red("x"),
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
  #' - `rui::begin()` to start a task,
  #' - `rui::update()` to update the task description,
  #' - for ending the single-line feedback, any of
  #'   - `rui::succeed()` for succesful completion,
  #'   - `rui::fail()` for unsuccesful completion, and
  #'   - `rui::end()` to remove the feedback line.
  #'
  #' @param ... Character vectors supporting **glue** strings and **cli** inline
  #' styles.
  #' @seealso [glue::glue()], [`cli::inline-markup`]
  #' @name single-line-feedback
  NULL

  #' @rdname single-line-feedback
  #' @export
  begin <- function(...) {
    cli::cli_status(msg = paste0("{cli::col_yellow('~')} ",
                                 paste(...), " ..."),
                    msg_done = paste0("{cli::col_green('v')} ",
                                      paste(...), " ... done"),
                    msg_failed = paste0("{cli::col_red('x')} ",
                                        paste(...), " ... failed"),
                    .envir = parent.frame())
  }

  #' @rdname single-line-feedback
  #' @export
  update <- function(...) {
    cli::cli_status_update(msg = paste0("{cli::col_yellow('~')} ",
                                        paste(...), " ..."),
                           msg_done = paste0("{cli::col_green('v')} ",
                                             paste(...), " ... done"),
                           msg_failed = paste0("{cli::col_red('x')} ",
                                               paste(...), " ... failed"),
                           .envir = parent.frame())
  }

  #' @rdname single-line-feedback
  #' @export
  end <- function(.envir = parent.frame()) {
    cli::cli_status_clear(.envir = .envir)
  }

  #' @rdname single-line-feedback
  #' @export
  succeed <- function(.envir = parent.frame()) {
    cli::cli_process_done(.envir = .envir)
  }

  #' @rdname single-line-feedback
  #' @export
  fail <- function(.envir = parent.frame()) {
    cli::cli_process_failed(.envir = .envir)
  }

# User interaction ----

  #' User interaction
  #'
  #' These functions provide a way to interact with the user. Use
  #' - `rui::copy()` to print and copy R code to the clipboard,
  #' - `rui::do()` to list a to do item,
  #' - `rui::ask()` to ask a yes/no question.
  #' Note `rui::copy()` does not support **cli** styles at the moment.
  #'
  #' @param ... Character vectors supporting **glue** strings and **cli** inline
  #' styles.
  #' @seealso [glue::glue()], [`cli::inline-markup`]
  #' @name user-interaction
  NULL

  # NOTE this does not work with cli styling, but that's OK?
  #' @rdname user-interaction
  #' @export
  copy <- function(...) {
    usethis::ui_code_block(...)
  }

  #' @rdname user-interaction
  #' @export
  do <- function(...) {
    tell(cli::col_red('*'),
         ...,
         .envir = parent.frame())
  }

  #' @rdname user-interaction
  #' @export
  ask <- function(..., .demo = FALSE) {
    # NOTE this is based on usethis::ui_yeah, but uses text symbols only for
    # consistent behaviour on all consoles/operating systems
    tell(cli::col_yellow('?'), ..., .envir = parent.frame())
    if (!interactive() & !.demo) {
      rui::stop("User input required, but session is not interactive.")
    }
    if (.demo) {
      tell("")
      tell("1: Yes please.")
      tell("2: No thanks.")
      tell("")
      return(invisible())
    }
    selection <- utils::menu(c("Yes please.", "No thanks."))
    if (selection == 0) stop("A choice is required.")
    as.logical(2 - selection)
  }

# Conditions ----

  #' Conditions
  #'
  #' These functions provide a way for signaling conditions. `rui::warn()` and
  #' `rui::stop()` are drop-in replacements for [base::warning()] and
  #' [base::stop()], which support **glue** strings. The function `rui::alert()`
  #' can be used for alerts with **cli** inline styles as well. We recommend
  #' doing so before issuing a warning or error.
  #' @name conditions
  NULL

  #' @rdname conditions
  #' @export
  alert <- function(...) {
    tell(cli::col_red("!"),
         ...,
         .envir = parent.frame())
  }

  #' @rdname conditions
  #' @export
  warn <- function(..., .demo = FALSE) {
    if (.demo) {
      message(paste0("Warning: ", ...))
      return(invisible())
    }
    usethis::ui_warn(paste(...))
  }

  #' @rdname conditions
  #' @export
  stop <- function(..., .demo = FALSE) {
    if (.demo) {
      message(paste0("Error: ", ...))
      return(invisible())
    }
    usethis::ui_stop(paste(...))
  }

# Object inspection ----

  #' Object inspection
  #'
  #' These functions provide a means to inspect the internal structure of
  #' objects, or design a UI to do so, which is consistent with the rest of the
  #' {rui} functionality. `rui::extract()` can be used to print information on
  #' different parts of an object (where the `.` in the prefix should be
  #' familiar to users of the {magrittr} pipe, and the `$` is one of the
  #' subsetting operators), whereas `rui::show()` would be more appropriate for
  #' the atomic types. `rui::inspect()` attempts to use both these functions
  #' together with `rui::title()` to process the output of a `utils::str()` call,
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
  extract <- function(..., level = 1) {
    prefix <- paste0(
      rep(c(cli::col_cyan(".$"), cli::col_yellow(".$")),
          ceiling(level / 2))[1:level],
      collapse = ""
    )
    tell(prefix, ..., .envir = parent.frame())
    invisible()
  }

  #' @rdname object-inspection
  #' @export
  show <- function(...) {
    tell(cli::col_cyan("."),
         ...,
         .envir = parent.frame())
  }

  #' @rdname object-inspection
  #' @param object Object to print the structure of.
  #' @param levels Number of levels to include. Defaults to 1.
  #' @export
  inspect <- function(object, levels = 1) {
    if ("tbl" %in% class(object)) levels <- levels + 1
    txt <- capture.output(str(object, give.attr = FALSE,
                              vec.len = 2,
                              max.level = levels))
    # TODO condense this, in a separate function, by applying gsub to a nx2
    # character matrix?
    txt <- gsub(": num ", ": {.emph <dbl>} ", txt, fixed = TRUE)
    txt <- gsub(": int ", ": {.emph <int>} ", txt, fixed = TRUE)
    txt <- gsub(": chr ", ": {.emph <chr>} ", txt, fixed = TRUE)
    txt <- gsub(": logi ", ": {.emph <lgl>} ", txt, fixed = TRUE)
    txt <- gsub("^ num ", "{.emph <dbl>} ", txt)
    txt <- gsub("^ int ", "{.emph <int>} ", txt)
    txt <- gsub("^ chr ", "{.emph <chr>} ", txt)
    txt <- gsub("^ logi ", "{.emph <lgl>} ", txt)
    txt <- gsub("'difftime'", "{.emph <drtn>}", txt, fixed = TRUE)
    txt <- gsub(" '", " {.val '", txt)
    txt <- gsub("' ", "'} ", txt)
    txt <- gsub("':", "'} ", txt)
    txt <- gsub(":'", ": {.val '", txt)
    txt <- gsub("^'", "{.val '", txt)
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
    txt <- gsub(">}  ", ">} ", txt, fixed = TRUE)

    if (is.null(dim(object))) {
      dimensions <- paste0("[1:", length(object), "]")
    } else {
      dimensions <- paste0("[", paste0(dim(object), collapse = "x"), "]")
    }
    first_line <- txt[1]
    txt <- txt[-1]
    if (is.vector(object) & is.atomic(object)) {
      title("vector of type {.val {typeof(object)[1]}} {dimensions}")
    } else {
      title("object of class {.val {class(object)[1]}}",
                 "type {.val {typeof(object)[1]}} {dimensions}")
    }
    if (is.atomic(object)) {
      show(first_line)
      return(invisible())
    }
    levels <- inspect_extract_level(txt)
    txt <- inspect_remove_level_prefix(txt)
    for (i in 1:length(txt)) {
      extract(txt[i], level = levels[i])
    }
    invisible()
  }

  inspect_extract_level <- function(txt) {
    dots <- substring(txt, 1, regexpr("$", txt, fixed = TRUE) - 1)
    dots <- gsub(" ", "", dots)
    nchar(dots) / 2 + 1
  }
  inspect_remove_level_prefix <- function(txt) {
    substring(txt,
              regexpr("$", txt, fixed = TRUE) + 2,
              nchar(txt))
  }