# Standard message ----

  #' Output standard text messages
  #'
  #' @param ... Character vectors supporting **glue** strings and **cli** inline
  #' styles.
  #' @seealso [glue::glue()], [`cli::inline-markup`]
  #' @name standard-text

  #' @rdname standard-text
  #' @export
  tell <- function(...) cli::cli_text(paste(...), .envir = parent.frame())

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
  title <- function(...) cli::cli_text(cli::col_yellow("#"), " ", paste(...),
                                       .envir = parent.frame())

  #' @rdname multi-line-feedback
  #' @export
  inform <- function(...) cli::cli_text(cli::col_cyan("i"), " ", paste(...),
                                        .envir = parent.frame())

  #' @rdname multi-line-feedback
  #' @export
  approve <- function(...) cli::cli_text(cli::col_green("v"), " ", paste(...),
                                         .envir = parent.frame())

  #' @rdname multi-line-feedback
  #' @export
  disapprove <- function(...) cli::cli_text(cli::col_red("x"), " ", paste(...),
                                            .envir = parent.frame())

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
  end <- cli::cli_status_clear

  #' @rdname single-line-feedback
  #' @export
  succeed <- cli::cli_process_done

  #' @rdname single-line-feedback
  #' @export
  fail <- cli::cli_process_failed

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
  copy <- usethis::ui_code_block

  #' @rdname user-interaction
  #' @export
  do <- function(...) cli::cli_text(cli::col_red('o'), " ", paste(...),
                                    .envir = parent.frame())

  #' @rdname user-interaction
  #' @export
  ask <- function(...) {
    # NOTE this is based on usethis::ui_yeah, but uses text symbols only for
    # consistent behaviour on all consoles/operating systems
    cli::cli_text(cli::col_yellow('?'), " ", paste(...), .envir = parent.frame())
    if (!interactive()) {
      ui_stop("User input required, but session is not interactive.")
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
  NULL

  #' @rdname conditions
  #' @export
  alert <- function(...) cli::cli_text(cli::col_red("!"), " ", paste(...),
                                       .envir = parent.frame())

  #' @rdname conditions
  #' @export
  warn <- function(...) usethis::ui_warn(paste(...))

  #' @rdname conditions
  #' @export
  stop <- function(...) usethis::ui_stop(paste(...))
