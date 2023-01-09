# Single function to rule them all ----

#' The console API
#'
#' This function provides an alternative to the verb-based API, by mapping the
#' set of prefixes to the corresponding verbs.
#'
#' @param ... Character vectors supporting **glue** strings and **cli** inline
#' styles. This should start with a supported prefix, to map the action to one
#' of the {rui} verbs. Otherwise, `rui::tell()` is used. Supported prefixes are
#' # (entitle), i (inform), v (approve), x (disapprove), ~ (begin/proceed), =
#' (give), * (suggest), ? (ask), ! (alert), w (warn), e (stop), . (display), and
#' $ (expose). Status bars can be resolved by providing single characters "c",
#' "v", or "x" for clear, succeed and fail respectively.
#' @param object Object to print the {rui} way through `rui::inspect()`.
#' @export
console <- function(..., object = NULL, levels = 1, .envir = parent.frame()) {
  if (!is.null(object)) {inspect(object, levels); return(invisible())}
  txt <- paste(...)
  type <- substr(txt, 1, 1)
  types <- c("i", "v", "x", "~", "*", "?", "=", "!", "#", ".", "c", "$", "e", "w")
  space <- substr(txt, 2, 2)
  if (!(type %in% types & (space %in% c(" ", "")))) {
    tell(txt)
    return(invisible())
  }
  if (space == "") {
    if (type == "v") {cli::cli_progress_cleanup(); succeed(.envir = .envir); return(invisible())}
    if (type == "x") {cli::cli_progress_cleanup(); fail(.envir = .envir); return(invisible())}
    if (type == "c") {cli::cli_progress_cleanup(); return(invisible())}
  }
  txt <- substr(txt, 3, nchar(txt))
  switch(
    type,
    `#` = entitle(txt),
    i = inform(txt),
    v = approve(txt),
    x = disapprove(txt),
    `~` = proceed(txt, .envir = .envir),
    `*` = suggest(txt),
    `?` = ask(txt),
    `=` = give(txt),
    `!` = alert(txt),
    `$` = expose(txt),
    `.` = display(txt),
    e = error(txt),
    w = warn(txt)
  )
}

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
  cli::cli_div(theme = rui_theme())
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
    cli::cli_div(theme = rui_theme())
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
clear <- function(.envir = parent.frame()) {
  cli::cli_progress_done(.envir = .envir, result = "clear")
}

#' @rdname single-line-feedback
#' @export
succeed <- function(.envir = parent.frame()) {
  clear(parent.frame())
  tell(msg_done, .envir = parent.frame())
  assignInNamespace("msg_done", NULL, "rui")
  assignInNamespace("msg_failed", NULL, "rui")
}

#' @rdname single-line-feedback
#' @export
fail <- function(.envir = parent.frame()) {
  clear(parent.frame())
  tell(msg_failed, .envir = parent.frame())
  assignInNamespace("msg_done", NULL, "rui")
  assignInNamespace("msg_failed", NULL, "rui")
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

#' @rdname user-interaction
#' @export
give <- function(...) {
  cli::cli_div(theme = rui_theme())
  # TODO use {cli} instead
  usethis::ui_code_block(c(...), copy = TRUE)
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
#' These functions provide a way for signalling conditions. `rui::warn()` and
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
    rep(paste0(c(cli::col_cyan("$"), cli::col_yellow("$")), "."),
        ceiling(level / 2))[1:level],
    collapse = ""
  )
  prefix <- prefix |> substr(1, nchar(prefix) - 1)
  tell(prefix, ..., .envir = parent.frame())
  invisible()
}

#' @rdname object-inspection
#' @export
display <- function(...) {
  tell(cli::col_cyan("."),
       ...,
       .envir = parent.frame())
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

# CLI Theme ----

rui_theme <- function() {
  # red <- cli::col_red
  # green <- cli::col_green
  # magenta <- cli::col_magenta
  # blue <- cli::col_blue
  # yellow <- cli::col_yellow
  # cyan <- cli::col_cyan
  make_link <- function(x, type) {
    if (interactive()) return(cli:::make_link(x, type = type))
    if (type == "href") return(cli:::re_match(x, "^\\[(?<text>.*)\\]\\((?<url>.*)\\)$")$text)
    x
  }
  list(
    # span.dt = list(postfix = ": "),
    # span.dd = list(),
    # .code = list(fmt = cli:::format_code(dark)),
    # .code.R = list(fmt = cli:::format_r_code(dark)),
    span.emph = list(`font-style` = "italic"),
    span.strong = list(`font-weight` = "bold"),
    # span.code = cli:::theme_code_tick(dark),
    # span.q = list(fmt = cli:::quote_weird_name2),
    span.pkg = list(before = "{", after = "}", transform = cli::col_yellow,
                    color = NULL),
    # span.fn = cli:::theme_function(dark),
    span.fun = list(before = "`", after = "`", transform = \(x) make_link(x, type = "fun") |> paste0("()") |> cli::col_blue()),
    # span.arg = cli:::theme_code_tick(dark),
    # span.kbd = list(before = "[", after = "]", color = "blue"),
    # span.key = list(before = "[", after = "]", color = "red"),
    # span.file = cli:::theme_file(),
    # span.path = cli:::theme_file(),
    span.email = list(
      color = NULL,
      transform = function(x) make_link(x, type = "email") |> cli::col_cyan(),
      fmt = cli:::quote_weird_name
    ),
    span.url = list(
      before = "<",
      after = ">",
      color = NULL,
      `font-style` = NULL,
      transform = function(x) make_link(x, type = "url") |> cli::col_cyan()
    ),
    span.href = list(
      transform = function(x) make_link(x, type = "href") |> cli::col_cyan()
    ),
    # span.help = list(transform = function(x) cli:::make_link(x, type = "help")),
    # span.topic = list(transform = function(x) cli:::make_link(x, type = "topic")),
    # span.vignette = list(transform = function(x) cli:::make_link(x, type = "vignette")),
    # span.run = list(transform = function(x) cli:::make_link(x, type = "run")),
    # span.var = cli:::theme_code_tick(dark),
    # span.col = cli:::theme_code_tick(dark), span.str = list(fmt = cli:::encode_string),
    # span.envvar = cli:::theme_code_tick(dark),
    # span.val = list(transform = function(x, ...) cli_format(x, ...), color = "blue"),
    # span.field = list(color = "green"),
    # span.cls = list(collapse = "/", color = "blue", before = "<",
    #                after = ">"),
    # `span.progress-bar` = list(transform = cli:::theme_progress_bar,
    #                                                         color = "green"),
    # span.obj_type_friendly = list(transform = function(x) cli::format_inline(cli:::typename(x))),
    # span.type = list(transform = function(x) cli::format_inline(cli:::typename(x))),
    # span.or = list(`vec-sep2` = " or ", `vec-last` = ", or "),
    # span.timestamp = list(before = "[", after = "]", color = "grey")),
    NULL
  )
}
