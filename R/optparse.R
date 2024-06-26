#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
help_formatter <- function(object) {
  as_string <- function(x) {
    if (is.null(x)) return("NULL")
    if (is.na(x)) return("NA")
    if (!length(x)) return(paste0(typeof(x), "(0)"))
    as.character(x)
  }
  argument <- function(x) {
    switch(
      x@action,
      store = TRUE,
      callback = ifelse(x@type == "NULL", FALSE, TRUE),
      FALSE
    )
  }
  rui::console("# Usage")
  rui::console(gsub("Usage: ", "", object@usage))
  if (object@description != "") rui::console("i ", object@description)
  cat("\n")
  rui::console("# Options")
  options_list <- object@options
  for (ii in seq_along(options_list)) {
    option <- options_list[[ii]]
    flag_text <- ""
    if (!is.null(option@long_flag)) {
      flag_text <- paste0(flag_text, "{.strong ", option@long_flag, "}")
      if (argument(option)) {
        flag_text <- paste0(flag_text, "=", "{.emph ", toupper(option@metavar), "}")
      }
    }
    if (!is.na(option@short_flag)) {
      flag_text <- paste0(flag_text, ", ", "{.strong ", option@short_flag, "}")
      if (argument(option)) {
        flag_text <- paste0(flag_text, " ", toupper(option@metavar))
      }
    }
    rui::console(flag_text)
    rui::console("i ", sub("%default", as_string(option@default), option@help))
    # cat("\n")
  }
  rui::console(object@epilogue)
  return(invisible(NULL))
}