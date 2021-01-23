#' Make {rui} functions available in python through reticulate
#'
python <- function() {

  if (length(find.package("reticulate", quiet = TRUE)) == 0) {
    rui::alert("It seems {.pkg {{reticulate}}} is not installed.")
    rui::stop("Dependency issue.")
  }
  py <- reticulate::py
  py$rui <- reticulate::r_to_py(list(
    tell = rui::tell,
    entitle = rui::entitle,
    inform = rui::inform,
    approve = rui::approve,
    disapprove = rui::disapprove,
    begin = rui::begin,
    proceed = rui::proceed,
    clear = rui::clear,
    succeed = rui::succeed,
    fail = rui::fail,
    give = rui::give,
    suggest = rui::suggest,
    ask = rui::ask,
    alert = rui::alert,
    warn = rui::warn,
    error = rui::error,
    expose = rui::expose,
    display = rui::display,
    inspect = rui::inspect
  ))
  reticulate::py_run_string(
  '
class dotdict(dict):
  """dot.notation access to dictionary attributes"""
  __getattr__ = dict.get
  __setattr__ = dict.__setitem__
  __delattr__ = dict.__delitem__
  '
  )
  reticulate::py_run_string("rui = dotdict(rui)")
}