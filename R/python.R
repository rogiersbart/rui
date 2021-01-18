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
    title = rui::title,
    inform = rui::inform,
    approve = rui::approve,
    disapprove = rui::disapprove,
    begin = rui::begin,
    change = rui::update,
    end = rui::end,
    succeed = rui::succeed,
    fail = rui::fail,
    clip = rui::copy,
    do = rui::do,
    ask = rui::ask,
    alert = rui::alert,
    warn = rui::warn,
    stop = rui::stop,
    extract = rui::extract,
    show = rui::show,
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