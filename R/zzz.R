.onAttach <- function(libname, pkgname) {
  alert("{.pkg {{rui}}} is still in its experimental lifecycle stage.")
  alert("Use at your own risk, and submit issues here:")
  alert("{.url https://github.com/rogiersbart/rui/issues}")
  tell("")
  inform("You are attaching the {.pkg {{rui}}} namespace.")
  inform("We advise using {.code rui::function()} instead!")
  invisible()
}