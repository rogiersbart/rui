raylibr_is_installed <- NULL
audio_device_is_initiated <- FALSE
.onAttach <- function(libname, pkgname) {
  alert("{.pkg rui} is still in its experimental lifecycle stage.")
  alert("Use at your own risk, and submit issues here:")
  alert("{.url https://github.com/rogiersbart/rui/issues}")
  tell("")
  inform("You are attaching the {.pkg rui} namespace.")
  inform("We advise using {.fun rui::function} instead!")
  invisible()
}
.onLoad <- function(libname, pkgname) {
  assignInNamespace(
    "raylibr_is_installed",
    rlang::is_installed("raylibr"),
    "rui"
  )
}
.onUnload <- function(libname, pkgname) {
  if (audio_device_is_initiated) {
    raylibr::close_audio_device()
  }
}