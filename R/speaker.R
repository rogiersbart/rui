#' Play a sound
#'
#' @param path Audio file name
#'
#' @return
#' @export
#'
#' @examples
speaker <- function(path) {
  if (!raylibr_is_installed) {
    rui::alert("Package {.pkg raylibr} is required for {.fun speaker} output!")
    if (rui::ask("Do you want to install it?")) {
      if (!rlang::is_installed("pak")) {
        rui::alert("Package {.pkg pak} is required for installing {.pkg raylibr}!")
        if (rui::ask("Do you want to install {.pkg pak}?")) {
          install.packages("pak")
        }
      }
      pak::pkg_install("jeroenjanssens/raylibr")
    }
  }
  if (!audio_device_is_initiated) {
    raylibr::set_trace_log_level(raylibr::log$none)
    raylibr::init_audio_device()
    assignInNamespace(
      "audio_device_is_initiated",
      TRUE,
      "rui"
    )
  }
  sound <- raylibr::load_sound(path)
  raylibr::play_sound_multi(sound)
}

