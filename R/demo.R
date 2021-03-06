demo_glue_strings <- function() {
  rui::tell("Use expressions like {5 + 5} for inline R code.")
  rui::tell("{.emph [Use expressions like {{5 + 5}} for inline R code.]}")
  rui::tell("Vectors like {1:5} are automatically integrated.")
  rui::tell("{.emph [Vectors like {{1:5}} are automatically integrated.]}")
  invisible()
}

demo_cli_inline_styles <- function() {
  rui::tell("Use {{.emph text}} for {.emph emphasized text}")
  rui::tell("Use {{.strong text}} for {.strong strong importance}")
  rui::tell("Use {{.code text}} for {.code piece of code}")
  rui::tell("Use {{.pkg text}} for {.pkg package name}")
  rui::tell("Use {{.fun text}} for {.fun function name}")
  rui::tell("Use {{.arg text}} for {.arg function argument}")
  rui::tell("Use {{.key text}} for {.key keyboard key}")
  rui::tell("Use {{.kbd text}} for {.kbd keyboard key}")
  rui::tell("Use {{.file text}} for {.file file name}")
  rui::tell("Use {{.path text}} for {.path path}")
  rui::tell("Use {{.email text}} for {.email an email address}")
  rui::tell("Use {{.url text}} for {.url URL}")
  rui::tell("Use {{.var text}} for {.var variable name}")
  rui::tell("Use {{.envvar text}} for {.envvar name of an environment variable}")
  rui::tell("Use {{.val text}} for {.val value}")
  rui::tell("Use {{.field text}} for {.field field}")
  invisible()
}

demo_standard_text <- function() {
  include_source("rui::tell", "tell")
  invisible()
}

demo_multi_line_feedback <- function() {
  include_source("rui::entitle", "entitle")
  include_source("rui::inform", "inform")
  include_source("rui::approve", "approve")
  include_source("rui::disapprove", "disapprove")
  invisible()
}

demo_single_line_feedback <- function() {
  begin("Task that will succeed")
  Sys.sleep(2)
  succeed()
  begin("Task that will fail")
  Sys.sleep(2)
  fail()
  begin("Task that will disappear")
  Sys.sleep(2)
  clear()
  begin("Ongoing task")
  Sys.sleep(2)
  inform("Information provided during an ongoing task.")
  Sys.sleep(2)
  proceed("Ongoing task - Updated")
  Sys.sleep(2)
  succeed()
}

demo_user_interaction <- function() {
  rui::give('give [rui::give("give")]')
  include_source("rui::suggest", "suggest")
  rui::ask('ask {.emph [rui::ask("ask")]}', .demo = TRUE)
  invisible()
}

demo_conditions <- function() {
  include_source("rui::alert", "alert")
  rui::warn('warn [rui::warn("warn")]', .demo = TRUE)
  rui::error('error [rui::error("error")]', .demo = TRUE)
  invisible()
}

demo_object_inspection <- function() {
  include_source("rui::entitle", "entitle")
  include_source("rui::display", "display")
  include_source("rui::expose", "expose")
  rui::tell("")
  rui::inspect(1:5)
  rui::inspect(rnorm(10))
  rui::inspect(letters)
  rui::inspect(TRUE)
  rui::inspect(Sys.Date())
  rui::inspect(Sys.time())

  rui::inspect(cars)
  rui::inspect(volcano)
  rui::inspect(list(1:5, letters, cars, volcano))
  rui::inspect(list(1:5, letters, cars, volcano), 2)
}

include_source <- function(fname, text) {
  f <- eval(parse(text = fname))
  f('{text} {.emph [{fname}("{text}")]}')
  invisible()
}

demo_all <- function() {
  entitle("{.strong cli inline styles}")
  demo_cli_inline_styles()
  entitle("{.strong conditions}")
  demo_conditions()
  entitle("{.strong multi-line feedback}")
  demo_multi_line_feedback()
  entitle("{.strong single-line feedback}")
  demo_single_line_feedback()
  entitle("{.strong object inspection}")
  demo_object_inspection()
  entitle("{.strong standard text}")
  demo_standard_text()
  entitle("{.strong user interaction}")
  demo_user_interaction()
  entitle("{.strong glue strings}")
  demo_glue_strings()
}