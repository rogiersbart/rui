demo <- list(
  cli_styles = function() {
    rui::tell("Use {{.emph text}} for {.emph emphasized text}")
    rui::tell("Use {{.strong text}} for {.strong strong importance}")
    rui::tell("Use {{.code text}} for {.code a piece of code}")
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
  },
  conditions = function() {
    include_source("rui::alert", "alert")
    rui::warn('warn {.code rui::warn("warn")}', .demo = TRUE)
    rui::error('error {.code rui::error("error")}', .demo = TRUE)
    invisible()
  },
  multi_line_feedback = function() {
    include_source("rui::entitle", "entitle")
    include_source("rui::inform", "inform")
    include_source("rui::approve", "approve")
    include_source("rui::disapprove", "disapprove")
    invisible()
  },
  single_line_feedback = function() {
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
  },
  object_inspection = function() {
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
  },
  standard_text = function() {
    include_source("rui::tell", "tell")
    invisible()
  },
  user_interaction = user_interaction <- function() {
    rui::give('give [rui::give("give")]')
    include_source("rui::suggest", "suggest")
    rui::ask('ask {.code rui::ask("ask")}', .demo = TRUE)
    invisible()
  },
  glue_strings = function() {
    rui::tell("Use expressions like {5 + 5} for inline R code.")
    rui::tell("{.code Use expressions like {{5 + 5}} for inline R code.}")
    rui::tell("Vectors like {1:5} are automatically integrated.")
    rui::tell("{.code Vectors like {{1:5}} are automatically integrated.}")
    invisible()
  },
  all = function() {
    entitle("{.strong cli inline styles}")
    demo$cli_styles()
    entitle("{.strong conditions}")
    demo$conditions()
    entitle("{.strong multi-line feedback}")
    demo$multi_line_feedback()
    entitle("{.strong single-line feedback}")
    demo$single_line_feedback()
    entitle("{.strong object inspection}")
    demo$object_inspection()
    entitle("{.strong standard text}")
    demo$standard_text()
    entitle("{.strong user interaction}")
    demo$user_interaction()
    entitle("{.strong glue strings}")
    demo$glue_strings()
  }
)

include_source <- function(fname, text) {
  f <- eval(parse(text = fname))
  f('{text} {.code {fname}("{text}")}')
  invisible()
}
