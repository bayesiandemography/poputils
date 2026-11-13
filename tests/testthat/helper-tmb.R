## Compile a test TMB model once per session, in a temporary
## directory, and return the name of its DLL. Templates live in
## tests/testthat/tmb/ and may include poputils' headers. Skips if
## TMB is not installed, and on CRAN (compilation is slow).
tmb_test_dll <- local({
  dlls <- character()
  function(name = "hier") {
    testthat::skip_on_cran()
    testthat::skip_if_not_installed("TMB")
    if (!name %in% dlls) {
      dir <- tempfile("poputils-tmb-")
      dir.create(dir)
      src <- testthat::test_path("tmb", paste0(name, ".cpp"))
      file.copy(src, dir)
      cpp <- file.path(dir, paste0(name, ".cpp"))
      inc <- system.file("include", package = "poputils")
      flags <- paste0("-w -I", shQuote(inc))
      utils::capture.output(suppressMessages(TMB::compile(cpp, framework = "TMBad", flags = flags)))
      dyn.load(TMB::dynlib(file.path(dir, name)))
      dlls <<- c(dlls, name)
    }
    name
  }
})

## Data for the test model: 4 groups
tmb_test_data <- function() {
  group <- rep(0:3, times = c(3, 5, 2, 6))
  set.seed(0)
  y <- stats::rnorm(length(group), mean = c(1, 2, 0, 3)[group + 1L], sd = 0.5)
  list(y = y, group = group)
}

## Objective function for the test model
make_tmb_test_f <- function(map = list(), random = "u", log_sd_u = log(0.8), log_sd_y = log(0.5)) {
  dll <- tmb_test_dll()
  TMB::MakeADFun(data = tmb_test_data(),
                 parameters = list(mu = 0, log_sd_u = log_sd_u, log_sd_y = log_sd_y, u = rep(0, 4)),
                 map = map,
                 random = random,
                 DLL = dll,
                 silent = TRUE)
}
