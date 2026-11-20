## MetaVidence launcher. It installs the app on first use, updates it when a
## new version exists, and opens it. Run it with:
##
##   source("https://metavidence.com/start.R")
##
## or by clicking "Source" in the MetaVidence.R file offered in the tutorials,
## which calls this same script.
local({
  repos <- c("https://valbertocf.r-universe.dev", "https://cloud.r-project.org")

  if (getRversion() < "4.1.0") {
    stop("MetaVidence needs R 4.1 or newer. Please update R from https://cran.r-project.org",
         call. = FALSE)
  }

  ## On a first install on Windows, R stops to ask whether to use a "personal
  ## library" when it cannot write to the system one. Creating that library
  ## first means the question never appears.
  lib <- Sys.getenv("R_LIBS_USER")
  if (nzchar(lib) && !dir.exists(lib)) dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  if (nzchar(lib) && dir.exists(lib)) .libPaths(c(lib, .libPaths())) else lib <- .libPaths()[1]

  ## The newest version on offer, or NULL when there is no internet.
  latest <- tryCatch({
    ap <- utils::available.packages(repos = repos)
    if ("metavidence" %in% rownames(ap)) ap["metavidence", "Version"] else NULL
  }, error = function(e) NULL, warning = function(w) NULL)

  installed <- requireNamespace("metavidence", quietly = TRUE)

  install <- function() {
    ## Never stop to ask about compiling from source: the prebuilt versions are
    ## enough, and that question is where people give up.
    old <- options(install.packages.compile.from.source = "never")
    on.exit(options(old))
    utils::install.packages("metavidence", repos = repos, lib = lib)
  }

  if (!installed) {
    if (is.null(latest)) {
      stop("No internet connection, and MetaVidence is not installed yet. ",
           "Connect to the internet and try again.", call. = FALSE)
    }
    message("Installing MetaVidence (first time only, this takes a few minutes)...")
    install()
  } else if (!is.null(latest) &&
             utils::packageVersion("metavidence") < package_version(latest)) {
    message("Updating MetaVidence to version ", latest, "...")
    install()
  } else if (is.null(latest)) {
    message("No internet connection: opening the installed version.")
  }

  if (!requireNamespace("metavidence", quietly = TRUE)) {
    stop("MetaVidence could not be installed. See the messages above, or the ",
         "tutorial at https://metavidence.com/tutorials/en/get-started.html", call. = FALSE)
  }
  message("Opening MetaVidence...")
  metavidence::run_app()
})
