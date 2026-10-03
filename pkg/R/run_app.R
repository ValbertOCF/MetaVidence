#' Run the MetaVidence application
#'
#' Starts the MetaVidence 'shiny' application, a guided three-step interface for
#' meta-analysis: load the data, choose the parameters, review and export the
#' results. Everything runs locally; no data is sent anywhere.
#'
#' @param ... Arguments passed to [shiny::runApp()], for example `port` or
#'   `launch.browser`.
#'
#' @return A 'shiny' application object, invoked for the side effect of
#'   launching the application.
#'
#' @examples
#' # The application blocks the console while it runs, so it is only started
#' # in an interactive session.
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
#'
## O R CMD check so' le R/, nunca inst/. Como o app inteiro vive em
## inst/app/app.R, os 14 pacotes que ele usa apareceriam como "declared and
## not used". Estes @importFrom declaram uma funcao real de cada um: a
## dependencia passa a ser verificavel em vez de so' anunciada no DESCRIPTION.
#' @importFrom meta metabin
#' @importFrom metafor rma
#' @importFrom netmeta netmeta
#' @importFrom mada reitsma
#' @importFrom lme4 glmer
#' @importFrom lmtest lrtest
#' @importFrom ggplot2 element_text
#' @importFrom readxl read_excel
#' @importFrom openxlsx writeData
#' @importFrom zip zipr
#' @importFrom stats plogis
#' @importFrom utils capture.output
#' @importFrom grDevices adjustcolor
#' @importFrom graphics par
run_app <- function(...) {
  app_dir <- system.file("app", package = "metavidence")
  if (!nzchar(app_dir)) {
    stop("Could not find the application directory. Try reinstalling metavidence.")
  }
  shiny::runApp(app_dir, ...)
}

## O app.R usa avaliacao nao-padrao em varias chamadas de meta/metafor
## (por exemplo metafor::rma(yi, vi, data = ...)), onde os nomes existem so'
## dentro do data.frame passado. Sem isto o R CMD check acusa "no visible
## binding for global variable" para cada um deles.
utils::globalVariables(c(
  "yi", "vi", "studlab", "easy_meta_metareg_covariate",
  "event.e", "n.e", "event.c", "n.c",
  "mean.e", "sd.e", "mean.c", "sd.c",
  "event", "n", "mean", "sd",
  "TP", "FP", "FN", "TN",
  "TE", "seTE", "lower", "upper",
  "true", "sens", "spec", "seA1", "seB1", "spA1", "spB1",
  "tsens", "tfpr", "ID"
))
