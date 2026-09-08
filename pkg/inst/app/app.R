library(shiny)

# ============================================================================
# ASSETS AND CITATION
#
# Where the app looks for its CSS and JS, and the one place the citation is
# written down. Every citation surface, the card inside the app, the tutorial page
# and the package CITATION file, is generated from METAVIDENCE_CITATION, so a new
# version or a published paper is updated here and nowhere else.
# ============================================================================

## Where the CSS and JS live, resolved at startup.
##
## The same app.R runs in three places: straight from the repository, inside the
## WebAssembly bundle, and installed as an R package. In the first two the assets
## sit in the working directory; in the third they sit in inst/app/. This has to
## be resolved here because shiny::runApp() does NOT change the working directory
## to the app directory (checked in the source: there is no setwd in runApp).
##
## Order matters. Local comes first, otherwise anyone editing the repository with
## the package also installed would silently be served the package's older CSS.

metavidence_assets <- function() {
  local <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE),
                    error = function(e) "")
  if (nzchar(local) && file.exists(file.path(local, "easymeta.css"))) return(local)
  do_pacote <- system.file("app", package = "metavidence")
  if (nzchar(do_pacote) && file.exists(file.path(do_pacote, "easymeta.css"))) return(do_pacote)
  stop("Could not locate easymeta.css. Run the app from its own directory, ",
       "or install the metavidence package.")
}

addResourcePath("assets", metavidence_assets())

## ---------------------------------------------------------------------------
## Citation: SINGLE SOURCE OF TRUTH.
##
## The same citation has to appear in four places: the app's "How to cite"
## screen, the package's inst/CITATION (what citation("metavidence") reads), the
## tutorial pages and the README. Keeping four copies by hand guarantees one
## falls behind. So everything derives from here: build_package.R generates
## inst/CITATION from this list, and design/check_citation.R verifies the other
## two have not drifted.
##
## WHEN THE PAPER IS PUBLISHED: fill in the `paper` block below. It then becomes
## the primary citation everywhere, and the software entry stays listed second,
## which is the convention for crediting the version actually run.
METAVIDENCE_CITATION <- list(
  software = list(
    ## Given names as a VECTOR, not one string. R's person() abbreviates each
    ## element: c("Valbert", "Oliveira") renders as "VO", whereas the single
    ## string "Valbert Oliveira" is treated as one given name and renders "V".
    given   = c("Valbert", "Oliveira"),
    family  = "Costa Filho",
    authors = "Costa Filho VO",
    orcid   = "0009-0003-2864-0966",
    year    = 2026,
    title   = "MetaVidence: no-code meta-analysis in the browser and in R",
    version = "0.1.0",
    url     = "https://metavidence.com"
  ),
  ## Where the current citation always lives. An installed copy can only show
  ## what shipped with it, and the paper is expected after this release.
  cite_page = "https://metavidence.com/tutorials/en/how-to-cite.html",
  paper = NULL
)

## The tutorials link has to be ABSOLUTE. A relative path breaks in both places
## where the app actually runs:
##   - in the R package, Shiny serves only the app, so /tutorials/ is a 404;
##   - on the website, shinylive mounts the app inside an <iframe class=
##     "app-frame">, so a relative path resolves against the iframe URL rather
##     than the site root.
## The loading screen is a different case: it lives at the top of
## docs/index.html, where a relative path is correct.
METAVIDENCE_TUTORIALS <- "https://metavidence.com/tutorials/en/"


# Builds the citation text from METAVIDENCE_CITATION. While paper is NULL only
# the software line exists; filling that field in once the article is published
# makes both lines appear in the app, the tutorial and the package at once.
metavidence_citation_lines <- function(cit = METAVIDENCE_CITATION) {
  s <- cit$software
  software <- sprintf("%s (%d). %s. Version %s. %s",
                      s$authors, s$year, s$title, s$version, s$url)
  if (is.null(cit$paper)) return(list(principal = software, software = NULL))

  p <- cit$paper
  opcional <- function(x, prefixo) {
    if (is.null(x) || !nzchar(x)) "" else paste0(prefixo, x)
  }
  artigo <- sprintf("%s (%d). %s. %s%s%s",
                    p$authors, p$year, p$title, p$journal,
                    opcional(p$detail, ". "), opcional(p$doi, ". doi:"))
  list(principal = artigo, software = software)
}

## -------------------------------------------------------------------------
## Module catalogue
##
## Label, subtitle and icon for each card on the home page. The order here is the
## order the cards appear in. Adding a module means adding an entry here, a page
## builder, a validator, an analyzer and an entry in module_data_specs.
## -------------------------------------------------------------------------


# ============================================================================
# HOME SCREEN
#
# The landing page: the list of modules, the icon and card for each one, and the
# card used by the two screens that ask the user to pick a workflow.
# ============================================================================

module_choices <- list(
  binary = list(
    label = "Binary outcome data",
    subtitle = "Meta-analysis for dichotomous outcomes",
    icon = "01"
  ),
  continuous = list(
    label = "Continuous outcome data",
    subtitle = "Mean, SD, median and IQR workflows",
    icon = "02"
  ),
  precalculated = list(
    label = "Pre-calculated effect size data",
    subtitle = "Use treatment effects already calculated",
    icon = "03"
  ),
  single_arm = list(
    label = "Single arm meta-analysis",
    subtitle = "Single proportions and single means",
    icon = "04"
  ),
  network = list(
    label = "Network meta-analysis",
    subtitle = "Frequentist NMA workflows",
    icon = "05"
  ),
  diagnostic = list(
    label = "Diagnostic meta-analysis",
    subtitle = "Sensitivity, specificity, summary point and DOR",
    icon = "06"
  )
)

# The inline SVG on a module card.
module_icon <- function(id) {
  icons <- c(
    binary = '<svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"><circle cx="12" cy="12" r="8.2" stroke="currentColor" stroke-width="1.8"/><path d="M12 3.8a8.2 8.2 0 0 1 0 16.4z" fill="currentColor"/></svg>',
    continuous = '<svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M3 19.5h18" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" opacity="0.55"/><path d="M3 18.5c4.2 0 4.8-12.5 9-12.5s4.8 12.5 9 12.5" stroke="currentColor" stroke-width="1.9" stroke-linecap="round"/></svg>',
    precalculated = '<svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M2.5 12h4M17.5 12h4" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" opacity="0.55"/><path d="M6.5 12l5.5-4.4L17.5 12 12 16.4z" fill="currentColor"/></svg>',
    single_arm = '<svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M12 3.5v17M8.5 3.5h7M8.5 20.5h7" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"/><rect x="9.2" y="9.2" width="5.6" height="5.6" rx="1.2" fill="currentColor"/></svg>',
    network = '<svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M12 6.5L5.8 16M12 6.5l6.2 9.5M7 17h10" stroke="currentColor" stroke-width="1.7" stroke-linecap="round"/><circle cx="12" cy="5" r="2.6" fill="currentColor"/><circle cx="5" cy="17.5" r="2.6" fill="currentColor"/><circle cx="19" cy="17.5" r="2.6" fill="currentColor"/></svg>',
    diagnostic = '<svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"><circle cx="12" cy="12" r="8.2" stroke="currentColor" stroke-width="1.8"/><circle cx="12" cy="12" r="4.4" stroke="currentColor" stroke-width="1.8"/><circle cx="12" cy="12" r="1.4" fill="currentColor"/><path d="M12 1.8v3M12 19.2v3M1.8 12h3M19.2 12h3" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"/></svg>'
  )
  if (!id %in% names(icons)) {
    return(NULL)
  }
  HTML(icons[[id]])
}

# One card on the home screen: icon, title, one line of description, and the
# button that opens the module.
module_card <- function(id, details) {
  actionButton(
    inputId = paste0("go_", id),
    label = tags$span(
      class = "module-card-content",
      tags$span(class = "module-number", module_icon(id)),
      tags$span(class = "module-text",
        tags$span(class = "module-title", HTML(details$label)),
        tags$span(class = "module-subtitle", details$subtitle)
      ),
      tags$span(class = "module-arrow", HTML("&rarr;"))
    ),
    class = "module-card"
  )
}

# The card used by the four screens where a module offers more than one entry
# point, single-arm, diagnostic, pre-calculated and network, so the choice
# of workflow is made before any data is asked for.
workflow_option_card <- function(input_id, title, text, cols = NULL) {
  actionButton(
    inputId = input_id,
    label = tags$span(
      class = "wf-option-content",
      tags$span(
        class = "wf-option-body",
        tags$span(class = "wf-option-title", title),
        tags$span(class = "wf-option-text", text),
        if (!is.null(cols)) {
          tags$span(
            class = "wf-option-cols",
            tags$span(class = "wf-option-cols-label", "Required columns"),
            lapply(cols, function(col) tags$code(col))
          )
        }
      ),
      tags$span(class = "wf-option-go", HTML("Start&nbsp;&rarr;"))
    ),
    class = "wf-option-card"
  )
}

## -------------------------------------------------------------------------
## Pages: the interface of each module
##
## Each function returns one module's screen, laid out as the three steps: data,
## parameters, results. These are tag trees only. No statistics happen here, and
## nothing here reads the data: the server fills the outputs in later.
## -------------------------------------------------------------------------


# ============================================================================
# MODULE SCREENS
#
# One function per screen, markup only. A *_page() builds the three steps of a
# module, data then parameters then results, as a hidden tabsetPanel the server
# moves through. Nothing is computed here: every number, plot and status line is
# an output the server fills in.
# 
# These screens are spread across the file rather than gathered in one block
# because each sits next to the module logic it belongs to.
# ============================================================================

# Choice screen for single-arm data: proportions or means.
single_arm_page <- function() {
  div(
    class = "workflow-page",
    actionButton("back_home_single_arm", "Back to home", class = "back-button"),
    div(
      class = "workflow-header",
      tags$span(class = "wf-header-icon", module_icon("single_arm")),
      tags$p(class = "eyebrow", "Single arm meta-analysis"),
      tags$h2("What do you want to pool?"),
      tags$p("For single-group studies without a comparator arm. Pick the summary your studies report.")
    ),
    div(
      class = "workflow-options",
      workflow_option_card(
        "go_single_proportions",
        "Single proportions",
        "Pool event rates across single-group studies, with GLMM or inverse-variance methods.",
        c("study", "event", "n")
      ),
      workflow_option_card(
        "go_single_mean",
        "Single means",
        "Pool a continuous measurement, reported as mean and SD or as median and quartiles.",
        c("study", "n", "mean + sd", "or median + q1 + q3")
      )
    )
  )
}

# Choice screen for diagnostic accuracy: one test, or two tests compared.
diagnostic_page <- function() {
  div(
    class = "workflow-page",
    actionButton("back_home_diagnostic", "Back to home", class = "back-button"),
    div(
      class = "workflow-header",
      tags$span(class = "wf-header-icon", module_icon("diagnostic")),
      tags$p(class = "eyebrow", "Diagnostic meta-analysis"),
      tags$h2("Choose your diagnostic workflow"),
      tags$p("Pool the accuracy of one index test, or compare two tests head-to-head within the same studies.")
    ),
    div(
      class = "workflow-options",
      workflow_option_card(
        "go_diagnostic_single",
        "Single test accuracy",
        "Summary sensitivity, specificity and diagnostic odds ratio for one index test, from the bivariate model.",
        c("study", "TP", "FP", "FN", "TN")
      ),
      workflow_option_card(
        "go_diagnostic_comparative",
        "Compare two tests",
        "Head-to-head comparison of two diagnostic tests using paired rows per study.",
        c("study", "test", "TP", "FP", "FN", "TN")
      )
    )
  )
}

# Workflow screen: single-arm proportions.
single_proportions_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_single_arm", "Back to Single arm", class = "back-button"),
      actionButton("back_home_from_single_prop", "Back to home", class = "back-button secondary-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Single proportions"),
      tags$h2("Single proportions workflow"),
      tags$p("Move step by step: add your dataset, choose the analysis parameters, then review and export your results.")
    ),
    div(class = "step-indicator", textOutput("single_prop_step_label")),
    tabsetPanel(
      id = "single_prop_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Paste directly from Google Sheets, upload an Excel/CSV file, or load the example dataset with 12 studies."),
            tags$p(class = "help-text required-columns", HTML("<strong>Required columns (use these exact Excel column names):</strong><br><strong>study</strong>: the study label, for example first author and year.<br><strong>event</strong>: number of participants with the outcome/event of interest.<br><strong>n</strong>: total sample size in the study arm.<br>Optional columns can be used later for subgroup analysis or meta-regression.")),
            actionButton("load_single_prop_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_single_prop_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "single_prop_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "single_prop_paste",
              "Paste data from Google Sheets",
              placeholder = "study\tevent\tn\tRegion\nStudy 1\t12\t100\tNorth",
              rows = 10
            ),
            actionButton("use_single_prop_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("single_prop_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("More than one outcome"),
            tags$p(class = "help-text microcopy", "Upload one Excel file. Each sheet = one outcome."),
            tags$p(class = "help-text microcopy", "Name each sheet with the outcome name. MetaVidence uses sheet names in plots and exported files."),
            tags$p(class = "help-text microcopy", "Column names must match exactly. Batch mode exports all outcomes together."),
            tags$div(
              class = "em-upload-row",
              fileInput(
                "single_prop_multi_file",
                "Import multi-outcome workbook (.xlsx)",
                accept = c(".xlsx", ".xls")
              ),
              downloadButton("download_single_prop_batch_template", "Download XLSX template", class = "secondary-button")
            ),
            tags$div(class = "status-message", textOutput("single_prop_multi_status"))
          ),
          data_check_card(
            "single_prop_data_check",
            actionButton("single_prop_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
              div(id = "single_prop_outcome_wrap", textInput("single_prop_outcome", "Outcome name", value = "Outcome")),
              selectInput("single_prop_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
              selectInput("single_prop_sm", "Summary measure (sm)", choices = c("PLOGIT", "PFT", "PAS", "PLN", "PRAW"), selected = "PLOGIT"),
              selectInput("single_prop_method", "Method", choices = c("Inverse", "GLMM"), selected = "Inverse"),
              selectInput("single_prop_method_tau", "Tau method", choices = c("REML", "ML", "DL", "PM", "SJ", "HE", "HS", "EB"), selected = "REML"),
              selectInput("single_prop_method_i2", "I-squared method", choices = c("Q (Higgins and Thompson)" = "Q", "From tau-squared" = "tau2"), selected = "Q"),
                selectInput("single_prop_method_random_ci", "Random CI method", choices = c("classic", "HK", "KR"), selected = "classic"),
              selectInput(
                "single_prop_col_square",
                "Forest square color",
                choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Dodger blue" = "dodgerblue3", "Navy" = "navy", "Black" = "black", "Gray" = "gray40", "Red" = "red3", "Dark green" = "darkgreen"),
                selected = "red3",
                selectize = FALSE
              ),
              selectInput(
                "single_prop_col_square_lines",
                "Forest square line color",
                choices = c("Black" = "black", "Dark blue" = "darkblue", "Gray" = "gray40", "White" = "white", "Navy" = "navy"),
                selected = "black",
                selectize = FALSE
              ),
              selectInput(
                "single_prop_forest_sort",
                "Sort studies by effect size",
                choices = c("No" = "no", "Yes" = "yes"),
                selected = "no",
                selectize = FALSE
              ),
              forest_column_selector("single_prop_forest_cols")
            ),
            tags$div(class = "status-message prominent-status", textOutput("single_prop_run_status")),
            div(
              class = "step-actions",
              actionButton("single_prop_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_single_prop", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("single_prop_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Forest plot"),
              tags$p(class = "help-text", "Open the forest plot, inspect the model summary, or download the figure."),
              result_action_group("single_prop_forest", "download_single_prop_forest", "preview_single_prop_forest", "summary_single_prop_main", "prepare_single_prop_forest")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Leave-one-out analysis"),
              tags$p(class = "help-text", "Preview the sensitivity analysis or download the leave-one-out forest plot."),
              result_action_group("single_prop_loo", "download_single_prop_loo", "preview_single_prop_loo", NULL, "prepare_single_prop_loo")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Funnel plot"),
              tags$p(class = "help-text", "Preview the funnel plot or download it for reporting."),
              result_action_group("single_prop_funnel", "download_single_prop_funnel", "preview_single_prop_funnel", NULL, "prepare_single_prop_funnel")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Small-study effects"),
              tags$p(class = "help-text", "Open the Egger test result and interpretation note."),
              div(
                class = "result-button-row",
                actionButton("summary_single_prop_bias", "Summary", class = "result-action")
              )
            ),
            div(
              id = "single_prop_subgroup_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Subgroup analysis"),
              tags$p(id = "single_prop_subgroup_help_single", class = "help-text", "Pick a column to draw its subgroup forest plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "single_prop_subgroup_help_batch", class = "help-text", style = "display:none;", "One subgroup forest plot per outcome, all using the column above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("single_prop_subgroup_pick", "Subgroup column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("single_prop_subgroup", "download_single_prop_subgroup", "preview_single_prop_subgroup", "summary_single_prop_subgroup", "prepare_single_prop_subgroup")
            ),
            div(
              id = "single_prop_metareg_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Meta-regression"),
              tags$p(id = "single_prop_metareg_help_single", class = "help-text", "Pick a numeric moderator to draw its bubble plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "single_prop_metareg_help_batch", class = "help-text", style = "display:none;", "One bubble plot per outcome, all using the moderator above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("single_prop_metareg_pick", "Moderator column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("single_prop_metareg", "download_single_prop_metareg", "preview_single_prop_metareg", "summary_single_prop_metareg", "prepare_single_prop_metareg")
            ),
            summary_table_cards("single_prop")
          ),
          div(
            class = "step-actions",
            actionButton("single_prop_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

# Workflow screen: single-arm means.
single_mean_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_single_arm_mean", "Back to Single arm", class = "back-button"),
      actionButton("back_home_from_single_mean", "Back to home", class = "back-button secondary-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Single mean"),
      tags$h2("Single mean workflow"),
      tags$p("Move step by step: add your dataset, choose the analysis parameters, then review and export your results.")
    ),
    div(class = "step-indicator", textOutput("single_mean_step_label")),
    tabsetPanel(
      id = "single_mean_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Paste directly from Google Sheets, upload an Excel/CSV file, or load the example dataset with 12 studies."),
              tags$p(class = "help-text required-columns", HTML(paste0("<strong>Required columns (use these exact Excel column names):</strong><br>","<strong>study</strong>: the study label, for example first author and year.<br>","<strong>n</strong>: total sample size in the study.<br>","<br><strong>Then, how the outcome was reported.</strong> ","Mean and standard deviation is the ideal input:<br>","<strong>mean</strong>, <strong>sd</strong>.<br>","<br>A study that only reports a median is accepted too. Give the median with its quartiles:<br>","<strong>median</strong>, <strong>q1</strong>, <strong>q3</strong>. ","<strong>min</strong> and <strong>max</strong> can replace the quartiles, less precisely.<br>","<br>You can mix the two in one file: fill the columns each study reports and leave the rest empty. ","MetaVidence converts the median studies into a mean and an SD and tells you how many it converted.<br>","Optional columns can be used later for subgroup analysis or meta-regression."))),
            actionButton("load_single_mean_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_single_mean_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "single_mean_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "single_mean_paste",
              "Paste data from Google Sheets",
              placeholder = "study\tmean\tsd\tn\tRegion\nStudy 1\t6.4\t1.2\t80\tNorth",
              rows = 10
            ),
            actionButton("use_single_mean_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("single_mean_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("More than one outcome"),
            tags$p(class = "help-text microcopy", "Upload one Excel file. Each sheet = one outcome."),
            tags$p(class = "help-text microcopy", "Name each sheet with the outcome name. MetaVidence uses sheet names in plots and exported files."),
            tags$p(class = "help-text microcopy", "Column names must match exactly. Batch mode exports all outcomes together."),
            tags$div(
              class = "em-upload-row",
              fileInput(
                "single_mean_multi_file",
                "Import multi-outcome workbook (.xlsx)",
                accept = c(".xlsx", ".xls")
              ),
              downloadButton("download_single_mean_batch_template", "Download XLSX template", class = "secondary-button")
            ),
            tags$div(class = "status-message", textOutput("single_mean_multi_status"))
          ),
          data_check_card(
            "single_mean_data_check",
            actionButton("single_mean_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
              div(id = "single_mean_outcome_wrap", textInput("single_mean_outcome", "Outcome name", value = "Outcome")),
              selectInput("single_mean_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
              selectInput("single_mean_sm", "Summary measure (sm)", choices = c("MRAW", "MLN"), selected = "MRAW"),
              selectInput("single_mean_method_tau", "Tau method", choices = c("REML", "ML", "DL", "PM", "SJ", "HE", "HS", "EB"), selected = "REML"),
              selectInput("single_mean_method_i2", "I-squared method", choices = c("Q (Higgins and Thompson)" = "Q", "From tau-squared" = "tau2"), selected = "Q"),
                selectInput("single_mean_method_random_ci", "Random CI method", choices = c("classic", "HK", "KR"), selected = "classic"),
              selectInput("single_mean_prediction", "Prediction interval", choices = c("Yes" = "yes", "No" = "no"), selected = "yes"),
              selectInput(
                "single_mean_col_square",
                "Forest square color",
                choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Dodger blue" = "dodgerblue3", "Navy" = "navy", "Black" = "black", "Gray" = "gray40", "Red" = "red3", "Dark green" = "darkgreen"),
                selected = "red3",
                selectize = FALSE
              ),
              selectInput(
                "single_mean_col_square_lines",
                "Forest square line color",
                choices = c("Black" = "black", "Dark blue" = "darkblue", "Gray" = "gray40", "White" = "white", "Navy" = "navy"),
                selected = "black",
                selectize = FALSE
              ),
              selectInput(
                "single_mean_forest_sort",
                "Sort studies by effect size",
                choices = c("No" = "no", "Yes" = "yes"),
                selected = "no",
                selectize = FALSE
              ),
              forest_column_selector("single_mean_forest_cols"),
              tags$div()
            ),
            tags$div(class = "status-message prominent-status", textOutput("single_mean_run_status")),
            div(
              class = "step-actions",
              actionButton("single_mean_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_single_mean", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("single_mean_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Forest plot"),
              tags$p(class = "help-text", "Open the forest plot, inspect the model summary, or download the figure."),
              result_action_group("single_mean_forest", "download_single_mean_forest", "preview_single_mean_forest", "summary_single_mean_main", "prepare_single_mean_forest")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Leave-one-out analysis"),
              tags$p(class = "help-text", "Preview the sensitivity analysis or download the leave-one-out forest plot."),
              result_action_group("single_mean_loo", "download_single_mean_loo", "preview_single_mean_loo", NULL, "prepare_single_mean_loo")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Funnel plot"),
              tags$p(class = "help-text", "Preview the funnel plot or download it for reporting."),
              result_action_group("single_mean_funnel", "download_single_mean_funnel", "preview_single_mean_funnel", NULL, "prepare_single_mean_funnel")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Small-study effects"),
              tags$p(class = "help-text", "Open the Egger test result and interpretation note."),
              div(
                class = "result-button-row",
                actionButton("summary_single_mean_bias", "Summary", class = "result-action")
              )
            ),
            div(
              id = "single_mean_subgroup_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Subgroup analysis"),
              tags$p(id = "single_mean_subgroup_help_single", class = "help-text", "Pick a column to draw its subgroup forest plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "single_mean_subgroup_help_batch", class = "help-text", style = "display:none;", "One subgroup forest plot per outcome, all using the column above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("single_mean_subgroup_pick", "Subgroup column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("single_mean_subgroup", "download_single_mean_subgroup", "preview_single_mean_subgroup", "summary_single_mean_subgroup", "prepare_single_mean_subgroup")
            ),
            div(
              id = "single_mean_metareg_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Meta-regression"),
              tags$p(id = "single_mean_metareg_help_single", class = "help-text", "Pick a numeric moderator to draw its bubble plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "single_mean_metareg_help_batch", class = "help-text", style = "display:none;", "One bubble plot per outcome, all using the moderator above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("single_mean_metareg_pick", "Moderator column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("single_mean_metareg", "download_single_mean_metareg", "preview_single_mean_metareg", "summary_single_mean_metareg", "prepare_single_mean_metareg")
            ),
            summary_table_cards("single_mean")
          ),
          div(
            class = "step-actions",
            actionButton("single_mean_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

# Workflow screen: two-arm binary outcome, the RR, OR and RD module.
binary_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_home_from_binary", "Back to home", class = "back-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Binary outcome data"),
      tags$h2("Binary outcome workflow"),
      tags$p("Move step by step: add your dataset, choose the analysis parameters, then review and export your results.")
    ),
    div(class = "step-indicator", textOutput("binary_step_label")),
    tabsetPanel(
      id = "binary_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Paste directly from Google Sheets, upload an Excel/CSV file, or load the example dataset with 12 studies."),
            tags$p(class = "help-text required-columns", HTML("<strong>Required columns (use these exact Excel column names):</strong><br><strong>study</strong>: the study label, for example first author and year.<br><strong>event.e</strong>: number of events in the experimental group.<br><strong>n.e</strong>: total sample size in the experimental group.<br><strong>event.c</strong>: number of events in the control group.<br><strong>n.c</strong>: total sample size in the control group.<br>Optional columns can be used later for subgroup analysis or meta-regression.")),
            actionButton("load_binary_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_binary_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "binary_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "binary_paste",
              "Paste data from Google Sheets",
              placeholder = "study\tevent.e\tn.e\tevent.c\tn.c\tRegion\nStudy 1\t12\t100\t20\t100\tNorth",
              rows = 10
            ),
            actionButton("use_binary_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("binary_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("More than one outcome"),
            tags$p(class = "help-text microcopy", "Upload one Excel file. Each sheet = one outcome."),
            tags$p(class = "help-text microcopy", "Name each sheet with the outcome name. MetaVidence uses sheet names in plots and exported files."),
            tags$p(class = "help-text microcopy", "Column names must match exactly. Batch mode exports all outcomes together."),
            tags$div(
              class = "em-upload-row",
              fileInput(
                "binary_multi_file",
                "Import multi-outcome workbook (.xlsx)",
                accept = c(".xlsx", ".xls")
              ),
              downloadButton("download_binary_batch_template", "Download XLSX template", class = "secondary-button")
            ),
            tags$div(class = "status-message", textOutput("binary_multi_status"))
          ),
          data_check_card(
            "binary_data_check",
            actionButton("binary_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
                div(id = "binary_outcome_wrap", textInput("binary_outcome", "Outcome name", value = "Outcome")),
                selectInput("binary_outcome_direction", "If the outcome increases, is that beneficial or harmful?", choices = c("Harmful" = "harmful", "Beneficial" = "beneficial"), selected = "harmful"),
                selectInput("binary_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
                selectInput("binary_sm", "Summary measure (sm)", choices = c("RR", "OR", "RD"), selected = "RR"),
                selectInput("binary_method", "Method", choices = c("inverse", "MH", "Peto", "GLMM"), selected = "inverse"),
                selectInput("binary_method_tau", "Tau method", choices = c("REML", "ML", "DL", "PM", "SJ", "HE", "HS", "EB"), selected = "REML"),
                selectInput("binary_method_i2", "I-squared method", choices = c("Q (Higgins and Thompson)" = "Q", "From tau-squared" = "tau2"), selected = "Q"),
                selectInput("binary_prediction", "Prediction interval", choices = c("Yes" = "yes", "No" = "no"), selected = "no"),
                selectInput("binary_method_random_ci", "Random CI method", choices = c("classic", "HK", "KR"), selected = "classic"),
                selectInput("binary_method_predict", "Prediction method", choices = c("V", "HTS"), selected = "V"),
                textInput("binary_label_e", "Experimental group name", value = "Experimental"),
                textInput("binary_label_c", "Control group name", value = "Control"),
                selectInput(
                "binary_col_square",
                "Forest square color",
                choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Dodger blue" = "dodgerblue3", "Navy" = "navy", "Black" = "black", "Gray" = "gray40", "Red" = "red3", "Dark green" = "darkgreen"),
                selected = "red3",
                selectize = FALSE
              ),
              selectInput(
                "binary_col_square_lines",
                "Forest square line color",
                choices = c("Black" = "black", "Dark blue" = "darkblue", "Gray" = "gray40", "White" = "white", "Navy" = "navy"),
                selected = "black",
                selectize = FALSE
              ),
              selectInput(
                "binary_forest_sort",
                "Sort studies by effect size",
                choices = c("No" = "no", "Yes" = "yes"),
                selected = "no",
                selectize = FALSE
              ),
              forest_column_selector("binary_forest_cols")
            ),
            tags$div(class = "status-message prominent-status", textOutput("binary_run_status")),
            div(
              class = "step-actions",
              actionButton("binary_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_binary", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("binary_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Forest plot"),
              tags$p(class = "help-text", "Open the forest plot, inspect the model summary, or download the figure."),
              result_action_group("binary_forest", "download_binary_forest", "preview_binary_forest", "summary_binary_main", "prepare_binary_forest")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Leave-one-out analysis"),
              tags$p(class = "help-text", "Preview the sensitivity analysis or download the leave-one-out forest plot."),
              result_action_group("binary_loo", "download_binary_loo", "preview_binary_loo", NULL, "prepare_binary_loo")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Funnel plot"),
              tags$p(class = "help-text", "Preview the funnel plot or download it for reporting."),
              result_action_group("binary_funnel", "download_binary_funnel", "preview_binary_funnel", NULL, "prepare_binary_funnel")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Small-study effects"),
              tags$p(class = "help-text", "Open the Egger test result and interpretation note."),
              div(
                class = "result-button-row",
                actionButton("summary_binary_bias", "Summary", class = "result-action")
              )
            ),
            div(
              id = "binary_subgroup_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Subgroup analysis"),
              tags$p(id = "binary_subgroup_help_single", class = "help-text", "Pick a column to draw its subgroup forest plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "binary_subgroup_help_batch", class = "help-text", style = "display:none;", "One subgroup forest plot per outcome, all using the column above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("binary_subgroup_pick", "Subgroup column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("binary_subgroup", "download_binary_subgroup", "preview_binary_subgroup", "summary_binary_subgroup", "prepare_binary_subgroup")
            ),
            div(
              id = "binary_metareg_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Meta-regression"),
              tags$p(id = "binary_metareg_help_single", class = "help-text", "Pick a numeric moderator to draw its bubble plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "binary_metareg_help_batch", class = "help-text", style = "display:none;", "One bubble plot per outcome, all using the moderator above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("binary_metareg_pick", "Moderator column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("binary_metareg", "download_binary_metareg", "preview_binary_metareg", "summary_binary_metareg", "prepare_binary_metareg")
            ),
            summary_table_cards("binary")
          ),
          div(
            class = "step-actions",
            actionButton("binary_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

# Workflow screen: two-arm continuous outcome reported as mean and SD.
continuous_mean_sd_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_home_from_cont_mean", "Back to home", class = "back-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Continuous outcome data"),
      tags$h2("Continuous outcome workflow"),
      tags$p("Move step by step: add your dataset, choose the analysis parameters, then review and export your results.")
    ),
    div(class = "step-indicator", textOutput("cont_mean_step_label")),
    tabsetPanel(
      id = "cont_mean_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Paste directly from Google Sheets, upload an Excel/CSV file, or load the example dataset with 12 studies."),
            tags$p(class = "help-text required-columns", HTML(paste0("<strong>Required columns (use these exact Excel column names):</strong><br>","<strong>study</strong>: the study label, for example first author and year.<br>","<strong>n.e</strong>, <strong>n.c</strong>: sample size in the experimental and the control group.<br>","<br><strong>Then, for each group, how the outcome was reported.</strong> ","Mean and standard deviation is the ideal input:<br>","<strong>mean.e</strong>, <strong>sd.e</strong>, <strong>mean.c</strong>, <strong>sd.c</strong>.<br>","<br>A study that only reports a median is accepted too. Give the median with its quartiles:<br>","<strong>median.e</strong>, <strong>q1.e</strong>, <strong>q3.e</strong>, ","<strong>median.c</strong>, <strong>q1.c</strong>, <strong>q3.c</strong>. ","<strong>min</strong> and <strong>max</strong> can replace the quartiles, less precisely.<br>","<br>You can mix the two in one file: fill the columns each study reports and leave the rest empty. ","MetaVidence converts the median studies into a mean and an SD and tells you how many it converted.<br>","Optional columns can be used later for subgroup analysis or meta-regression."))),
            actionButton("load_cont_mean_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_cont_mean_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput("cont_mean_file", "Import Excel or CSV", accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")),
            textAreaInput(
              "cont_mean_paste",
              "Paste data from Google Sheets",
              placeholder = "study\tn.e\tmean.e\tsd.e\tn.c\tmean.c\tsd.c\tRegion\nStudy 1\t80\t6.4\t1.2\t78\t7.3\t1.5\tNorth",
              rows = 10
            ),
            actionButton("use_cont_mean_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("cont_mean_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("More than one outcome"),
            tags$p(class = "help-text microcopy", "Upload one Excel file. Each sheet = one outcome."),
            tags$p(class = "help-text microcopy", "Name each sheet with the outcome name. MetaVidence uses sheet names in plots and exported files."),
            tags$p(class = "help-text microcopy", "Column names must match exactly. Batch mode exports all outcomes together."),
            tags$div(
              class = "em-upload-row",
              fileInput(
                "cont_mean_multi_file",
                "Import multi-outcome workbook (.xlsx)",
                accept = c(".xlsx", ".xls")
              ),
              downloadButton("download_cont_mean_batch_template", "Download XLSX template", class = "secondary-button")
            ),
            tags$div(class = "status-message", textOutput("cont_mean_multi_status"))
          ),
          data_check_card(
            "cont_mean_data_check",
            actionButton("cont_mean_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
                div(id = "cont_mean_outcome_wrap", textInput("cont_mean_outcome", "Outcome name", value = "Outcome")),
                selectInput("cont_mean_outcome_direction", "If the outcome increases, is that beneficial or harmful?", choices = c("Harmful" = "harmful", "Beneficial" = "beneficial"), selected = "harmful"),
                selectInput("cont_mean_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
                selectInput("cont_mean_sm", "Summary measure (sm)", choices = c("MD", "SMD"), selected = "MD"),
                selectInput("cont_mean_method_tau", "Tau method", choices = c("REML", "ML", "DL", "PM", "SJ", "HE", "HS", "EB"), selected = "REML"),
                selectInput("cont_mean_method_i2", "I-squared method", choices = c("Q (Higgins and Thompson)" = "Q", "From tau-squared" = "tau2"), selected = "Q"),
                selectInput("cont_mean_prediction", "Prediction interval", choices = c("Yes" = "yes", "No" = "no"), selected = "yes"),
                selectInput("cont_mean_method_random_ci", "Random CI method", choices = c("classic", "HK", "KR"), selected = "classic"),
                selectInput("cont_mean_method_predict", "Prediction method", choices = c("V", "HTS"), selected = "V"),
                textInput("cont_mean_label_e", "Experimental group name", value = "Experimental"),
                textInput("cont_mean_label_c", "Control group name", value = "Control"),
                selectInput(
                "cont_mean_col_square",
                "Forest square color",
                choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Dodger blue" = "dodgerblue3", "Navy" = "navy", "Black" = "black", "Gray" = "gray40", "Red" = "red3", "Dark green" = "darkgreen"),
                selected = "red3",
                selectize = FALSE
              ),
              selectInput(
                "cont_mean_col_square_lines",
                "Forest square line color",
                choices = c("Black" = "black", "Dark blue" = "darkblue", "Gray" = "gray40", "White" = "white", "Navy" = "navy"),
                selected = "black",
                selectize = FALSE
              ),
              selectInput(
                "cont_mean_forest_sort",
                "Sort studies by effect size",
                choices = c("No" = "no", "Yes" = "yes"),
                selected = "no",
                selectize = FALSE
              ),
              forest_column_selector("cont_mean_forest_cols")
            ),
            tags$div(class = "status-message prominent-status", textOutput("cont_mean_run_status")),
            div(
              class = "step-actions",
              actionButton("cont_mean_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_cont_mean", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("cont_mean_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Forest plot"),
              tags$p(class = "help-text", "Open the forest plot, inspect the model summary, or download the figure."),
              result_action_group("cont_mean_forest", "download_cont_mean_forest", "preview_cont_mean_forest", "summary_cont_mean_main", "prepare_cont_mean_forest")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Leave-one-out analysis"),
              tags$p(class = "help-text", "Preview the sensitivity analysis or download the leave-one-out forest plot."),
              result_action_group("cont_mean_loo", "download_cont_mean_loo", "preview_cont_mean_loo", NULL, "prepare_cont_mean_loo")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Funnel plot"),
              tags$p(class = "help-text", "Preview the funnel plot or download it for reporting."),
              result_action_group("cont_mean_funnel", "download_cont_mean_funnel", "preview_cont_mean_funnel", NULL, "prepare_cont_mean_funnel")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Small-study effects"),
              tags$p(class = "help-text", "Open the Egger test result and interpretation note."),
              div(
                class = "result-button-row",
                actionButton("summary_cont_mean_bias", "Summary", class = "result-action")
              )
            ),
            div(
              id = "cont_mean_subgroup_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Subgroup analysis"),
              tags$p(id = "cont_mean_subgroup_help_single", class = "help-text", "Pick a column to draw its subgroup forest plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "cont_mean_subgroup_help_batch", class = "help-text", style = "display:none;", "One subgroup forest plot per outcome, all using the column above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("cont_mean_subgroup_pick", "Subgroup column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("cont_mean_subgroup", "download_cont_mean_subgroup", "preview_cont_mean_subgroup", "summary_cont_mean_subgroup", "prepare_cont_mean_subgroup")
            ),
            div(
              id = "cont_mean_metareg_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Meta-regression"),
              tags$p(id = "cont_mean_metareg_help_single", class = "help-text", "Pick a numeric moderator to draw its bubble plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "cont_mean_metareg_help_batch", class = "help-text", style = "display:none;", "One bubble plot per outcome, all using the moderator above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("cont_mean_metareg_pick", "Moderator column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("cont_mean_metareg", "download_cont_mean_metareg", "preview_cont_mean_metareg", "summary_cont_mean_metareg", "prepare_cont_mean_metareg")
            ),
            summary_table_cards("cont_mean")
          ),
          div(
            class = "step-actions",
            actionButton("cont_mean_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

## -------------------------------------------------------------------------
## Example datasets
##
## Twelve fictional studies per module, used by the Load example dataset button
## and by the spreadsheet templates. The numbers come from no real study; they
## exist so a new user can walk the whole flow before preparing anything.
## design/check_examples.R verifies each one still passes its own validator.
## -------------------------------------------------------------------------


# ============================================================================
# EXAMPLE DATASETS
#
# What Load example dataset puts on screen. They are synthetic, and each carries
# 12 studies, which is what the tutorial pages state. The optional columns,
# Region, Design, RiskOfBias, MeanAge and FollowUpMonths, exist so the subgroup
# and meta-regression cards have something to offer straight away.
# ============================================================================

# Example: 12 single-arm studies reporting events out of a total.
single_prop_example_data <- function() {
  data.frame(
    study = paste("Study", LETTERS[1:12]),
    event = c(12, 18, 9, 31, 22, 15, 28, 17, 20, 14, 26, 11),
    n = c(120, 150, 95, 210, 180, 130, 240, 160, 175, 125, 220, 105),
    Region = c("North America", "Europe", "Europe", "Asia", "Asia", "North America", "Latin America", "Latin America", "North America", "Europe", "Asia", "Latin America"),
    Design = c("Prospective", "Prospective", "Retrospective", "Prospective", "Retrospective", "Prospective", "Prospective", "Retrospective", "Prospective", "Retrospective", "Prospective", "Retrospective"),
    RiskOfBias = c("Low", "Low", "Some concerns", "Low", "High", "Some concerns", "Low", "Some concerns", "High", "Low", "Some concerns", "High"),
    MeanAge = c(61, 58, 65, 57, 63, 60, 55, 59, 62, 56, 64, 58),
    FollowUpMonths = c(12, 18, 9, 24, 18, 12, 24, 15, 18, 12, 24, 9),
    check.names = FALSE
  )
}

# Example: 12 single-arm studies. Eight report a mean with its SD, which is the
# ideal input, and the last four report a median with quartiles instead. The
# quartiles were chosen around each median so the converted mean lands close to
# the value the study would have reported.
single_mean_example_data <- function() {
  na4 <- rep(NA_real_, 4)
  na8 <- rep(NA_real_, 8)
  data.frame(
    study = paste("Study", LETTERS[1:12]),
    n = c(80, 95, 110, 130, 90, 120, 75, 140, 105, 115, 125, 85),
    mean = c(c(5.8, 6.4, 7.1, 8.2, 6.9, 7.8, 5.9, 8.5), na4),
    sd = c(c(1.2, 1.5, 1.7, 2.1, 1.4, 1.8, 1.1, 2.0), na4),
    median = c(na8, 6.6, 7.4, 8.0, 6.2),
    q1 = c(na8, 5.7, 6.3, 6.7, 5.4),
    q3 = c(na8, 7.5, 8.5, 9.3, 7.0),
    Region = c("North America", "Europe", "Europe", "Asia", "Asia", "North America", "Latin America", "Latin America", "North America", "Europe", "Asia", "Latin America"),
    Design = c("Prospective", "Prospective", "Retrospective", "Prospective", "Retrospective", "Prospective", "Prospective", "Retrospective", "Prospective", "Retrospective", "Prospective", "Retrospective"),
    RiskOfBias = c("Low", "Low", "Some concerns", "Low", "High", "Some concerns", "Low", "Some concerns", "High", "Low", "Some concerns", "High"),
    MeanAge = c(61, 58, 65, 57, 63, 60, 55, 59, 62, 56, 64, 58),
    FollowUpMonths = c(12, 18, 9, 24, 18, 12, 24, 15, 18, 12, 24, 9),
    check.names = FALSE
  )
}

# Example: 12 two-arm trials with events and arm sizes on both sides.
binary_example_data <- function() {
  data.frame(
    study = paste("Study", LETTERS[1:12]),
    event.e = c(12, 18, 9, 31, 22, 15, 28, 17, 20, 14, 26, 11),
    n.e = c(120, 150, 95, 210, 180, 130, 240, 160, 175, 125, 220, 105),
    event.c = c(20, 24, 13, 40, 31, 23, 36, 26, 28, 20, 34, 17),
    n.c = c(118, 148, 97, 208, 182, 128, 238, 158, 173, 127, 218, 107),
    Region = c("North America", "Europe", "Europe", "Asia", "Asia", "North America", "Latin America", "Latin America", "North America", "Europe", "Asia", "Latin America"),
    Design = c("RCT", "RCT", "Observational", "RCT", "Observational", "RCT", "RCT", "Observational", "RCT", "Observational", "RCT", "Observational"),
    RiskOfBias = c("Low", "Low", "Some concerns", "Low", "High", "Some concerns", "Low", "Some concerns", "High", "Low", "Some concerns", "High"),
    MeanAge = c(61, 58, 65, 57, 63, 60, 55, 59, 62, 56, 64, 58),
    FollowUpMonths = c(12, 18, 9, 24, 18, 12, 24, 15, 18, 12, 24, 9),
    check.names = FALSE
  )
}

# Example: 12 studies of one diagnostic test, as 2x2 counts.
diagnostic_single_example_data <- function() {
  # Simulated with real between-study heterogeneity (logit-scale SD around 0.35 for
  # sensitivity, 0.24 for specificity, negatively correlated). The earlier version was
  # too consistent: the random-effects variance collapsed to zero, which made the
  # bivariate fit singular and the prediction region coincide with the confidence one.
  data.frame(
    study = paste("Study", LETTERS[1:12]),
    TP = c(  43,   53,   41,   65,   39,   50,   60,   36,   45,   36,   53,   43),
    FP = c(  18,    7,    7,   12,   11,    9,    8,   12,    6,   12,    8,    8),
    FN = c(   5,    9,   14,    6,    5,    8,    6,   15,   15,   11,   16,   10),
    TN = c(  74,   98,   81,  108,   85,  101,   91,   72,  109,   79,   95,   89),
    Region = c("North America", "Europe", "Europe", "North America", "Europe", "North America", "Europe", "North America", "North America", "Europe", "North America", "Europe"),
    check.names = FALSE
  )
}

# Example: the same studies evaluating two tests, one row per test per study.
diagnostic_comparative_example_data <- function() {
  # Same twelve studies, both tests in each, drawn from the shared-variance model the
  # comparative card fits: one random-effects pair per study used by both tests. Like
  # the single-test example it carries genuine heterogeneity, so the fit is not singular.
  data.frame(
    study = rep(paste("Study", LETTERS[1:12]), each = 2),
    test = rep(c("MRI", "ctDNA"), times = 12),
    TP = c(  49,   39,   61,   54,   43,   35,   62,   52,   50,   45,   54,   46,
             60,   49,   41,   34,   55,   45,   44,   33,   51,   36,   43,   34),
    FP = c(  15,    6,   29,   13,   18,    7,    8,    3,   34,   15,   10,    4,
             15,    6,   27,   11,   20,    8,   13,    5,    8,    3,   33,   14),
    FN = c(   9,   19,    5,   12,    6,   14,   10,   20,    4,    9,    7,   15,
             10,   21,    6,   13,    8,   18,   12,   23,   17,   32,    9,   18),
    TN = c(  87,   96,   66,   82,  100,  111,   80,   85,   76,   95,   89,   95,
             69,   78,   94,  110,   73,   85,   94,  102,   82,   87,   80,   99),
    check.names = FALSE
  )
}

# Example: a connected network of 12 studies, 5 treatments and 26 arms.
network_binary_example_data <- function() {
  data.frame(
    study = c(
      "Study A", "Study A",
      "Study B", "Study B",
      "Study C", "Study C",
      "Study D", "Study D",
      "Study E", "Study E", "Study E",
      "Study F", "Study F",
      "Study G", "Study G",
      "Study H", "Study H", "Study H",
      "Study I", "Study I",
      "Study J", "Study J",
      "Study K", "Study K",
      "Study L", "Study L"
    ),
    treatment = c(
      "Placebo", "Drug A",
      "Placebo", "Drug B",
      "Drug A", "Drug C",
      "Placebo", "Drug C",
      "Placebo", "Drug A", "Drug B",
      "Drug B", "Drug C",
      "Placebo", "Drug D",
      "Drug A", "Drug C", "Drug D",
      "Placebo", "Drug B",
      "Drug A", "Drug D",
      "Placebo", "Drug A",
      "Drug B", "Drug D"
    ),
    responders = c(22, 15, 24, 16, 18, 13, 30, 21, 25, 17, 19, 20, 16, 28, 18, 15, 14, 12,
                   26, 18, 17, 13, 23, 16, 21, 15),
    sampleSize = c(120, 118, 130, 128, 122, 121, 150, 149, 140, 138, 137, 132, 130, 148,
                   146, 126, 125, 124, 142, 140, 128, 127, 134, 133, 136, 135),
    check.names = FALSE
  )
}

# Example: the same 12 studies and 5 treatments, continuous outcome.
network_continuous_example_data <- function() {
  data.frame(
    study = c(
      "Study A", "Study A",
      "Study B", "Study B",
      "Study C", "Study C",
      "Study D", "Study D",
      "Study E", "Study E", "Study E",
      "Study F", "Study F",
      "Study G", "Study G",
      "Study H", "Study H", "Study H",
      "Study I", "Study I",
      "Study J", "Study J",
      "Study K", "Study K",
      "Study L", "Study L"
    ),
    treatment = c(
      "Placebo", "Drug A",
      "Placebo", "Drug B",
      "Drug A", "Drug C",
      "Placebo", "Drug C",
      "Placebo", "Drug A", "Drug B",
      "Drug B", "Drug C",
      "Placebo", "Drug D",
      "Drug A", "Drug C", "Drug D",
      "Placebo", "Drug B",
      "Drug A", "Drug D",
      "Placebo", "Drug A",
      "Drug B", "Drug D"
    ),
    mean = c(136, 128, 134, 126, 129, 124, 138, 130, 135, 127, 129, 128, 123, 137, 125,
             126, 122, 121, 139, 130, 127, 122, 135, 127, 128, 123),
    std.dev = c(16, 14, 17, 15, 14, 13, 18, 16, 16, 14, 15, 15, 13, 17, 14, 13, 12, 12,
                17, 15, 14, 13, 16, 14, 15, 13),
    sampleSize = c(120, 118, 130, 128, 122, 121, 150, 149, 140, 138, 137, 132, 130, 148,
                   146, 126, 125, 124, 142, 140, 128, 127, 134, 133, 136, 135),
    check.names = FALSE
  )
}

# Example: the same 5 treatments given as 12 contrasts, one per comparison.
network_precalc_ci_example_data <- function() {
  data.frame(
    study = paste("Study", LETTERS[1:12]),
    treat1 = c("Drug A", "Drug B", "Drug A", "Drug C", "Drug A", "Drug B", "Drug D",
               "Drug C", "Drug D", "Drug B", "Drug A", "Drug C"),
    treat2 = c("Placebo", "Placebo", "Drug C", "Placebo", "Drug B", "Drug C", "Placebo",
               "Drug D", "Drug A", "Drug D", "Drug D", "Drug B"),
    TE = c(0.72, 0.78, 0.92, 0.70, 0.88, 1.04, 0.66, 0.95, 0.82, 0.90, 0.85, 0.98),
    lower = c(0.55, 0.60, 0.70, 0.51, 0.68, 0.78, 0.48, 0.71, 0.61, 0.66, 0.64, 0.74),
    upper = c(0.94, 1.02, 1.21, 0.96, 1.14, 1.39, 0.91, 1.27, 1.10, 1.23, 1.13, 1.30),
    check.names = FALSE
  )
}

# Example: 12 two-arm trials. Eight report mean and SD, which is the ideal
# input, and the last four report a median with quartiles instead. Loading it
# shows in one click that the two shapes travel in the same file.
continuous_mean_example_data <- function() {
  na4 <- rep(NA_real_, 4)
  na8 <- rep(NA_real_, 8)
  data.frame(
    study = paste("Study", LETTERS[1:12]),
    n.e = c(80, 95, 110, 130, 90, 120, 75, 140, 105, 115, 125, 85),
    mean.e = c(c(5.8, 6.4, 7.1, 8.2, 6.9, 7.8, 5.9, 8.5), na4),
    sd.e = c(c(1.2, 1.5, 1.7, 2.1, 1.4, 1.8, 1.1, 2.0), na4),
    median.e = c(na8, 6.6, 7.4, 8.0, 6.2),
    q1.e = c(na8, 5.7, 6.3, 6.7, 5.4),
    q3.e = c(na8, 7.5, 8.5, 9.3, 7.0),
    n.c = c(78, 97, 108, 128, 88, 118, 77, 138, 103, 117, 123, 87),
    mean.c = c(c(6.9, 7.0, 7.8, 8.8, 7.5, 8.4, 6.6, 9.1), na4),
    sd.c = c(c(1.4, 1.6, 1.9, 2.2, 1.5, 1.9, 1.3, 2.1), na4),
    median.c = c(na8, 7.2, 8.1, 8.7, 6.8),
    q1.c = c(na8, 6.2, 6.9, 7.4, 5.9),
    q3.c = c(na8, 8.2, 9.3, 10.0, 7.7),
    Region = c("North America", "Europe", "Europe", "Asia", "Asia", "North America", "Latin America", "Latin America", "North America", "Europe", "Asia", "Latin America"),
    Design = c("RCT", "RCT", "Observational", "RCT", "Observational", "RCT", "RCT", "Observational", "RCT", "Observational", "RCT", "RCT"),
    RiskOfBias = c("Low", "Low", "Some concerns", "Low", "High", "Some concerns", "Low", "Some concerns", "High", "Low", "Low", "Some concerns"),
    MeanAge = c(61, 58, 65, 57, 63, 60, 55, 59, 62, 56, 64, 58),
    FollowUpMonths = c(12, 18, 9, 24, 18, 12, 24, 15, 18, 12, 24, 9),
    check.names = FALSE
  )
}


# ============================================================================
# READING AND VALIDATING THE DATA
#
# Everything between the user's spreadsheet and an analysis. read_single_prop_table()
# covers the three input routes, upload, paste and example, and there is one
# validate_*() per module for the columns that module needs.
# 
# The validators fail with a sentence the user can act on, naming the column and
# the row, instead of letting an R error surface. That is the difference between
# a no-code tool and a console.
# ============================================================================

# The one door for tabular input, whatever route the user took. Pasted text is
# sniffed for tab, semicolon or comma so a copy out of Excel works without the
# user declaring a separator; a path is read by extension. Despite the name it
# serves every module, since at this stage a table is just a table.
read_single_prop_table <- function(path = NULL, pasted = NULL) {
  if (!is.null(pasted) && nzchar(trimws(pasted))) {
    separator <- if (grepl("\t", pasted)) "\t" else if (grepl(";", pasted)) ";" else ","
    ## quote = "\"" and not the default "\"'".
    ##
    ## read.table treats a single quote as opening a quoted field, so a study
    ## called O'Rourke 1998 swallowed every line after it: 46 studies pasted, 23
    ## read, and the 23rd arrived with a name and four blank counts. Irish and
    ## Italian surnames are common in a reference list, and nothing about the
    ## error pointed at the apostrophe. A spreadsheet paste never uses a single
    ## quote to delimit a field, so removing it costs nothing.
    tabela <- read.table(
      text = pasted,
      header = TRUE,
      sep = separator,
      quote = "\"",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    ## And a count, so the next reason a paste is read short announces itself
    ## instead of arriving as a puzzling validation error further down.
    linhas_coladas <- sum(nzchar(trimws(strsplit(pasted, "\r?\n")[[1]]))) - 1L
    if (linhas_coladas > 0 && nrow(tabela) < linhas_coladas) {
      stop(sprintf(paste0("Only %d of the %d pasted rows could be read. This usually ",
                          "means a value contains a quote or a stray separator. Check ",
                          "row %d, or import the file instead."),
                   nrow(tabela), linhas_coladas, nrow(tabela) + 1L), call. = FALSE)
    }
    return(tabela)
  }

  extension <- tolower(tools::file_ext(path))
  if (extension %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("The readxl package is required to import Excel files. Install it with install.packages('readxl').")
    }
    return(as.data.frame(readxl::read_excel(path)))
  }

  if (extension %in% c("tsv", "txt")) {
    return(read.delim(path, stringsAsFactors = FALSE, check.names = FALSE))
  }

  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

# Turns a list of problems into one readable error. Called even when the list is
# empty, so a validator can end with it unconditionally and stay linear.
friendly_validation_error <- function(problems) {
  problems <- unique(problems[nzchar(problems)])
  if (length(problems) == 0) return(invisible(NULL))
  stop(paste(c("Please fix the dataset before running the analysis:", paste0("- ", problems)), collapse = "\n"), call. = FALSE)
}

# Reports missing columns by name and points at the exact Excel spelling, which
# is the mistake behind most failed imports.
required_column_problems <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) == 0) return(character(0))
  paste0("Missing required column(s): ", paste(missing_cols, collapse = ", "), ". Use the exact Excel column names.")
}

# Empty cells in a text column, typically a study without a label. Only the
# first eight offending rows are listed: a whole column of blanks would other-
# wise produce an error message longer than the screen.
blank_text_problems <- function(data, cols) {
  problems <- character(0)
  for (col in intersect(cols, names(data))) {
    values <- trimws(as.character(data[[col]]))
    bad_rows <- which(is.na(data[[col]]) | !nzchar(values))
    if (length(bad_rows) > 0) {
      problems <- c(problems, paste0("Column ", col, " has blank values in row(s): ", paste(head(bad_rows, 8), collapse = ", "), "."))
    }
  }
  problems
}

# Converts the columns a module needs into numbers and separates the two ways
# that can fail: a cell holding text, which is a typo the user must fix, and an
# empty cell, which is missing data. They read as different problems, so they
# are reported as different messages.
coerce_numeric_columns <- function(data, cols) {
  problems <- character(0)
  for (col in intersect(cols, names(data))) {
    original <- data[[col]]
    converted <- suppressWarnings(as.numeric(original))
    blank <- is.na(original) | !nzchar(trimws(as.character(original)))
    bad_rows <- which(is.na(converted) & !blank)
    if (length(bad_rows) > 0) {
      problems <- c(problems, paste0("Column ", col, " must be numeric. Check row(s): ", paste(head(bad_rows, 8), collapse = ", "), "."))
    }
    missing_rows <- which(is.na(converted) & blank)
    if (length(missing_rows) > 0) {
      problems <- c(problems, paste0("Column ", col, " has blank values in row(s): ", paste(head(missing_rows, 8), collapse = ", "), "."))
    }
    data[[col]] <- converted
  }
  list(data = data, problems = problems)
}

# The same conversion, but an empty cell is data the study did not report, not a
# mistake. The continuous module needs this: a study giving a median leaves the
# mean and SD columns blank on purpose, and the row is still valid.
coerce_numeric_columns_allowing_blanks <- function(data, cols) {
  problems <- character(0)
  for (col in unique(intersect(cols, names(data)))) {
    original <- data[[col]]
    converted <- suppressWarnings(as.numeric(original))
    blank <- is.na(original) | !nzchar(trimws(as.character(original)))
    bad_rows <- which(is.na(converted) & !blank)
    if (length(bad_rows) > 0) {
      problems <- c(problems, paste0("Column ", col, " must be numeric. Check row(s): ",
                                     paste(head(bad_rows, 8), collapse = ", "), "."))
    }
    data[[col]] <- converted
  }
  list(data = data, problems = problems)
}

# Attaches the offending row numbers to a rule that failed. Every content check
# in the validators is written through this, so they all report the same way.
row_problem <- function(condition, message) {
  rows <- which(condition)
  if (length(rows) == 0) return(character(0))
  paste0(message, " Row(s): ", paste(head(rows, 8), collapse = ", "), ".")
}

# Up to the previous version the study label column was called "Author" in the
# paired modules and "study" in the network ones. It is always "study" now, but
# older spreadsheets stay valid: the old names are accepted as aliases.
normalize_study_column <- function(data) {
  if (is.null(data) || !is.data.frame(data)) return(data)
  if (!"study" %in% names(data)) {
    alias <- which(tolower(trimws(names(data))) %in% c("author", "studlab", "study label"))
    if (length(alias) > 0) names(data)[alias[1]] <- "study"
  }
  data
}

## -------------------------------------------------------------------------
## Spreadsheet validators
##
## One per module. They check the required columns, coerce text into numbers and
## collect every problem into a single message instead of stopping at the first
## one, so the user fixes the whole spreadsheet in one pass rather than
## discovering the errors one reload at a time.
## -------------------------------------------------------------------------

# Every validator below follows the same four steps: accept the old "Author"
# spelling of the study column, refuse early when a required column is missing,
# coerce the numeric columns and report the cells that would not convert, then
# check the rules that make a row impossible rather than merely unusual. What
# comes back is the cleaned data frame, so the caller never re-parses anything.

# Single-arm proportion: study, event and n. A study cannot report more events
# than participants, which is the check that catches a swapped pair of columns.
validate_single_prop_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "event", "n")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  numeric_check <- coerce_numeric_columns(data, c("event", "n"))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, "study"))
  problems <- c(problems, row_problem(data$event < 0, "Column event must be 0 or greater."))
  problems <- c(problems, row_problem(data$n <= 0, "Column n must be greater than 0."))
  problems <- c(problems, row_problem(data$event > data$n, "Column event cannot be greater than n."))
  friendly_validation_error(problems)

  data
}

# Single-arm mean, in whatever shape the study reported it.
#
# What a study needs is n, one measure of centre, and one of spread:
#
#   mean   + ( sd  or  q1 and q3  or  min and max )
#   median + (         q1 and q3  or  min and max )
#
# The combination this refuses is median with sd and nothing else. meta cannot
# turn that into a mean, and it does not complain: it drops the study from the
# pooling with no message at all.
validate_single_mean_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "n")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  # Absent columns are created as NA so metamean receives the full set and
  # decides per study which scenario applies.
  optional_cols <- c("mean", "sd", "median", "q1", "q3", "min", "max")
  present <- intersect(optional_cols, names(data))
  for (col in optional_cols) if (!col %in% names(data)) data[[col]] <- NA_real_

  numeric_check <- coerce_numeric_columns_allowing_blanks(data, c("n", present))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, "study"))
  problems <- c(problems, row_problem(is.na(data[["n"]]) | data[["n"]] <= 0,
                                      "Column n must be greater than 0."))

  g <- function(nome) data[[nome]]
  tem <- function(nome) !is.na(g(nome))
  spread <- tem("sd") | (tem("q1") & tem("q3")) | (tem("min") & tem("max"))
  centre <- tem("mean") | tem("median")
  # a median needs quartiles or a range: sd alone cannot produce a mean
  usable <- (tem("mean") & spread) |
    (tem("median") & ((tem("q1") & tem("q3")) | (tem("min") & tem("max"))))

  problems <- c(problems, row_problem(
    !centre, "Each study needs either mean or median."))
  problems <- c(problems, row_problem(
    centre & !spread, "Each study needs sd, or q1 and q3, or min and max."))
  problems <- c(problems, row_problem(
    centre & spread & !usable,
    paste0("A study reports median with sd only. A median needs q1 and q3, or ",
           "min and max, to be converted into a mean; otherwise the study is ",
           "silently dropped from the analysis.")))

  problems <- c(problems, row_problem(!is.na(g("sd")) & g("sd") < 0,
                                      "Column sd must be non-negative."))
  problems <- c(problems, row_problem(
    tem("q1") & tem("q3") & tem("median") & !(g("q1") <= g("median") & g("median") <= g("q3")),
    "Each study must satisfy q1 <= median <= q3."))
  problems <- c(problems, row_problem(tem("q1") & tem("q3") & g("q1") > g("q3"),
                                      "Column q1 cannot be greater than q3."))
  problems <- c(problems, row_problem(tem("min") & tem("max") & g("min") > g("max"),
                                      "Column min cannot be greater than max."))
  friendly_validation_error(problems)

  data
}

# Two-arm binary outcome: events and arm size for the experimental and the
# control group. The events-cannot-exceed-n rule is applied to both arms.
validate_binary_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "event.e", "n.e", "event.c", "n.c")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  numeric_check <- coerce_numeric_columns(data, c("event.e", "n.e", "event.c", "n.c"))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, "study"))
  problems <- c(problems, row_problem(data[["event.e"]] < 0, "Column event.e must be 0 or greater."))
  problems <- c(problems, row_problem(data[["event.c"]] < 0, "Column event.c must be 0 or greater."))
  problems <- c(problems, row_problem(data[["n.e"]] <= 0, "Column n.e must be greater than 0."))
  problems <- c(problems, row_problem(data[["n.c"]] <= 0, "Column n.c must be greater than 0."))
  problems <- c(problems, row_problem(data[["event.e"]] > data[["n.e"]], "Column event.e cannot be greater than n.e."))
  problems <- c(problems, row_problem(data[["event.c"]] > data[["n.c"]], "Column event.c cannot be greater than n.c."))
  friendly_validation_error(problems)

  data
}

# One diagnostic test: the four cells of the 2x2 table, TP, FP, FN and TN, with
# one row per study.
validate_diagnostic_single_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "TP", "FP", "FN", "TN")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  numeric_check <- coerce_numeric_columns(data, c("TP", "FP", "FN", "TN"))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, "study"))
  problems <- c(problems, row_problem(data$TP < 0, "Column TP must be 0 or greater."))
  problems <- c(problems, row_problem(data$FP < 0, "Column FP must be 0 or greater."))
  problems <- c(problems, row_problem(data$FN < 0, "Column FN must be 0 or greater."))
  problems <- c(problems, row_problem(data$TN < 0, "Column TN must be 0 or greater."))
  problems <- c(problems, row_problem((data$TP + data$FN) <= 0, "Each study must have TP + FN greater than 0."))
  problems <- c(problems, row_problem((data$TN + data$FP) <= 0, "Each study must have TN + FP greater than 0."))
  friendly_validation_error(problems)

  data
}

# Two or more diagnostic tests compared: the same 2x2 cells plus a test column,
# so one study contributes one row per test.
validate_diagnostic_comparative_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "test", "TP", "FP", "FN", "TN")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  data$study <- trimws(as.character(data$study))
  data$test <- trimws(as.character(data$test))
  numeric_check <- coerce_numeric_columns(data, c("TP", "FP", "FN", "TN"))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, c("study", "test")))
  problems <- c(problems, row_problem(data$TP < 0, "Column TP must be 0 or greater."))
  problems <- c(problems, row_problem(data$FP < 0, "Column FP must be 0 or greater."))
  problems <- c(problems, row_problem(data$FN < 0, "Column FN must be 0 or greater."))
  problems <- c(problems, row_problem(data$TN < 0, "Column TN must be 0 or greater."))
  problems <- c(problems, row_problem((data$TP + data$FN) <= 0, "Each row must have TP + FN greater than 0."))
  problems <- c(problems, row_problem((data$TN + data$FP) <= 0, "Each row must have TN + FP greater than 0."))

  if (length(unique(data$test)) < 2) {
    problems <- c(problems, "Comparative diagnostic meta-analysis requires at least two diagnostic tests in the test column.")
  }
  tests_per_study <- stats::aggregate(test ~ study, data, function(x) length(unique(x)))
  incomplete <- tests_per_study$study[tests_per_study$test < 2]
  if (length(incomplete) > 0) {
    problems <- c(problems, paste0("Each study should include at least two tests. Check: ", paste(incomplete, collapse = ", "), "."))
  }
  friendly_validation_error(problems)

  data
}

# Network meta-analysis from arm-level binary data: one row per treatment arm,
# with the study column tying the arms of a trial together.
validate_network_binary_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "treatment", "responders", "sampleSize")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  data$study <- trimws(as.character(data$study))
  data$treatment <- trimws(as.character(data$treatment))
  numeric_check <- coerce_numeric_columns(data, c("responders", "sampleSize"))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, c("study", "treatment")))
  problems <- c(problems, row_problem(data$sampleSize <= 0, "Column sampleSize must be greater than 0."))
  problems <- c(problems, row_problem(data$responders < 0, "Column responders must be 0 or greater."))
  problems <- c(problems, row_problem(data$responders > data$sampleSize, "Column responders cannot be greater than sampleSize."))

  arms_per_study <- table(data$study)
  if (any(arms_per_study < 2)) {
    problems <- c(problems, paste0("Each study must include at least two treatment arms. Check: ", paste(names(arms_per_study)[arms_per_study < 2], collapse = ", "), "."))
  }
  if (length(unique(data$treatment)) < 3) {
    problems <- c(problems, "Network meta-analysis usually requires at least three treatments.")
  }
  friendly_validation_error(problems)

  data
}

# Network meta-analysis from arm-level continuous data: one row per treatment
# arm, carrying mean, std.dev and sampleSize.
validate_network_continuous_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "treatment", "mean", "std.dev", "sampleSize")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  data$study <- trimws(as.character(data$study))
  data$treatment <- trimws(as.character(data$treatment))
  numeric_check <- coerce_numeric_columns(data, c("mean", "std.dev", "sampleSize"))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, c("study", "treatment")))
  problems <- c(problems, row_problem(data[["std.dev"]] < 0, "Column std.dev must be non-negative."))
  problems <- c(problems, row_problem(data$sampleSize <= 0, "Column sampleSize must be greater than 0."))

  arms_per_study <- table(data$study)
  if (any(arms_per_study < 2)) {
    problems <- c(problems, paste0("Each study must include at least two treatment arms. Check: ", paste(names(arms_per_study)[arms_per_study < 2], collapse = ", "), "."))
  }
  if (length(unique(data$treatment)) < 3) {
    problems <- c(problems, "Network meta-analysis usually requires at least three treatments.")
  }
  friendly_validation_error(problems)

  data
}

# Network meta-analysis from contrast-level data: one row per comparison, naming
# the two treatments and the effect between them. A row comparing a treatment
# with itself is rejected, because it carries no information about the network.
validate_network_precalc_ci_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "treat1", "treat2", "TE", "lower", "upper")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  data$study <- trimws(as.character(data$study))
  data$treat1 <- trimws(as.character(data$treat1))
  data$treat2 <- trimws(as.character(data$treat2))
  numeric_check <- coerce_numeric_columns(data, c("TE", "lower", "upper"))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, c("study", "treat1", "treat2")))
  problems <- c(problems, row_problem(data$treat1 == data$treat2, "Columns treat1 and treat2 must identify different treatments."))
  problems <- c(problems, row_problem(data$lower > data$upper, "Column lower cannot be greater than upper."))
  if (length(unique(c(data$treat1, data$treat2))) < 3) {
    problems <- c(problems, "Network meta-analysis usually requires at least three treatments.")
  }
  friendly_validation_error(problems)

  data
}

# Two-arm continuous outcome. Mean and SD is the ideal input and the one the
# module is built around, but a study reporting a median with quartiles is
# accepted too: meta::metacont converts it, using Luo (2018) for the mean and
# Shi (2020) for the SD. The two shapes can be mixed in one file, row by row.
#
# What each arm needs is n, one measure of centre, and one of spread:
#
#   mean   + ( sd  or  q1 and q3  or  min and max )
#   median + (         q1 and q3  or  min and max )
#
# The combination this refuses is median with sd and nothing else. meta cannot
# turn that into a mean, and it does not complain: it drops the study from the
# pooling with no message at all. Catching it here is the whole reason this
# validator is more than a column list.
validate_cont_mean_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "n.e", "n.c")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  # Absent columns are created as NA so metacont receives the full set and
  # decides per study which scenario applies.
  optional_cols <- c("mean.e", "sd.e", "median.e", "q1.e", "q3.e", "min.e", "max.e",
                     "mean.c", "sd.c", "median.c", "q1.c", "q3.c", "min.c", "max.c")
  present <- intersect(optional_cols, names(data))
  for (col in optional_cols) if (!col %in% names(data)) data[[col]] <- NA_real_

  numeric_check <- coerce_numeric_columns_allowing_blanks(data, c("n.e", "n.c", present))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, "study"))
  problems <- c(problems, row_problem(is.na(data[["n.e"]]) | data[["n.e"]] <= 0,
                                      "Column n.e must be greater than 0."))
  problems <- c(problems, row_problem(is.na(data[["n.c"]]) | data[["n.c"]] <= 0,
                                      "Column n.c must be greater than 0."))

  for (arm in c("e", "c")) {
    g <- function(nome) data[[paste0(nome, ".", arm)]]
    tem <- function(nome) !is.na(g(nome))
    lado <- if (arm == "e") "Experimental" else "Control"

    spread <- (tem("sd") | (tem("q1") & tem("q3")) | (tem("min") & tem("max")))
    centre <- tem("mean") | tem("median")
    # a median needs quartiles or a range: sd alone cannot produce a mean
    usable <- (tem("mean") & spread) | (tem("median") & (tem("q1") & tem("q3") | tem("min") & tem("max")))

    problems <- c(problems, row_problem(
      !centre,
      paste0(lado, " arm needs either mean.", arm, " or median.", arm, ".")))
    problems <- c(problems, row_problem(
      centre & !spread,
      paste0(lado, " arm needs sd.", arm, ", or q1.", arm, " and q3.", arm,
             ", or min.", arm, " and max.", arm, ".")))
    problems <- c(problems, row_problem(
      centre & spread & !usable,
      paste0(lado, " arm reports median.", arm, " with sd.", arm, " only. A median needs ",
             "q1.", arm, " and q3.", arm, ", or min.", arm, " and max.", arm,
             ", to be converted into a mean; otherwise the study is silently ",
             "dropped from the analysis.")))

    problems <- c(problems, row_problem(!is.na(g("sd")) & g("sd") < 0,
                                        paste0("Column sd.", arm, " must be non-negative.")))
    problems <- c(problems, row_problem(
      tem("q1") & tem("q3") & tem("median") & !(g("q1") <= g("median") & g("median") <= g("q3")),
      paste0(lado, " arm must satisfy q1.", arm, " <= median.", arm, " <= q3.", arm, ".")))
    problems <- c(problems, row_problem(tem("q1") & tem("q3") & g("q1") > g("q3"),
                                        paste0("Column q1.", arm, " cannot be greater than q3.", arm, ".")))
    problems <- c(problems, row_problem(tem("min") & tem("max") & g("min") > g("max"),
                                        paste0("Column min.", arm, " cannot be greater than max.", arm, ".")))
  }
  friendly_validation_error(problems)

  data
}

# Effect size already calculated, supplied with its confidence interval. The
# interval is what carries the precision here, so no standard error is asked
# for. n.e and n.c are optional and only feed the sample-size columns of the
# forest plot.
validate_precalc_te_ci_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "TE", "lower", "upper")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  numeric_cols <- c("TE", "lower", "upper")
  optional_numeric <- intersect(c("n.e", "n.c"), names(data))
  numeric_check <- coerce_numeric_columns(data, c(numeric_cols, optional_numeric))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, "study"))
  problems <- c(problems, row_problem(data$lower > data$TE | data$upper < data$TE | data$lower > data$upper, "Confidence interval must satisfy lower <= TE <= upper."))
  problems <- c(problems, row_problem("n.e" %in% names(data) & data[["n.e"]] <= 0, "Column n.e must be greater than 0."))
  problems <- c(problems, row_problem("n.c" %in% names(data) & data[["n.c"]] <= 0, "Column n.c must be greater than 0."))
  friendly_validation_error(problems)

  data
}

# Effect size already calculated, supplied with its standard error. seTE must be
# strictly positive: a zero would hand that study infinite weight.
validate_precalc_te_sete_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "TE", "seTE")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  numeric_cols <- c("TE", "seTE")
  optional_numeric <- intersect(c("n.e", "n.c"), names(data))
  numeric_check <- coerce_numeric_columns(data, c(numeric_cols, optional_numeric))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, "study"))
  problems <- c(problems, row_problem(data$seTE <= 0, "Column seTE must be greater than 0."))
  problems <- c(problems, row_problem("n.e" %in% names(data) & data[["n.e"]] <= 0, "Column n.e must be greater than 0."))
  problems <- c(problems, row_problem("n.c" %in% names(data) & data[["n.c"]] <= 0, "Column n.c must be greater than 0."))
  friendly_validation_error(problems)

  data
}

# Effect size already calculated, supplied with both a standard error and a
# confidence interval. TE, seTE and the interval are all handed to metagen, so
# the interval the user reported is the one drawn, not one recomputed from seTE.
validate_precalc_te_sete_ci_data <- function(data) {
  data <- normalize_study_column(data)
  required_cols <- c("study", "TE", "seTE", "lower", "upper")
  problems <- required_column_problems(data, required_cols)
  if (length(problems) > 0) friendly_validation_error(problems)

  numeric_cols <- c("TE", "seTE", "lower", "upper")
  optional_numeric <- intersect(c("n.e", "n.c"), names(data))
  numeric_check <- coerce_numeric_columns(data, c(numeric_cols, optional_numeric))
  data <- numeric_check$data
  problems <- c(numeric_check$problems, blank_text_problems(data, "study"))
  problems <- c(problems, row_problem(data$seTE <= 0, "Column seTE must be greater than 0."))
  problems <- c(problems, row_problem(data$lower > data$TE | data$upper < data$TE | data$lower > data$upper, "Confidence interval must satisfy lower <= TE <= upper."))
  problems <- c(problems, row_problem("n.e" %in% names(data) & data[["n.e"]] <= 0, "Column n.e must be greater than 0."))
  problems <- c(problems, row_problem("n.c" %in% names(data) & data[["n.c"]] <= 0, "Column n.c must be greater than 0."))
  friendly_validation_error(problems)

  data
}


# ============================================================================
# BATCH MODE: ONE WORKBOOK, MANY OUTCOMES
#
# Batch mode reads a single Excel file with one sheet per outcome, and the sheet
# name becomes the outcome name everywhere.
#
# common_optional_columns() returns the union of the extra columns across the
# sheets, not the intersection. A column present in only some of them is still
# offered in the subgroup picker, and analyze_outcome_batch() then drops it for
# the outcomes that lack it and records why. Offering less would hide a column
# the user deliberately put in the sheets where it applies.
# ============================================================================

# Reads a batch workbook: every sheet becomes one outcome, named after the
# sheet, and each is passed through the module's own validator. A sheet that
# fails is reported with its name, so the user knows which tab to fix.
read_outcome_workbook <- function(path, validator) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("The readxl package is required to import Excel workbooks. Install it with install.packages('readxl').")
  }

  sheet_names <- readxl::excel_sheets(path)
  if (length(sheet_names) == 0) {
    stop("The workbook does not contain any sheets.")
  }

  outcomes <- lapply(sheet_names, function(sheet_name) {
    sheet_data <- readxl::read_excel(path, sheet = sheet_name)
    sheet_data <- as.data.frame(sheet_data)
    tryCatch(
      validator(sheet_data),
      error = function(error) {
        stop(paste0("Sheet '", sheet_name, "':\n", error$message), call. = FALSE)
      }
    )
  })
  names(outcomes) <- sheet_names
  outcomes
}

# The extra columns a batch can offer, as the union across the sheets. See the
# section note above for why the union and not the intersection.
common_optional_columns <- function(outcomes, required_cols) {
  if (is.null(outcomes) || length(outcomes) == 0) return(character(0))
  sort(unique(unlist(lapply(outcomes, function(data) setdiff(names(data), required_cols)))))
}

# The same list narrowed to columns that are numeric in at least one sheet,
# which is what the meta-regression picker can accept as a moderator.
common_numeric_optional_columns <- function(outcomes, required_cols) {
  common_cols <- common_optional_columns(outcomes, required_cols)
  if (length(common_cols) == 0) return(character(0))

  common_cols[vapply(common_cols, function(col) {
    any(vapply(outcomes, function(data) col %in% names(data) && is.numeric(data[[col]]), logical(1)))
  }, logical(1))]
}

reserved_forest_cols <- c(
  "studlab", "TE", "seTE", "lower", "upper", "event", "n", "mean", "sd",
  "event.e", "n.e", "event.c", "n.c", "mean.e", "sd.e", "mean.c", "sd.c",
  "w.random", "w.common", "effect", "ci", "subgroup"
)


# ============================================================================
# FOREST COLUMNS AND SHARED RESULT HELPERS
#
# Small pieces used by every module: which extra columns ride along in the forest
# plot, the citation reminder shown on the results step, and the test that tells a
# batch result from a single-outcome one.
# ============================================================================

# Keeps only columns that exist in the data and are not already drawn by the
# forest plot. reserved_forest_cols holds the names meta uses itself, so asking
# for one of them would duplicate a column instead of adding one.
sanitize_forest_columns <- function(cols, data = NULL) {
  cols <- unique(trimws(as.character(cols %||% character(0))))
  cols <- cols[nzchar(cols)]
  cols <- setdiff(cols, reserved_forest_cols)
  if (!is.null(data)) cols <- intersect(cols, names(data))
  cols
}

# The picker for those extra columns. It starts empty and is filled by
# update_forest_column_selector() once a dataset has been read.
forest_column_selector <- function(input_id) {
  selectizeInput(
    input_id,
    "Additional forest plot columns (optional)",
    choices = character(0),
    selected = character(0),
    multiple = TRUE,
    options = list(
      plugins = list("remove_button"),
      create = FALSE,
      closeAfterSelect = FALSE,
      dropdownParent = "body",
      placeholder = "Select optional columns"
    )
  )
}

# Fills that picker once a dataset is known. The choices are the optional
# columns of the file, so a module offers exactly what the user brought.
update_forest_column_selector <- function(session, input_id, choices) {
  updateSelectizeInput(
    session,
    input_id,
    choices = sanitize_forest_columns(choices),
    selected = character(0),
    server = TRUE
  )
}

# Copies the chosen columns onto the meta object. That is how meta::forest
# finds them: leftcols are looked up by name on the object itself, not on the
# data frame the analysis started from.
attach_forest_columns <- function(meta_object, data, forest_cols) {
  forest_cols <- sanitize_forest_columns(forest_cols, data)
  if (length(forest_cols) == 0 || is.null(meta_object)) return(meta_object)

  for (col in forest_cols) {
    values <- data[[col]]
    if (is.factor(values)) values <- as.character(values)
    meta_object[[col]] <- values
  }
  meta_object
}

## The citation reminder lives in step 3, which is where the user has just
## produced what goes into the paper. One function rather than the same text
## pasted into thirteen cards: when the paper is published, the wording changes
## in a single place.
citation_reminder <- function() {
  tags$div(
    class = "cite-reminder",
    tags$p(class = "cite-reminder-lead",
           "Publishing these results? Please cite MetaVidence."),
    tags$p(class = "cite-reminder-body",
           "Citations are what keep this free academic tool alive and ",
           "maintained, and recording which software produced your numbers is ",
           "part of a reproducible analysis."),
    tags$a(href = METAVIDENCE_CITATION$cite_page, target = "_blank", rel = "noopener",
           class = "cite-reminder-action", "How to cite")
  )
}

# The optional columns the user chose to display alongside the forest plot.
forest_extra_columns <- function(result) {
  sanitize_forest_columns(result$forest_cols, result$data)
}

# A GLMM fit produces no per-study weight: meta returns w.random and w.common
# entirely NA, and the Weight column renders as "--%" on every row. When that is
# the case the column is dropped from the forest instead of printing empty.
# Testing the weights, rather than the method name, covers single proportions
# and the binary module with GLMM + OR in one go.
forest_weight_column <- function(meta_object) {
  pesos <- c(meta_object$w.random, meta_object$w.common)
  if (length(pesos) == 0 || all(is.na(pesos))) character(0) else "w.random"
}

# One outcome or many. Every function that has to behave differently in batch
# mode asks this rather than inspecting the shape of the object.
is_batch_result <- function(result) {
  !is.null(result) && isTRUE(result$batch)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (length(x) == 1 && is.na(x)) {
    return(y)
  }
  x
}


# ============================================================================
# SPREADSHEET TEMPLATES
#
# The files behind Download XLSX template. module_data_specs is the single source
# for what each module requires, what it accepts as optional, and which outcome
# names its batch template ships with.
# ============================================================================

# The rows of a single-outcome template, taken from module_data_specs so the
# template and the validator can never disagree about the column names.
#
# order_hint puts the columns in reading order rather than in the order they
# happen to be declared. It matters in the continuous module, where required and
# optional interleave: without it n.c lands next to n.e, ahead of every column
# describing the experimental arm.
csv_template_data <- function(required_cols, optional_cols = character(0), order_hint = NULL) {
  cols <- unique(c(required_cols, optional_cols))
  if (length(order_hint) > 0) cols <- unique(c(intersect(order_hint, cols), cols))
  template <- as.data.frame(as.list(rep("", length(cols))), stringsAsFactors = FALSE)
  names(template) <- cols
  template
}

## -------------------------------------------------------------------------
## Data specification for each module
##
## Required columns, optional ones, the example dataset and the sheet names for
## the batch template. The Data check card, the XLSX templates and the subgroup
## and moderator selectors are all driven from here, so a column added in this
## list becomes visible across the whole app without touching anything else.
## -------------------------------------------------------------------------

# The columns the continuous module treats as outcome data. Because a study may
# report a mean or a median, most of them are optional, and without this list
# they would be offered as subgroup columns and as moderators.
CONT_OUTCOME_COLS <- c(
  "study",
  "n.e", "mean.e", "sd.e", "median.e", "q1.e", "q3.e", "min.e", "max.e",
  "n.c", "mean.c", "sd.c", "median.c", "q1.c", "q3.c", "min.c", "max.c"
)

# Same idea for single-arm means. A single arm has no .e/.c suffix, so the
# columns are bare, and all but study and n are optional.
SINGLE_MEAN_OUTCOME_COLS <- c(
  "study", "n", "mean", "sd", "median", "q1", "q3", "min", "max"
)

module_data_specs <- list(
  single_prop = list(required = c("study", "event", "n"), optional = c("Region", "Design", "RiskOfBias", "MeanAge", "FollowUpMonths"), example = function() single_prop_example_data(), batch_outcomes = c("Mortality", "Complication rate", "Recurrence rate")),
  single_mean = list(required = c("study", "n"), data_cols = SINGLE_MEAN_OUTCOME_COLS, optional = c("mean", "sd", "median", "q1", "q3", "min", "max", "Region", "Design", "RiskOfBias", "MeanAge", "FollowUpMonths"), example = function() single_mean_example_data(), batch_outcomes = c("Length of stay", "Pain score", "Quality of life")),
  binary = list(required = c("study", "event.e", "n.e", "event.c", "n.c"), optional = c("Region", "Design", "RiskOfBias", "MeanAge", "FollowUpMonths"), example = function() binary_example_data(), batch_outcomes = c("Mortality", "Recurrence", "Serious adverse events")),
  diagnostic_single = list(required = c("study", "TP", "FP", "FN", "TN"), optional = c("Region"), example = function() diagnostic_single_example_data()),
  diagnostic_comparative = list(required = c("study", "test", "TP", "FP", "FN", "TN"), optional = c("Region"), example = function() diagnostic_comparative_example_data()),
  network_binary = list(required = c("study", "treatment", "responders", "sampleSize"), optional = character(0), example = function() network_binary_example_data()),
  network_continuous = list(required = c("study", "treatment", "mean", "std.dev", "sampleSize"), optional = character(0), example = function() network_continuous_example_data()),
  network_precalc_ci = list(required = c("study", "treat1", "treat2", "TE", "lower", "upper"), optional = character(0), example = function() network_precalc_ci_example_data()),
  cont_mean = list(required = c("study", "n.e", "n.c"), data_cols = CONT_OUTCOME_COLS, optional = c("mean.e", "sd.e", "median.e", "q1.e", "q3.e", "min.e", "max.e", "mean.c", "sd.c", "median.c", "q1.c", "q3.c", "min.c", "max.c", "Region", "Design", "RiskOfBias", "MeanAge", "FollowUpMonths"), example = function() continuous_mean_example_data(), batch_outcomes = c("Pain score", "Length of stay", "Quality of life")),
  precalc_te_ci = list(required = c("study", "TE", "lower", "upper"), optional = c("n.e", "n.c", "Region", "Design", "RiskOfBias", "MeanAge", "FollowUpMonths"), example = function() precalc_te_ci_example_data(), batch_outcomes = c("Overall survival", "Progression-free survival", "Recurrence-free survival")),
  precalc_te_sete = list(required = c("study", "TE", "seTE"), optional = c("n.e", "n.c", "Region", "Design", "RiskOfBias", "MeanAge", "FollowUpMonths"), example = function() precalc_te_sete_example_data(), batch_outcomes = c("Overall survival", "Progression-free survival", "Recurrence-free survival")),
  precalc_te_sete_ci = list(required = c("study", "TE", "seTE", "lower", "upper"), optional = c("n.e", "n.c", "Region", "Design", "RiskOfBias", "MeanAge", "FollowUpMonths"), example = function() precalc_te_sete_ci_example_data(), batch_outcomes = c("Overall survival", "Progression-free survival", "Recurrence-free survival"))
)

# Writes a list of data frames as one workbook, one sheet each. Used both for
# the templates and for the summary tables the user exports.
write_xlsx_workbook <- function(file, sheets) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The openxlsx package is required to download Excel files. Install it with install.packages('openxlsx').", call. = FALSE)
  }

  workbook <- openxlsx::createWorkbook()
  for (sheet_name in names(sheets)) {
    openxlsx::addWorksheet(workbook, sheet_name)
    openxlsx::writeData(workbook, sheet_name, sheets[[sheet_name]])
    openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(workbook, sheet_name, cols = seq_along(sheets[[sheet_name]]), widths = "auto")
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
}

# The single-outcome template: one sheet with the required columns filled in
# with the example data, so the user can overwrite it row by row.
write_xlsx_template <- function(file, required_cols, optional_cols = character(0), example_data = NULL, order_hint = NULL) {
  sheets <- list(Template = csv_template_data(required_cols, optional_cols, order_hint))
  if (!is.null(example_data)) {
    sheets$Example <- example_data
  }
  write_xlsx_workbook(file, sheets)
}

# In batch mode every Excel sheet is one outcome, and the sheet name becomes the
# outcome name in the plots and the exported files. That is why the batch
# template cannot reuse the single-outcome structure: an empty "Template" sheet
# there would be read as an outcome called Template.
#
# The sheets carry the same set of columns and a decreasing number of studies.
# Trimming rows, rather than perturbing the numbers, keeps the data valid in
# every module (no trim can create event > n, or quartiles out of order) and
# still makes the outcomes produce different results, which is what demonstrates
# what batch mode is for.
#
# The sheet names come from batch_outcomes in module_data_specs and are plausible
# outcomes for each module rather than generic labels: the sheet name is the only
# place the user declares the outcome name, so the template shows that already
# done. Excel caps a sheet name at 31 characters.
BATCH_TEMPLATE_SHEETS <- 3

# The batch template. See the note above BATCH_TEMPLATE_SHEETS for why the
# sheets carry trimmed copies of the same dataset rather than altered numbers.
write_xlsx_batch_template <- function(file, example_data, outcome_names = NULL) {
  if (is.null(outcome_names) || length(outcome_names) == 0) {
    outcome_names <- paste("Outcome", seq_len(BATCH_TEMPLATE_SHEETS))
  }
  total <- nrow(example_data)
  sheets <- list()
  for (i in seq_along(outcome_names)) {
    mantidas <- max(3, total - 3 * (i - 1))
    sheets[[outcome_names[i]]] <- example_data[seq_len(min(mantidas, total)), , drop = FALSE]
  }
  write_xlsx_workbook(file, sheets)
}

# ============================================================================
# DATA CHECK
#
# What the Data check card reports before anything is analysed: how many studies
# were read, which optional columns survived, and which cells are a problem. It is
# meant to catch a broken spreadsheet here, not after a full batch run.
# ============================================================================

# The verdict shown before the analysis runs: how many studies were read, which
# optional columns are available for subgroups and moderators, and which cells
# would stop the analysis. Reading it here is cheaper than discovering the same
# problem after a batch of plots has been drawn.
#
# optional_analyses and metareg exist because diagnostic accuracy and network
# have neither subgroup analysis nor meta-regression: listing the candidate
# columns there would promise something the module cannot do.
data_check_summary <- function(data, required_cols, optional_analyses = TRUE, metareg = TRUE, data_cols = required_cols) {
  if (is.null(data)) {
    return(data.frame(
      Item = c("Status"),
      Result = c("Load, import, or paste data to run checks."),
      check.names = FALSE
    ))
  }

  missing_cols <- setdiff(required_cols, names(data))
  optional_cols <- setdiff(names(data), union(required_cols, data_cols))
  numeric_optional <- optional_cols[vapply(optional_cols, function(col) is.numeric(data[[col]]), logical(1))]
  # a study reporting a median leaves the mean and SD cells empty on purpose
  missing_cells <- sum(is.na(data[setdiff(names(data), setdiff(data_cols, required_cols))]))

  problems <- character(0)
  if (nrow(data) == 0) problems <- c(problems, "no rows detected")
  if (length(missing_cols) > 0) problems <- c(problems, paste("missing:", paste(missing_cols, collapse = ", ")))
  if (missing_cells > 0) problems <- c(problems, paste(missing_cells, "blank/NA cells"))
  if (all(c("event", "n") %in% names(data)) && any(data$event > data$n, na.rm = TRUE)) problems <- c(problems, "event is greater than n")
  if (all(c("event.e", "n.e") %in% names(data)) && any(data$event.e > data$n.e, na.rm = TRUE)) problems <- c(problems, "event.e is greater than n.e")
  if (all(c("event.c", "n.c") %in% names(data)) && any(data$event.c > data$n.c, na.rm = TRUE)) problems <- c(problems, "event.c is greater than n.c")
  if (all(c("TP", "FN") %in% names(data)) && any((data$TP + data$FN) <= 0, na.rm = TRUE)) problems <- c(problems, "TP + FN must be greater than 0")
  if (all(c("TN", "FP") %in% names(data)) && any((data$TN + data$FP) <= 0, na.rm = TRUE)) problems <- c(problems, "TN + FP must be greater than 0")
  for (col in intersect(c("n", "n.e", "n.c"), names(data))) {
    if (any(data[[col]] <= 0, na.rm = TRUE)) problems <- c(problems, paste(col, "must be greater than 0"))
  }
  for (col in intersect(c("TP", "FP", "FN", "TN"), names(data))) {
    if (any(data[[col]] < 0, na.rm = TRUE)) problems <- c(problems, paste(col, "must be 0 or greater"))
  }
  for (col in intersect(c("sd", "sd.e", "sd.c", "seTE"), names(data))) {
    if (any(data[[col]] <= 0, na.rm = TRUE)) problems <- c(problems, paste(col, "must be greater than 0"))
  }
  if (all(c("lower", "upper") %in% names(data)) && any(data$lower > data$upper, na.rm = TRUE)) problems <- c(problems, "lower is greater than upper")

  itens <- c("Required columns", "Studies detected")
  valores <- c(
    if (length(missing_cols) == 0) paste0(length(required_cols), "/", length(required_cols), " found") else paste("Missing", paste(missing_cols, collapse = ", ")),
    as.character(nrow(data))
  )
  if (isTRUE(optional_analyses)) {
    itens <- c(itens, "Optional subgroup columns")
    valores <- c(valores,
                 if (length(optional_cols) == 0) "None detected" else paste(optional_cols, collapse = ", "))
    if (isTRUE(metareg)) {
      itens <- c(itens, "Numeric meta-regression columns")
      valores <- c(valores,
                   if (length(numeric_optional) == 0) "None detected" else paste(numeric_optional, collapse = ", "))
    }
  }
  # Which studies arrive as a median matters to whoever reads the review, so it
  # is reported here and not left to be discovered in the forest plot.
  if (all(c("median.e", "median.c") %in% names(data))) {
    convertidos <- sum(is.na(data$mean.e) & !is.na(data$median.e), na.rm = TRUE)
    itens <- c(itens, "Mean and SD estimated from quartiles")
    valores <- c(valores, if (convertidos == 0) "None, every study reports mean and SD" else
                 paste0(convertidos, " of ", nrow(data),
                        " studies (Luo 2018 for the mean, Shi 2020 for the SD)"))
  }
  if (all(c("median", "mean") %in% names(data)) && !"median.e" %in% names(data)) {
    convertidos <- sum(is.na(data$mean) & !is.na(data$median), na.rm = TRUE)
    itens <- c(itens, "Mean and SD estimated from quartiles")
    valores <- c(valores, if (convertidos == 0) "None, every study reports mean and SD" else
                 paste0(convertidos, " of ", nrow(data),
                        " studies (Luo 2018 for the mean, Shi 2020 for the SD)"))
  }
  itens <- c(itens, "Problems")
  valores <- c(valores, if (length(problems) == 0) "None" else paste(problems, collapse = "; "))

  data.frame(Item = itens, Result = valores, check.names = FALSE)
}

# Data check for an arm-level binary network: besides the usual column report,
# it counts studies, treatments and arms, which is what tells the user whether
# the network is the one they meant to build.
network_binary_data_check_summary <- function(data) {
  required_cols <- module_data_specs$network_binary$required
  if (is.null(data)) {
    return(data.frame(
      Item = c("Status"),
      Result = c("Load, import, or paste network data to run checks."),
      check.names = FALSE
    ))
  }

  missing_cols <- setdiff(required_cols, names(data))
  problems <- character(0)
  if (length(missing_cols) > 0) problems <- c(problems, paste("missing:", paste(missing_cols, collapse = ", ")))
  if (all(c("responders", "sampleSize") %in% names(data))) {
    if (any(data$responders > data$sampleSize, na.rm = TRUE)) problems <- c(problems, "responders is greater than sampleSize")
    if (any(data$sampleSize <= 0, na.rm = TRUE)) problems <- c(problems, "sampleSize must be greater than 0")
  }
  if ("study" %in% names(data)) {
    arms_per_study <- table(data$study)
    if (any(arms_per_study < 2)) problems <- c(problems, "each study must include at least two arms")
  }

  data.frame(
    Item = c("Required columns", "Studies detected", "Treatment arms", "Treatments detected", "Multi-arm studies", "Problems"),
    Result = c(
      if (length(missing_cols) == 0) paste0(length(required_cols), "/", length(required_cols), " found") else paste("Missing", paste(missing_cols, collapse = ", ")),
      if ("study" %in% names(data)) as.character(length(unique(data$study))) else "NA",
      as.character(nrow(data)),
      if ("treatment" %in% names(data)) as.character(length(unique(data$treatment))) else "NA",
      if ("study" %in% names(data)) as.character(sum(table(data$study) > 2)) else "NA",
      if (length(problems) == 0) "None" else paste(problems, collapse = "; ")
    ),
    check.names = FALSE
  )
}

# The same check for an arm-level continuous network.
network_continuous_data_check_summary <- function(data) {
  required_cols <- module_data_specs$network_continuous$required
  if (is.null(data)) {
    return(data.frame(
      Item = c("Status"),
      Result = c("Load, import, or paste network data to run checks."),
      check.names = FALSE
    ))
  }

  missing_cols <- setdiff(required_cols, names(data))
  problems <- character(0)
  if (length(missing_cols) > 0) problems <- c(problems, paste("missing:", paste(missing_cols, collapse = ", ")))
  if ("std.dev" %in% names(data) && any(data[["std.dev"]] < 0, na.rm = TRUE)) problems <- c(problems, "std.dev must be non-negative")
  if ("sampleSize" %in% names(data) && any(data$sampleSize <= 0, na.rm = TRUE)) problems <- c(problems, "sampleSize must be greater than 0")
  if ("study" %in% names(data)) {
    arms_per_study <- table(data$study)
    if (any(arms_per_study < 2)) problems <- c(problems, "each study must include at least two arms")
  }

  data.frame(
    Item = c("Required columns", "Studies detected", "Treatment arms", "Treatments detected", "Multi-arm studies", "Problems"),
    Result = c(
      if (length(missing_cols) == 0) paste0(length(required_cols), "/", length(required_cols), " found") else paste("Missing", paste(missing_cols, collapse = ", ")),
      if ("study" %in% names(data)) as.character(length(unique(data$study))) else "NA",
      as.character(nrow(data)),
      if ("treatment" %in% names(data)) as.character(length(unique(data$treatment))) else "NA",
      if ("study" %in% names(data)) as.character(sum(table(data$study) > 2)) else "NA",
      if (length(problems) == 0) "None" else paste(problems, collapse = "; ")
    ),
    check.names = FALSE
  )
}

# The same check for a contrast-level network, counting comparisons instead of
# arms.
network_precalc_ci_data_check_summary <- function(data) {
  required_cols <- module_data_specs$network_precalc_ci$required
  if (is.null(data)) {
    return(data.frame(
      Item = c("Status"),
      Result = c("Load, import, or paste pre-calculated network data to run checks."),
      check.names = FALSE
    ))
  }

  missing_cols <- setdiff(required_cols, names(data))
  problems <- character(0)
  if (length(missing_cols) > 0) problems <- c(problems, paste("missing:", paste(missing_cols, collapse = ", ")))
  if (all(c("treat1", "treat2") %in% names(data)) && any(data$treat1 == data$treat2, na.rm = TRUE)) problems <- c(problems, "treat1 and treat2 must be different")
  if (all(c("lower", "upper") %in% names(data)) && any(data$lower > data$upper, na.rm = TRUE)) problems <- c(problems, "lower is greater than upper")
  treatment_count <- if (all(c("treat1", "treat2") %in% names(data))) length(unique(c(data$treat1, data$treat2))) else NA_integer_

  data.frame(
    Item = c("Required columns", "Studies/comparisons detected", "Treatments detected", "Problems"),
    Result = c(
      if (length(missing_cols) == 0) paste0(length(required_cols), "/", length(required_cols), " found") else paste("Missing", paste(missing_cols, collapse = ", ")),
      as.character(nrow(data)),
      as.character(treatment_count),
      if (length(problems) == 0) "None" else paste(problems, collapse = "; ")
    ),
    check.names = FALSE
  )
}

# The same verdict for a batch, one line per sheet, so a single bad tab is
# visible before anything runs.
batch_data_check_summary <- function(outcomes, required_cols) {
  if (is.null(outcomes)) {
    return(data.frame(
      Item = c("Status"),
      Result = c("Upload a multi-outcome workbook to run checks."),
      check.names = FALSE
    ))
  }

  sheet_count <- length(outcomes)
  missing_by_sheet <- vapply(names(outcomes), function(sheet) {
    missing_cols <- setdiff(required_cols, names(outcomes[[sheet]]))
    if (length(missing_cols) == 0) "" else paste0(sheet, ": ", paste(missing_cols, collapse = ", "))
  }, character(1))
  missing_by_sheet <- missing_by_sheet[nzchar(missing_by_sheet)]
  all_cols <- unique(unlist(lapply(outcomes, names), use.names = FALSE))
  optional_cols <- setdiff(all_cols, required_cols)
  numeric_optional <- optional_cols[vapply(optional_cols, function(col) {
    any(vapply(outcomes, function(data) col %in% names(data) && is.numeric(data[[col]]), logical(1)))
  }, logical(1))]

  data.frame(
    Item = c("Outcomes detected", "Required columns", "Optional subgroup columns", "Numeric meta-regression columns"),
    Result = c(
      as.character(sheet_count),
      if (length(missing_by_sheet) == 0) paste0(length(required_cols), "/", length(required_cols), " found in every sheet") else paste(missing_by_sheet, collapse = "; "),
      if (length(optional_cols) == 0) "None detected" else paste(optional_cols, collapse = ", "),
      if (length(numeric_optional) == 0) "None detected" else paste(numeric_optional, collapse = ", ")
    ),
    check.names = FALSE
  )
}


# ============================================================================
# RESULT CARD BUILDING BLOCKS
#
# The repeated markup of the results step. result_action_group() builds one card's
# row of buttons and its export settings, and it is what gives batch mode its
# 1. Run analysis and 2. Download pair in place of a plain Preview.
# ============================================================================

# Rounds an effect for display. Rounding lives here and not in the analysis, so
# the stored result always keeps full precision.
format_effect_value <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("NA")
  formatC(as.numeric(x), digits = digits, format = "f")
}

# A compact table of the batch: one row per outcome with its pooled effect, used
# by the summary modal, where a full print of every model would be unreadable.
batch_result_overview <- function(result) {
  if (!is_batch_result(result)) return(NULL)

  data.frame(
    Outcome = names(result$outcomes),
    Studies = vapply(result$outcomes, function(x) nrow(x$data), integer(1)),
    Subgroup = vapply(result$outcomes, function(x) if (isTRUE(x$subgroup_used)) "Done" else "Skipped", character(1)),
    Metareg = vapply(result$outcomes, function(x) if (isTRUE(x$metareg_used)) "Done" else "Skipped", character(1)),
    Notes = vapply(result$outcomes, function(x) {
      note <- if (!is.null(x$batch_note)) x$batch_note else ""
      if (!nzchar(note)) "OK" else note
    }, character(1)),
    check.names = FALSE
  )
}

# The controls of one result card. Which buttons appear depends on the mode:
# Preview and Summary for a single outcome, where the plot is drawn on screen,
# and the numbered 1. Run analysis and 2. Download pair in batch, where nothing
# is rendered and the files have to be built before they can be handed over.
result_action_group <- function(prefix, download_id, preview_id = NULL, summary_id = NULL, prepare_id = NULL) {
  buttons <- list()

  if (!is.null(preview_id)) {
    buttons <- c(buttons, list(actionButton(preview_id, "Preview", class = "result-action")))
  }
  if (!is.null(summary_id)) {
    buttons <- c(buttons, list(actionButton(summary_id, "Summary", class = "result-action")))
  }
  if (!is.null(prepare_id)) {
    buttons <- c(buttons, list(
      actionButton(
        prepare_id,
        "1. Run analysis",
        class = "result-action prepare-action",
        style = "display:none;"
      )
    ))
  }
  buttons <- c(buttons, list(uiOutput(paste0(download_id, "_gate"))))

  tagList(
    div(class = "result-button-row", buttons),
    div(
      id = paste0(download_id, "_status"),
      class = "batch-download-status",
      "For batch exports, click 1. Run analysis before downloading."
    ),
    div(
      class = "card-export-controls",
      div(
        class = "compact-input",
        tags$label("Format"),
        selectInput(paste0(prefix, "_format"), NULL, choices = c("PNG" = "png", "PDF" = "pdf"), selected = "png")
      ),
      div(
        class = "compact-input",
        tags$label("Width"),
        numericInput(paste0(prefix, "_width"), NULL, value = 10, min = 4, max = 30, step = 0.5)
      ),
      div(
        class = "compact-input",
        tags$label("Height"),
        numericInput(paste0(prefix, "_height"), NULL, value = 8, min = 4, max = 30, step = 0.5)
      )
    )
  )
}

# The two publication-ready tables, shared by every pairwise and single-arm
# module. Server side is wired by register_summary_tables().
summary_table_cards <- function(prefix) {
  tagList(
    div(
      class = "analysis-card result-control-card main-result-card",
      tags$h3("Subgroup summary table"),
      tags$p(class = "help-text", "Build a publication-ready table: pick one or more columns from your dataset and get the pooled effect, number of studies and total patients for each subgroup, side by side. Categorical columns such as region or sex work best. A numeric column creates one subgroup per distinct value."),
      selectizeInput(
        paste0(prefix, "_subgroup_table_cols"),
        "Columns to include",
        choices = character(0),
        selected = character(0),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), dropdownParent = "body", placeholder = "Select one or more columns")
      ),
      div(
        class = "result-button-row",
        actionButton(paste0("preview_", prefix, "_subgroup_table"), "Preview table", class = "result-action"),
        downloadButton(paste0("download_", prefix, "_subgroup_table"), "Download XLSX", class = "result-action")
      )
    ),
    div(
      class = "analysis-card result-control-card main-result-card",
      tags$h3("Meta-regression summary table"),
      tags$p(class = "help-text", "Test one or more numeric moderators at once. Each row reports the change in the effect per 1-unit increase. To compare categories, use the subgroup table instead."),
      selectizeInput(
        paste0(prefix, "_metareg_table_cols"),
        "Moderators to include",
        choices = character(0),
        selected = character(0),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), dropdownParent = "body", placeholder = "Select one or more columns")
      ),
      div(
        class = "result-button-row",
        actionButton(paste0("preview_", prefix, "_metareg_table"), "Preview table", class = "result-action"),
        downloadButton(paste0("download_", prefix, "_metareg_table"), "Download XLSX", class = "result-action")
      )
    )
  )
}

# The Data check card with the Next button in its own footer, so the verdict
# and the step that follows it are read in one place.
data_check_card <- function(output_id, next_button = NULL) {
  # The Next button lives in this card's footer: the user reads the verdict of
  # the check and the next step is immediately below it. It is kept inside a
  # .step-actions wrapper because the attention pulse in easymeta.js looks for
  # that class within the active panel.
  div(
    class = "analysis-card data-check-card",
    tags$h3("Data check"),
    tags$p(class = "help-text", "MetaVidence checks required columns, study count, optional subgroup columns and numeric meta-regression candidates."),
    tableOutput(output_id),
    if (!is.null(next_button)) div(class = "step-actions data-check-actions", next_button)
  )
}


# ============================================================================
# EXPORT SETTINGS
#
# Format, width and height as the user typed them, cleaned into values the export
# code can trust, plus the signature that decides whether a prepared batch archive
# still matches what is on screen.
# ============================================================================

# The export fields are free text, so they arrive as whatever the user typed.
# This clamps them to a format the device supports and to sizes that produce a
# readable plot, rather than failing on an empty or absurd value.
normalize_export_settings <- function(file_format, width, height) {
  if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
  if (is.null(width) || is.na(width) || width <= 0) width <- 10
  if (is.null(height) || is.na(height) || height <= 0) height <- 8
  list(
    file_format = file_format,
    width = width,
    height = height
  )
}

# The picker columns are part of what a prepared export IS: two archives built
# from different subgroup columns are different files. Carrying the columns in
# the signature lets the existing lock machinery invalidate a prepared export as
# soon as the column changes, instead of leaving a stale archive downloadable.
batch_export_signature <- function(file_format, width, height, subgroup_col = "", metareg_col = "") {
  paste(file_format, format(width, nsmall = 2, trim = TRUE), format(height, nsmall = 2, trim = TRUE),
        subgroup_col, metareg_col, sep = "|")
}

# Example: 12 effects with their confidence intervals, on the original scale.
precalc_te_ci_example_data <- function() {
  data.frame(
    study = paste("Study", LETTERS[1:12]),
    TE = c(0.76, 0.85, 0.71, 0.89, 0.78, 0.92, 0.80, 0.84, 0.82, 0.75, 0.88, 0.79),
    lower = c(0.61, 0.72, 0.54, 0.76, 0.64, 0.79, 0.66, 0.71, 0.68, 0.59, 0.74, 0.63),
    upper = c(0.95, 1.01, 0.93, 1.05, 0.96, 1.07, 0.97, 0.99, 0.99, 0.95, 1.04, 0.98),
    n.e = c(120, 150, 95, 210, 180, 130, 240, 160, 175, 125, 220, 105),
    n.c = c(118, 148, 97, 208, 182, 128, 238, 158, 173, 127, 218, 107),
    Region = c("North America", "Europe", "Europe", "Asia", "Asia", "North America", "Latin America", "Latin America", "North America", "Europe", "Asia", "Latin America"),
    Design = c("RCT", "RCT", "Observational", "RCT", "Observational", "RCT", "RCT", "Observational", "RCT", "Observational", "RCT", "Observational"),
    RiskOfBias = c("Low", "Low", "Some concerns", "Low", "High", "Some concerns", "Low", "Some concerns", "High", "Low", "Some concerns", "High"),
    MeanAge = c(61, 58, 65, 57, 63, 60, 55, 59, 62, 56, 64, 58),
    FollowUpMonths = c(12, 18, 9, 24, 18, 12, 24, 15, 18, 12, 24, 9),
    check.names = FALSE
  )
}

# Example: 12 effects with their standard errors.
precalc_te_sete_example_data <- function() {
  data.frame(
    study = paste("Study", LETTERS[1:12]),
    TE = c(0.76, 0.85, 0.71, 0.89, 0.78, 0.92, 0.80, 0.84, 0.82, 0.75, 0.88, 0.79),
    seTE = c(0.1128, 0.0864, 0.1388, 0.0826, 0.1035, 0.0768, 0.0982, 0.0848,
             0.0951, 0.1204, 0.0873, 0.1112),
    n.e = c(120, 150, 95, 210, 180, 130, 240, 160, 175, 125, 220, 105),
    n.c = c(118, 148, 97, 208, 182, 128, 238, 158, 173, 127, 218, 107),
    Region = c("North America", "Europe", "Europe", "Asia", "Asia", "North America", "Latin America", "Latin America", "North America", "Europe", "Asia", "Latin America"),
    Design = c("RCT", "RCT", "Observational", "RCT", "Observational", "RCT", "RCT", "Observational", "RCT", "Observational", "RCT", "Observational"),
    RiskOfBias = c("Low", "Low", "Some concerns", "Low", "High", "Some concerns", "Low", "Some concerns", "High", "Low", "Some concerns", "High"),
    MeanAge = c(61, 58, 65, 57, 63, 60, 55, 59, 62, 56, 64, 58),
    FollowUpMonths = c(12, 18, 9, 24, 18, 12, 24, 15, 18, 12, 24, 9),
    check.names = FALSE
  )
}

# Example: 12 effects carrying both a standard error and an interval.
precalc_te_sete_ci_example_data <- function() {
  data.frame(
    study = paste("Study", LETTERS[1:12]),
    TE = c(0.76, 0.85, 0.71, 0.89, 0.78, 0.92, 0.80, 0.84, 0.82, 0.75, 0.88, 0.79),
    seTE = c(0.1128, 0.0864, 0.1388, 0.0826, 0.1035, 0.0768, 0.0982, 0.0848,
             0.0951, 0.1204, 0.0873, 0.1112),
    lower = c(0.61, 0.72, 0.54, 0.76, 0.64, 0.79, 0.66, 0.71, 0.68, 0.59, 0.74, 0.63),
    upper = c(0.95, 1.01, 0.93, 1.05, 0.96, 1.07, 0.97, 0.99, 0.99, 0.95, 1.04, 0.98),
    n.e = c(120, 150, 95, 210, 180, 130, 240, 160, 175, 125, 220, 105),
    n.c = c(118, 148, 97, 208, 182, 128, 238, 158, 173, 127, 218, 107),
    Region = c("North America", "Europe", "Europe", "Asia", "Asia", "North America", "Latin America", "Latin America", "North America", "Europe", "Asia", "Latin America"),
    Design = c("RCT", "RCT", "Observational", "RCT", "Observational", "RCT", "RCT", "Observational", "RCT", "Observational", "RCT", "Observational"),
    RiskOfBias = c("Low", "Low", "Some concerns", "Low", "High", "Some concerns", "Low", "Some concerns", "High", "Low", "Some concerns", "High"),
    MeanAge = c(61, 58, 65, 57, 63, 60, 55, 59, 62, 56, 64, 58),
    FollowUpMonths = c(12, 18, 9, 24, 18, 12, 24, 15, 18, 12, 24, 9),
    check.names = FALSE
  )
}

# Choice screen for effect sizes already calculated. The three workflows differ
# only in what the user has at hand: a confidence interval, a standard error,
# or both.
precalculated_page <- function() {
  div(
    class = "workflow-page",
    actionButton("back_home_precalculated", "Back to home", class = "back-button"),
    div(
      class = "workflow-header",
      tags$span(class = "wf-header-icon", module_icon("precalculated")),
      tags$p(class = "eyebrow", "Pre-calculated effect size data"),
      tags$h2("Which effect estimates do you have?"),
      tags$p("Start from the effect sizes you already extracted or calculated, in whichever combination your table contains.")
    ),
    div(
      class = "workflow-options",
      workflow_option_card(
        "go_precalc_te_ci",
        "TE + 95% CI",
        "Treatment effect with lower and upper confidence limits.",
        c("study", "TE", "lower", "upper")
      ),
      workflow_option_card(
        "go_precalc_te_sete",
        "TE + seTE",
        "Treatment effect with its standard error.",
        c("study", "TE", "seTE")
      ),
      workflow_option_card(
        "go_precalc_te_sete_ci",
        "TE + seTE + 95% CI",
        "Complete tables with treatment effect, standard error and confidence interval.",
        c("study", "TE", "seTE", "lower", "upper")
      )
    )
  )
}

# Choice screen for network meta-analysis: arm-level binary, arm-level
# continuous, or contrast-level effects.
network_page <- function() {
  div(
    class = "workflow-page",
    actionButton("back_home_network", "Back to home", class = "back-button"),
    div(
      class = "workflow-header",
      tags$span(class = "wf-header-icon", module_icon("network")),
      tags$p(class = "eyebrow", "Network meta-analysis"),
      tags$h2("Choose your network data format"),
      tags$p("Frequentist network meta-analysis, one outcome at a time, to keep assumptions and outputs transparent.")
    ),
    div(
      class = "workflow-options",
      workflow_option_card(
        "go_network_binary",
        "Binary outcome",
        "Arm-level counts: one row per treatment arm in each study.",
        c("study", "treatment", "responders", "sampleSize")
      ),
      workflow_option_card(
        "go_network_continuous",
        "Continuous outcome",
        "Arm-level means and standard deviations for each treatment arm.",
        c("study", "treatment", "mean", "std.dev", "sampleSize")
      ),
      workflow_option_card(
        "go_network_precalculated",
        "Pre-calculated TE",
        "Comparison-level treatment effects with 95% confidence intervals.",
        c("study", "treat1", "treat2", "TE", "lower", "upper")
      )
    )
  )
}

# The result card controls for the network and diagnostic modules. They have no
# batch mode, so there is no prepare step here: Preview, Summary and a plain
# Download.
network_result_action_group <- function(prefix, download_id, preview_id = NULL, summary_id = NULL, default_width = 10, default_height = 8) {
  buttons <- list()
  if (!is.null(preview_id)) buttons <- c(buttons, list(actionButton(preview_id, "Preview", class = "result-action")))
  if (!is.null(summary_id)) buttons <- c(buttons, list(actionButton(summary_id, "Summary", class = "result-action")))
  # On a card with a Summary button but no Preview, a bare "Download" reads as if it
  # would fetch the summary text. Naming the plot is the only thing that separates them.
  rotulo <- if (is.null(preview_id) && !is.null(summary_id)) "Download plot" else "Download"
  buttons <- c(buttons, list(downloadButton(download_id, rotulo, class = "result-action")))

  tagList(
    div(class = "result-button-row", buttons),
    div(
      class = "card-export-controls",
      div(
        class = "compact-input",
        tags$label("Format"),
        selectInput(paste0(prefix, "_format"), NULL, choices = c("PNG" = "png", "PDF" = "pdf"), selected = "png")
      ),
      div(
        class = "compact-input",
        tags$label("Width"),
        numericInput(paste0(prefix, "_width"), NULL, value = default_width, min = 4, max = 30, step = 0.5)
      ),
      div(
        class = "compact-input",
        tags$label("Height"),
        numericInput(paste0(prefix, "_height"), NULL, value = default_height, min = 4, max = 30, step = 0.5)
      )
    )
  )
}

# Workflow screen: one diagnostic test, with SROC and threshold effect.
diagnostic_single_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_diagnostic_from_single", "Back to Diagnostic", class = "back-button"),
      actionButton("back_home_from_diagnostic_single", "Back to home", class = "back-button secondary-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Diagnostic meta-analysis"),
      tags$h2("Single-arm diagnostic workflow"),
      tags$p("Add 2x2 diagnostic accuracy data and export sensitivity, specificity, DOR and summary-point results.")
    ),
    div(class = "step-indicator", textOutput("diagnostic_single_step_label")),
    tabsetPanel(
      id = "diagnostic_single_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Upload one diagnostic accuracy table, paste from Google Sheets, or load the example dataset."),
            tags$p(
              class = "help-text required-columns",
              HTML("<strong>Required columns (use these exact Excel column names):</strong><br><strong>study</strong>: the study label, for example first author and year.<br><strong>TP</strong>: true positives.<br><strong>FP</strong>: false positives.<br><strong>FN</strong>: false negatives.<br><strong>TN</strong>: true negatives.<br>Optional columns can be kept in the dataset for future extensions.")
            ),
            actionButton("load_diagnostic_single_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_diagnostic_single_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "diagnostic_single_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "diagnostic_single_paste",
              "Paste data from Google Sheets",
              placeholder = "study\tTP\tFP\tFN\tTN\nStudy A\t42\t8\t6\t84\nStudy B\t35\t12\t9\t76",
              rows = 10
            ),
            actionButton("use_diagnostic_single_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("diagnostic_single_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("Diagnostic data format"),
            tags$p(class = "help-text microcopy", "Use one row per study."),
            tags$p(class = "help-text microcopy", "Each row must contain the complete 2x2 table: TP, FP, FN and TN."),
            tags$p(class = "help-text microcopy", "This diagnostic module currently runs one outcome at a time.")
          ),
          data_check_card(
            "diagnostic_single_data_check",
            actionButton("diagnostic_single_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
              textInput("diagnostic_single_outcome", "Outcome name", value = "Outcome"),
              selectInput("diagnostic_single_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
              selectInput("diagnostic_single_col_square", "Forest square color", choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Black" = "black", "Red" = "red3", "Dark green" = "darkgreen"), selected = "red3", selectize = FALSE),
              selectInput("diagnostic_single_col_square_lines", "Forest square line color", choices = c("Black" = "black", "Dark blue" = "darkblue", "Gray" = "gray40", "White" = "white", "Navy" = "navy"), selected = "black", selectize = FALSE)
            ),
            tags$div(class = "status-message prominent-status", textOutput("diagnostic_single_run_status")),
            div(
              class = "step-actions",
              actionButton("diagnostic_single_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_diagnostic_single", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("diagnostic_single_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Sensitivity forest plot"),
              tags$p(class = "help-text", "Pooled sensitivity using TP / (TP + FN)."),
              network_result_action_group("diagnostic_single_sens", "download_diagnostic_single_sens", "preview_diagnostic_single_sens", "summary_diagnostic_single_sens")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Specificity forest plot"),
              tags$p(class = "help-text", "Pooled specificity using TN / (TN + FP)."),
              network_result_action_group("diagnostic_single_spec", "download_diagnostic_single_spec", "preview_diagnostic_single_spec", "summary_diagnostic_single_spec")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Summary point"),
              tags$p(
                class = "help-text",
                HTML("Summary sensitivity and specificity from the bivariate binomial GLMM, with 95% confidence and prediction regions. The forest plots show the individual studies; <strong>this is the only pooled estimate in the module</strong>.")
              ),
              network_result_action_group("diagnostic_single_sroc", "download_diagnostic_single_sroc", NULL, "summary_diagnostic_single_bivariate", default_width = 10, default_height = 8)
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Diagnostic odds ratio"),
              tags$p(class = "help-text", "Forest plot of diagnostic odds ratio from each 2x2 table."),
              network_result_action_group("diagnostic_single_dor", "download_diagnostic_single_dor", "preview_diagnostic_single_dor", "summary_diagnostic_single_dor")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Threshold effect"),
              tags$p(class = "help-text", "Correlation between sensitivity and the false-positive rate across studies, plus the Deeks test for small-study effects."),
              tags$p(class = "help-text microcopy", "A positive correlation suggests the studies used different positivity thresholds, which is the confounder specific to diagnostic accuracy: a test can look better in one group only because its cut-off moved."),
              div(
                class = "result-button-row",
                actionButton("summary_diagnostic_single_threshold", "Summary", class = "result-action")
              )
            )
          ),
          div(
            class = "step-actions",
            actionButton("diagnostic_single_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

# Workflow screen: two or more diagnostic tests compared on the same studies.
diagnostic_comparative_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_diagnostic_from_comparative", "Back to Diagnostic", class = "back-button"),
      actionButton("back_home_from_diagnostic_comparative", "Back to home", class = "back-button secondary-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Diagnostic meta-analysis"),
      tags$h2("Comparative diagnostic workflow"),
      tags$p("Compare two diagnostic tests from paired study-level 2x2 tables.")
    ),
    div(class = "step-indicator", textOutput("diagnostic_comparative_step_label")),
    tabsetPanel(
      id = "diagnostic_comparative_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Upload one comparative diagnostic table, paste from Google Sheets, or load the example dataset."),
            tags$p(
              class = "help-text required-columns",
              HTML("<strong>Required columns (use these exact Excel column names):</strong><br><strong>study</strong>: the study label, for example first author and year.<br><strong>test</strong>: diagnostic test name. Each paired study should have one row per test.<br><strong>TP</strong>: true positives.<br><strong>FP</strong>: false positives.<br><strong>FN</strong>: false negatives.<br><strong>TN</strong>: true negatives.<br>Optional columns can be kept in the dataset for future extensions.")
            ),
            actionButton("load_diagnostic_comparative_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_diagnostic_comparative_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "diagnostic_comparative_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "diagnostic_comparative_paste",
              "Paste data from Google Sheets",
              placeholder = "study\ttest\tTP\tFP\tFN\tTN\nStudy A\tMRI\t57\t22\t8\t105\nStudy A\tctDNA\t48\t9\t17\t118",
              rows = 10
            ),
            actionButton("use_diagnostic_comparative_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("diagnostic_comparative_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("Comparative data format"),
            tags$p(class = "help-text microcopy", "Use a long format table: one row per study/test combination."),
            tags$p(class = "help-text microcopy", "Example: Study A has one MRI row and one ctDNA row, both with TP, FP, FN and TN."),
            tags$p(class = "help-text microcopy", "This module currently compares two tests for one outcome at a time.")
          ),
          data_check_card(
            "diagnostic_comparative_data_check",
            actionButton("diagnostic_comparative_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
              textInput("diagnostic_comparative_outcome", "Outcome name", value = "Outcome"),
              selectInput("diagnostic_comparative_test_a", "Test A", choices = character(0), selected = NULL, selectize = FALSE),
              selectInput("diagnostic_comparative_test_b", "Test B", choices = character(0), selected = NULL, selectize = FALSE),
              selectInput("diagnostic_comparative_col_a", "Test A color", choices = c("Black" = "black", "Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Red" = "red3", "Dark green" = "darkgreen"), selected = "darkblue", selectize = FALSE),
              selectInput("diagnostic_comparative_col_b", "Test B color", choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Black" = "black", "Red" = "red3", "Dark green" = "darkgreen"), selected = "red3", selectize = FALSE),
              selectInput("diagnostic_comparative_col_square", "Forest square color", choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Black" = "black", "Red" = "red3", "Dark green" = "darkgreen"), selected = "red3", selectize = FALSE),
              selectInput("diagnostic_comparative_col_square_lines", "Forest square line color", choices = c("Black" = "black", "Dark blue" = "darkblue", "Gray" = "gray40", "White" = "white", "Navy" = "navy"), selected = "black", selectize = FALSE)
            ),
            tags$div(class = "status-message prominent-status", textOutput("diagnostic_comparative_run_status")),
            div(
              class = "step-actions",
              actionButton("diagnostic_comparative_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_diagnostic_comparative", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("diagnostic_comparative_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Sensitivity by test"),
              tags$p(class = "help-text", "Sensitivity forest plot grouped by diagnostic test."),
              network_result_action_group("diagnostic_comparative_sens", "download_diagnostic_comparative_sens", "preview_diagnostic_comparative_sens", "summary_diagnostic_comparative_sens")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Specificity by test"),
              tags$p(class = "help-text", "Specificity forest plot grouped by diagnostic test."),
              network_result_action_group("diagnostic_comparative_spec", "download_diagnostic_comparative_spec", "preview_diagnostic_comparative_spec", "summary_diagnostic_comparative_spec")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Comparative summary points"),
              tags$p(
                class = "help-text",
                HTML("Summary point for each test, from one joint model with the test as a covariate, grouped by study so a paired design keeps its pairing. <strong>The per-test points and the tests below come from the same fit</strong>.")
              ),
              network_result_action_group("diagnostic_comparative_sroc", "download_diagnostic_comparative_sroc", NULL, "summary_diagnostic_comparative_bivariate")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Overall comparison"),
              tags$p(class = "help-text", "Likelihood-ratio test for overall diagnostic accuracy between tests."),
              actionButton("summary_diagnostic_comparative_overall", "Summary", class = "result-action")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Sensitivity comparison"),
              tags$p(class = "help-text", "Likelihood-ratio test focused on sensitivity differences."),
              actionButton("summary_diagnostic_comparative_sens_test", "Summary", class = "result-action")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Specificity comparison"),
              tags$p(class = "help-text", "Likelihood-ratio test focused on specificity differences."),
              actionButton("summary_diagnostic_comparative_spec_test", "Summary", class = "result-action")
              )
          ),
          div(
            class = "step-actions",
            actionButton("diagnostic_comparative_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

# Workflow screen: network meta-analysis from arm-level binary data.
network_binary_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_network_from_binary", "Back to Network", class = "back-button"),
      actionButton("back_home_from_network_binary", "Back to home", class = "back-button secondary-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Network meta-analysis"),
      tags$h2("Binary outcome workflow"),
      tags$p("Use arm-level binary data to build a frequentist network meta-analysis.")
    ),
    div(class = "step-indicator", textOutput("network_binary_step_label")),
    tabsetPanel(
      id = "network_binary_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Upload one arm-level table, paste from Google Sheets, or load the example dataset."),
            tags$p(class = "help-text required-columns", HTML("<strong>Required columns (use these exact Excel column names):</strong><br><strong>study</strong>: study name or trial identifier. Each treatment arm from the same study must use the same study value.<br><strong>treatment</strong>: treatment or intervention name.<br><strong>responders</strong>: number of events/responders in that treatment arm.<br><strong>sampleSize</strong>: total sample size in that treatment arm.")),
            actionButton("load_network_binary_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_network_binary_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "network_binary_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "network_binary_paste",
              "Paste data from Google Sheets",
              placeholder = "study\ttreatment\tresponders\tsampleSize\nStudy 1\tDrug A\t12\t100\nStudy 1\tPlacebo\t20\t100",
              rows = 10
            ),
            actionButton("use_network_binary_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("network_binary_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("Network data format"),
            tags$p(class = "help-text microcopy", "Use one row per treatment arm, not one row per study comparison."),
            tags$p(class = "help-text microcopy", "Multi-arm studies are allowed when the same study name appears in three or more rows."),
            tags$p(class = "help-text microcopy", "Network modules currently run one outcome at a time.")
          ),
          data_check_card(
            "network_binary_data_check",
            actionButton("network_binary_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
              textInput("network_binary_outcome", "Outcome name", value = "Outcome"),
              selectInput("network_binary_sm", "Summary measure (sm)", choices = c("RR", "OR", "RD"), selected = "RR"),
              selectInput("network_binary_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
              textInput("network_binary_reference", "Reference treatment (optional)", value = ""),
              selectInput("network_binary_method_tau", "Tau method", choices = c("REML", "ML", "DL", "PM", "SJ", "HE", "HS", "EB"), selected = "REML"),
              selectInput("network_binary_small_values", "For ranking, smaller values are", choices = c("Good" = "good", "Bad" = "bad"), selected = "good"),
              selectInput("network_binary_col_points", "Network node color", choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Black" = "black", "Red" = "red3", "Dark green" = "darkgreen"), selected = "darkblue", selectize = FALSE)
            ),
            tags$div(class = "status-message prominent-status", textOutput("network_binary_run_status")),
            div(
              class = "step-actions",
              actionButton("network_binary_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_network_binary", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("network_binary_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Forest plot"),
              tags$p(class = "help-text", "Compare all treatments against the selected reference treatment."),
              network_result_action_group("network_binary_forest", "download_network_binary_forest", "preview_network_binary_forest", "summary_network_binary_main")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Network graph"),
              tags$p(class = "help-text", "Visualize treatment nodes and direct comparisons."),
              network_result_action_group("network_binary_graph", "download_network_binary_graph", "preview_network_binary_graph", "summary_network_binary_graph")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("League table"),
              tags$p(class = "help-text", "Export the pairwise network estimates as an Excel table."),
              div(class = "result-button-row", actionButton("summary_network_binary_league", "Summary", class = "result-action"), downloadButton("download_network_binary_league", "Download XLSX", class = "result-action"))
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Between-study heterogeneity"),
              tags$p(class = "help-text", "Inspect the design-based decomposition and Q statistics."),
              div(class = "result-button-row", actionButton("summary_network_binary_qtest", "Summary", class = "result-action"), downloadButton("download_network_binary_qtest", "Download TXT", class = "result-action"))
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Node splitting"),
              tags$p(class = "help-text", "Compare direct and indirect evidence when available."),
              network_result_action_group("network_binary_split", "download_network_binary_split", NULL, "summary_network_binary_split", default_width = 10, default_height = 12)
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("P-score ranking"),
              tags$p(class = "help-text", "View a fast treatment ranking plot based on P-scores."),
              network_result_action_group("network_binary_rankogram", "download_network_binary_rankogram", "preview_network_binary_rankogram", "summary_network_binary_rank")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Comparison-adjusted funnel plot"),
              tags$p(class = "help-text", "Explore small-study effects across network comparisons."),
              network_result_action_group("network_binary_funnel", "download_network_binary_funnel", "preview_network_binary_funnel", NULL)
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Pairwise object"),
              tags$p(class = "help-text", "Download the pairwise object used to create the network."),
              div(class = "result-button-row", downloadButton("download_network_binary_pairwise", "Download CSV", class = "result-action"))
            )
          ),
          div(
            class = "step-actions",
            actionButton("network_binary_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

# Workflow screen: effect size with a confidence interval.
precalc_te_ci_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_precalculated", "Back to Pre-calculated", class = "back-button"),
      actionButton("back_home_from_precalc_te_ci", "Back to home", class = "back-button secondary-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Pre-calculated effect size data"),
      tags$h2("TE and 95% CI workflow"),
      tags$p("Use this workflow when the treatment effect and its 95% confidence interval are already available.")
    ),
    div(class = "step-indicator", textOutput("precalc_te_ci_step_label")),
    tabsetPanel(
      id = "precalc_te_ci_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Paste directly from Google Sheets, upload an Excel/CSV file, or load the example dataset with 12 studies."),
            tags$p(class = "help-text required-columns", HTML("<strong>Required columns (use these exact Excel column names):</strong><br><strong>study</strong>: the study label, for example first author and year.<br><strong>TE</strong>: the effect estimate exactly as reported in the paper, for example HR, RR, OR, MD or SMD.<br><strong>lower</strong>: the lower 95% confidence interval limit for TE.<br><strong>upper</strong>: the upper 95% confidence interval limit for TE.<br>Optional columns such as <strong>n.e</strong>, <strong>n.c</strong>, <strong>Region</strong> or <strong>MeanAge</strong> can be used later for forest display, subgroup analysis or meta-regression.")),
            actionButton("load_precalc_te_ci_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_precalc_te_ci_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "precalc_te_ci_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "precalc_te_ci_paste",
              "Paste data from Google Sheets",
              placeholder = "study\tTE\tlower\tupper\tn.e\tn.c\tRegion\nStudy 1\t-0.23\t-0.41\t-0.05\t120\t118\tNorth",
              rows = 10
            ),
            actionButton("use_precalc_te_ci_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("precalc_te_ci_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("More than one outcome"),
            tags$p(class = "help-text microcopy", "Upload one Excel file. Each sheet = one outcome."),
            tags$p(class = "help-text microcopy", "Name each sheet with the outcome name. MetaVidence uses sheet names in plots and exported files."),
            tags$p(class = "help-text microcopy", "Column names must match exactly. Batch mode exports all outcomes together."),
            tags$div(
              class = "em-upload-row",
              fileInput(
                "precalc_te_ci_multi_file",
                "Import multi-outcome workbook (.xlsx)",
                accept = c(".xlsx", ".xls")
              ),
              downloadButton("download_precalc_te_ci_batch_template", "Download XLSX template", class = "secondary-button")
            ),
            tags$div(class = "status-message", textOutput("precalc_te_ci_multi_status"))
          ),
          data_check_card(
            "precalc_te_ci_data_check",
            actionButton("precalc_te_ci_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
                div(id = "precalc_te_ci_outcome_wrap", textInput("precalc_te_ci_outcome", "Outcome name", value = "Outcome")),
                selectInput("precalc_te_ci_outcome_direction", "If the outcome increases, is that beneficial or harmful?", choices = c("Harmful" = "harmful", "Beneficial" = "beneficial"), selected = "harmful"),
                selectInput("precalc_te_ci_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
                selectInput("precalc_te_ci_sm", "Summary measure (sm)", choices = c("HR", "RR", "OR", "RD", "MD", "SMD"), selected = "HR"),
                selectInput("precalc_te_ci_method_tau", "Tau method", choices = c("REML", "ML", "DL", "PM", "SJ", "HE", "HS", "EB"), selected = "REML"),
                selectInput("precalc_te_ci_method_i2", "I-squared method", choices = c("Q (Higgins and Thompson)" = "Q", "From tau-squared" = "tau2"), selected = "Q"),
                selectInput("precalc_te_ci_prediction", "Prediction interval", choices = c("Yes" = "yes", "No" = "no"), selected = "no"),
                selectInput("precalc_te_ci_method_random_ci", "Random CI method", choices = c("classic", "HK", "KR"), selected = "classic"),
                selectInput("precalc_te_ci_method_predict", "Prediction method", choices = c("V", "HTS"), selected = "V"),
                textInput("precalc_te_ci_label_e", "Experimental group name", value = "Experimental"),
                textInput("precalc_te_ci_label_c", "Control group name", value = "Control"),
                selectInput(
                "precalc_te_ci_col_square",
                "Forest square color",
                choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Dodger blue" = "dodgerblue3", "Navy" = "navy", "Black" = "black", "Gray" = "gray40", "Red" = "red3", "Dark green" = "darkgreen"),
                selected = "red3",
                selectize = FALSE
              ),
              selectInput(
                "precalc_te_ci_col_square_lines",
                "Forest square line color",
                choices = c("Black" = "black", "Dark blue" = "darkblue", "Gray" = "gray40", "White" = "white", "Navy" = "navy"),
                selected = "black",
                selectize = FALSE
              ),
              selectInput(
                "precalc_te_ci_forest_sort",
                "Sort studies by effect size",
                choices = c("No" = "no", "Yes" = "yes"),
                selected = "no",
                selectize = FALSE
              ),
              forest_column_selector("precalc_te_ci_forest_cols")
            ),
            tags$p(class = "help-text", "For HR, RR and OR, enter TE and confidence limits on the original ratio scale, not log-transformed values. For MD, SMD and RD, enter the effect exactly as reported."),
            tags$div(class = "status-message prominent-status", textOutput("precalc_te_ci_run_status")),
            div(
              class = "step-actions",
              actionButton("precalc_te_ci_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_precalc_te_ci", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("precalc_te_ci_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Forest plot"),
              tags$p(class = "help-text", "Open the forest plot, inspect the model summary, or download the figure."),
              result_action_group("precalc_te_ci_forest", "download_precalc_te_ci_forest", "preview_precalc_te_ci_forest", "summary_precalc_te_ci_main", "prepare_precalc_te_ci_forest")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Leave-one-out analysis"),
              tags$p(class = "help-text", "Preview the sensitivity analysis or download the leave-one-out forest plot."),
              result_action_group("precalc_te_ci_loo", "download_precalc_te_ci_loo", "preview_precalc_te_ci_loo", NULL, "prepare_precalc_te_ci_loo")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Funnel plot"),
              tags$p(class = "help-text", "Preview the funnel plot or download it for reporting."),
              result_action_group("precalc_te_ci_funnel", "download_precalc_te_ci_funnel", "preview_precalc_te_ci_funnel", NULL, "prepare_precalc_te_ci_funnel")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Small-study effects"),
              tags$p(class = "help-text", "Open the Egger test result and interpretation note."),
              div(
                class = "result-button-row",
                actionButton("summary_precalc_te_ci_bias", "Summary", class = "result-action")
              )
            ),
            div(
              id = "precalc_te_ci_subgroup_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Subgroup analysis"),
              tags$p(id = "precalc_te_ci_subgroup_help_single", class = "help-text", "Pick a column to draw its subgroup forest plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "precalc_te_ci_subgroup_help_batch", class = "help-text", style = "display:none;", "One subgroup forest plot per outcome, all using the column above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("precalc_te_ci_subgroup_pick", "Subgroup column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("precalc_te_ci_subgroup", "download_precalc_te_ci_subgroup", "preview_precalc_te_ci_subgroup", "summary_precalc_te_ci_subgroup", "prepare_precalc_te_ci_subgroup")
            ),
            div(
              id = "precalc_te_ci_metareg_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Meta-regression"),
              tags$p(id = "precalc_te_ci_metareg_help_single", class = "help-text", "Pick a numeric moderator to draw its bubble plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "precalc_te_ci_metareg_help_batch", class = "help-text", style = "display:none;", "One bubble plot per outcome, all using the moderator above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("precalc_te_ci_metareg_pick", "Moderator column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("precalc_te_ci_metareg", "download_precalc_te_ci_metareg", "preview_precalc_te_ci_metareg", "summary_precalc_te_ci_metareg", "prepare_precalc_te_ci_metareg")
            ),
            summary_table_cards("precalc_te_ci")
          ),
          div(
            class = "step-actions",
            actionButton("precalc_te_ci_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

# Workflow screen: effect size with a standard error.
precalc_te_sete_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_precalculated_sete", "Back to Pre-calculated", class = "back-button"),
      actionButton("back_home_from_precalc_te_sete", "Back to home", class = "back-button secondary-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Pre-calculated effect size data"),
      tags$h2("TE and seTE workflow"),
      tags$p("Use this workflow when the treatment effect and its standard error are already available.")
    ),
    div(class = "step-indicator", textOutput("precalc_te_sete_step_label")),
    tabsetPanel(
      id = "precalc_te_sete_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Paste directly from Google Sheets, upload an Excel/CSV file, or load the example dataset with 12 studies."),
            tags$p(class = "help-text required-columns", HTML("<strong>Required columns (use these exact Excel column names):</strong><br><strong>study</strong>: the study label, for example first author and year.<br><strong>TE</strong>: the effect estimate exactly as reported in the paper, for example HR, RR, OR, MD or SMD. For HR, RR and OR use the original ratio scale, not the logarithm.<br><strong>seTE</strong>: the standard error of TE, on the log scale for HR, RR and OR, which is the scale papers report it on.<br>Optional columns such as <strong>n.e</strong>, <strong>n.c</strong>, <strong>Region</strong> or <strong>MeanAge</strong> can be used later for forest display, subgroup analysis or meta-regression.")),
            actionButton("load_precalc_te_sete_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_precalc_te_sete_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "precalc_te_sete_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "precalc_te_sete_paste",
              "Paste data from Google Sheets",
              placeholder = "study\tTE\tseTE\tn.e\tn.c\tRegion\nStudy 1\t0.76\t0.11\t120\t118\tNorth",
              rows = 10
            ),
            actionButton("use_precalc_te_sete_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("precalc_te_sete_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("More than one outcome"),
            tags$p(class = "help-text microcopy", "Upload one Excel file. Each sheet = one outcome."),
            tags$p(class = "help-text microcopy", "Name each sheet with the outcome name. MetaVidence uses sheet names in plots and exported files."),
            tags$p(class = "help-text microcopy", "Column names must match exactly. Batch mode exports all outcomes together."),
            tags$div(
              class = "em-upload-row",
              fileInput(
                "precalc_te_sete_multi_file",
                "Import multi-outcome workbook (.xlsx)",
                accept = c(".xlsx", ".xls")
              ),
              downloadButton("download_precalc_te_sete_batch_template", "Download XLSX template", class = "secondary-button")
            ),
            tags$div(class = "status-message", textOutput("precalc_te_sete_multi_status"))
          ),
          data_check_card(
            "precalc_te_sete_data_check",
            actionButton("precalc_te_sete_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
                div(id = "precalc_te_sete_outcome_wrap", textInput("precalc_te_sete_outcome", "Outcome name", value = "Outcome")),
                selectInput("precalc_te_sete_outcome_direction", "If the outcome increases, is that beneficial or harmful?", choices = c("Harmful" = "harmful", "Beneficial" = "beneficial"), selected = "harmful"),
                selectInput("precalc_te_sete_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
                selectInput("precalc_te_sete_sm", "Summary measure (sm)", choices = c("HR", "RR", "OR", "RD", "MD", "SMD"), selected = "HR"),
                selectInput("precalc_te_sete_method_tau", "Tau method", choices = c("REML", "ML", "DL", "PM", "SJ", "HE", "HS", "EB"), selected = "REML"),
                selectInput("precalc_te_sete_method_i2", "I-squared method", choices = c("Q (Higgins and Thompson)" = "Q", "From tau-squared" = "tau2"), selected = "Q"),
                selectInput("precalc_te_sete_prediction", "Prediction interval", choices = c("Yes" = "yes", "No" = "no"), selected = "no"),
                selectInput("precalc_te_sete_method_random_ci", "Random CI method", choices = c("classic", "HK", "KR"), selected = "classic"),
                selectInput("precalc_te_sete_method_predict", "Prediction method", choices = c("V", "HTS"), selected = "V"),
                textInput("precalc_te_sete_label_e", "Experimental group name", value = "Experimental"),
                textInput("precalc_te_sete_label_c", "Control group name", value = "Control"),
                selectInput(
                "precalc_te_sete_col_square",
                "Forest square color",
                choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Dodger blue" = "dodgerblue3", "Navy" = "navy", "Black" = "black", "Gray" = "gray40", "Red" = "red3", "Dark green" = "darkgreen"),
                selected = "red3",
                selectize = FALSE
              ),
              selectInput(
                "precalc_te_sete_col_square_lines",
                "Forest square line color",
                choices = c("Black" = "black", "Dark blue" = "darkblue", "Gray" = "gray40", "White" = "white", "Navy" = "navy"),
                selected = "black",
                selectize = FALSE
              ),
              selectInput(
                "precalc_te_sete_forest_sort",
                "Sort studies by effect size",
                choices = c("No" = "no", "Yes" = "yes"),
                selected = "no",
                selectize = FALSE
              ),
              forest_column_selector("precalc_te_sete_forest_cols")
            ),
              tags$p(class = "help-text", "Important: in this workflow, if you select HR, RR or OR, enter TE as log(HR)/log(RR)/log(OR) and enter seTE on the same log scale. For MD, SMD and RD, enter the effect exactly as reported."),
            tags$div(class = "status-message prominent-status", textOutput("precalc_te_sete_run_status")),
            div(
              class = "step-actions",
              actionButton("precalc_te_sete_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_precalc_te_sete", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("precalc_te_sete_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Forest plot"),
              tags$p(class = "help-text", "Open the forest plot, inspect the model summary, or download the figure."),
              result_action_group("precalc_te_sete_forest", "download_precalc_te_sete_forest", "preview_precalc_te_sete_forest", "summary_precalc_te_sete_main", "prepare_precalc_te_sete_forest")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Leave-one-out analysis"),
              tags$p(class = "help-text", "Preview the sensitivity analysis or download the leave-one-out forest plot."),
              result_action_group("precalc_te_sete_loo", "download_precalc_te_sete_loo", "preview_precalc_te_sete_loo", NULL, "prepare_precalc_te_sete_loo")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Funnel plot"),
              tags$p(class = "help-text", "Preview the funnel plot or download it for reporting."),
              result_action_group("precalc_te_sete_funnel", "download_precalc_te_sete_funnel", "preview_precalc_te_sete_funnel", NULL, "prepare_precalc_te_sete_funnel")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Small-study effects"),
              tags$p(class = "help-text", "Open the Egger test result and interpretation note."),
              div(class = "result-button-row",
                  actionButton("summary_precalc_te_sete_bias", "Summary", class = "result-action"))
            ),
            div(
              id = "precalc_te_sete_subgroup_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Subgroup analysis"),
              tags$p(id = "precalc_te_sete_subgroup_help_single", class = "help-text", "Pick a column to draw its subgroup forest plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "precalc_te_sete_subgroup_help_batch", class = "help-text", style = "display:none;", "One subgroup forest plot per outcome, all using the column above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("precalc_te_sete_subgroup_pick", "Subgroup column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("precalc_te_sete_subgroup", "download_precalc_te_sete_subgroup", "preview_precalc_te_sete_subgroup", "summary_precalc_te_sete_subgroup", "prepare_precalc_te_sete_subgroup")
            ),
            div(
              id = "precalc_te_sete_metareg_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Meta-regression"),
              tags$p(id = "precalc_te_sete_metareg_help_single", class = "help-text", "Pick a numeric moderator to draw its bubble plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "precalc_te_sete_metareg_help_batch", class = "help-text", style = "display:none;", "One bubble plot per outcome, all using the moderator above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("precalc_te_sete_metareg_pick", "Moderator column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("precalc_te_sete_metareg", "download_precalc_te_sete_metareg", "preview_precalc_te_sete_metareg", "summary_precalc_te_sete_metareg", "prepare_precalc_te_sete_metareg")
            ),
            summary_table_cards("precalc_te_sete")
          ),
          div(
            class = "step-actions",
            actionButton("precalc_te_sete_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

# Workflow screen: effect size with a standard error and a confidence interval.
precalc_te_sete_ci_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_precalculated_sete_ci", "Back to Pre-calculated", class = "back-button"),
      actionButton("back_home_from_precalc_te_sete_ci", "Back to home", class = "back-button secondary-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Pre-calculated effect size data"),
      tags$h2("TE, seTE and 95% CI workflow"),
      tags$p("Use this workflow when treatment effect, standard error and confidence interval are already available.")
    ),
    div(class = "step-indicator", textOutput("precalc_te_sete_ci_step_label")),
    tabsetPanel(
      id = "precalc_te_sete_ci_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Paste directly from Google Sheets, upload an Excel/CSV file, or load the example dataset with 12 studies."),
            tags$p(class = "help-text required-columns", HTML("<strong>Required columns (use these exact Excel column names):</strong><br><strong>study</strong>: the study label, for example first author and year.<br><strong>TE</strong>: the effect estimate exactly as reported in the paper, for example HR, RR, OR, MD or SMD.<br><strong>seTE</strong>: the standard error of TE.<br><strong>lower</strong>: the lower 95% confidence interval limit for TE.<br><strong>upper</strong>: the upper 95% confidence interval limit for TE.<br>Optional columns such as <strong>n.e</strong>, <strong>n.c</strong>, <strong>Region</strong> or <strong>MeanAge</strong> can be used later for forest display, subgroup analysis or meta-regression.")),
            actionButton("load_precalc_te_sete_ci_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_precalc_te_sete_ci_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "precalc_te_sete_ci_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "precalc_te_sete_ci_paste",
              "Paste data from Google Sheets",
              placeholder = "study\tTE\tseTE\tlower\tupper\tn.e\tn.c\tRegion\nStudy 1\t0.76\t0.11\t0.61\t0.95\t120\t118\tNorth",
              rows = 10
            ),
            actionButton("use_precalc_te_sete_ci_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("precalc_te_sete_ci_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("More than one outcome"),
            tags$p(class = "help-text microcopy", "Upload one Excel file. Each sheet = one outcome."),
            tags$p(class = "help-text microcopy", "Name each sheet with the outcome name. MetaVidence uses sheet names in plots and exported files."),
            tags$p(class = "help-text microcopy", "Column names must match exactly. Batch mode exports all outcomes together."),
            tags$div(
              class = "em-upload-row",
              fileInput(
                "precalc_te_sete_ci_multi_file",
                "Import multi-outcome workbook (.xlsx)",
                accept = c(".xlsx", ".xls")
              ),
              downloadButton("download_precalc_te_sete_ci_batch_template", "Download XLSX template", class = "secondary-button")
            ),
            tags$div(class = "status-message", textOutput("precalc_te_sete_ci_multi_status"))
          ),
          data_check_card(
            "precalc_te_sete_ci_data_check",
            actionButton("precalc_te_sete_ci_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
                div(id = "precalc_te_sete_ci_outcome_wrap", textInput("precalc_te_sete_ci_outcome", "Outcome name", value = "Outcome")),
                selectInput("precalc_te_sete_ci_outcome_direction", "If the outcome increases, is that beneficial or harmful?", choices = c("Harmful" = "harmful", "Beneficial" = "beneficial"), selected = "harmful"),
                selectInput("precalc_te_sete_ci_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
                selectInput("precalc_te_sete_ci_sm", "Summary measure (sm)", choices = c("HR", "RR", "OR", "RD", "MD", "SMD"), selected = "HR"),
                selectInput("precalc_te_sete_ci_method_tau", "Tau method", choices = c("REML", "ML", "DL", "PM", "SJ", "HE", "HS", "EB"), selected = "REML"),
                selectInput("precalc_te_sete_ci_method_i2", "I-squared method", choices = c("Q (Higgins and Thompson)" = "Q", "From tau-squared" = "tau2"), selected = "Q"),
                selectInput("precalc_te_sete_ci_prediction", "Prediction interval", choices = c("Yes" = "yes", "No" = "no"), selected = "no"),
                selectInput("precalc_te_sete_ci_method_random_ci", "Random CI method", choices = c("classic", "HK", "KR"), selected = "classic"),
                selectInput("precalc_te_sete_ci_method_predict", "Prediction method", choices = c("V", "HTS"), selected = "V"),
                textInput("precalc_te_sete_ci_label_e", "Experimental group name", value = "Experimental"),
                textInput("precalc_te_sete_ci_label_c", "Control group name", value = "Control"),
                selectInput(
                "precalc_te_sete_ci_col_square",
                "Forest square color",
                choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Dodger blue" = "dodgerblue3", "Navy" = "navy", "Black" = "black", "Gray" = "gray40", "Red" = "red3", "Dark green" = "darkgreen"),
                selected = "red3",
                selectize = FALSE
              ),
              selectInput(
                "precalc_te_sete_ci_col_square_lines",
                "Forest square line color",
                choices = c("Black" = "black", "Dark blue" = "darkblue", "Gray" = "gray40", "White" = "white", "Navy" = "navy"),
                selected = "black",
                selectize = FALSE
              ),
              selectInput(
                "precalc_te_sete_ci_forest_sort",
                "Sort studies by effect size",
                choices = c("No" = "no", "Yes" = "yes"),
                selected = "no",
                selectize = FALSE
              ),
              forest_column_selector("precalc_te_sete_ci_forest_cols")
            ),
            tags$p(class = "help-text", "For HR, RR and OR, enter TE and confidence limits on the original ratio scale. The seTE should match the analysis scale used in the lesson, typically the log scale for ratio measures."),
            tags$div(class = "status-message prominent-status", textOutput("precalc_te_sete_ci_run_status")),
            div(
              class = "step-actions",
              actionButton("precalc_te_sete_ci_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_precalc_te_sete_ci", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("precalc_te_sete_ci_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(class = "analysis-card result-control-card",
                tags$h3("Forest plot"),
                tags$p(class = "help-text", "Open the forest plot, inspect the model summary, or download the figure."),
                result_action_group("precalc_te_sete_ci_forest", "download_precalc_te_sete_ci_forest", "preview_precalc_te_sete_ci_forest", "summary_precalc_te_sete_ci_main", "prepare_precalc_te_sete_ci_forest")),
            div(class = "analysis-card result-control-card",
                tags$h3("Leave-one-out analysis"),
                tags$p(class = "help-text", "Preview the sensitivity analysis or download the leave-one-out forest plot."),
                result_action_group("precalc_te_sete_ci_loo", "download_precalc_te_sete_ci_loo", "preview_precalc_te_sete_ci_loo", NULL, "prepare_precalc_te_sete_ci_loo")),
            div(class = "analysis-card result-control-card",
                tags$h3("Funnel plot"),
                tags$p(class = "help-text", "Preview the funnel plot or download it for reporting."),
                result_action_group("precalc_te_sete_ci_funnel", "download_precalc_te_sete_ci_funnel", "preview_precalc_te_sete_ci_funnel", NULL, "prepare_precalc_te_sete_ci_funnel")),
            div(class = "analysis-card result-control-card",
                tags$h3("Small-study effects"),
                tags$p(class = "help-text", "Open the Egger test result and interpretation note."),
                div(class = "result-button-row",
                    actionButton("summary_precalc_te_sete_ci_bias", "Summary", class = "result-action"))),
            div(
              id = "precalc_te_sete_ci_subgroup_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Subgroup analysis"),
              tags$p(id = "precalc_te_sete_ci_subgroup_help_single", class = "help-text", "Pick a column to draw its subgroup forest plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "precalc_te_sete_ci_subgroup_help_batch", class = "help-text", style = "display:none;", "One subgroup forest plot per outcome, all using the column above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("precalc_te_sete_ci_subgroup_pick", "Subgroup column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("precalc_te_sete_ci_subgroup", "download_precalc_te_sete_ci_subgroup", "preview_precalc_te_sete_ci_subgroup", "summary_precalc_te_sete_ci_subgroup", "prepare_precalc_te_sete_ci_subgroup")
            ),
            div(
              id = "precalc_te_sete_ci_metareg_card",
              class = "analysis-card result-control-card",
              style = "display:none;",
              tags$h3("Meta-regression"),
              tags$p(id = "precalc_te_sete_ci_metareg_help_single", class = "help-text", "Pick a numeric moderator to draw its bubble plot. Switching the column redraws the plot, the summary and the download."),
              tags$p(id = "precalc_te_sete_ci_metareg_help_batch", class = "help-text", style = "display:none;", "One bubble plot per outcome, all using the moderator above. Switching it locks the download again: click 1. Run analysis to rebuild the files."),
              div(class = "card-picker", selectInput("precalc_te_sete_ci_metareg_pick", "Moderator column", choices = c("None" = ""), selectize = FALSE)),
              result_action_group("precalc_te_sete_ci_metareg", "download_precalc_te_sete_ci_metareg", "preview_precalc_te_sete_ci_metareg", "summary_precalc_te_sete_ci_metareg", "prepare_precalc_te_sete_ci_metareg")
            ),
            summary_table_cards("precalc_te_sete_ci")
          ),
          div(
            class = "step-actions",
            actionButton("precalc_te_sete_ci_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}


# ============================================================================
# WRITING PLOTS TO DISK
#
# The single path from a result object to a file the user downloads. For one
# outcome it writes one image; for a batch it writes one image per outcome and
# zips them, naming each file after its sheet.
# ============================================================================

# Produces the file the user is about to receive. One outcome gives one image;
# a batch gives a ZIP holding one image per outcome, each named after its
# sheet. require_component is how a card refuses cleanly when the plot it
# exports needs a subgroup or a moderator that was never selected.
generate_export_artifact <- function(result, plot_label, file_format, width, height, plot_function, require_component = NULL) {
  export_settings <- normalize_export_settings(file_format, width, height)
  file_format <- export_settings$file_format
  width <- export_settings$width
  height <- export_settings$height
  output_file <- tempfile(
    pattern = paste0("easy_meta_", gsub("[^A-Za-z0-9]+", "_", plot_label), "_"),
    fileext = if (is_batch_result(result)) ".zip" else paste0(".", file_format)
  )

  if (!is_batch_result(result)) {
    if (!is.null(require_component) && is.null(result[[require_component]])) {
      stop(paste("This plot is unavailable because", require_component, "was not selected."))
    }
    if (identical(file_format, "pdf")) {
      grDevices::pdf(output_file, width = width, height = height)
      on.exit(grDevices::dev.off(), add = TRUE)
      plot_function(result)
    } else {
      grDevices::png(output_file, width = width, height = height, units = "in", res = 600)
      on.exit(grDevices::dev.off(), add = TRUE)
      plot_function(result)
    }
    return(output_file)
  }

  export_dir <- file.path(tempdir(), paste0("easy_meta_batch_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  generated_files <- character(0)

  for (outcome_name in names(result$outcomes)) {
    outcome_result <- result$outcomes[[outcome_name]]
    if (!is.null(require_component) && is.null(outcome_result[[require_component]])) next
    export_path <- file.path(
      export_dir,
      paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome_name), ".", file_format)
    )
    if (identical(file_format, "pdf")) {
      grDevices::pdf(export_path, width = width, height = height)
      plot_function(outcome_result)
      grDevices::dev.off()
    } else {
      grDevices::png(export_path, width = width, height = height, units = "in", res = 600)
      plot_function(outcome_result)
      grDevices::dev.off()
    }
    generated_files <- c(generated_files, export_path)
  }

  if (length(generated_files) == 0) {
    stop("No plots were available to export for the selected outcomes.")
  }

  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(
      zipfile = output_file,
      files = generated_files,
      recurse = FALSE,
      compression_level = 9,
      root = export_dir
    )
  } else {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(export_dir)
    zip_status <- utils::zip(
      zipfile = output_file,
      files = basename(generated_files),
      flags = "-j"
    )
    if (!identical(zip_status, 0L)) {
      stop("Unable to create the batch ZIP file. Please install the 'zip' package.")
    }
  }
  if (!file.exists(output_file) || is.na(file.info(output_file)$size) || file.info(output_file)$size == 0) {
    stop("The batch ZIP file could not be created.")
  }
  output_file
}

# The single-outcome download: generate into a temporary file, then copy it to
# wherever Shiny asked for it.
write_plot_export <- function(file, result, plot_label, file_format, width, height, plot_function, require_component = NULL) {
  generated_file <- generate_export_artifact(result, plot_label, file_format, width, height, plot_function, require_component)
  ok <- file.copy(generated_file, file, overwrite = TRUE)
  if (!ok) {
    stop("Unable to copy the prepared export file.")
  }
}


# ============================================================================
# MODEL OPTIONS AND EFFECT FORMATTING
#
# Translates what the user picked in step 2 into the arguments meta and metafor
# expect, and turns a fitted effect back into the text shown on screen. Ratio
# measures are exponentiated here and nowhere else, which is why
# precalc_sm_is_ratio() is consulted before every display.
# ============================================================================

# Turns the Analysis model choice into the random and common pair meta expects.
# Both is not a third model: it fits the two and shows them side by side.
resolve_meta_model_flags <- function(model_choice) {
  list(
    random = model_choice %in% c("random", "both"),
    common = model_choice %in% c("fixed", "both")
  )
}

# Which pooled estimate the leave-one-out plot recomputes. metainf takes one
# pool at a time, so a Both run follows the random-effects line.
resolve_metainf_pool <- function(model_choice) {
  if (model_choice == "fixed") "common" else "random"
}

# metafor spells the fixed-effect case FE instead of taking a tau estimator, so
# the tau method is only passed through for a random-effects run.
resolve_rma_method <- function(model_choice, tau_method) {
  if (model_choice == "fixed") "FE" else tau_method
}

# Whether the summary measure lives on a ratio scale. It decides two separate
# things: whether the input has to be positive, and whether the pooled estimate
# is exponentiated before it is shown.
precalc_sm_is_ratio <- function(sm) {
  sm %in% c("HR", "RR", "OR")
}

# Pooled estimates are stored on the model scale (log for RR/OR/HR, logit for
# PLOGIT, double arcsine for PFT...). Let the meta package do the inverse
# transformation, since it already handles every summary measure the app
# offers, including PFT, which needs the sample sizes.
## -------------------------------------------------------------------------
## Formatting and numeric summaries
##
## These take the model output to the scale the user reads: percentage, ratio,
## difference. The inverse transformation itself is done by the meta package, not
## here, so the numbers on screen and in the exported tables cannot drift from
## the ones in the forest plot.
## -------------------------------------------------------------------------

backtransform_effect <- function(x, sm, n = NULL) {
  converter <- tryCatch(meta::backtransf, error = function(e) NULL)
  if (!is.null(converter)) {
    converted <- tryCatch(converter(x, sm, n = n), error = function(e) {
      tryCatch(converter(x, sm), error = function(e2) NULL)
    })
    if (!is.null(converted) && length(converted) == length(x)) return(as.numeric(converted))
  }
  # Fallback if a future meta release renames the internal helper.
  if (precalc_sm_is_ratio(sm) || sm %in% c("PLN", "MLN")) return(exp(x))
  if (identical(sm, "PLOGIT")) return(stats::plogis(x))
  as.numeric(x)
}

# The p for interaction as it appears in the table. Below 0.001 it is written
# as <0.001 rather than rounded to a zero that would read as certainty.
format_subgroup_p <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  formatC(p, format = "f", digits = 3)
}

# Not every analyze_* function stores the summary measure in its result, but the
# meta object always carries it.
# The values in the table are already back-transformed, so labelling the column
# "PLOGIT" would be misleading: it is a proportion by then.
effect_column_label <- function(sm) {
  if (sm %in% c("PLOGIT", "PFT", "PAS", "PLN", "PRAW")) return("Proportion")
  if (sm %in% c("MRAW", "MLN")) return("Mean")
  if (!nzchar(sm)) return("Effect")
  sm
}

# The summary measure of a result. Not every analyze_* stores it at the top
# level, but the meta object always carries it, so that is the fallback.
result_summary_measure <- function(result) {
  sm <- result$sm
  if (is.null(sm) || !nzchar(sm)) sm <- result$meta$sm
  if (is.null(sm)) "" else as.character(sm)[1]
}

# ============================================================================
# SUBGROUP AND META-REGRESSION TABLES
#
# The publication-ready tables. Each column the user selects gets its own fit, so
# one table can cover several variables at once, and none of them has to be the
# column currently driving the forest plot. In batch mode the same rows are built
# per outcome and stacked, with the outcome named on its first row.
# ============================================================================

# Reads the per-subgroup results the meta package already computed. No new
# statistics are produced here: update() re-runs the very same meta object with
# a different subgroup variable, so the pooled estimates match the forest plot
# exactly.
subgroup_summary_rows <- function(result, column) {
  meta_object <- result$meta
  data <- result$data
  if (is.null(meta_object) || is.null(data) || !column %in% names(data)) return(NULL)

  values <- data[[column]]
  if (all(is.na(values)) || length(unique(values[!is.na(values)])) < 2) return(NULL)

  updated <- tryCatch(update(meta_object, subgroup = values), error = function(e) NULL)
  if (is.null(updated) || is.null(updated$subgroup.levels)) return(NULL)

  use_common <- identical(result$model_choice, "fixed")
  effect <- if (use_common) updated$TE.common.w else updated$TE.random.w
  lower <- if (use_common) updated$lower.common.w else updated$lower.random.w
  upper <- if (use_common) updated$upper.common.w else updated$upper.random.w
  p_interaction <- if (use_common) updated$pval.Q.b.common else updated$pval.Q.b.random

  patients <- if (!is.null(updated$n.e.w) && !is.null(updated$n.c.w)) {
    updated$n.e.w + updated$n.c.w
  } else if (!is.null(updated$n.w)) {
    updated$n.w
  } else {
    rep(NA_real_, length(updated$subgroup.levels))
  }

  sm <- result_summary_measure(result)
  effect <- backtransform_effect(effect, sm, n = updated$n.w)
  lower <- backtransform_effect(lower, sm, n = updated$n.w)
  upper <- backtransform_effect(upper, sm, n = updated$n.w)

  # Proportions and other values below 1 lose too much information at 2 decimals
  effect_digits <- if (all(abs(effect) < 1, na.rm = TRUE)) 3 else 2

  data.frame(
    Variable = c(column, rep("", length(updated$subgroup.levels) - 1)),
    Subgroup = as.character(updated$subgroup.levels),
    Studies = as.integer(updated$k.w),
    Patients = ifelse(is.na(patients), "", formatC(patients, format = "d", big.mark = "")),
    Effect = ifelse(
      is.na(effect),
      "",
      sprintf(
        "%s (%s to %s)",
        formatC(effect, format = "f", digits = effect_digits),
        formatC(lower, format = "f", digits = effect_digits),
        formatC(upper, format = "f", digits = effect_digits)
      )
    ),
    # I-squared is undefined for a subgroup holding a single study
    I2 = ifelse(
      is.na(updated$I2.w),
      "—",
      paste0(formatC(updated$I2.w * 100, format = "f", digits = 1), "%")
    ),
    P = c(format_subgroup_p(p_interaction), rep("", length(updated$subgroup.levels) - 1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Meta-regression rows for one covariate. Effect sizes come straight from the
# meta object (TE and seTE are numerically identical to metafor::escalc), so the
# model agrees with the forest plot. Categorical covariates are supported: rma
# dummy-codes them and every level is reported against the reference, instead of
# only the first one.
metareg_summary_rows <- function(result, column) {
  meta_object <- result$meta
  data <- result$data
  if (is.null(meta_object) || is.null(data) || !column %in% names(data)) return(NULL)

  covariate <- data[[column]]
  # Numeric moderators only, matching the meta-regression plot: a categorical
  # column has no bubble plot, and letting the table accept what the plot refuses
  # would only produce results with no matching figure. Comparing categories is
  # what subgroup analysis is for, and it is the same test.
  if (!is.numeric(covariate)) return(NULL)
  usable <- !is.na(covariate)
  if (sum(usable) < 3 || length(unique(covariate[usable])) < 2) return(NULL)

  model_data <- data.frame(
    yi = as.numeric(meta_object$TE),
    vi = as.numeric(meta_object$seTE)^2,
    cov = covariate
  )
  rma_method <- resolve_rma_method(result$model_choice, meta_object$method.tau %||% "REML")
  fit <- tryCatch(
    metafor::rma(yi = yi, vi = vi, mods = ~ cov, data = model_data, method = rma_method),
    error = function(e) NULL
  )
  if (is.null(fit) || nrow(fit$beta) < 2) return(NULL)

  keep <- seq(2, nrow(fit$beta))
  beta <- as.numeric(fit$beta)[keep]
  beta_lower <- as.numeric(fit$ci.lb)[keep]
  beta_upper <- as.numeric(fit$ci.ub)[keep]
  p_values <- as.numeric(fit$pval)[keep]

  # For ratio measures the coefficient lives on the log scale: keep it as the
  # reproducible model output and add the exponentiated value next to it, which
  # is the one readers interpret.
  is_ratio <- precalc_sm_is_ratio(result_summary_measure(result))
  ratio_text <- if (is_ratio) {
    sprintf(
      "%s (%s to %s)",
      formatC(exp(beta), format = "f", digits = 2),
      formatC(exp(beta_lower), format = "f", digits = 2),
      formatC(exp(beta_upper), format = "f", digits = 2)
    )
  } else {
    rep("", length(keep))
  }

  comparison <- rep("per 1-unit increase", length(keep))

  r_squared <- if (is.null(fit$R2) || is.na(fit$R2)) "" else paste0(formatC(fit$R2, format = "f", digits = 1), "%")

  data.frame(
    Variable = c(column, rep("", length(keep) - 1)),
    Comparison = comparison,
    Studies = c(as.integer(fit$k), rep(NA_integer_, length(keep) - 1)),
    Beta = sprintf(
      "%s (%s to %s)",
      formatC(beta, format = "f", digits = 3),
      formatC(beta_lower, format = "f", digits = 3),
      formatC(beta_upper, format = "f", digits = 3)
    ),
    Estimate = ratio_text,
    # With a single coefficient the Wald test and the omnibus test are the same
    # test, so one p value column is enough.
    P = vapply(p_values, format_subgroup_p, character(1)),
    R2 = c(r_squared, rep("", length(keep) - 1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# The same assembly for moderators. On a ratio measure the coefficient is also
# shown exponentiated; on any other measure that column is dropped, since it
# would only repeat the coefficient.
build_metareg_summary_table <- function(result, columns) {
  columns <- unique(columns[nzchar(columns)])
  if (length(columns) == 0) stop("Select at least one column to build the table.")
  if (!requireNamespace("metafor", quietly = TRUE)) {
    stop("The metafor package is required. Install it with install.packages('metafor').")
  }

  build_for <- function(res, outcome_label = NULL) {
    blocks <- Filter(Negate(is.null), lapply(columns, function(col) metareg_summary_rows(res, col)))
    if (length(blocks) == 0) return(NULL)
    rows <- do.call(rbind, blocks)
    if (!is.null(outcome_label)) {
      rows <- cbind(Outcome = c(outcome_label, rep("", nrow(rows) - 1)), rows, stringsAsFactors = FALSE)
    }
    rows
  }

  table_data <- if (is_batch_result(result)) {
    parts <- Filter(Negate(is.null), lapply(names(result$outcomes), function(name) build_for(result$outcomes[[name]], name)))
    if (length(parts) == 0) NULL else do.call(rbind, parts)
  } else {
    build_for(result)
  }

  if (is.null(table_data)) {
    stop("None of the selected columns could be used as a moderator. A moderator must be numeric, with at least two distinct values and three studies.")
  }

  sm_label <- result_summary_measure(if (is_batch_result(result)) result$outcomes[[1]] else result)
  if (precalc_sm_is_ratio(sm_label)) {
    names(table_data)[names(table_data) == "Estimate"] <- paste0(effect_column_label(sm_label), " ratio (95% CI)")
  } else {
    # Without a ratio measure the coefficient is already on the reported scale,
    # so the exponentiated column would just duplicate it.
    table_data$Estimate <- NULL
  }
  names(table_data)[names(table_data) == "Beta"] <- "Beta (95% CI)"
  names(table_data)[names(table_data) == "P"] <- "p"
  names(table_data)[names(table_data) == "R2"] <- "R-squared"
  table_data
}

# Assembles the table from the per-column rows: adds the Outcome column in
# batch mode, drops the patient count when the module has no sample sizes, and
# names the effect column after the measure actually in use.
build_subgroup_summary_table <- function(result, columns) {
  columns <- unique(columns[nzchar(columns)])
  if (length(columns) == 0) stop("Select at least one column to build the table.")

  build_for <- function(res, outcome_label = NULL) {
    blocks <- lapply(columns, function(col) subgroup_summary_rows(res, col))
    blocks <- Filter(Negate(is.null), blocks)
    if (length(blocks) == 0) return(NULL)
    rows <- do.call(rbind, blocks)
    if (!is.null(outcome_label)) {
      rows <- cbind(Outcome = c(outcome_label, rep("", nrow(rows) - 1)), rows, stringsAsFactors = FALSE)
    }
    rows
  }

  table_data <- if (is_batch_result(result)) {
    parts <- lapply(names(result$outcomes), function(name) build_for(result$outcomes[[name]], name))
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) NULL else do.call(rbind, parts)
  } else {
    build_for(result)
  }

  if (is.null(table_data)) {
    stop("None of the selected columns could be used as a subgroup. They need at least two distinct values.")
  }

  # Pre-calculated effect sizes usually carry no sample sizes: an empty column
  # would just be noise in a manuscript table.
  if (all(!nzchar(table_data$Patients))) table_data$Patients <- NULL

  sm_label <- result_summary_measure(if (is_batch_result(result)) result$outcomes[[1]] else result)
  names(table_data)[names(table_data) == "Effect"] <- paste0(effect_column_label(sm_label), " (95% CI)")
  names(table_data)[names(table_data) == "I2"] <- "I-squared"
  names(table_data)[names(table_data) == "P"] <- "p for interaction"
  table_data
}

# The two labels printed under the forest plot. Which side reads Favors <group>
# depends on whether an increase in the outcome is good or bad, which is the
# question asked in step 2. Getting this backwards is the easiest way to read a
# forest plot the wrong way round, which is why it is asked and not guessed.
resolve_comparison_labels <- function(group_e, group_c, outcome_direction) {
  group_e <- if (is.null(group_e) || !nzchar(trimws(group_e))) "Experimental" else trimws(group_e)
  group_c <- if (is.null(group_c) || !nzchar(trimws(group_c))) "Control" else trimws(group_c)
  outcome_direction <- if (is.null(outcome_direction) || !nzchar(outcome_direction)) "harmful" else outcome_direction

  if (identical(outcome_direction, "beneficial")) {
    label_left <- paste("Favors", group_c)
    label_right <- paste("Favors", group_e)
  } else {
    label_left <- paste("Favors", group_e)
    label_right <- paste("Favors", group_c)
  }

  list(
    label_e = group_e,
    label_c = group_c,
    label_left = label_left,
    label_right = label_right
  )
}

# Copies the moderator into a fixed column name, so the metafor formula is
# written once here instead of being built from a user-supplied string.
prepare_metareg_covariate <- function(data, metareg_col) {
  if (!nzchar(metareg_col)) return(data)
  covariate <- data[[metareg_col]]
  if (!is.numeric(covariate)) {
    stop("Meta-regression requires a numeric column.")
  }
  data[["easy_meta_metareg_covariate"]] <- covariate
  data
}

# The columns to the left of a pre-calculated forest plot. Sample sizes appear
# only when the user supplied them, since they are optional in these modules.
precalc_left_columns <- function(result) {
  base_cols <- c("studlab", "TE", "seTE")
  base_labs <- c("Studies", NA, NA)

  if ("n.e" %in% names(result$data)) {
    base_cols <- c(base_cols, "n.e")
    base_labs <- c(base_labs, result$label_e)
  }
  if ("n.c" %in% names(result$data)) {
    base_cols <- c(base_cols, "n.c")
    base_labs <- c(base_labs, result$label_c)
  }

  extra_cols <- forest_extra_columns(result)
  weight_col <- if (result$model_choice == "fixed") "w.common" else "w.random"
  weight_lab <- if (result$model_choice == "fixed") "Weight" else NA

  list(
    cols = c(base_cols, extra_cols, weight_col, "effect", "ci"),
    labs = c(base_labs, extra_cols, weight_lab, NA, NA)
  )
}


# ============================================================================
# PLOTS
#
# One function per plot per module. They stay separate on purpose: each meta object
# carries different columns, labels and back-transformations, and a single
# parameterised plotter would need more branches than the copies it replaces.
# 
# Each takes the result object and returns nothing, drawing on whatever device the
# caller opened. That is what lets the same function serve both the on-screen
# preview and the exported file.
# ============================================================================

# Pre-calculated effects. The five plots below share one trait: the model was
# fitted on values the user supplied rather than computed from raw counts, so
# the labels come from the summary measure chosen in step 2 and not from the
# data. On a ratio measure everything is back-transformed before it is drawn.
plot_precalc_te_ci_forest <- function(result, col_square, col_square_lines, sort_studies = FALSE) {
  cols <- precalc_left_columns(result)
  meta::forest(
    result$meta,
    layout = "Revman5",
    label.left = result$label_left,
    label.right = result$label_right,
    leftcols = cols$cols,
    leftlabs = cols$labs,
    just = "center",
    test.overall.random = result$model_choice %in% c("random", "both"),
    test.overall.common = result$model_choice %in% c("fixed", "both"),
    colgap = "2mm",
    col.square = col_square,
    col.square.lines = col_square_lines,
    sortvar = if (sort_studies) result$meta$TE else NULL
  )
}

# Leave-one-out: refits without each study in turn, to see whether any single one is carrying the result.
plot_precalc_te_ci_loo <- function(result, col_square = "lightblue") {
  meta_loo <- meta::metainf(result$meta, pooled = resolve_metainf_pool(result$model_choice))
  meta::forest(
    meta_loo,
    col.bg = col_square,
    col.diamond = "black",
    xlab = paste(result$label_left, "   ", result$label_right),
    ff.xlab = "bold",
    rightcols = c("effect", "ci", "I2", "pval"),
    colgap.right = "4mm",
    just = "center"
  )
}

# Funnel plot for small-study effects. It is offered regardless of the number of studies, but reading it below about ten is guesswork.
plot_precalc_te_ci_funnel <- function(result, col_square = "red") {
  meta::funnel(
    result$meta,
    studlab = TRUE,
    bg = col_square,
    random = result$model_choice %in% c("random", "both"),
    common = result$model_choice %in% c("fixed", "both"),
    backtransf = FALSE
  )
}

# The forest split by the chosen column, with the test for subgroup differences in the footer.
plot_precalc_te_ci_subgroup <- function(result, col_square, col_square_lines, sort_studies = FALSE) {
  cols <- precalc_left_columns(result)
  meta::forest(
    result$subgroup,
    layout = "Revman5",
    label.left = result$label_left,
    label.right = result$label_right,
    leftcols = cols$cols,
    leftlabs = cols$labs,
    just = "center",
    test.overall.random = FALSE,
    test.overall.common = FALSE,
    colgap = "2mm",
    col.square = col_square,
    col.square.lines = col_square_lines,
    sortvar = if (sort_studies) result$subgroup$TE else NULL,
    print.subgroup.name = FALSE,
    test.effect.subgroup.random = result$model_choice %in% c("random", "both"),
    test.effect.subgroup.common = result$model_choice %in% c("fixed", "both"),
    overall = FALSE,
    overall.hetstat = FALSE
  )
}

# Bubble plot: each study is a bubble sized by its weight, against the moderator.
plot_precalc_te_ci_metareg <- function(result, col_square = "red") {
  table_df <- extract_precalc_te_ci_metareg_table(result)
  plot_metareg_with_header(table_df, function() {
    y_axis_label <- if (isTRUE(result$ratio_scale)) paste("Log", result$sm) else result$sm
    bubble_object <- meta::metareg(
      result$meta,
      stats::as.formula(paste0("~ `", result$metareg_col, "`"))
    )
    meta::bubble(
      bubble_object,
      xlab = result$metareg_col,
      ylab = y_axis_label,
      studlab = TRUE,
      bg = col_square,
      backtransf = FALSE
    )
  })
}

# The five extract_*_metareg_table() functions below share one shape: read the
# fitted metafor object, pick the row of the moderator, and return a one-row
# data frame ready to print. They stay separate because the effect column is
# labelled after the module's own summary measure, and because each returns a
# different message when there is no moderator selected yet.
#
# The moderator always sits in a column named easy_meta_metareg_covariate, put
# there by prepare_metareg_covariate(), so the row can be found by name instead
# of by position.

# Meta-regression table for pre-calculated effect sizes.
extract_precalc_te_ci_metareg_table <- function(result) {
  if (is.null(result) || is.null(result$metareg)) {
    return(data.frame(Message = if (is.null(result)) "Run the analysis first." else "Pick a moderator column to build this table."))
  }
  s <- summary(result$metareg)
  beta_values <- as.numeric(s$beta)
  rownames_beta <- rownames(s$beta)
  row_index <- if ("easy_meta_metareg_covariate" %in% rownames_beta) which(rownames_beta == "easy_meta_metareg_covariate")[1] else min(2, length(beta_values))
  estimate <- beta_values[row_index]
  lower_ci <- as.numeric(s$ci.lb)[row_index]
  upper_ci <- as.numeric(s$ci.ub)[row_index]
  p_value <- as.numeric(s$pval)[row_index]
  r2_value <- suppressWarnings(as.numeric(s$R2)[1])
  if (is.na(r2_value) && !is.null(result$metareg_null$tau2) && !is.null(result$metareg$tau2)) {
    tau2_null <- as.numeric(result$metareg_null$tau2)[1]
    tau2_model <- as.numeric(result$metareg$tau2)[1]
    if (!is.na(tau2_null) && !is.na(tau2_model) && tau2_null > 0) {
      r2_value <- max(0, (tau2_null - tau2_model) / tau2_null) * 100
    }
  }
  data.frame(
    Covariate = result$metareg_col,
    `Coefficient (Î²)` = sprintf("%.4f", estimate),
    `95% CI` = sprintf("%.4f to %.4f", lower_ci, upper_ci),
    `P-value` = format.pval(p_value, digits = 3, eps = 0.001),
    R2 = if (is.na(r2_value)) "NA" else if (r2_value <= 1) sprintf("%.1f%%", 100 * r2_value) else sprintf("%.2f%%", r2_value),
    check.names = FALSE
  )
}

## -------------------------------------------------------------------------
## Plots
##
## One function per plot per module. They receive the already analysed object and
## only draw it. None of them recomputes any statistic, which is what guarantees
## the preview on screen and the downloaded file show the same numbers.
## -------------------------------------------------------------------------

# Single-arm proportions. The pooled value is stored on the model scale, logit
# by default, so every plot here goes through the back-transformation before
# drawing, and the axis is a proportion, not a log-odds.
plot_single_prop_forest <- function(result, col_square, col_square_lines, sort_studies = FALSE) {
  extra_cols <- forest_extra_columns(result)
  weight_col <- forest_weight_column(result$meta)
  meta::forest(
    result$meta,
    layout = "Revman5",
    leftcols = c("studlab", extra_cols, "event", "n", weight_col, "effect", "ci"),
    leftlabs = c("Studies", extra_cols, NA, NA, rep(NA, length(weight_col)), "Prevalence", NA),
    colgap = "2mm",
    pscale = 100,
    pooled.events = TRUE,
    colgap.forest.left = "6mm",
    just = "center",
    col.square = col_square,
    col.square.lines = col_square_lines,
    sortvar = if (sort_studies) result$meta$TE else NULL
  )
}

# Leave-one-out for proportions.
plot_single_prop_loo <- function(result, col_square = "lightblue") {
  meta_loo <- meta::metainf(result$meta, pooled = resolve_metainf_pool(result$model_choice))
  meta::forest(
    meta_loo,
    col.bg = col_square,
    col.diamond = "black",
    pscale = 100,
    ff.xlab = "bold",
    rightcols = c("effect", "ci", "I2"),
    colgap.right = "4mm",
    just = "center"
  )
}

# Funnel plot for proportions.
plot_single_prop_funnel <- function(result, col_square = "red") {
  meta::funnel(
    result$meta,
    studlab = TRUE,
    bg = col_square,
    random = result$model_choice %in% c("random", "both"),
    common = result$model_choice %in% c("fixed", "both"),
    backtransf = FALSE
  )
}

# Subgroup forest for proportions.
plot_single_prop_subgroup <- function(result, col_square, col_square_lines, sort_studies = FALSE) {
  extra_cols <- forest_extra_columns(result)
  weight_col <- forest_weight_column(result$subgroup)
  meta::forest(
    result$subgroup,
    layout = "Revman5",
    leftcols = c("studlab", extra_cols, "event", "n", weight_col, "effect", "ci"),
    leftlabs = c("Studies", extra_cols, NA, NA, rep(NA, length(weight_col)), "Prevalence", NA),
    colgap = "2mm",
    pscale = 100,
    pooled.events = TRUE,
    colgap.forest.left = "6mm",
    just = "center",
    col.square = col_square,
    col.square.lines = col_square_lines,
    sortvar = if (sort_studies) result$subgroup$TE else NULL,
    print.subgroup.name = FALSE,
    overall = FALSE,
    overall.hetstat = FALSE,
    test.overall.random = FALSE
  )
}

# Bubble plot for proportions.
plot_single_prop_metareg <- function(result, col_square = "red") {
  table_df <- extract_single_prop_metareg_table(result)
  plot_metareg_with_header(table_df, function() {
    bubble_object <- meta::metareg(result$meta, stats::as.formula(paste0("~ `", result$metareg_col, "`")))
    meta::bubble(
      bubble_object,
      xlab = result$metareg_col,
      ylab = "Logit proportion",
      studlab = TRUE,
      bg = col_square,
      backtransf = FALSE
    )
  })
}

# Single-arm means. Same five plots, no back-transformation needed: the effect
# is already on the scale the user reads.
plot_single_mean_forest <- function(result, col_square, col_square_lines, sort_studies = FALSE) {
  extra_cols <- forest_extra_columns(result)
  meta::forest(
    result$meta,
    layout = "Revman5",
    leftcols = c("studlab", extra_cols, "mean", "sd", "n", "w.random", "effect", "ci"),
    leftlabs = c("Studies", extra_cols, NA, NA, NA, NA, "Mean", NA),
    colgap = "2mm",
    digits = 2,
    digits.sd = 2,
    colgap.forest.left = "6mm",
    just = "center",
    col.square = col_square,
    col.square.lines = col_square_lines,
    sortvar = if (sort_studies) result$meta$TE else NULL
  )
}

# Leave-one-out for single-arm means.
plot_single_mean_loo <- function(result, col_square = "lightblue") {
  meta_loo <- meta::metainf(result$meta, pooled = resolve_metainf_pool(result$model_choice))
  meta::forest(
    meta_loo,
    col.bg = col_square,
    col.diamond = "black",
    rightcols = c("effect", "ci", "I2"),
    colgap.right = "4mm",
    just = "center"
  )
}

# Funnel plot for single-arm means.
plot_single_mean_funnel <- function(result, col_square = "red") {
  meta::funnel(
    result$meta,
    studlab = TRUE,
    bg = col_square,
    random = result$model_choice %in% c("random", "both"),
    common = result$model_choice %in% c("fixed", "both"),
    backtransf = FALSE
  )
}

# Subgroup forest for single-arm means.
plot_single_mean_subgroup <- function(result, col_square, col_square_lines, sort_studies = FALSE) {
  extra_cols <- forest_extra_columns(result)
  meta::forest(
    result$subgroup,
    layout = "Revman5",
    leftcols = c("studlab", extra_cols, "mean", "sd", "n", "w.random", "effect", "ci"),
    leftlabs = c("Studies", extra_cols, NA, NA, NA, NA, "Mean", NA),
    colgap = "2mm",
    digits = 2,
    digits.sd = 2,
    colgap.forest.left = "6mm",
    just = "center",
    col.square = col_square,
    col.square.lines = col_square_lines,
    sortvar = if (sort_studies) result$subgroup$TE else NULL,
    print.subgroup.name = FALSE,
    overall = FALSE,
    overall.hetstat = FALSE,
    test.overall.random = FALSE
  )
}

# Bubble plot for single-arm means.
plot_single_mean_metareg <- function(result, col_square = "red") {
  table_df <- extract_single_mean_metareg_table(result)
  plot_metareg_with_header(table_df, function() {
    y_axis_label <- if (identical(result$sm, "MLN")) "Log mean" else "Mean"
    bubble_object <- meta::metareg(result$meta, stats::as.formula(paste0("~ `", result$metareg_col, "`")))
    meta::bubble(
      bubble_object,
      xlab = result$metareg_col,
      ylab = y_axis_label,
      studlab = TRUE,
      bg = col_square,
      backtransf = FALSE
    )
  })
}


# ============================================================================
# DIAGNOSTIC TEST ACCURACY
#
# The module that does not use meta for its statistics. Sensitivity and
# specificity are correlated through the positivity threshold, so they are
# fitted jointly by a bivariate binomial GLMM, the model the Cochrane DTA
# Handbook v2.0 specifies in chapter 10. meta is kept here only to draw the
# per-study rows of the forest plots; every summary the user sees comes from
# the GLMM. A subgroup and a test comparison are the same thing to that model,
# which is why both go through dta_bivariate_groups().
# ============================================================================

# The logit used throughout this module, with the input clipped just inside 0
# and 1. A study with no false positives would otherwise give an infinite
# value, and one perfect study would take the whole SROC curve with it.
diagnostic_logit <- function(x) {
  stats::qlogis(pmin(pmax(x, 1e-6), 1 - 1e-6))
}

# The numbers every diagnostic card shows, read straight off the bivariate fit.
# Nothing is recomputed here, so the forest footer, the summary modal and the
# summary-point plot can never disagree.
diagnostic_bivariate_lines <- function(b, label = NULL, include_ci = TRUE) {
  if (is.null(b)) return(character(0))
  prefixo <- if (!is.null(label) && nzchar(label)) paste0(label, ": ") else ""
  fmt <- function(v, lo, hi) if (isTRUE(include_ci))
    sprintf("%.1f%% (95%% CI %.1f%% to %.1f%%)", 100 * v, 100 * lo, 100 * hi) else
    sprintf("%.1f%%", 100 * v)
  linhas <- c(
    paste0(prefixo, "Summary sensitivity: ", fmt(b$sensitivity, b$sensitivity_ci[1], b$sensitivity_ci[2])),
    paste0(prefixo, "Summary specificity: ", fmt(b$specificity, b$specificity_ci[1], b$specificity_ci[2])),
    paste0(prefixo, "Diagnostic odds ratio: ",
           sprintf("%.2f (95%% CI %.2f to %.2f)", b$dor, b$dor_ci[1], b$dor_ci[2])),
    paste0(prefixo, "Studies: ", b$k)
  )
  if (isTRUE(b$singular)) {
    linhas <- c(linhas, paste0(prefixo, "Between-study variance estimated at zero: these studies agree to ",
      "within sampling error, so the summary equals pooling every 2x2 table and the prediction ",
      "region coincides with the confidence region."))
  }
  linhas
}

# Two short lines for annotating a plot, where the modal text would not fit.
diagnostic_bivariate_compact <- function(b) {
  if (is.null(b)) return(character(0))
  c(sprintf("Sensitivity %.1f%% (%.1f-%.1f)", 100 * b$sensitivity,
            100 * b$sensitivity_ci[1], 100 * b$sensitivity_ci[2]),
    sprintf("Specificity %.1f%% (%.1f-%.1f)", 100 * b$specificity,
            100 * b$specificity_ci[1], 100 * b$specificity_ci[2]))
}

## ---- subgroup analysis in the diagnostic module -------------------------
## The outcome here is bivariate (sensitivity and specificity, correlated), so
## "subgroup" needs three pieces: the two forest plots with subgroup=, one
## bivariate fit per level, and a joint test. The comparative module already did
## all of that, but wired to the "test" column; here the same machinery accepts
## any categorical column, with k levels and UNPAIRED studies (each study belongs
## to exactly one group).

DIAGNOSTIC_MIN_STUDIES_PER_LEVEL <- 4

# The bivariate model estimates five parameters. Cochrane DTA reviews pool only
# when at least four studies are available. The underlying packages warn too, but
# the warning never reached the user.
diagnostic_few_studies_warning <- function(k) {
  if (is.null(k) || is.na(k) || k >= DIAGNOSTIC_MIN_STUDIES_PER_LEVEL) return(NULL)
  paste0("Only ", k, " ", if (k == 1) "study" else "studies",
       " were included. The bivariate model estimates 5 parameters, so summary ",
         "estimates from fewer than ", DIAGNOSTIC_MIN_STUDIES_PER_LEVEL,
         " studies are unstable. Treat them as descriptive.")
}

## -------------------------------------------------------------------------
## The bivariate binomial GLMM, as the Cochrane DTA Handbook v2.0 specifies
##
## Section 10.2.3 rules out mada and mvmeta for Cochrane reviews: they fit the
## bivariate model by normal approximation instead of a binomial likelihood,
## which biases results when arm sizes are small and forces an ad hoc
## continuity correction on zero cells. lme4::glmer is what it recommends, and
## the code below follows Appendix 5 of the supplementary material.
##
## Validated against the handbook's own published estimates for the anti-CCP
## example: see design/check_dta_bivariate.R.
## -------------------------------------------------------------------------

# One row per study and per arm: the 2x2 table becomes two binomial records,
# one for the diseased and one for the non-diseased. sens and spec are the
# dummy variables the model formula switches on.
dta_long_format <- function(data, study = NULL) {
  d <- data
  d$n1 <- d$TP + d$FN
  d$n0 <- d$FP + d$TN
  d$true1 <- d$TP
  d$true0 <- d$TN
  d$dta_row <- seq_len(nrow(d))
  d$dta_study <- if (is.null(study)) as.character(d$dta_row) else as.character(study)
  long <- stats::reshape(
    d, direction = "long", idvar = "dta_row",
    varying = list(c("n1", "n0"), c("true1", "true0")),
    timevar = "sens", times = c(1, 0), v.names = c("n", "true"))
  long <- long[order(long$dta_row), ]
  long$spec <- 1 - long$sens
  long
}

DTA_GLMER_CONTROL <- function() {
  lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5),
                     check.conv.singular = "ignore")
}

# Summary point for one test. Returns the estimates on both scales plus the two
# covariance matrices the ellipses are drawn from: vcov for the confidence
# region, vcov + psi for the prediction region.
dta_bivariate <- function(data, study = NULL) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("The lme4 package is required for the bivariate model. Install it with install.packages('lme4').")
  }
  long <- dta_long_format(data, study)
  fit <- lme4::glmer(
    cbind(true, n - true) ~ 0 + sens + spec + (0 + sens + spec | dta_study),
    data = long, family = stats::binomial, nAGQ = 1, control = DTA_GLMER_CONTROL())
  dta_summary_from_fit(fit, c("sens", "spec"), nrow(data))
}

# Reads one pair of coefficients out of a fitted model and turns it into the
# numbers the cards display. Shared by the single-test fit and by each level of
# a covariate model, so a subgroup point and a main point are produced the same
# way and cannot drift apart.
dta_summary_from_fit <- function(fit, terms, k, psi_name = "dta_study") {
  co <- summary(fit)$coefficients
  if (!all(terms %in% rownames(co))) return(NULL)
  lsens <- co[terms[1], 1]; se_sens <- co[terms[1], 2]
  lspec <- co[terms[2], 1]; se_spec <- co[terms[2], 2]
  V <- as.matrix(stats::vcov(fit))[terms, terms, drop = FALSE]
  psi <- tryCatch(as.matrix(lme4::VarCorr(fit)[[psi_name]]), error = function(e) NULL)
  z <- stats::qnorm(0.975)
  # log(DOR) = logit(sens) + logit(spec), so its variance is the sum of the two
  # variances plus twice the covariance. No delta-method package needed.
  log_dor <- lsens + lspec
  se_log_dor <- sqrt(V[1, 1] + V[2, 2] + 2 * V[1, 2])
  # A singular fit means the between-study variance came out exactly zero: the
  # studies agree to within sampling error. The estimates stay valid, the model just
  # collapses to pooling every 2x2 table. But the prediction region then collapses
  # onto the confidence region, and a reader who is not told reads that overlap as
  # precision instead of as absence of measurable heterogeneity.
  singular <- isTRUE(tryCatch(lme4::isSingular(fit), error = function(e) FALSE))
  list(
    fit = fit, k = k,
    logit_sens = lsens, logit_spec = lspec,
    sensitivity = stats::plogis(lsens),
    sensitivity_ci = stats::plogis(lsens + c(-1, 1) * z * se_sens),
    specificity = stats::plogis(lspec),
    specificity_ci = stats::plogis(lspec + c(-1, 1) * z * se_spec),
    dor = exp(log_dor),
    dor_ci = exp(log_dor + c(-1, 1) * z * se_log_dor),
    vcov = V, psi = psi, singular = singular)
}

# Points of an ellipse on the ROC axes. The confidence region uses the
# uncertainty of the mean; the prediction region adds the between-study
# variability, which is why it is so much wider when heterogeneity is high.
dta_ellipse <- function(stats_obj, kind = c("confidence", "prediction"), level = 0.95, n = 200) {
  kind <- match.arg(kind)
  V <- stats_obj$vcov
  if (identical(kind, "prediction")) {
    if (is.null(stats_obj$psi)) return(NULL)
    V <- V + stats_obj$psi
  }
  ev <- tryCatch(eigen(V, symmetric = TRUE), error = function(e) NULL)
  if (is.null(ev) || any(ev$values <= 0)) return(NULL)
  raio <- sqrt(stats::qchisq(level, df = 2))
  ang <- seq(0, 2 * pi, length.out = n)
  circulo <- rbind(cos(ang), sin(ang))
  pontos <- ev$vectors %*% diag(sqrt(ev$values)) %*% circulo * raio
  data.frame(
    specificity = stats::plogis(stats_obj$logit_spec + pontos[2, ]),
    sensitivity = stats::plogis(stats_obj$logit_sens + pontos[1, ]))
}

# A categorical covariate turns the model into a meta-regression: one summary
# point per level, plus likelihood-ratio tests of whether sensitivity and
# specificity differ across levels. The handbook makes no distinction between a
# subgroup and a test comparison here: section 10.4 says subgroups or tests
# "will simply be referred to as 'group'".
#
# The per-level points must come from this joint fit, not from separate fits of
# each level. The levels share the between-study structure, so separate fits
# give different numbers from the ones the tests below are about.
dta_bivariate_groups <- function(data, grupo, study = NULL) {
  if (!requireNamespace("lme4", quietly = TRUE) || !requireNamespace("lmtest", quietly = TRUE)) {
    return(list(available = FALSE,
                error = "The lme4 and lmtest packages are required for group comparisons."))
  }
  niveis <- levels(factor(grupo[!is.na(grupo)]))
  if (length(niveis) < 2) return(list(available = FALSE, error = "At least two levels are required."))

  manter <- !is.na(grupo)
  d <- data[manter, , drop = FALSE]
  estudo <- if (is.null(study)) NULL else study[manter]
  d$dta_group <- factor(as.character(grupo[!is.na(grupo)]), levels = niveis)
  long <- dta_long_format(d, estudo)
  rotulo <- function(i) paste0("g", i)
  for (i in seq_along(niveis)) {
    marca <- as.numeric(long$dta_group == niveis[i])
    long[[paste0("se", rotulo(i))]] <- marca * long$sens
    long[[paste0("sp", rotulo(i))]] <- marca * long$spec
  }
  se_terms <- paste0("se", rotulo(seq_along(niveis)))
  sp_terms <- paste0("sp", rotulo(seq_along(niveis)))
  ctl <- DTA_GLMER_CONTROL()
  aleatorio <- "(0 + sens + spec | dta_study)"
  ajusta <- function(fixos) tryCatch(lme4::glmer(
    stats::as.formula(paste("cbind(true, n - true) ~ 0 +", fixos, "+", aleatorio)),
    data = long, family = stats::binomial, control = ctl), error = function(e) NULL)

  modelo_a <- ajusta("sens + spec")
  modelo_b <- ajusta(paste(c(se_terms, sp_terms), collapse = " + "))
  if (is.null(modelo_a) || is.null(modelo_b)) {
    return(list(available = FALSE, error = "The group model did not converge."))
  }
  modelo_c <- ajusta(paste(c("sens", sp_terms), collapse = " + "))
  modelo_d <- ajusta(paste(c(se_terms, "spec"), collapse = " + "))

  por_nivel <- lapply(seq_along(niveis), function(i)
    dta_summary_from_fit(modelo_b, c(se_terms[i], sp_terms[i]), sum(d$dta_group == niveis[i])))
  names(por_nivel) <- niveis

  p_de <- function(m) if (is.null(m)) NA_real_ else
    tryCatch(lmtest::lrtest(modelo_b, m)[["Pr(>Chisq)"]][2], error = function(e) NA_real_)
  list(
    available = TRUE, error = NULL, levels = niveis, by_level = por_nivel,
    studies = length(unique(long$dta_study)),
    p_overall = tryCatch(lmtest::lrtest(modelo_a, modelo_b)[["Pr(>Chisq)"]][2],
                         error = function(e) NA_real_),
    p_sensitivity = p_de(modelo_c), p_specificity = p_de(modelo_d))
}

# Fits one diagnostic test. A subgroup column with fewer than two usable levels
# is dropped here rather than passed on, because the model would fail on it and the
# user would get a package error instead of an answer.
analyze_diagnostic_single <- function(data, params) {
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("The meta package is required. Install it with install.packages('meta').")
  }

  data <- validate_diagnostic_single_data(data)
  model_flags <- resolve_meta_model_flags(params$model_choice)


  # metaprop and metabin are kept only to DRAW the per-study rows of the forest
  # plots. Their pooled estimates are never displayed: handbook section 10.2.3
  # rules out pooling sensitivity and specificity separately, because it ignores
  # the correlation between them induced by threshold variation. Every summary
  # the user sees comes from the bivariate fit below.
  proporcao <- function(evento, total) {
    args <- list(
      event = evento, n = total, studlab = data$study, data = data,
      random = model_flags$random, common = model_flags$common,
      sm = params$sm, method.ci = params$method_ci, method.tau = params$method_tau
    )
    do.call(meta::metaprop, args)
  }

  sensitivity_object <- proporcao(data$TP, data$TP + data$FN)
  specificity_object <- proporcao(data$TN, data$TN + data$FP)

  dor_args <- list(
    event.e = data$TP, n.e = data$TP + data$FN,
    event.c = data$FP, n.c = data$FP + data$TN,
    studlab = data$study, data = data, sm = "OR", method = "Inverse",
    random = model_flags$random, common = model_flags$common,
    method.tau = params$method_tau, incr = 0.5, allstudies = TRUE,
    # metabin heads the count columns with "Experimental" and "Control" unless it is
    # told otherwise. Neither word means anything here: the two "arms" of a DOR are the
    # diseased and the non-diseased, and the column labels already say TP and FP.
    label.e = "", label.c = ""
  )
  dor_object <- do.call(meta::metabin, dor_args)

  bivariate_error <- NULL
  bivariate <- tryCatch(
    dta_bivariate(data, data$study),
    error = function(error) {
      bivariate_error <<- error$message
      NULL
    })


  sens <- data$TP / (data$TP + data$FN)
  spec <- data$TN / (data$TN + data$FP)
  fpr <- 1 - spec
  threshold_spearman <- suppressWarnings(stats::cor(diagnostic_logit(sens), diagnostic_logit(fpr), method = "spearman"))
  threshold_pearson <- suppressWarnings(stats::cor(diagnostic_logit(sens), diagnostic_logit(fpr), method = "pearson"))

  list(
    outcome_name = params$outcome_name,
    data = data,
    sensitivity = sensitivity_object,
    specificity = specificity_object,
    dor = dor_object,
    few_studies_warning = diagnostic_few_studies_warning(nrow(data)),
    bivariate = bivariate,
    bivariate_error = bivariate_error,
    threshold_spearman = threshold_spearman,
    threshold_pearson = threshold_pearson,
    model_choice = params$model_choice,
    sm = params$sm,
    method_ci = params$method_ci,
    method_tau = params$method_tau
  )
}

# Paired forest: sensitivity and specificity side by side, since one is meaningless without the other.
plot_diagnostic_single_forest <- function(meta_object, right_label, numerator_label, denominator_label, col_square, col_square_lines) {
  meta::forest(
    meta_object,
    # subgroup heading reads "Europe", not "subgroup = Europe"
    print.subgroup.name = FALSE,
    # Same layout as the other modules (binary, continuous, pre-calculated).
    layout = "Revman5",
    pscale = 100,
    sortvar = studlab,
    digits = 1,
    leftcols = c("studlab", "event", "n", "effect.ci"),
    leftlabs = c("Study", numerator_label, denominator_label,
                 paste0(right_label, " [95% CI]")),
    xlim = c(0, 100),
    # No pooled diamond: a univariate summary of sensitivity or specificity
    # is exactly what the handbook rules out, and the pooled DOR now comes
    # from the bivariate fit. The summary lives on the summary-point card.
    random = FALSE,
    common = FALSE,
    overall = FALSE,
    overall.hetstat = FALSE,
    pooled.events = FALSE,
    print.tau2 = FALSE,
    text.random = "Total",
    test.overall.random = FALSE,
    test.overall.common = FALSE,
    # The binary forest does not overlap because it has 8 columns on the left
    # and the plot starts much further right; here there are only 4. Pushing the
    # plot with colgap.forest.left gives the footer text room WITHOUT stretching
    # the study column, which is what calcwidth.* did (it opened a gap between
    # Study and TP). 35mm holds real labels such as "Wojciechowski 2023".
    # See design/diag_gap_robustez.R.
    colgap.forest.left = "35mm",
    colgap.forest = "3mm",
    just = "center",
    col.square = col_square,
    col.square.lines = col_square_lines,
    col.diamond = "black",
    col.diamond.lines = "black",
    diamond.random = FALSE
  )
}

# Sensitivity alone, for a report that needs the two panels separately.
plot_diagnostic_single_sensitivity <- function(result, col_square = "darkblue", col_square_lines = "black") {
  plot_diagnostic_single_forest(
    result$sensitivity,
    "Sensitivity",
    "TP",
    "TP + FN",
    col_square,
    col_square_lines
  )
}

# Specificity alone.
plot_diagnostic_single_specificity <- function(result, col_square = "darkblue", col_square_lines = "black") {
  plot_diagnostic_single_forest(
    result$specificity,
    "Specificity",
    "TN",
    "TN + FP",
    col_square,
    col_square_lines
  )
}

# Diagnostic odds ratio, which folds sensitivity and specificity into one number
# and therefore hides which of the two moved. Like the other two forests it
# carries no pooled diamond: the summary DOR comes from the bivariate fit, as
# exp(logit(sens) + logit(spec)), and lives on the summary-point card.
#
# Dropping the pooled row also frees the space the heterogeneity footer needed,
# which is what makes room for the per-study DOR column on the left.
plot_diagnostic_single_dor <- function(result, col_square = "darkblue", col_square_lines = "black") {
  meta::forest(
    result$dor,
    layout = "Revman5",
    leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"),
    leftlabs = c("Study", "TP", "TP + FN", "FP", "FP + TN", "DOR [95% CI]"),
    random = FALSE,
    common = FALSE,
    overall = FALSE,
    overall.hetstat = FALSE,
    pooled.events = FALSE,
    test.overall.random = FALSE,
    test.overall.common = FALSE,
    just = "center",
    colgap = "2mm",
    colgap.forest.left = "8mm",
    col.square = col_square,
    col.square.lines = col_square_lines
  )
}


# The bivariate model estimates a summary POINT, not a curve: a curve belongs to
# the HSROC model, which the handbook notes cannot be fitted with lme4. So this
# draws what the model does estimate.
#
# The key is placed in the right margin rather than inside the panel. A test with
# poor accuracy puts its studies in the bottom-right corner, which is exactly
# where an in-panel key used to sit, and the data would disappear underneath it.
plot_diagnostic_summary_point <- function(result, col_point = "darkblue") {
  b <- result$bivariate
  if (is.null(b)) {
    graphics::plot.new()
    graphics::text(0.5, 0.55, "Summary point could not be estimated.")
    graphics::text(0.5, 0.45, result$bivariate_error %||% "Check the data and rerun.", cex = 0.85)
    return(invisible(NULL))
  }
  d <- result$data
  sens <- d$TP / (d$TP + d$FN)
  fpr <- 1 - d$TN / (d$TN + d$FP)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(5, 4.5, 4, 14))
  graphics::plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "1 - specificity",
                 ylab = "Sensitivity", main = paste("Summary point -", result$outcome_name),
                 cex.lab = 1.05, cex.axis = 0.95)
  graphics::grid(col = "gray92", lty = 1)

  pred <- dta_ellipse(b, "prediction")
  if (!is.null(pred)) graphics::lines(1 - pred$specificity, pred$sensitivity, lty = "31", lwd = 1.6, col = "gray45")
  conf <- dta_ellipse(b, "confidence")
  if (!is.null(conf)) graphics::lines(1 - conf$specificity, conf$sensitivity, lty = 1, lwd = 2, col = col_point)
  graphics::points(fpr, sens, pch = 1, cex = 0.9, col = "#e74c3c")
  graphics::points(1 - b$specificity, b$sensitivity, pch = 23, cex = 2, lwd = 1.2,
                   col = col_point, bg = grDevices::adjustcolor("#ffd84d", alpha.f = 0.85))

  graphics::par(xpd = NA)
  x_marca <- graphics::grconvertX(0.775, "ndc", "user")
  x_texto <- graphics::grconvertX(0.805, "ndc", "user")
  y_de <- function(p) graphics::grconvertY(p, "ndc", "user")

  graphics::text(x_marca, y_de(0.87), "Bivariate summary", adj = 0, cex = 0.95, font = 2)
  graphics::points(x_marca + (x_texto - x_marca) / 2, y_de(0.80), pch = 1, cex = 0.9, col = "#e74c3c")
  graphics::text(x_texto, y_de(0.80), "Observed studies", adj = 0, cex = 0.8, col = "gray20")
  graphics::points(x_marca + (x_texto - x_marca) / 2, y_de(0.745), pch = 23, cex = 1.1,
                   col = col_point, bg = "#ffd84d")
  graphics::text(x_texto, y_de(0.745), "Summary point", adj = 0, cex = 0.8, col = "gray20")
  graphics::segments(x_marca, y_de(0.69), x_texto - 0.01, y_de(0.69), col = col_point, lwd = 2)
  graphics::text(x_texto, y_de(0.69), "95% confidence", adj = 0, cex = 0.8, col = "gray20")
  graphics::segments(x_marca, y_de(0.635), x_texto - 0.01, y_de(0.635), col = "gray45", lwd = 2, lty = "31")
  graphics::text(x_texto, y_de(0.635), "95% prediction", adj = 0, cex = 0.8, col = "gray20")

  linhas <- list(
    c("Sensitivity", sprintf("%.1f%% (%.1f-%.1f)", 100 * b$sensitivity,
                             100 * b$sensitivity_ci[1], 100 * b$sensitivity_ci[2])),
    c("Specificity", sprintf("%.1f%% (%.1f-%.1f)", 100 * b$specificity,
                             100 * b$specificity_ci[1], 100 * b$specificity_ci[2])),
    c("Diagnostic OR", sprintf("%.1f (%.1f-%.1f)", b$dor, b$dor_ci[1], b$dor_ci[2])),
    c("Studies", as.character(b$k)))
  y <- 0.545
  for (par_ in linhas) {
    graphics::text(x_marca, y_de(y), par_[1], adj = 0, cex = 0.78, font = 2, col = "gray35")
    graphics::text(x_marca, y_de(y - 0.042), par_[2], adj = 0, cex = 0.82, col = col_point)
    y <- y - 0.105
  }
  invisible(NULL)
}

# The threshold-effect card. A positive correlation between logit sensitivity
# and logit false-positive rate points at studies having used different
# positivity thresholds, which is the confounder specific to this module: a
# test can look better in a subgroup only because its threshold moved.
diagnostic_single_threshold_text <- function(result) {
  lines <- c(
    paste0("Threshold effect diagnostics for ", result$outcome_name),
    "",
    paste0("Spearman correlation between logit(sensitivity) and logit(false-positive rate): ", format_effect_value(result$threshold_spearman, 3)),
    paste0("Pearson correlation between logit(sensitivity) and logit(false-positive rate): ", format_effect_value(result$threshold_pearson, 3)),
    "",
    "Interpretation note: stronger positive correlation may suggest threshold effects across studies.",
    "",
    "Deeks test for DOR small-study effects:"
  )
  deeks <- tryCatch(
    utils::capture.output(print(meta::metabias(result$dor, method.bias = "deeks", k.min = 5))),
    error = function(error) paste("Unavailable:", error$message)
  )
  c(lines, deeks)
}

# Fits two or more tests on the same studies, so the comparison is within-study
# rather than between separate meta-analyses.
analyze_diagnostic_comparative <- function(data, params) {
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("The meta package is required. Install it with install.packages('meta').")
  }

  data <- validate_diagnostic_comparative_data(data)
  test_levels <- unique(as.character(data$test))
  test_a <- params$test_a %||% test_levels[1]
  test_b <- params$test_b %||% test_levels[min(2, length(test_levels))]
  if (!test_a %in% test_levels || !test_b %in% test_levels || identical(test_a, test_b)) {
    stop("Please select two different tests available in the test column.")
  }

  data <- data[data$test %in% c(test_a, test_b), , drop = FALSE]
  data$test <- factor(data$test, levels = c(test_a, test_b))

  # Drawing only, as in the single-test module.
  desenho <- function(evento, total) meta::metaprop(
    event = evento, n = total, studlab = data$study, data = data,
    subgroup = data$test, random = TRUE, common = FALSE,
    sm = "PLOGIT", method.ci = "CP", method.tau = "ML")

  sensitivity_object <- desenho(data$TP, data$TP + data$FN)
  specificity_object <- desenho(data$TN, data$TN + data$FP)

  # One joint model with the test as covariate, grouped by study so the two
  # arms of a paired design share their random effect. Fitting each test
  # separately would break that pairing and give per-arm points that do not
  # correspond to the tests reported beside them.
  groups <- dta_bivariate_groups(data, data$test, data$study)

  list(
    outcome_name = params$outcome_name,
    data = data,
    test_a = test_a,
    test_b = test_b,
    sensitivity = sensitivity_object,
    specificity = specificity_object,
    groups = groups,
    few_studies_warning = diagnostic_few_studies_warning(length(unique(data$study))),
    bivariate_error = if (isTRUE(groups$available)) NULL else groups$error
  )
}

# The paired forest with one row block per test, so the two tests are read on the same studies.
plot_diagnostic_comparative_forest <- function(meta_object, right_label, numerator_label, denominator_label, col_square, col_square_lines) {
  meta::forest(
    meta_object,
    # Same layout as the other modules (binary, continuous, pre-calculated).
    layout = "Revman5",
    pscale = 100,
    sortvar = studlab,
    digits = 1,
    leftcols = c("studlab", "event", "n", "effect.ci"),
    leftlabs = c("Study", numerator_label, denominator_label,
                 paste0(right_label, " [95% CI]")),
    xlim = c(0, 100),
    # No pooled diamond: a univariate summary of sensitivity or specificity
    # is exactly what the handbook rules out, and the pooled DOR now comes
    # from the bivariate fit. The summary lives on the summary-point card.
    random = FALSE,
    common = FALSE,
    overall = FALSE,
    overall.hetstat = FALSE,
    pooled.events = FALSE,
    print.tau2 = FALSE,
    print.subgroup.name = FALSE,
    # metaprop with GLMM does not produce this per-subgroup test: it printed as
    # "Test for effect in subgroup: z = . (p = .)", an empty line.
    test.effect.subgroup.random = FALSE,
    test.effect.subgroup.common = FALSE,
    test.overall.random = FALSE,
    test.overall.common = FALSE,
    # The binary forest does not overlap because it has 8 columns on the left
    # and the plot starts much further right; here there are only 4. Pushing the
    # plot with colgap.forest.left gives the footer text room WITHOUT stretching
    # the study column, which is what calcwidth.* did (it opened a gap between
    # Study and TP). 35mm holds real labels such as "Wojciechowski 2023".
    # See design/diag_gap_robustez.R.
    colgap.forest.left = "35mm",
    colgap.forest = "3mm",
    just = "center",
    col.square = col_square,
    col.square.lines = col_square_lines,
    col.diamond = "black",
    col.diamond.lines = "black"
  )
}

# Sensitivity for every test compared.
plot_diagnostic_comparative_sensitivity <- function(result, col_square = "darkblue", col_square_lines = "black") {
  plot_diagnostic_comparative_forest(
    result$sensitivity,
    "Sensitivity",
    "TP",
    "TP + FN",
    col_square,
    col_square_lines
  )
}

# Specificity for every test compared.
plot_diagnostic_comparative_specificity <- function(result, col_square = "darkblue", col_square_lines = "black") {
  plot_diagnostic_comparative_forest(
    result$specificity,
    "Specificity",
    "TN",
    "TN + FP",
    col_square,
    col_square_lines
  )
}

# The same picture for two tests, both points read off the single joint model so
# they match the tests reported beside them. Key in the right margin, for the
# same reason as the single-test plot.
plot_diagnostic_comparative_points <- function(result, col_a = "darkblue", col_b = "red3") {
  g <- result$groups
  if (is.null(g) || !isTRUE(g$available)) {
    graphics::plot.new()
    graphics::text(0.5, 0.55, "Summary points could not be estimated.")
    graphics::text(0.5, 0.45, (g$error %||% "Check the data and rerun."), cex = 0.85)
    return(invisible(NULL))
  }
  cores <- c(col_a, col_b)
  simbolos <- c(23, 24)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(5, 4.5, 4, 14))
  graphics::plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "1 - specificity",
                 ylab = "Sensitivity", main = paste("Summary points -", result$outcome_name),
                 cex.lab = 1.05, cex.axis = 0.95)
  graphics::grid(col = "gray92", lty = 1)

  for (i in seq_along(g$levels)) {
    b <- g$by_level[[i]]
    if (is.null(b)) next
    d <- result$data[result$data$test == g$levels[i], , drop = FALSE]
    graphics::points(1 - d$TN / (d$TN + d$FP), d$TP / (d$TP + d$FN),
                     pch = 1, cex = 0.85, col = grDevices::adjustcolor(cores[i], alpha.f = 0.55))
    conf <- dta_ellipse(b, "confidence")
    if (!is.null(conf)) graphics::lines(1 - conf$specificity, conf$sensitivity, lty = i, lwd = 2, col = cores[i])
    graphics::points(1 - b$specificity, b$sensitivity, pch = simbolos[i], cex = 1.9, lwd = 1.2,
                     col = cores[i], bg = grDevices::adjustcolor(cores[i], alpha.f = 0.35))
  }

  # The right margin carries the whole key. Each test owns a line style as well as a
  # colour and a symbol, so the style has to be shown here or the two ellipses on the
  # plot are unexplained. The p values used to sit at the bottom; they belong to the
  # three comparison cards, which state what was tested and against which model.
  graphics::par(xpd = NA)
  x_de <- function(p) graphics::grconvertX(p, "ndc", "user")
  y_de <- function(p) graphics::grconvertY(p, "ndc", "user")
  x_marca <- x_de(0.775); x_traco <- c(x_de(0.792), x_de(0.812)); x_texto <- x_de(0.818)

  graphics::text(x_marca, y_de(0.87), "Bivariate summary", adj = 0, cex = 0.95, font = 2)
  graphics::points(x_de(0.780), y_de(0.815), pch = 1, cex = 0.9, col = "gray45")
  graphics::text(x_texto, y_de(0.815), "Observed studies", adj = 0, cex = 0.8, col = "gray20")

  y <- 0.735
  for (i in seq_along(g$levels)) {
    b <- g$by_level[[i]]
    if (is.null(b)) next
    graphics::points(x_de(0.780), y_de(y), pch = simbolos[i], cex = 1.2,
                     col = cores[i], bg = grDevices::adjustcolor(cores[i], alpha.f = 0.35))
    graphics::segments(x_traco[1], y_de(y), x_traco[2], y_de(y), col = cores[i], lwd = 2, lty = i)
    graphics::text(x_texto, y_de(y), g$levels[i], adj = 0, cex = 0.85, font = 2, col = cores[i])
    graphics::text(x_marca, y_de(y - 0.048), sprintf("Sens %.1f%% (%.1f-%.1f)", 100 * b$sensitivity,
                   100 * b$sensitivity_ci[1], 100 * b$sensitivity_ci[2]), adj = 0, cex = 0.75, col = "gray20")
    graphics::text(x_marca, y_de(y - 0.090), sprintf("Spec %.1f%% (%.1f-%.1f)", 100 * b$specificity,
                   100 * b$specificity_ci[1], 100 * b$specificity_ci[2]), adj = 0, cex = 0.75, col = "gray20")
    y <- y - 0.165
  }

  graphics::text(x_marca, y_de(y - 0.01), "Each ellipse is the 95%", adj = 0, cex = 0.75, col = "gray35")
  graphics::text(x_marca, y_de(y - 0.048), "confidence region for that", adj = 0, cex = 0.75, col = "gray35")
  graphics::text(x_marca, y_de(y - 0.086), "summary point.", adj = 0, cex = 0.75, col = "gray35")
  invisible(NULL)
}

# Sensitivity and specificity per test, formatted for the comparison summary.
diagnostic_comparative_accuracy_lines <- function(result) {
  data <- result$data
  tests <- c(result$test_a, result$test_b)
  lines <- c("Descriptive accuracy by test:", "Test | Studies | Sensitivity | Specificity")

  for (test_name in tests) {
    subset_data <- data[as.character(data$test) == test_name, , drop = FALSE]
    sensitivity <- sum(subset_data$TP) / sum(subset_data$TP + subset_data$FN)
    specificity <- sum(subset_data$TN) / sum(subset_data$TN + subset_data$FP)
    lines <- c(
      lines,
      paste(
        test_name,
        length(unique(subset_data$study)),
        paste0(format_effect_value(100 * sensitivity, 1), "%"),
        paste0(format_effect_value(100 * specificity, 1), "%"),
        sep = " | "
      )
    )
  }

  c(lines, "", "Direction note: the likelihood-ratio test tells whether the two tests differ; the descriptive table helps show which test looks higher.")
}

# The same test written out for the summary modal.
diagnostic_comparative_lr_text <- function(result, component) {
  g <- result$groups
  if (is.null(g) || !isTRUE(g$available)) {
    return(g$error %||% "Comparative bivariate model unavailable.")
  }

  heading <- switch(
    component,
    overall = "Overall diagnostic accuracy comparison",
    sensitivity = "Sensitivity comparison",
    specificity = "Specificity comparison",
    "Comparative diagnostic test"
  )
  interpretation_target <- switch(
    component,
    overall = "overall diagnostic accuracy",
    sensitivity = "sensitivity",
    specificity = "specificity",
    "diagnostic performance"
  )
  p <- switch(component, overall = g$p_overall, sensitivity = g$p_sensitivity,
              specificity = g$p_specificity, NA_real_)
  p_text <- if (is.na(p)) "NA" else format.pval(p, digits = 3, eps = 0.001)
  conclusion <- if (!is.na(p) && p < 0.05) {
    paste0("There is statistical evidence that ", interpretation_target, " differs between the selected tests.")
  } else {
    paste0("There is no clear statistical evidence that ", interpretation_target, " differs between the selected tests.")
  }

  c(
    paste0(heading, " for ", result$test_a, " vs ", result$test_b),
    paste0("Paired studies used: ", g$studies),
    paste0("Likelihood-ratio test against the model with no test effect: p = ", p_text, "."),
    conclusion,
    "",
    diagnostic_comparative_accuracy_lines(result),
    "",
    "Summary points from the joint bivariate model:",
    "",
    unlist(lapply(g$levels, function(n) diagnostic_bivariate_lines(g$by_level[[n]], n)))
  )
}


# ============================================================================
# NETWORK META-ANALYSIS
#
# The other module that does not use meta directly: netmeta pools direct and
# indirect comparisons in a single model. Everything a pairwise module reads off
# one effect, this one reads relative to a reference treatment, which is why the
# reference is a parameter of almost every function here.
# ============================================================================

# The random and common pair for netmeta, which spells the choice differently
# from meta.
network_model_flags <- function(model_choice) {
  list(
    random = model_choice %in% c("random", "both"),
    common = model_choice %in% c("fixed", "both")
  )
}

# Reads a default out of the meta package. Wrapped so there is one place to
# look if meta changes a default between releases.
meta_setting <- function(name) {
  meta::gs(name)
}

# Pools an arm-level binary network. netmeta wants pairwise contrasts, so the
# arms are turned into comparisons first and the model is fitted on those.
analyze_network_binary <- function(data, params) {
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("The meta package is required. Install it with install.packages('meta').")
  }
  if (!requireNamespace("netmeta", quietly = TRUE)) {
    stop("The netmeta package is required. Install it with install.packages('netmeta').")
  }

  data <- validate_network_binary_data(data)
  model_flags <- network_model_flags(params$model_choice)
  reference_group <- trimws(params$reference_group %||% "")
  if (!nzchar(reference_group)) reference_group <- ""

  pairwise_object <- meta::pairwise(
    treat = treatment,
    event = responders,
    n = sampleSize,
    data = data,
    studlab = study,
    sm = params$sm,
    allstudies = TRUE
  )

  netmeta_object <- netmeta::netmeta(
    pairwise_object,
    random = model_flags$random,
    common = model_flags$common,
    reference.group = reference_group,
    details.chkmultiarm = TRUE,
    sep.trts = " vs ",
    sm = params$sm,
    method.tau = params$method_tau
  )

  list(
    outcome_name = params$outcome_name,
    data = data,
    pairwise = pairwise_object,
    nma = netmeta_object,
    sm = params$sm,
    model_choice = params$model_choice,
    method_tau = params$method_tau,
    small_values = params$small_values,
    reference_group = reference_group
  )
}

# The same path for a continuous outcome, pooling mean differences instead of
# ratios.
analyze_network_continuous <- function(data, params) {
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("The meta package is required. Install it with install.packages('meta').")
  }
  if (!requireNamespace("netmeta", quietly = TRUE)) {
    stop("The netmeta package is required. Install it with install.packages('netmeta').")
  }

  data <- validate_network_continuous_data(data)
  model_flags <- network_model_flags(params$model_choice)
  reference_group <- trimws(params$reference_group %||% "")
  if (!nzchar(reference_group)) reference_group <- ""

  pairwise_object <- meta::pairwise(
    treat = treatment,
    mean = mean,
    sd = std.dev,
    n = sampleSize,
    data = data,
    studlab = study,
    sm = params$sm,
    allstudies = TRUE
  )

  netmeta_object <- netmeta::netmeta(
    pairwise_object,
    random = model_flags$random,
    common = model_flags$common,
    reference.group = reference_group,
    details.chkmultiarm = TRUE,
    sep.trts = " vs ",
    sm = params$sm,
    method.tau = params$method_tau
  )

  list(
    outcome_name = params$outcome_name,
    data = data,
    pairwise = pairwise_object,
    nma = netmeta_object,
    sm = params$sm,
    model_choice = params$model_choice,
    method_tau = params$method_tau,
    small_values = params$small_values,
    reference_group = reference_group,
    backtransf = FALSE
  )
}

# Whether the network measure is a ratio, which decides whether estimates are
# exponentiated before being shown.
network_sm_is_ratio <- function(sm) {
  sm %in% c("HR", "RR", "OR")
}

# Pools a network already given as contrasts. On a ratio measure the input is
# checked for positivity and kept in TE_original before the log transform, so
# the plots can label the original scale the user typed.
analyze_network_precalc_ci <- function(data, params) {
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("The meta package is required. Install it with install.packages('meta').")
  }
  if (!requireNamespace("netmeta", quietly = TRUE)) {
    stop("The netmeta package is required. Install it with install.packages('netmeta').")
  }

  data <- validate_network_precalc_ci_data(data)
  ratio_scale <- network_sm_is_ratio(params$sm)
  if (ratio_scale && any(data$TE <= 0 | data$lower <= 0 | data$upper <= 0)) {
    stop("For HR, RR and OR, TE, lower and upper must be positive values on the original ratio scale.")
  }

  data$TE_original <- data$TE
  data$lower_original <- data$lower
  data$upper_original <- data$upper
  data$TE_analysis <- if (ratio_scale) log(data$TE) else data$TE
  data$seTE <- if (ratio_scale) {
    (log(data$upper) - log(data$lower)) / (2 * stats::qnorm(0.975))
  } else {
    (data$upper - data$lower) / (2 * stats::qnorm(0.975))
  }

  model_flags <- network_model_flags(params$model_choice)
  reference_group <- trimws(params$reference_group %||% "")
  if (!nzchar(reference_group)) reference_group <- ""

  netmeta_object <- netmeta::netmeta(
    TE = data$TE_analysis,
    seTE = data$seTE,
    treat1 = data$treat1,
    treat2 = data$treat2,
    studlab = data$study,
    random = model_flags$random,
    common = model_flags$common,
    reference.group = reference_group,
    details.chkmultiarm = TRUE,
    sep.trts = " vs ",
    sm = params$sm,
    method.tau = params$method_tau
  )

  list(
    outcome_name = params$outcome_name,
    data = data,
    pairwise = data[, c("study", "treat1", "treat2", "TE_original", "lower_original", "upper_original", "TE_analysis", "seTE")],
    nma = netmeta_object,
    sm = params$sm,
    model_choice = params$model_choice,
    method_tau = params$method_tau,
    small_values = params$small_values,
    reference_group = reference_group,
    backtransf = ratio_scale,
    ratio_scale = ratio_scale
  )
}

# The first usable string in a value that may arrive empty, NA or absent.
# netmeta returns several of its labels in shapes that vary with the input, so
# they are normalised here instead of at each call site.
network_first_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return(default)
  }
  x[[1]]
}

# Which treatment everything is compared against. The user's choice wins; with
# no choice, the first treatment in the network is used, since a network forest
# plot has no meaning without a reference.
network_forest_reference <- function(result) {
  reference_group <- network_first_text(result$reference_group)
  if (isTRUE(nzchar(reference_group))) {
    return(reference_group)
  }

  trts <- result$nma$trts %||% character(0)
  if (length(trts) == 0 && "treatment" %in% names(result$data)) {
    trts <- unique(as.character(result$data$treatment))
  }
  if (length(trts) == 0 && all(c("treat1", "treat2") %in% names(result$data))) {
    trts <- unique(as.character(c(result$data$treat1, result$data$treat2)))
  }
  trts <- trts[nzchar(trts)]
  if (length(trts) == 0) {
    return("")
  }
  network_first_text(trts)
}

# The league table: every treatment against every other, which is the table a
# network meta-analysis is usually published with.
network_binary_league_table <- function(result) {
  if (is.null(result)) {
    return(data.frame(Message = "Run the analysis to create the league table.", check.names = FALSE))
  }
  league <- netmeta::netleague(
    result$nma,
    random = result$model_choice %in% c("random", "both"),
    common = result$model_choice %in% c("fixed", "both"),
    backtransf = if (is.null(result$backtransf)) TRUE else isTRUE(result$backtransf),
    ci = TRUE,
    bracket = meta_setting("CIbracket"),
    separator = meta_setting("CIseparator"),
    lower.blank = meta_setting("CIlower.blank"),
    upper.blank = meta_setting("CIupper.blank"),
    digits = 2
  )

  usavel <- function(x) !is.null(x) && length(x) > 1
  aleatorio <- result$model_choice %in% c("random", "both")
  # netleague returns both slots whichever model was asked for, and fills the
  # unused one with a single NA, so length is what separates them, not is.null
  matrix_like <- if (aleatorio && usavel(league$random)) league$random
    else if (!aleatorio && usavel(league$common)) league$common
      else if (usavel(league$random)) league$random
      else if (usavel(league$common)) league$common
    else league
  as.data.frame(matrix_like, stringsAsFactors = FALSE, check.names = FALSE)
}

# The network geometry: nodes are treatments, edge width is how much direct evidence connects them. It is the first thing to look at, because a barely connected network makes every later estimate fragile.
plot_network_binary_graph <- function(result, col_points = "darkblue") {
  trts <- result$nma$trts
  if (is.null(trts) || length(trts) == 0) {
    trts <- if ("treatment" %in% names(result$data)) sort(unique(result$data$treatment)) else sort(unique(c(result$data$treat1, result$data$treat2)))
  }

  if ("sampleSize" %in% names(result$data) && "treatment" %in% names(result$data)) {
    treatment_counts <- stats::aggregate(sampleSize ~ treatment, data = result$data, sum)
    labels <- paste0(treatment_counts$treatment, " (n=", treatment_counts$sampleSize, ")")
    names(labels) <- treatment_counts$treatment
  } else if (all(c("treat1", "treat2") %in% names(result$data))) {
    comparison_counts <- table(c(result$data$treat1, result$data$treat2))
    labels <- paste0(names(comparison_counts), " (k=", as.integer(comparison_counts), ")")
    names(labels) <- names(comparison_counts)
  } else {
    labels <- trts
    names(labels) <- trts
  }

  graph_labels <- labels[trts]
  graph_labels[is.na(graph_labels)] <- trts[is.na(graph_labels)]

  netmeta::netgraph(
    result$nma,
    lwd = 3,
    plastic = FALSE,
    points = TRUE,
    cex = 1,
    cex.points = 1,
    col.points = col_points,
    points.min = 6,
    points.max = 12,
    col = "black",
    number.of.studies = TRUE,
    labels = graph_labels,
    thickness = "number.of.studies"
  )
}

# All treatments against the reference, from the pooled network model.
plot_network_binary_forest <- function(result) {
  reference_group <- network_forest_reference(result)
  if (!isTRUE(nzchar(reference_group))) {
    stop("No reference treatment is available for the network forest plot.")
  }

  pooled_model <- if (identical(result$model_choice, "common")) "common" else "random"
  # forest.netmeta is not exported; the meta::forest generic dispatches to it.
  forest_netmeta <- meta::forest
  forest_args <- list(
    x = result$nma,
    reference.group = reference_group,
    pooled = pooled_model,
    backtransf = if (is.null(result$backtransf)) TRUE else isTRUE(result$backtransf),
    smlab = paste0(result$sm, " vs ", reference_group)
  )
  do.call(forest_netmeta, forest_args)
}

# The heterogeneity and inconsistency test, split into within-design and
# between-design, which is where inconsistency in a network shows up.
network_binary_qtest_text <- function(result) {
  utils::capture.output(print(netmeta::decomp.design(result$nma)))
}

# Direct against indirect evidence per comparison, drawn from netsplit.
plot_network_binary_split <- function(result, col_points = "darkblue") {
  split_object <- netmeta::netsplit(
    result$nma,
    random = result$model_choice %in% c("random", "both"),
    common = result$model_choice %in% c("fixed", "both"),
    backtransf = if (is.null(result$backtransf)) TRUE else isTRUE(result$backtransf),
    sep.trts = " vs ",
    only.reference = FALSE,
    ci = TRUE,
    overall = TRUE,
    sm = result$sm,
    show = "all",
    lower.blank = meta_setting("CIlower.blank"),
    upper.blank = meta_setting("CIupper.blank"),
    digits = 2
  )

  plot(
    split_object,
    just = "center",
    col.studlab = "10mm",
    col.square = col_points,
    col.square.lines = "black",
    col.diamond = "black",
    rightcols = c("effect", "ci", "p"),
    rightlabs = c(NA, NA, "P-value")
  )
}

# Direct against indirect evidence, comparison by comparison. A disagreement
# between the two is the practical warning that the network is inconsistent.
network_binary_split_text <- function(result) {
  split_object <- netmeta::netsplit(
    result$nma,
    random = result$model_choice %in% c("random", "both"),
    common = result$model_choice %in% c("fixed", "both"),
    backtransf = if (is.null(result$backtransf)) TRUE else isTRUE(result$backtransf),
    sep.trts = " vs ",
    only.reference = FALSE,
    ci = TRUE,
    overall = TRUE,
    sm = result$sm,
    show = "all",
    lower.blank = meta_setting("CIlower.blank"),
    upper.blank = meta_setting("CIupper.blank"),
    digits = 2
  )
  utils::capture.output(print(split_object))
}

# The ranking written out, with the caveat that a rank is not an effect size.
network_binary_rank_text <- function(result) {
  rank_object <- netmeta::netrank(
    result$nma,
    small.values = result$small_values,
    method = "P-Score"
  )
  utils::capture.output(print(rank_object))
}

# P-scores from netrank. small.values carries whether a low value is good, so
# the ranking follows the direction the user declared for the outcome.
network_binary_rank_scores <- function(result) {
  rank_object <- netmeta::netrank(
    result$nma,
    small.values = result$small_values,
    method = "P-Score"
  )

  rank_list <- unclass(rank_object)
  preferred_names <- if (result$model_choice %in% c("random", "both")) {
    c("Pscore.random", "Pscore")
  } else {
    c("Pscore.common", "Pscore.fixed", "Pscore")
  }
  score_names <- c(preferred_names, grep("pscore", names(rank_list), ignore.case = TRUE, value = TRUE))

  scores <- NULL
  for (score_name in unique(score_names)) {
    candidate <- rank_list[[score_name]]
    if (is.numeric(candidate) && length(candidate) > 0) {
      scores <- candidate
      break
    }
  }

  if (is.null(scores)) {
    numeric_candidates <- Filter(function(x) is.numeric(x) && length(x) > 0 && !is.null(names(x)), rank_list)
    if (length(numeric_candidates) > 0) {
      scores <- numeric_candidates[[1]]
    }
  }

  if (is.null(scores) || length(scores) == 0) {
    return(data.frame(Treatment = character(0), P_score = numeric(0)))
  }

  treatments <- names(scores)
  if (is.null(treatments) || any(!nzchar(treatments))) {
    treatments <- paste0("Treatment ", seq_along(scores))
  }

  scores <- as.numeric(scores)
  if (all(is.finite(scores), na.rm = TRUE) && max(scores, na.rm = TRUE) > 1) {
    scores <- scores / 100
  }

  rank_df <- data.frame(
    Treatment = treatments,
    P_score = scores,
    stringsAsFactors = FALSE
  )
  rank_df <- rank_df[is.finite(rank_df$P_score), , drop = FALSE]
  rank_df[order(rank_df$P_score, decreasing = TRUE), , drop = FALSE]
}

# The ranking as probabilities per position, rather than as a single ordering.
plot_network_binary_rankogram <- function(result, col_points = "darkblue") {
  rank_df <- network_binary_rank_scores(result)
  if (nrow(rank_df) == 0) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Treatment ranking could not be extracted from netrank().")
    return(invisible(NULL))
  }

  rank_df$Treatment <- factor(rank_df$Treatment, levels = rev(rank_df$Treatment))
  plot_object <- ggplot2::ggplot(rank_df, ggplot2::aes(x = Treatment, y = P_score)) +
    ggplot2::geom_col(width = 0.72, fill = col_points) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(P_score * 100, 1), "%")),
      hjust = -0.12,
      color = "#071D49",
      fontface = "bold",
      size = 4
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      limits = c(0, max(1, max(rank_df$P_score, na.rm = TRUE) * 1.12)),
      labels = function(x) paste0(round(x * 100), "%"),
      expand = ggplot2::expansion(mult = c(0, 0.04))
    ) +
    ggplot2::labs(
      title = "Treatment ranking by P-score",
      subtitle = "Higher P-score indicates a higher probability of being among the best treatments.",
      x = NULL,
      y = "P-score"
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#071D49", size = 18),
      plot.subtitle = ggplot2::element_text(color = "#41547A", size = 12),
      axis.text.y = ggplot2::element_text(color = "#071D49", face = "bold"),
      axis.text.x = ggplot2::element_text(color = "#41547A"),
      axis.title.x = ggplot2::element_text(color = "#071D49", face = "bold"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  print(plot_object)
  invisible(plot_object)
}

# Comparison-adjusted funnel plot, which needs the reference to be meaningful.
plot_network_binary_funnel <- function(result) {
  order_values <- if ("treatment" %in% names(result$data)) {
    unique(as.character(result$data$treatment))
  } else if (all(c("treat1", "treat2") %in% names(result$data))) {
    unique(as.character(c(result$data$treat1, result$data$treat2)))
  } else {
    unique(as.character(result$nma$trts %||% character(0)))
  }
  meta::funnel(
    result$nma,
    order = order_values,
    linreg = TRUE,
    pos.legend = "topright",
    pos.tests = "topleft"
  )
}

# Workflow screen: network meta-analysis from arm-level continuous data.
network_continuous_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_network_from_continuous", "Back to Network", class = "back-button"),
      actionButton("back_home_from_network_continuous", "Back to home", class = "back-button secondary-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Network meta-analysis"),
      tags$h2("Continuous outcome workflow"),
      tags$p("Use arm-level continuous data to build a frequentist network meta-analysis.")
    ),
    div(class = "step-indicator", textOutput("network_cont_step_label")),
    tabsetPanel(
      id = "network_cont_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Upload one arm-level table, paste from Google Sheets, or load the example dataset."),
            tags$p(class = "help-text required-columns", HTML("<strong>Required columns (use these exact Excel column names):</strong><br><strong>study</strong>: study name or trial identifier. Each treatment arm from the same study must use the same study value.<br><strong>treatment</strong>: treatment or intervention name.<br><strong>mean</strong>: mean outcome value in that treatment arm.<br><strong>std.dev</strong>: standard deviation in that treatment arm.<br><strong>sampleSize</strong>: total sample size in that treatment arm.")),
            actionButton("load_network_cont_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_network_continuous_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "network_cont_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "network_cont_paste",
              "Paste data from Google Sheets",
              placeholder = "study\ttreatment\tmean\tstd.dev\tsampleSize\nStudy 1\tDrug A\t128\t14\t100\nStudy 1\tPlacebo\t136\t16\t100",
              rows = 10
            ),
            actionButton("use_network_cont_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("network_cont_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("Network data format"),
            tags$p(class = "help-text microcopy", "Use one row per treatment arm, not one row per study comparison."),
            tags$p(class = "help-text microcopy", "Multi-arm studies are allowed when the same study name appears in three or more rows."),
            tags$p(class = "help-text microcopy", "Network modules currently run one outcome at a time.")
          ),
          data_check_card(
            "network_cont_data_check",
            actionButton("network_cont_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
              textInput("network_cont_outcome", "Outcome name", value = "Outcome"),
              selectInput("network_cont_sm", "Summary measure (sm)", choices = c("MD", "SMD"), selected = "MD"),
              selectInput("network_cont_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
              textInput("network_cont_reference", "Reference treatment (optional)", value = ""),
              selectInput("network_cont_method_tau", "Tau method", choices = c("REML", "ML", "DL", "PM", "SJ", "HE", "HS", "EB"), selected = "REML"),
              selectInput("network_cont_small_values", "For ranking, smaller values are", choices = c("Good" = "good", "Bad" = "bad"), selected = "good"),
              selectInput("network_cont_col_points", "Network node color", choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Black" = "black", "Red" = "red3", "Dark green" = "darkgreen"), selected = "darkblue", selectize = FALSE)
            ),
            tags$div(class = "status-message prominent-status", textOutput("network_cont_run_status")),
            div(
              class = "step-actions",
              actionButton("network_cont_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_network_cont", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("network_cont_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Forest plot"),
              tags$p(class = "help-text", "Compare all treatments against the selected reference treatment."),
              network_result_action_group("network_cont_forest", "download_network_cont_forest", "preview_network_cont_forest", "summary_network_cont_main")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Network graph"),
              tags$p(class = "help-text", "Visualize treatment nodes and direct comparisons."),
              network_result_action_group("network_cont_graph", "download_network_cont_graph", "preview_network_cont_graph", "summary_network_cont_graph")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("League table"),
              tags$p(class = "help-text", "Export the pairwise network estimates as an Excel table."),
              div(class = "result-button-row", actionButton("summary_network_cont_league", "Summary", class = "result-action"), downloadButton("download_network_cont_league", "Download XLSX", class = "result-action"))
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Between-study heterogeneity"),
              tags$p(class = "help-text", "Inspect the design-based decomposition and Q statistics."),
              div(class = "result-button-row", actionButton("summary_network_cont_qtest", "Summary", class = "result-action"), downloadButton("download_network_cont_qtest", "Download TXT", class = "result-action"))
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Node splitting"),
              tags$p(class = "help-text", "Compare direct and indirect evidence when available."),
              network_result_action_group("network_cont_split", "download_network_cont_split", NULL, "summary_network_cont_split", default_width = 10, default_height = 12)
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("P-score ranking"),
              tags$p(class = "help-text", "View a fast treatment ranking plot based on P-scores."),
              network_result_action_group("network_cont_rankogram", "download_network_cont_rankogram", "preview_network_cont_rankogram", "summary_network_cont_rank")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Comparison-adjusted funnel plot"),
              tags$p(class = "help-text", "Explore small-study effects across network comparisons."),
              network_result_action_group("network_cont_funnel", "download_network_cont_funnel", "preview_network_cont_funnel", NULL)
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Pairwise object"),
              tags$p(class = "help-text", "Download the pairwise object used to create the network."),
              div(class = "result-button-row", downloadButton("download_network_cont_pairwise", "Download CSV", class = "result-action"))
            )
          ),
          div(
            class = "step-actions",
            actionButton("network_cont_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

# Workflow screen: network meta-analysis from contrast-level effects.
network_precalc_ci_page <- function() {
  div(
    class = "analysis-page",
    div(
      class = "analysis-topbar",
      actionButton("back_network_from_precalc_ci", "Back to Network", class = "back-button"),
      actionButton("back_home_from_network_precalc_ci", "Back to home", class = "back-button secondary-button")
    ),
    div(
      class = "workflow-header analysis-header",
      tags$p(class = "eyebrow", "Network meta-analysis"),
      tags$h2("Pre-calculated TE + 95% CI workflow"),
      tags$p("Use comparison-level effect estimates to build a frequentist network meta-analysis.")
    ),
    div(class = "step-indicator", textOutput("network_precalc_ci_step_label")),
    tabsetPanel(
      id = "network_precalc_ci_steps",
      type = "hidden",
      tabPanel(
        "data",
        div(
          class = "analysis-two-column data-step-grid",
          div(
            class = "analysis-card",
            tags$h3("Single outcome"),
            tags$p(class = "help-text", "Upload one comparison-level table, paste from Google Sheets, or load the example dataset."),
            tags$p(class = "help-text required-columns", HTML("<strong>Required columns (use these exact Excel column names):</strong><br><strong>study</strong>: study name or trial identifier.<br><strong>treat1</strong>: first treatment in the direct comparison.<br><strong>treat2</strong>: second treatment in the direct comparison.<br><strong>TE</strong>: treatment effect exactly as reported in the paper, for example HR, RR, OR, MD or SMD.<br><strong>lower</strong>: lower 95% confidence interval limit for TE.<br><strong>upper</strong>: upper 95% confidence interval limit for TE.")),
            actionButton("load_network_precalc_ci_example", "Load example dataset", class = "primary-action"),
            downloadButton("download_network_precalc_ci_template", "Download XLSX template", class = "secondary-button"),
            tags$hr(),
            fileInput(
              "network_precalc_ci_file",
              "Import Excel or CSV",
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
            ),
            textAreaInput(
              "network_precalc_ci_paste",
              "Paste data from Google Sheets",
              placeholder = "study\ttreat1\ttreat2\tTE\tlower\tupper\nStudy 1\tDrug A\tPlacebo\t0.72\t0.55\t0.94",
              rows = 10
            ),
            actionButton("use_network_precalc_ci_paste", "Use pasted data", class = "primary-action outline-action"),
            tags$div(class = "status-message", textOutput("network_precalc_ci_data_status"))
          ),
          div(
            class = "analysis-card preview-card",
            tags$h3("Pre-calculated network format"),
            tags$p(class = "help-text microcopy", "Use one row per direct comparison."),
            tags$p(class = "help-text microcopy", "For HR, RR and OR, enter TE/lower/upper as reported; MetaVidence handles the log scale internally."),
            tags$p(class = "help-text microcopy", "Network modules currently run one outcome at a time.")
          ),
          data_check_card(
            "network_precalc_ci_data_check",
            actionButton("network_precalc_ci_to_params", "Next: parameters", class = "run-action")
          )
        )
      ),
      tabPanel(
        "parameters",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card",
            tags$h3("2. Parameters"),
            div(
              class = "parameter-grid",
              textInput("network_precalc_ci_outcome", "Outcome name", value = "Outcome"),
              selectInput("network_precalc_ci_sm", "Summary measure (sm)", choices = c("HR", "RR", "OR", "MD", "SMD"), selected = "HR"),
              selectInput("network_precalc_ci_model", "Analysis model", choices = c("Random-effects" = "random", "Fixed-effect" = "fixed", "Both" = "both"), selected = "random"),
              textInput("network_precalc_ci_reference", "Reference treatment (optional)", value = ""),
              selectInput("network_precalc_ci_method_tau", "Tau method", choices = c("REML", "ML", "DL", "PM", "SJ", "HE", "HS", "EB"), selected = "REML"),
              selectInput("network_precalc_ci_small_values", "For ranking, smaller values are", choices = c("Good" = "good", "Bad" = "bad"), selected = "good"),
              selectInput("network_precalc_ci_col_points", "Network node color", choices = c("Dark blue" = "darkblue", "Blue" = "blue", "Steel blue" = "steelblue", "Black" = "black", "Red" = "red3", "Dark green" = "darkgreen"), selected = "darkblue", selectize = FALSE)
            ),
            tags$div(class = "status-message prominent-status", textOutput("network_precalc_ci_run_status")),
            div(
              class = "step-actions",
              actionButton("network_precalc_ci_back_to_data", "Back to data", class = "back-button secondary-button"),
              actionButton("run_network_precalc_ci", "Continue", class = "run-action")
            )
          )
        )
      ),
      tabPanel(
        "results",
        div(
          class = "analysis-single-column",
          div(
            class = "analysis-card download-card",
            tags$h3("3. Results and citation"),
            tags$div(class = "status-message prominent-status", textOutput("network_precalc_ci_run_status_results")),
            citation_reminder()
          ),
          div(
            class = "results-grid",
            div(
              class = "analysis-card result-control-card",
              tags$h3("Forest plot"),
              tags$p(class = "help-text", "Compare all treatments against the selected reference treatment."),
              network_result_action_group("network_precalc_ci_forest", "download_network_precalc_ci_forest", "preview_network_precalc_ci_forest", "summary_network_precalc_ci_main")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Network graph"),
              tags$p(class = "help-text", "Visualize treatment nodes and direct comparisons."),
              network_result_action_group("network_precalc_ci_graph", "download_network_precalc_ci_graph", "preview_network_precalc_ci_graph", "summary_network_precalc_ci_graph")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("League table"),
              tags$p(class = "help-text", "Export the pairwise network estimates as an Excel table."),
              div(class = "result-button-row", actionButton("summary_network_precalc_ci_league", "Summary", class = "result-action"), downloadButton("download_network_precalc_ci_league", "Download XLSX", class = "result-action"))
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Between-study heterogeneity"),
              tags$p(class = "help-text", "Inspect the design-based decomposition and Q statistics."),
              div(class = "result-button-row", actionButton("summary_network_precalc_ci_qtest", "Summary", class = "result-action"), downloadButton("download_network_precalc_ci_qtest", "Download TXT", class = "result-action"))
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Node splitting"),
              tags$p(class = "help-text", "Compare direct and indirect evidence when available."),
              network_result_action_group("network_precalc_ci_split", "download_network_precalc_ci_split", NULL, "summary_network_precalc_ci_split", default_width = 10, default_height = 12)
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("P-score ranking"),
              tags$p(class = "help-text", "View a fast treatment ranking plot based on P-scores."),
              network_result_action_group("network_precalc_ci_rankogram", "download_network_precalc_ci_rankogram", "preview_network_precalc_ci_rankogram", "summary_network_precalc_ci_rank")
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Comparison-adjusted funnel plot"),
              tags$p(class = "help-text", "Explore small-study effects across network comparisons."),
              network_result_action_group("network_precalc_ci_funnel", "download_network_precalc_ci_funnel", "preview_network_precalc_ci_funnel", NULL)
            ),
            div(
              class = "analysis-card result-control-card",
              tags$h3("Network input object"),
              tags$p(class = "help-text", "Download the comparison-level table used to create the network, including calculated seTE."),
              div(class = "result-button-row", downloadButton("download_network_precalc_ci_pairwise", "Download CSV", class = "result-action"))
            )
          ),
          div(
            class = "step-actions",
            actionButton("network_precalc_ci_back_to_params", "Back to parameters", class = "back-button secondary-button")
          )
        )
      )
    )
  )
}

# Two-arm binary outcome. The forest carries both arms' counts, and the labels
# under it come from resolve_comparison_labels(), which is what decides which
# side reads Favors the experimental group.
plot_binary_forest <- function(result, col_square, col_square_lines, sort_studies = FALSE) {
  extra_cols <- forest_extra_columns(result)
  weight_col <- forest_weight_column(result$meta)
  meta::forest(
    result$meta,
    layout = "Revman5",
    label.e = result$label_e,
    label.c = result$label_c,
    label.left = result$label_left,
    label.right = result$label_right,
    leftcols = c("studlab", extra_cols, "event.e", "n.e", "event.c", "n.c", weight_col, "effect", "ci"),
    leftlabs = c("Studies", extra_cols, NA, NA, NA, NA, rep(NA, length(weight_col)), NA, NA),
    test.overall.random = result$model_choice %in% c("random", "both"),
    test.overall.common = result$model_choice %in% c("fixed", "both"),
    colgap = "2mm",
    pooled.events = TRUE,
    col.square = col_square,
    col.square.lines = col_square_lines,
    sortvar = if (sort_studies) result$meta$TE else NULL
  )
}

# Leave-one-out for the binary module.
plot_binary_loo <- function(result, col_square = "lightblue") {
  meta_loo <- meta::metainf(result$meta, pooled = resolve_metainf_pool(result$model_choice))
  meta::forest(
    meta_loo,
    col.bg = col_square,
    col.diamond = "black",
    xlab = paste(result$label_left, "   ", result$label_right),
    ff.xlab = "bold",
    rightcols = c("effect", "ci", "I2", "pval"),
    colgap.right = "4mm",
    just = "center"
  )
}

# Funnel plot for the binary module.
plot_binary_funnel <- function(result, col_square = "red") {
  meta::funnel(
    result$meta,
    studlab = TRUE,
    bg = col_square,
    random = result$model_choice %in% c("random", "both"),
    common = result$model_choice %in% c("fixed", "both"),
    backtransf = FALSE
  )
}

# Subgroup forest for the binary module.
plot_binary_subgroup <- function(result, col_square, col_square_lines, sort_studies = FALSE) {
  extra_cols <- forest_extra_columns(result)
  weight_col <- forest_weight_column(result$subgroup)
  meta::forest(
    result$subgroup,
    layout = "Revman5",
    label.e = result$label_e,
    label.c = result$label_c,
    label.left = result$label_left,
    label.right = result$label_right,
    leftcols = c("studlab", extra_cols, "event.e", "n.e", "event.c", "n.c", weight_col, "effect", "ci"),
    leftlabs = c("Studies", extra_cols, NA, NA, NA, NA, rep(NA, length(weight_col)), NA, NA),
    colgap = "2mm",
    pooled.events = TRUE,
    col.square = col_square,
    col.square.lines = col_square_lines,
    sortvar = if (sort_studies) result$subgroup$TE else NULL,
    print.subgroup.name = FALSE,
    test.effect.subgroup.random = result$model_choice %in% c("random", "both"),
    test.effect.subgroup.common = result$model_choice %in% c("fixed", "both"),
    overall = FALSE,
    overall.hetstat = FALSE,
    test.overall.random = FALSE,
    test.overall.common = FALSE
  )
}

# Bubble plot for the binary module.
plot_binary_metareg <- function(result, col_square = "red") {
  table_df <- extract_binary_metareg_table(result)
  plot_metareg_with_header(table_df, function() {
    ylab_value <- if (result$sm %in% c("RR", "OR")) paste("Log", result$sm) else result$sm
    bubble_object <- meta::metareg(result$meta, stats::as.formula(paste0("~ `", result$metareg_col, "`")))
    meta::bubble(
      bubble_object,
      xlab = result$metareg_col,
      ylab = ylab_value,
      studlab = TRUE,
      bg = col_square,
      backtransf = FALSE
    )
  })
}

# Meta-regression table for the two-arm binary module.
extract_binary_metareg_table <- function(result) {
  if (is.null(result) || is.null(result$metareg)) {
    return(data.frame(Message = if (is.null(result)) "Run the analysis first." else "Pick a moderator column to build this table."))
  }

  s <- summary(result$metareg)
  beta_values <- as.numeric(s$beta)
  rownames_beta <- rownames(s$beta)
  row_index <- if ("easy_meta_metareg_covariate" %in% rownames_beta) {
    which(rownames_beta == "easy_meta_metareg_covariate")[1]
  } else {
    min(2, length(beta_values))
  }

  estimate <- beta_values[row_index]
  lower_ci <- as.numeric(s$ci.lb)[row_index]
  upper_ci <- as.numeric(s$ci.ub)[row_index]
  p_value <- as.numeric(s$pval)[row_index]
  r2_value <- suppressWarnings(as.numeric(s$R2)[1])
  if (is.na(r2_value) && !is.null(result$metareg_null$tau2) && !is.null(result$metareg$tau2)) {
    tau2_null <- as.numeric(result$metareg_null$tau2)[1]
    tau2_model <- as.numeric(result$metareg$tau2)[1]
    if (!is.na(tau2_null) && !is.na(tau2_model) && tau2_null > 0) {
      r2_value <- max(0, (tau2_null - tau2_model) / tau2_null) * 100
    }
  }

  data.frame(
    Covariate = result$metareg_col,
    `Coefficient (β)` = sprintf("%.4f", estimate),
    `95% CI` = sprintf("%.4f to %.4f", lower_ci, upper_ci),
    `P-value` = format.pval(p_value, digits = 3, eps = 0.001),
    R2 = if (is.na(r2_value)) "NA" else if (r2_value <= 1) sprintf("%.1f%%", 100 * r2_value) else sprintf("%.2f%%", r2_value),
    check.names = FALSE
  )
}

# Two-arm continuous outcome. The median and IQR module reuses these same
# plotters: by the time an analysis exists, the quartiles have already been
# converted to a mean and an SD, so there is nothing left to distinguish.
plot_cont_mean_forest <- function(result, col_square, col_square_lines, sort_studies = FALSE) {
  extra_cols <- forest_extra_columns(result)
  meta::forest(
    result$meta,
    layout = "Revman",
    sortvar = if (sort_studies) result$meta$TE else NULL,
    label.e = result$label_e,
    label.c = result$label_c,
    label.left = result$label_left,
    label.right = result$label_right,
    leftcols = c("studlab", extra_cols, "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c", "w.random", "effect", "ci"),
    leftlabs = c("Studies", extra_cols, "Mean", "SD", "Total", "Mean", "SD", "Total", "Weight", result$sm, "95% CI"),
    test.overall.random = result$model_choice %in% c("random", "both"),
    test.overall.common = result$model_choice %in% c("fixed", "both"),
    colgap = "2mm",
    digits = 2,
    digits.sd = 2,
    digits.pval = 2,
    col.square = col_square,
    col.square.lines = col_square_lines
  )
}

# Leave-one-out for the continuous module.
plot_cont_mean_loo <- function(result, col_square = "lightblue") {
  meta_loo <- meta::metainf(result$meta, pooled = resolve_metainf_pool(result$model_choice))
  meta::forest(
    meta_loo,
    col.bg = col_square,
    col.diamond = "black",
    xlab = paste(result$label_left, "   ", result$label_right),
    ff.xlab = "bold",
    rightcols = c("effect", "ci", "I2", "pval"),
    colgap.right = "4mm",
    just = "center"
  )
}

# Funnel plot for the continuous module.
plot_cont_mean_funnel <- function(result, col_square = "red") {
  meta::funnel(
    result$meta,
    studlab = TRUE,
    bg = col_square,
    random = result$model_choice %in% c("random", "both"),
    common = result$model_choice %in% c("fixed", "both"),
    backtransf = FALSE
  )
}

# Subgroup forest for the continuous module.
plot_cont_mean_subgroup <- function(result, col_square, col_square_lines, sort_studies = FALSE) {
  extra_cols <- forest_extra_columns(result)
  meta::forest(
    result$subgroup,
    layout = "Revman",
    sortvar = if (sort_studies) result$subgroup$TE else NULL,
    label.e = result$label_e,
    label.c = result$label_c,
    label.left = result$label_left,
    label.right = result$label_right,
    leftcols = c("studlab", extra_cols, "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c", "w.random", "effect", "ci"),
    leftlabs = c("Studies", extra_cols, "Mean", "SD", "Total", "Mean", "SD", "Total", "Weight", result$sm, "95% CI"),
    colgap = "2mm",
    digits = 2,
    digits.sd = 2,
    digits.pval = 2,
    col.square = col_square,
    col.square.lines = col_square_lines,
    print.subgroup.name = FALSE,
    test.effect.subgroup.random = result$model_choice %in% c("random", "both"),
    test.effect.subgroup.common = result$model_choice %in% c("fixed", "both"),
    overall = FALSE,
    overall.hetstat = FALSE,
    test.overall.random = FALSE,
    test.overall.common = FALSE
  )
}

# Bubble plot for the continuous module.
plot_cont_mean_metareg <- function(result, col_square = "red") {
  table_df <- extract_cont_mean_metareg_table(result)
  plot_metareg_with_header(table_df, function() {
    y_axis_label <- if (identical(result$sm, "SMD")) "Standardized mean difference" else "Mean difference"
    bubble_object <- meta::metareg(result$meta, stats::as.formula(paste0("~ `", result$metareg_col, "`")))
    meta::bubble(
      bubble_object,
      xlab = result$metareg_col,
      ylab = y_axis_label,
      studlab = TRUE,
      bg = col_square,
      backtransf = FALSE
    )
  })
}

# Meta-regression table for two-arm continuous, mean and SD.
extract_cont_mean_metareg_table <- function(result) {
  if (is.null(result) || is.null(result$metareg)) {
    return(data.frame(Message = if (is.null(result)) "Run the analysis first." else "Pick a moderator column to build this table."))
  }
  s <- summary(result$metareg)
  beta_values <- as.numeric(s$beta)
  rownames_beta <- rownames(s$beta)
  row_index <- if ("easy_meta_metareg_covariate" %in% rownames_beta) which(rownames_beta == "easy_meta_metareg_covariate")[1] else min(2, length(beta_values))
  estimate <- beta_values[row_index]
  lower_ci <- as.numeric(s$ci.lb)[row_index]
  upper_ci <- as.numeric(s$ci.ub)[row_index]
  p_value <- as.numeric(s$pval)[row_index]
  r2_value <- suppressWarnings(as.numeric(s$R2)[1])
  if (is.na(r2_value) && !is.null(result$metareg_null$tau2) && !is.null(result$metareg$tau2)) {
    tau2_null <- as.numeric(result$metareg_null$tau2)[1]
    tau2_model <- as.numeric(result$metareg$tau2)[1]
    if (!is.na(tau2_null) && !is.na(tau2_model) && tau2_null > 0) {
      r2_value <- max(0, (tau2_null - tau2_model) / tau2_null) * 100
    }
  }
  data.frame(
    Covariate = result$metareg_col,
    `Coefficient (β)` = sprintf("%.4f", estimate),
    `95% CI` = sprintf("%.4f to %.4f", lower_ci, upper_ci),
    `P-value` = format.pval(p_value, digits = 3, eps = 0.001),
    R2 = if (is.na(r2_value)) "NA" else if (r2_value <= 1) sprintf("%.1f%%", 100 * r2_value) else sprintf("%.2f%%", r2_value),
    check.names = FALSE
  )
}

# Meta-regression table for single-arm means.
extract_single_mean_metareg_table <- function(result) {
  if (is.null(result) || is.null(result$metareg)) {
    return(data.frame(Message = if (is.null(result)) "Run the analysis first." else "Pick a moderator column to build this table."))
  }

  s <- summary(result$metareg)
  beta_values <- as.numeric(s$beta)
  rownames_beta <- rownames(s$beta)
  row_index <- if ("easy_meta_metareg_covariate" %in% rownames_beta) {
    which(rownames_beta == "easy_meta_metareg_covariate")[1]
  } else {
    min(2, length(beta_values))
  }

  estimate <- beta_values[row_index]
  lower_ci <- as.numeric(s$ci.lb)[row_index]
  upper_ci <- as.numeric(s$ci.ub)[row_index]
  p_value <- as.numeric(s$pval)[row_index]
  r2_value <- suppressWarnings(as.numeric(s$R2)[1])
  if (is.na(r2_value) && !is.null(result$metareg_null$tau2) && !is.null(result$metareg$tau2)) {
    tau2_null <- as.numeric(result$metareg_null$tau2)[1]
    tau2_model <- as.numeric(result$metareg$tau2)[1]
    if (!is.na(tau2_null) && !is.na(tau2_model) && tau2_null > 0) {
      r2_value <- max(0, (tau2_null - tau2_model) / tau2_null) * 100
    }
  }

  data.frame(
    Covariate = result$metareg_col,
    `Coefficient (β)` = sprintf("%.4f", estimate),
    `95% CI` = sprintf("%.4f to %.4f", lower_ci, upper_ci),
    `P-value` = format.pval(p_value, digits = 3, eps = 0.001),
    R2 = if (is.na(r2_value)) "NA" else if (r2_value <= 1) sprintf("%.1f%%", 100 * r2_value) else sprintf("%.2f%%", r2_value),
    check.names = FALSE
  )
}

# Meta-regression table for single-arm proportions.
extract_single_prop_metareg_table <- function(result) {
  if (is.null(result) || is.null(result$metareg)) {
    return(data.frame(Message = if (is.null(result)) "Run the analysis first." else "Pick a moderator column to build this table."))
  }

  # Reads one named number out of a list that may not carry it, so a missing
  # statistic comes back as NA instead of stopping the summary.
  extract_named_numeric <- function(x, candidates) {
    if (is.null(x)) return(NA_real_)

    x_names <- names(x)
    if (!is.null(x_names)) {
      for (candidate in candidates) {
        hit <- which(tolower(x_names) == tolower(candidate))
        if (length(hit) > 0) {
          value <- suppressWarnings(as.numeric(x[[hit[1]]]))
          if (length(value) > 0 && !is.na(value[1])) return(value[1])
        }
      }
    }

    if (is.list(x)) {
      for (element in x) {
        value <- extract_named_numeric(element, candidates)
        if (!is.na(value)) return(value)
      }
    }

    NA_real_
  }

  metareg_summary <- summary(result$metareg)
  coefficient_matrix <- NULL

  coefficient_matrix <- tryCatch(
    coef(metareg_summary),
    error = function(e) NULL
  )

  if (is.null(coefficient_matrix) && !is.null(metareg_summary$beta)) {
    beta_values <- as.numeric(metareg_summary$beta)
    names(beta_values) <- names(metareg_summary$beta)

    se_values <- if (!is.null(metareg_summary$se)) as.numeric(metareg_summary$se) else rep(NA_real_, length(beta_values))
    p_values <- if (!is.null(metareg_summary$pval)) as.numeric(metareg_summary$pval) else rep(NA_real_, length(beta_values))
    ci_lb_values <- if (!is.null(metareg_summary$ci.lb)) as.numeric(metareg_summary$ci.lb) else rep(NA_real_, length(beta_values))
    ci_ub_values <- if (!is.null(metareg_summary$ci.ub)) as.numeric(metareg_summary$ci.ub) else rep(NA_real_, length(beta_values))

    coefficient_matrix <- cbind(
      estimate = beta_values,
      se = se_values,
      pval = p_values,
      ci.lb = ci_lb_values,
      ci.ub = ci_ub_values
    )
    rownames(coefficient_matrix) <- names(beta_values)
  }

  if (is.null(coefficient_matrix)) {
    return(data.frame(
      Covariate = result$metareg_col,
      `Coefficient (β)` = NA_character_,
      `95% CI` = NA_character_,
      `P-value` = NA_character_,
      R2 = if (!is.null(result$metareg$R2)) as.character(result$metareg$R2) else NA_character_,
      check.names = FALSE
    ))
  }

  coefficient_matrix <- as.matrix(coefficient_matrix)
  row_index <- if ("easy_meta_metareg_covariate" %in% rownames(coefficient_matrix)) {
    "easy_meta_metareg_covariate"
  } else if (result$metareg_col %in% rownames(coefficient_matrix)) {
    result$metareg_col
  } else if (nrow(coefficient_matrix) >= 2) {
    rownames(coefficient_matrix)[2]
  } else {
    rownames(coefficient_matrix)[1]
  }

  estimate <- if ("estimate" %in% colnames(coefficient_matrix)) coefficient_matrix[row_index, "estimate"] else coefficient_matrix[row_index, 1]
  p_value <- if ("pval" %in% colnames(coefficient_matrix)) coefficient_matrix[row_index, "pval"] else if (ncol(coefficient_matrix) >= 4) coefficient_matrix[row_index, 4] else NA_real_
  lower_ci <- if ("ci.lb" %in% colnames(coefficient_matrix)) coefficient_matrix[row_index, "ci.lb"] else NA_real_
  upper_ci <- if ("ci.ub" %in% colnames(coefficient_matrix)) coefficient_matrix[row_index, "ci.ub"] else NA_real_

  r2_value <- NA_real_
  if (!is.null(metareg_summary$R2)) {
    r2_value <- suppressWarnings(as.numeric(metareg_summary$R2)[1])
  }
  if (is.na(r2_value) && !is.null(result$metareg$R2)) {
    r2_value <- suppressWarnings(as.numeric(result$metareg$R2)[1])
  }
  if (is.na(r2_value)) {
    r2_value <- extract_named_numeric(
      metareg_summary,
      candidates = c("R2", "r2", "R2 analog", "R2.analog", "R2_analog"),
    )
  }
  if (is.na(r2_value)) {
    r2_value <- extract_named_numeric(
      result$metareg,
      candidates = c("R2", "r2", "R2 analog", "R2.analog", "R2_analog"),
    )
  }

  if (is.na(r2_value)) {
    tau2_null <- suppressWarnings(as.numeric(result$metareg_null$tau2))
    tau2_model <- suppressWarnings(as.numeric(result$metareg$tau2))

    if (length(tau2_null) > 0 && length(tau2_model) > 0 &&
        !is.na(tau2_null[1]) && !is.na(tau2_model[1]) && tau2_null[1] > 0) {
      r2_value <- max(0, (tau2_null[1] - tau2_model[1]) / tau2_null[1]) * 100
    }
  }

  r2_display <- NA_character_
  if (!is.na(r2_value)) {
    r2_display <- if (r2_value <= 1) {
      sprintf("%.1f%%", 100 * r2_value)
    } else {
      sprintf("%.2f%%", r2_value)
    }
  }

  data.frame(
    Covariate = result$metareg_col,
    `Coefficient (β)` = if (is.na(estimate)) NA_character_ else sprintf("%.4f", estimate),
    `95% CI` = if (is.na(lower_ci) || is.na(upper_ci)) NA_character_ else sprintf("%.4f to %.4f", lower_ci, upper_ci),
    `P-value` = if (is.na(p_value)) NA_character_ else format.pval(p_value, digits = 3, eps = 0.001),
    R2 = r2_display,
    check.names = FALSE
  )
}


# ============================================================================
# META-REGRESSION TABLE ON THE BUBBLE PLOT
#
# The bubble plot carries its own statistics table drawn above the panel. These
# helpers pull the numbers out of the fitted model and lay them out, so the
# exported image is readable on its own, without the summary modal.
# ============================================================================

# Pulls one labelled value out of an assembled meta-regression table. The
# coefficient column is matched by prefix because its full name carries the
# moderator, which is only known at run time.
extract_metareg_table_value <- function(table_df, key) {
  if (!is.data.frame(table_df) || nrow(table_df) < 1) {
    return(NA_character_)
  }

  if (key == "Coefficient") {
    hit <- grep("^Coefficient", names(table_df))
    if (length(hit) > 0) {
      return(as.character(table_df[[hit[1]]][1]))
    }
  }

  if (!(key %in% names(table_df))) {
    return(NA_character_)
  }

  as.character(table_df[[key]][1])
}

# Draws the statistics table above a bubble plot, so the exported image can be
# read on its own without the summary modal.
draw_metareg_header_table <- function(table_df) {
  if (!is.data.frame(table_df) || nrow(table_df) < 1 || "Message" %in% names(table_df)) {
    return(invisible(NULL))
  }

  old_xpd <- graphics::par("xpd")
  on.exit(graphics::par(xpd = old_xpd), add = TRUE)

  labels <- c("Covariate", "Coefficient (β)", "95% CI", "P-value", "R2")
  values <- c(
    extract_metareg_table_value(table_df, "Covariate"),
    extract_metareg_table_value(table_df, "Coefficient"),
    extract_metareg_table_value(table_df, "95% CI"),
    extract_metareg_table_value(table_df, "P-value"),
    extract_metareg_table_value(table_df, "R2")
  )

  x_breaks <- c(0, 0.25, 0.5, 0.77, 0.91, 1)
  x_centers <- (x_breaks[-1] + x_breaks[-length(x_breaks)]) / 2

  graphics::plot.new()
  graphics::par(xpd = NA)
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  graphics::rect(0, 0, 1, 1, col = "white", border = NA)
  graphics::rect(0, 0.52, 1, 0.98, col = "#f5f8ff", border = "#d7dfef", lwd = 1.2)
  graphics::rect(0, 0.04, 1, 0.52, col = "white", border = "#d7dfef", lwd = 1.2)

  for (x in x_breaks[-c(1, length(x_breaks))]) {
    graphics::segments(x, 0.04, x, 0.98, col = "#d7dfef", lwd = 1)
  }
  graphics::segments(0, 0.52, 1, 0.52, col = "#d7dfef", lwd = 1)

  graphics::text(
    x = x_centers,
    y = 0.75,
    labels = labels,
    cex = 1.02,
    font = 2,
    col = "#16233f"
  )
  graphics::text(
    x = x_centers,
    y = 0.27,
    labels = ifelse(is.na(values) | values == "", "NA", values),
    cex = 0.98,
    col = "#223456"
  )
}

# Draws a bubble plot with its statistics table above it, so the exported image carries its own numbers.
plot_metareg_with_header <- function(table_df, plot_body) {
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(matrix(1))
    graphics::par(old_par)
  }, add = TRUE)

  has_table <- is.data.frame(table_df) && nrow(table_df) > 0 && !"Message" %in% names(table_df)

  if (has_table) {
    graphics::layout(matrix(c(1, 2), nrow = 2), heights = c(1.15, 5))
    graphics::par(mar = c(0.2, 0.6, 0.2, 0.6))
    draw_metareg_header_table(table_df)
    graphics::par(mar = c(4.6, 4.8, 0.8, 1.2))
  }

  plot_body()
}


## -------------------------------------------------------------------------
## The interface
##
## Assembles the home page, links the CSS and the JS, and declares the inline
## script blocks that easymeta.js relies on. Everything the user sees passes
## through here, including the pages built by the functions above.
## -------------------------------------------------------------------------


# ============================================================================
# USER INTERFACE
#
# The whole app is one page with a hidden tabsetPanel: the home screen and every
# module screen are in the DOM from the start, and the server shows one at a time.
# The inline JavaScript further down handles what Shiny has no input for, mainly
# showing and hiding controls when batch mode is on.
# ============================================================================

ui <- fluidPage(
  tags$head(
    tags$title("MetaVidence"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Manrope:wght@600;700;800&display=swap"
    ),
    tags$link(
      rel = "icon",
      type = "image/svg+xml",
      href = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 48 48'%3E%3Crect x='2' y='2' width='44' height='44' rx='13' fill='%231769e0'/%3E%3Cpath d='M12 16h14' stroke='white' stroke-width='3' stroke-linecap='round'/%3E%3Ccircle cx='19' cy='16' r='3.4' fill='white'/%3E%3Cpath d='M18 24h18' stroke='white' stroke-width='3' stroke-linecap='round'/%3E%3Ccircle cx='29' cy='24' r='3.4' fill='white'/%3E%3Cpath d='M17 34l7-5 7 5-7 5z' fill='white'/%3E%3C/svg%3E"
    ),
    tags$script(HTML("
      // Uma secao do passo 2 cujos campos foram todos escondidos nao deve
      // deixar o cabecalho orfao na tela. E o caso da secao de subgrupo e
      // meta-regressao no modo de desfecho unico: a coluna agora e escolhida
      // no passo 3, dentro do proprio card de resultado.
      function pruneEmptyParameterSections() {
        var sections = document.querySelectorAll('.em-param-section');
        Array.prototype.forEach.call(sections, function(section) {
          var grid = section.querySelector('.em-param-grid');
          if (!grid) return;
          var visivel = Array.prototype.some.call(grid.children, function(child) {
            return child.style.display !== 'none';
          });
          section.style.display = visivel ? '' : 'none';
        });
      }

      Shiny.addCustomMessageHandler('toggleBatchModeUI', function(message) {
        var outcomeEl = document.getElementById(message.outcome_id);
        if (outcomeEl) {
          outcomeEl.style.display = message.batch ? 'none' : '';
        }
        (message.toggle_ids || []).forEach(function(id) {
          var el = document.getElementById(id);
          if (el) {
            el.style.display = message.batch ? 'none' : '';
          }
        });
        (message.batch_only_ids || []).forEach(function(id) {
          var el = document.getElementById(id);
          if (el) {
            el.style.display = message.batch ? '' : 'none';
          }
        });
        pruneEmptyParameterSections();
      });

      Shiny.addCustomMessageHandler('toggleOptionalResultCards', function(message) {
        (message.cards || []).forEach(function(card) {
          var el = document.getElementById(card.id);
          if (el) {
            el.style.display = card.visible ? '' : 'none';
          }
        });
      });

      (function() {
        function markPrepareNotifications() {
          document.querySelectorAll('.prepare-notification').forEach(function(el) {
            var toast = el.closest('.shiny-notification');
            if (toast) {
              toast.classList.add('prepare-notification-shell');
            }
          });
        }

        var observer = new MutationObserver(function() {
          markPrepareNotifications();
        });

        document.addEventListener('DOMContentLoaded', function() {
          markPrepareNotifications();
          observer.observe(document.body, { childList: true, subtree: true });
        });
      })();

      (function() {
        function markStatusErrors() {
          document.querySelectorAll('.status-message').forEach(function(el) {
            var text = (el.textContent || '').toLowerCase();
            var isError =
              text.indexOf('error') !== -1 ||
              text.indexOf('erro') !== -1 ||
              text.indexOf('missing required column') !== -1 ||
              text.indexOf('please fix the dataset') !== -1 ||
              text.indexOf('must be numeric') !== -1 ||
              text.indexOf('cannot be greater') !== -1;
            el.classList.toggle('easy-meta-status-error-box', isError);
          });
        }

        var statusObserver = new MutationObserver(function() {
          markStatusErrors();
        });

        document.addEventListener('DOMContentLoaded', function() {
          markStatusErrors();
          statusObserver.observe(document.body, {
            childList: true,
            characterData: true,
            subtree: true
          });
        });
      })();
    ")),
    tags$link(rel = "stylesheet", href = "assets/easymeta.css?v=54"),
    tags$script(src = "assets/easymeta.js?v=56")
  ),
  tabsetPanel(
    id = "pages",
    type = "hidden",
    tabPanel(
      "home",
      div(
        class = "home-page",
        div(
          class = "home-shell home-split",
          div(
            class = "home-intro",
            div(
              class = "home-brand",
              tags$span(
                class = "home-brand-mark",
                HTML('<svg viewBox="0 0 48 48" xmlns="http://www.w3.org/2000/svg"><defs><linearGradient id="em-logo-grad" x1="0" y1="0" x2="1" y2="1"><stop offset="0" stop-color="#1769e0"/><stop offset="1" stop-color="#5bd1ff"/></linearGradient></defs><rect x="2" y="2" width="44" height="44" rx="13" fill="url(#em-logo-grad)"/><path d="M24 10v28" stroke="rgba(255,255,255,0.4)" stroke-width="2" stroke-linecap="round"/><path d="M12 16h14" stroke="#ffffff" stroke-width="2.6" stroke-linecap="round"/><circle cx="19" cy="16" r="3" fill="#ffffff"/><path d="M18 24h18" stroke="#ffffff" stroke-width="2.6" stroke-linecap="round"/><circle cx="29" cy="24" r="3" fill="#ffffff"/><path d="M17 34l7-5 7 5-7 5z" fill="#ffffff"/></svg>')
              ),
              # HTML() rather than two elements: R indents child tags, and the
              # whitespace would collapse into a visible gap between "Meta" and
              # "Vidence".
              tags$span(class = "home-brand-name", HTML('Meta<span>Vidence</span>'))
            ),
            tags$p(class = "eyebrow", "No-code meta-analysis"),
            tags$h1(HTML('Meta-analysis made <span class="grad-text">simple</span>')),
            tags$p(
              class = "home-sub",
              HTML("Choose your data type on the right and MetaVidence walks you through <strong>three guided steps</strong> up to publication-ready plots and reports.")
            ),
            div(
              class = "home-actions",
              tags$a(
                class = "home-tutorial-link",
                href = METAVIDENCE_TUTORIALS,
                target = "_blank",
                rel = "noopener",
                HTML('See our tutorial pages<span class="home-tutorial-arrow">&rarr;</span>')
              ),
              tags$a(href = METAVIDENCE_CITATION$cite_page, target = "_blank",
                     rel = "noopener", class = "home-cite-link", "How to cite")
            )
          ),
          div(
            class = "module-grid",
            lapply(names(module_choices), function(id) {
              module_card(id, module_choices[[id]])
            })
          )
        )
      )
    ),
    tabPanel("binary", binary_page()),
    tabPanel("continuous", continuous_mean_sd_page()),
    tabPanel("precalculated", precalculated_page()),
    tabPanel("network", network_page()),
    tabPanel("diagnostic", diagnostic_page()),
    tabPanel("network_binary", network_binary_page()),
    tabPanel("network_continuous", network_continuous_page()),
    tabPanel("network_precalc_ci", network_precalc_ci_page()),
    tabPanel("precalc_te_ci", precalc_te_ci_page()),
    tabPanel("precalc_te_sete", precalc_te_sete_page()),
    tabPanel("precalc_te_sete_ci", precalc_te_sete_ci_page()),
    tabPanel("single_arm", single_arm_page()),
    tabPanel("single_mean", single_mean_page()),
    tabPanel("single_proportions", single_proportions_page()),
    tabPanel("diagnostic_single", diagnostic_single_page()),
    tabPanel("diagnostic_comparative", diagnostic_comparative_page()),
  )
)

## -------------------------------------------------------------------------
## The server
##
## Reacts to the clicks: loads data, runs the analysis, draws, exports. The
## analyze_* functions live inside here because they depend on session state,
## which is also why the check scripts in design/ extract them by text before
## calling them.
## -------------------------------------------------------------------------


# ============================================================================
# SERVER
#
# Reads the data, runs the analyses and answers the cards. The shape repeats per
# module: a reactiveVal holding the result of the last Continue, a dynamic reactive
# that re-runs it when the step-3 pickers change, and one observer per button.
# ============================================================================

server <- function(input, output, session) {
  single_prop_data <- reactiveVal(NULL)
  single_prop_multi_data <- reactiveVal(NULL)
  single_prop_status <- reactiveVal("No dataset loaded yet.")
  single_prop_result <- reactiveVal(NULL)
  binary_data <- reactiveVal(NULL)
  binary_multi_data <- reactiveVal(NULL)
  binary_status <- reactiveVal("No dataset loaded yet.")
  binary_result <- reactiveVal(NULL)
  diagnostic_single_data <- reactiveVal(NULL)
  diagnostic_single_status <- reactiveVal("No dataset loaded yet.")
  diagnostic_single_result <- reactiveVal(NULL)
  diagnostic_comparative_data <- reactiveVal(NULL)
  diagnostic_comparative_status <- reactiveVal("No dataset loaded yet.")
  diagnostic_comparative_result <- reactiveVal(NULL)
  network_binary_data <- reactiveVal(NULL)
  network_binary_status <- reactiveVal("No dataset loaded yet.")
  network_binary_result <- reactiveVal(NULL)
  network_cont_data <- reactiveVal(NULL)
  network_cont_status <- reactiveVal("No dataset loaded yet.")
  network_cont_result <- reactiveVal(NULL)
  network_precalc_ci_data <- reactiveVal(NULL)
  network_precalc_ci_status <- reactiveVal("No dataset loaded yet.")
  network_precalc_ci_result <- reactiveVal(NULL)
  cont_mean_data <- reactiveVal(NULL)
  cont_mean_multi_data <- reactiveVal(NULL)
  cont_mean_status <- reactiveVal("No dataset loaded yet.")
  cont_mean_result <- reactiveVal(NULL)
  single_mean_data <- reactiveVal(NULL)
  single_mean_multi_data <- reactiveVal(NULL)
  single_mean_status <- reactiveVal("No dataset loaded yet.")
  single_mean_result <- reactiveVal(NULL)
  precalc_te_ci_data <- reactiveVal(NULL)
  precalc_te_ci_multi_data <- reactiveVal(NULL)
  precalc_te_ci_status <- reactiveVal("No dataset loaded yet.")
  precalc_te_ci_result <- reactiveVal(NULL)
  precalc_te_sete_data <- reactiveVal(NULL)
  precalc_te_sete_multi_data <- reactiveVal(NULL)
  precalc_te_sete_status <- reactiveVal("No dataset loaded yet.")
  precalc_te_sete_result <- reactiveVal(NULL)
  precalc_te_sete_ci_data <- reactiveVal(NULL)
  precalc_te_sete_ci_multi_data <- reactiveVal(NULL)
  precalc_te_sete_ci_status <- reactiveVal("No dataset loaded yet.")
  precalc_te_sete_ci_result <- reactiveVal(NULL)
  batch_export_cache <- reactiveVal(list())
  batch_download_locks <- reactiveVal(list())
  batch_download_ready_signatures <- reactiveVal(list())

  # Built from the data itself. These messages used to be written by hand per
  # module, and every one of them had drifted: none listed Design or RiskOfBias,
  # and the two that gained median columns still advertised only mean and sd.
  example_loaded_message <- function(data) {
    paste0("Example dataset loaded: ", nrow(data), " studies. Columns: ",
           paste(names(data), collapse = ", "), ".")
  }

  # Wires the Data check card and the template downloads for one module. Both
  # read module_data_specs, so a module is described once and the check, the
  # template and the validator cannot disagree about its columns. The card also
  # switches to the per-sheet report on its own when a batch workbook is loaded.
  register_data_support_outputs <- function(prefix, data_fun, multi_fun,
                                            optional_analyses = TRUE, metareg = TRUE,
                                            batch = FALSE) {
    spec <- module_data_specs[[prefix]]
    output[[paste0(prefix, "_data_check")]] <- renderTable({
      multi <- multi_fun()
      if (!is.null(multi)) {
        batch_data_check_summary(multi, spec$required)
      } else {
        data_check_summary(data_fun(), spec$required, optional_analyses, metareg,
                           data_cols = spec$data_cols %||% spec$required)
      }
    }, striped = TRUE, bordered = TRUE, spacing = "m")

    template_id <- paste0("download_", prefix, "_template")
    output[[template_id]] <- downloadHandler(
      filename = function() paste0(prefix, "_template.xlsx"),
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        example_data <- NULL
        if (is.function(spec$example)) {
          example_data <- spec$example()
        }
        write_xlsx_template(file, spec$required, spec$optional, example_data, spec$data_cols)
      }
    )
    # The template button lives inside a tab panel of the rebuilt importer, and
    # only one panel is visible at a time. Shiny suspends hidden outputs, so
    # without this the button would ship with an empty href and be dead on
    # arrival. Keep it even if the button moves between tabs.
    outputOptions(output, template_id, suspendWhenHidden = FALSE)

    if (batch && is.function(spec$example)) {
      batch_template_id <- paste0("download_", prefix, "_batch_template")
      output[[batch_template_id]] <- downloadHandler(
        filename = function() paste0(prefix, "_batch_template.xlsx"),
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        content = function(file) write_xlsx_batch_template(file, spec$example(), spec$batch_outcomes)
      )
      # Same reason as the button above: this one lives in the multi-outcome
      # tab, which stays hidden while the single-outcome tab is open.
      outputOptions(output, batch_template_id, suspendWhenHidden = FALSE)
    }
  }

  register_data_support_outputs("single_prop", single_prop_data, single_prop_multi_data, batch = TRUE)
  register_data_support_outputs("single_mean", single_mean_data, single_mean_multi_data, batch = TRUE)
  register_data_support_outputs("binary", binary_data, binary_multi_data, batch = TRUE)
  register_data_support_outputs("diagnostic_single", diagnostic_single_data, function() NULL, metareg = FALSE)
  register_data_support_outputs("diagnostic_comparative", diagnostic_comparative_data, function() NULL, optional_analyses = FALSE)
  register_data_support_outputs("network_binary", network_binary_data, function() NULL, optional_analyses = FALSE)
  register_data_support_outputs("network_continuous", network_cont_data, function() NULL, optional_analyses = FALSE)
  register_data_support_outputs("network_precalc_ci", network_precalc_ci_data, function() NULL, optional_analyses = FALSE)
  register_data_support_outputs("cont_mean", cont_mean_data, cont_mean_multi_data, batch = TRUE)
  register_data_support_outputs("precalc_te_ci", precalc_te_ci_data, precalc_te_ci_multi_data, batch = TRUE)
  register_data_support_outputs("precalc_te_sete", precalc_te_sete_data, precalc_te_sete_multi_data, batch = TRUE)
  register_data_support_outputs("precalc_te_sete_ci", precalc_te_sete_ci_data, precalc_te_sete_ci_multi_data, batch = TRUE)


  output$network_binary_data_check <- renderTable({
    network_binary_data_check_summary(network_binary_data())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$network_cont_data_check <- renderTable({
    network_continuous_data_check_summary(network_cont_data())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$network_precalc_ci_data_check <- renderTable({
    network_precalc_ci_data_check_summary(network_precalc_ci_data())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  # Which columns the result cards are pointing at right now. Empty string when
  # the module has no picker, or when nothing is selected.
  picked_columns <- function(module_key) {
    read_pick <- function(kind) {
      value <- input[[paste0(module_key, "_", kind, "_pick")]]
      if (is.null(value)) "" else value
    }
    c(read_pick("subgroup"), read_pick("metareg"))
  }

  # Throws away a module's prepared archives. Called whenever the analysis is
  # re-run, since anything built from the previous result is now wrong.
  clear_batch_export_cache <- function(module_key) {
    cache <- batch_export_cache()
    cache[[module_key]] <- NULL
    batch_export_cache(cache)
  }

  # Looks up an already-built archive. The lookup key includes the export settings
  # and the picker columns, so an archive built for other settings is not found
  # and gets rebuilt rather than handed over.
  get_cached_batch_export <- function(module_key, file_format, width, height, plot_label) {
    cache <- batch_export_cache()
    picked <- picked_columns(module_key)
    signature <- batch_export_signature(file_format, width, height, picked[1], picked[2])
    module_cache <- cache[[module_key]]
    if (is.null(module_cache)) return(NULL)
    signature_cache <- module_cache[[signature]]
    if (is.null(signature_cache)) return(NULL)
    signature_cache[[plot_label]]
  }

  # Serves the download. On a cache miss it rebuilds instead of failing, which is
  # deliberate: a stop() inside a downloadHandler reaches the user as an opaque
  # browser error, never as a readable message.
  copy_prepared_batch_export <- function(file, module_key, result, plot_label, file_format, width, height,
                                         plot_function = NULL, require_component = NULL) {
    export_settings <- normalize_export_settings(file_format, width, height)
    prepared_path <- get_cached_batch_export(
      module_key,
      export_settings$file_format,
      export_settings$width,
      export_settings$height,
      plot_label
    )
    # Cache miss: the export settings changed, the analysis was re-run, or the
    # temporary file is gone. Build the ZIP right here instead of failing --
    # stop() inside a downloadHandler reaches the user as an opaque browser
    # "file not available" error, never as a readable message.
    if (is.null(prepared_path) || !file.exists(prepared_path)) {
      if (is.null(plot_function)) {
        stop("Use 1. Run analysis in this result card before downloading this file.")
      }
      prepared_path <- generate_export_artifact(
        result,
        plot_label,
        export_settings$file_format,
        export_settings$width,
        export_settings$height,
        plot_function,
        require_component
      )
    }
    ok <- file.copy(prepared_path, file, overwrite = TRUE)
    if (!ok) {
      stop("Unable to copy the prepared batch export.")
    }
  }

  # Builds the archives behind 1. Run analysis. Specs whose required component is
  # missing from every outcome are skipped, so a subgroup export is not attempted
  # when no subgroup column was chosen.
  prepare_batch_exports <- function(module_key, result, file_format, width, height, plot_specs) {
    if (!is_batch_result(result)) {
      stop("Batch preparation is only needed when multiple outcomes are loaded.")
    }

    export_settings <- normalize_export_settings(file_format, width, height)
    file_format <- export_settings$file_format
    width <- export_settings$width
    height <- export_settings$height
    picked <- picked_columns(module_key)
    signature <- batch_export_signature(file_format, width, height, picked[1], picked[2])

    available_specs <- Filter(function(spec) {
      if (is.null(spec$require_component)) return(TRUE)
      any(vapply(result$outcomes, function(outcome_result) !is.null(outcome_result[[spec$require_component]]), logical(1)))
    }, plot_specs)

    if (length(available_specs) == 0) {
      stop("No batch exports are available for the selected outcomes.")
    }

    clear_batch_export_cache(module_key)
    cache <- batch_export_cache()
    cache[[module_key]] <- list()
    cache[[module_key]][[signature]] <- list()
    batch_export_cache(cache)

    for (i in seq_along(available_specs)) {
      spec <- available_specs[[i]]
      prepared_path <- generate_export_artifact(
        result = result,
        plot_label = spec$label,
        file_format = file_format,
        width = width,
        height = height,
        plot_function = spec$plot_function,
        require_component = spec$require_component
      )
      cache <- batch_export_cache()
      if (is.null(cache[[module_key]])) cache[[module_key]] <- list()
      if (is.null(cache[[module_key]][[signature]])) cache[[module_key]][[signature]] <- list()
      cache[[module_key]][[signature]][[spec$label]] <- prepared_path
      batch_export_cache(cache)
    }
  }

  # One card's worth of the above, which is what a single Run analysis button
  # needs.
  prepare_single_batch_export <- function(module_key, result, file_format, width, height, spec) {
    if (!is_batch_result(result)) {
      stop("Batch preparation is only needed when multiple outcomes are loaded.")
    }
    prepare_batch_exports(module_key, result, file_format, width, height, list(spec))
  }

  # Single-arm proportions. Beyond the main model it fits, when asked, a second
  # one carrying the subgroup variable and a metafor model for the moderator.
  # They are separate objects so the forest plot and the subgroup panel can be
  # drawn independently.
  analyze_single_prop <- function(data, params, outcome_name = NULL) {
    data <- validate_single_prop_data(data)
    subgroup_col <- params$subgroup_col
    metareg_col <- params$metareg_col
    forest_cols <- sanitize_forest_columns(params$forest_cols, data)
    model_choice <- params$model_choice
    model_flags <- resolve_meta_model_flags(model_choice)
    if (nzchar(metareg_col)) {
      if (!requireNamespace("metafor", quietly = TRUE)) {
        stop("The metafor package is required. Install it with install.packages('metafor').")
      }
      data <- prepare_metareg_covariate(data, metareg_col)
    }

    meta_object <- meta::metaprop(
      event = data[["event"]], n = data[["n"]], studlab = data[["study"]], data = data,
      sm = params$sm, method = params$method, method.tau = params$method_tau, method.random.ci = params$method_random_ci %||% "classic", method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
      random = model_flags$random, common = model_flags$common
    )
    meta_object <- attach_forest_columns(meta_object, data, forest_cols)

    subgroup_object <- NULL
    if (nzchar(subgroup_col)) {
      subgroup_object <- meta::metaprop(
        event = data[["event"]], n = data[["n"]], studlab = data[["study"]], data = data,
        sm = params$sm, method = params$method, method.tau = params$method_tau, method.random.ci = params$method_random_ci %||% "classic", method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
        random = model_flags$random, common = model_flags$common, prediction = TRUE,
        subgroup = data[[subgroup_col]]
      )
      subgroup_object <- attach_forest_columns(subgroup_object, data, forest_cols)
    }

    metareg_object <- NULL
    metareg_null_object <- NULL
    metareg_escalc_data <- NULL
    if (nzchar(metareg_col)) {
      escalc_data <- metafor::escalc(measure = "PLO", xi = event, ni = n, data = data)
      metareg_escalc_data <- escalc_data
      metareg_null_object <- metafor::rma(yi, vi, data = escalc_data, method = resolve_rma_method(model_choice, params$method_tau))
      metareg_object <- metafor::rma(yi, vi, mods = ~ easy_meta_metareg_covariate, data = escalc_data, method = resolve_rma_method(model_choice, params$method_tau))
    }

    list(
      outcome_name = if (is.null(outcome_name)) params$outcome_name else outcome_name,
      data = data, meta = meta_object, subgroup = subgroup_object, metareg = metareg_object,
      metareg_null = metareg_null_object, metareg_escalc = metareg_escalc_data,
      subgroup_col = subgroup_col, metareg_col = metareg_col, forest_cols = forest_cols,
      model_choice = model_choice
    )
  }

  # Single-arm means. metamean is handed mean and sd, median and quartiles, and
  # the range, and decides per study which it can use, so one file may mix the
  # shapes. The conversion follows Luo (2018) for the mean and Shi (2020) for
  # the SD, which are the meta defaults.
  analyze_single_mean <- function(data, params, outcome_name = NULL) {
    data <- validate_single_mean_data(data)
    subgroup_col <- params$subgroup_col
    metareg_col <- params$metareg_col
    forest_cols <- sanitize_forest_columns(params$forest_cols, data)
    model_choice <- params$model_choice
    prediction_flag <- params$prediction_flag
    model_flags <- resolve_meta_model_flags(model_choice)
    if (nzchar(metareg_col) && !requireNamespace("metafor", quietly = TRUE)) {
      stop("The metafor package is required. Install it with install.packages('metafor').")
    }

    meta_object <- meta::metamean(
      n = data[["n"]], mean = data[["mean"]], sd = data[["sd"]],
      median = data[["median"]], q1 = data[["q1"]], q3 = data[["q3"]],
      min = data[["min"]], max = data[["max"]],
      data = data, sm = params$sm, studlab = data[["study"]],
      random = model_flags$random, common = model_flags$common,
      method.tau = params$method_tau, method.random.ci = params$method_random_ci %||% "classic", prediction = prediction_flag, method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2
    )

    # metamean writes the converted mean and sd back onto the object. The
    # subgroup model and metafor need real numbers rather than quartiles, so
    # they read them from here instead of from what the user typed.
    data_for_reg <- data
    data_for_reg$mean <- meta_object$mean
    data_for_reg$sd <- meta_object$sd
    if (nzchar(metareg_col)) data_for_reg <- prepare_metareg_covariate(data_for_reg, metareg_col)
    forest_cols <- sanitize_forest_columns(forest_cols, data_for_reg)
    meta_object <- attach_forest_columns(meta_object, data_for_reg, forest_cols)

    subgroup_object <- NULL
    if (nzchar(subgroup_col)) {
      subgroup_object <- meta::metamean(
        n = data_for_reg[["n"]], mean = data_for_reg[["mean"]], sd = data_for_reg[["sd"]],
        data = data_for_reg, sm = params$sm, studlab = data_for_reg[["study"]],
        random = model_flags$random, common = model_flags$common,
        method.tau = params$method_tau, method.random.ci = params$method_random_ci %||% "classic", prediction = prediction_flag, method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
        subgroup = data_for_reg[[subgroup_col]]
      )
      subgroup_object <- attach_forest_columns(subgroup_object, data_for_reg, forest_cols)
    }

    metareg_object <- NULL
    metareg_null_object <- NULL
    metareg_escalc_data <- NULL
    if (nzchar(metareg_col)) {
      escalc_data <- if (identical(params$sm, "MLN")) {
        transform(data_for_reg, yi = log(mean), vi = (sd^2) / (n * mean^2))
      } else {
        transform(data_for_reg, yi = mean, vi = (sd^2) / n)
      }
      metareg_escalc_data <- escalc_data
      metareg_null_object <- metafor::rma(yi, vi, data = escalc_data, method = resolve_rma_method(model_choice, params$method_tau))
      metareg_object <- metafor::rma(yi, vi, mods = ~ easy_meta_metareg_covariate, data = escalc_data, method = resolve_rma_method(model_choice, params$method_tau))
    }

    list(
      outcome_name = if (is.null(outcome_name)) params$outcome_name else outcome_name,
      data = data_for_reg, meta = meta_object, subgroup = subgroup_object, metareg = metareg_object,
      metareg_null = metareg_null_object, metareg_escalc = metareg_escalc_data,
      subgroup_col = subgroup_col, metareg_col = metareg_col, sm = params$sm,
      model_choice = model_choice, prediction_flag = prediction_flag, forest_cols = forest_cols
    )
  }

  # Two-arm binary outcome, same three-model shape.
  analyze_binary <- function(data, params, outcome_name = NULL) {
    data <- validate_binary_data(data)
    subgroup_col <- params$subgroup_col
    metareg_col <- params$metareg_col
    forest_cols <- sanitize_forest_columns(params$forest_cols, data)
    model_choice <- params$model_choice
    prediction_flag <- params$prediction_flag
    model_flags <- resolve_meta_model_flags(model_choice)
    if (nzchar(metareg_col)) {
      if (!requireNamespace("metafor", quietly = TRUE)) {
        stop("The metafor package is required. Install it with install.packages('metafor').")
      }
      data <- prepare_metareg_covariate(data, metareg_col)
    }

    meta_object <- meta::metabin(
      event.e = data[["event.e"]], n.e = data[["n.e"]], event.c = data[["event.c"]], n.c = data[["n.c"]],
      studlab = data[["study"]], data = data, sm = params$sm, method = params$method,
      method.tau = params$method_tau, method.random.ci = params$method_random_ci, method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
      random = model_flags$random, common = model_flags$common, prediction = prediction_flag,
      method.predict = params$method_predict
    )
    meta_object <- attach_forest_columns(meta_object, data, forest_cols)

    subgroup_object <- NULL
    if (nzchar(subgroup_col)) {
      subgroup_object <- meta::metabin(
        event.e = data[["event.e"]], n.e = data[["n.e"]], event.c = data[["event.c"]], n.c = data[["n.c"]],
        studlab = data[["study"]], data = data, sm = params$sm, method = params$method,
        method.tau = params$method_tau, method.random.ci = params$method_random_ci, method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
        random = model_flags$random, common = model_flags$common, prediction = prediction_flag,
        method.predict = params$method_predict, subgroup = data[[subgroup_col]]
      )
      subgroup_object <- attach_forest_columns(subgroup_object, data, forest_cols)
    }

    metareg_object <- NULL
    metareg_null_object <- NULL
    metareg_escalc_data <- NULL
    if (nzchar(metareg_col)) {
      escalc_measure <- switch(params$sm, RR = "RR", OR = "OR", RD = "RD", "RR")
      escalc_data <- metafor::escalc(measure = escalc_measure, ai = event.e, bi = n.e - event.e, ci = event.c, di = n.c - event.c, data = data)
      metareg_escalc_data <- escalc_data
      metareg_null_object <- metafor::rma(yi, vi, data = escalc_data, method = resolve_rma_method(model_choice, params$method_tau))
      metareg_object <- metafor::rma(yi, vi, mods = ~ easy_meta_metareg_covariate, data = escalc_data, method = resolve_rma_method(model_choice, params$method_tau))
    }

    list(
      outcome_name = if (is.null(outcome_name)) params$outcome_name else outcome_name,
      data = data, meta = meta_object, subgroup = subgroup_object, metareg = metareg_object,
      metareg_null = metareg_null_object, metareg_escalc = metareg_escalc_data,
      subgroup_col = subgroup_col, metareg_col = metareg_col, model_choice = model_choice,
      prediction_flag = prediction_flag, sm = params$sm, label_e = params$label_e, label_c = params$label_c,
      label_left = params$label_left, label_right = params$label_right, forest_cols = forest_cols
    )
  }

  # Two-arm continuous outcome, whichever way the studies reported it. metacont
  # receives mean, SD, median, quartiles and range together and picks the scenario
  # per study, so a file mixing the two shapes pools in one model.
  #
  # The converted mean and SD are then read back off the fitted object into
  # data_for_reg. That step is what makes subgroup analysis and meta-regression
  # work for a study that arrived as quartiles: metafor::escalc needs an actual
  # mean and SD, and metacont is the only thing that knows the conversion.
  analyze_cont_mean <- function(data, params, outcome_name = NULL) {
    data <- validate_cont_mean_data(data)
    subgroup_col <- params$subgroup_col
    metareg_col <- params$metareg_col
    forest_cols <- sanitize_forest_columns(params$forest_cols, data)
    model_choice <- params$model_choice
    prediction_flag <- params$prediction_flag
    model_flags <- resolve_meta_model_flags(model_choice)
    if (nzchar(metareg_col) && !requireNamespace("metafor", quietly = TRUE)) {
      stop("The metafor package is required. Install it with install.packages('metafor').")
    }

    meta_object <- meta::metacont(
      mean.e = data[["mean.e"]], sd.e = data[["sd.e"]], n.e = data[["n.e"]],
      median.e = data[["median.e"]], q1.e = data[["q1.e"]], q3.e = data[["q3.e"]], min.e = data[["min.e"]], max.e = data[["max.e"]],
      mean.c = data[["mean.c"]], sd.c = data[["sd.c"]], n.c = data[["n.c"]],
      median.c = data[["median.c"]], q1.c = data[["q1.c"]], q3.c = data[["q3.c"]], min.c = data[["min.c"]], max.c = data[["max.c"]],
      data = data, method.tau = params$method_tau, method.random.ci = params$method_random_ci,
      method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2, prediction = prediction_flag, method.predict = params$method_predict,
      random = model_flags$random, common = model_flags$common, sm = params$sm, studlab = data[["study"]]
    )

    data_for_reg <- data
    data_for_reg$mean.e <- meta_object$mean.e
    data_for_reg$sd.e <- meta_object$sd.e
    data_for_reg$mean.c <- meta_object$mean.c
    data_for_reg$sd.c <- meta_object$sd.c
    if (nzchar(metareg_col)) data_for_reg <- prepare_metareg_covariate(data_for_reg, metareg_col)
    forest_cols <- sanitize_forest_columns(forest_cols, data_for_reg)
    meta_object <- attach_forest_columns(meta_object, data_for_reg, forest_cols)

    subgroup_object <- NULL
    if (nzchar(subgroup_col)) {
      subgroup_object <- meta::metacont(
        mean.e = data_for_reg[["mean.e"]], sd.e = data_for_reg[["sd.e"]], n.e = data_for_reg[["n.e"]],
        mean.c = data_for_reg[["mean.c"]], sd.c = data_for_reg[["sd.c"]], n.c = data_for_reg[["n.c"]],
        data = data_for_reg, method.tau = params$method_tau, method.random.ci = params$method_random_ci,
        method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2, prediction = prediction_flag, method.predict = params$method_predict,
        random = model_flags$random, common = model_flags$common, sm = params$sm,
        studlab = data_for_reg[["study"]], subgroup = data_for_reg[[subgroup_col]]
      )
      subgroup_object <- attach_forest_columns(subgroup_object, data_for_reg, forest_cols)
    }

    metareg_object <- NULL
    metareg_null_object <- NULL
    metareg_escalc_data <- NULL
    if (nzchar(metareg_col)) {
      escalc_measure <- if (identical(params$sm, "SMD")) "SMD" else "MD"
      escalc_data <- metafor::escalc(
        measure = escalc_measure,
        m1i = data_for_reg[["mean.e"]], sd1i = data_for_reg[["sd.e"]], n1i = data_for_reg[["n.e"]],
        m2i = data_for_reg[["mean.c"]], sd2i = data_for_reg[["sd.c"]], n2i = data_for_reg[["n.c"]],
        data = data_for_reg
      )
      metareg_escalc_data <- escalc_data
      metareg_null_object <- metafor::rma(yi, vi, data = escalc_data, method = resolve_rma_method(model_choice, params$method_tau))
      metareg_object <- metafor::rma(yi, vi, mods = ~ easy_meta_metareg_covariate, data = escalc_data, method = resolve_rma_method(model_choice, params$method_tau))
    }

    list(
      outcome_name = if (is.null(outcome_name)) params$outcome_name else outcome_name,
      data = data_for_reg, meta = meta_object, subgroup = subgroup_object, metareg = metareg_object,
      metareg_null = metareg_null_object, metareg_escalc = metareg_escalc_data,
      subgroup_col = subgroup_col, metareg_col = metareg_col, model_choice = model_choice,
      prediction_flag = prediction_flag, sm = params$sm, label_e = params$label_e, label_c = params$label_c,
      label_left = params$label_left, label_right = params$label_right, forest_cols = forest_cols
    )
  }

  # Pre-calculated effect with a confidence interval.
  analyze_precalc_te_ci <- function(data, params, outcome_name = NULL) {
    data <- validate_precalc_te_ci_data(data)
    subgroup_col <- params$subgroup_col
    metareg_col <- params$metareg_col
    forest_cols <- sanitize_forest_columns(params$forest_cols, data)
    model_choice <- params$model_choice
    prediction_flag <- params$prediction_flag
    model_flags <- resolve_meta_model_flags(model_choice)
    ratio_scale <- precalc_sm_is_ratio(params$sm)
    if (nzchar(metareg_col) && !requireNamespace("metafor", quietly = TRUE)) {
      stop("The metafor package is required. Install it with install.packages('metafor').")
    }
    data <- prepare_metareg_covariate(data, metareg_col)
    if (ratio_scale && any(data$TE <= 0 | data$lower <= 0 | data$upper <= 0)) {
      stop("For HR, RR and OR, TE, lower and upper must be positive values on the original ratio scale.")
    }

    meta_object <- meta::metagen(
      TE = data[["TE"]], lower = data[["lower"]], upper = data[["upper"]],
      data = data, studlab = data[["study"]],
      random = model_flags$random, common = model_flags$common,
      sm = params$sm, method.tau = params$method_tau,
      method.random.ci = params$method_random_ci, method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
      prediction = prediction_flag, method.predict = params$method_predict,
      transf = FALSE
    )

    data_for_reg <- data
    data_for_reg$seTE <- meta_object$seTE
    forest_cols <- sanitize_forest_columns(forest_cols, data_for_reg)
    meta_object <- attach_forest_columns(meta_object, data_for_reg, forest_cols)
    subgroup_object <- NULL
    if (nzchar(subgroup_col)) {
      subgroup_object <- meta::metagen(
        TE = data_for_reg[["TE"]], lower = data_for_reg[["lower"]], upper = data_for_reg[["upper"]],
        data = data_for_reg, studlab = data_for_reg[["study"]],
        random = model_flags$random, common = model_flags$common,
        sm = params$sm, method.tau = params$method_tau,
        method.random.ci = params$method_random_ci, method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
        prediction = prediction_flag, method.predict = params$method_predict,
        transf = FALSE, subgroup = data_for_reg[[subgroup_col]]
      )
      subgroup_object <- attach_forest_columns(subgroup_object, data_for_reg, forest_cols)
    }

    metareg_object <- NULL
    metareg_null_object <- NULL
    metareg_df <- NULL
    if (nzchar(metareg_col)) {
      metareg_df <- data.frame(
        yi = meta_object$TE,
        vi = meta_object$seTE^2,
        easy_meta_metareg_covariate = data_for_reg$easy_meta_metareg_covariate
      )
      metareg_null_object <- metafor::rma(yi, vi, data = metareg_df, method = resolve_rma_method(model_choice, params$method_tau))
      metareg_object <- metafor::rma(yi, vi, mods = ~ easy_meta_metareg_covariate, data = metareg_df, method = resolve_rma_method(model_choice, params$method_tau))
    }

    list(
      outcome_name = if (is.null(outcome_name)) params$outcome_name else outcome_name,
      data = data_for_reg, meta = meta_object, subgroup = subgroup_object, metareg = metareg_object,
      metareg_null = metareg_null_object, metareg_df = metareg_df,
      subgroup_col = subgroup_col, metareg_col = metareg_col, model_choice = model_choice,
      prediction_flag = prediction_flag, sm = params$sm, ratio_scale = ratio_scale,
      label_e = params$label_e, label_c = params$label_c, label_left = params$label_left,
      label_right = params$label_right, forest_cols = forest_cols
    )
  }

  # Pre-calculated effect with a standard error.
  analyze_precalc_te_sete <- function(data, params, outcome_name = NULL) {
    data <- validate_precalc_te_sete_data(data)
    subgroup_col <- params$subgroup_col
    metareg_col <- params$metareg_col
    forest_cols <- sanitize_forest_columns(params$forest_cols, data)
    model_choice <- params$model_choice
    prediction_flag <- params$prediction_flag
    model_flags <- resolve_meta_model_flags(model_choice)
    ratio_scale <- precalc_sm_is_ratio(params$sm)
    if (nzchar(metareg_col) && !requireNamespace("metafor", quietly = TRUE)) {
      stop("The metafor package is required. Install it with install.packages('metafor').")
    }
    data <- prepare_metareg_covariate(data, metareg_col)
    if (ratio_scale && any(data$TE <= 0)) {
      stop("For HR, RR and OR, TE must be a positive value on the original ratio scale.")
    }

    ## transf = FALSE says the input is on the ORIGINAL scale, which is what the
    ## validator above enforces for HR, RR and OR. Without it metagen assumes the
    ## values are already log-transformed and exponentiates them again: a
    ## reported HR of 0.76 came out as 2.14, quietly, with no error anywhere.
    ## The two sibling workflows (TE + CI, TE + seTE + CI) always passed it.
    meta_object <- meta::metagen(
      TE = data[["TE"]], seTE = data[["seTE"]],
      data = data, studlab = data[["study"]],
      random = model_flags$random, common = model_flags$common,
      sm = params$sm, method.tau = params$method_tau,
      method.random.ci = params$method_random_ci, method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
      prediction = prediction_flag, method.predict = params$method_predict,
      transf = FALSE
    )

    data_for_reg <- data
    forest_cols <- sanitize_forest_columns(forest_cols, data_for_reg)
    meta_object <- attach_forest_columns(meta_object, data_for_reg, forest_cols)
    subgroup_object <- NULL
    if (nzchar(subgroup_col)) {
      subgroup_object <- meta::metagen(
        TE = data[["TE"]], seTE = data[["seTE"]],
        data = data, studlab = data[["study"]],
        random = model_flags$random, common = model_flags$common,
        sm = params$sm, method.tau = params$method_tau,
        method.random.ci = params$method_random_ci, method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
        prediction = prediction_flag, method.predict = params$method_predict,
        transf = FALSE, subgroup = data[[subgroup_col]]
      )
      subgroup_object <- attach_forest_columns(subgroup_object, data_for_reg, forest_cols)
    }

    metareg_object <- NULL
    metareg_null_object <- NULL
    metareg_df <- NULL
    if (nzchar(metareg_col)) {
      metareg_df <- data.frame(
        yi = meta_object$TE,
        vi = meta_object$seTE^2,
        easy_meta_metareg_covariate = data$easy_meta_metareg_covariate
      )
      metareg_null_object <- metafor::rma(yi, vi, data = metareg_df, method = resolve_rma_method(model_choice, params$method_tau))
      metareg_object <- metafor::rma(yi, vi, mods = ~ easy_meta_metareg_covariate, data = metareg_df, method = resolve_rma_method(model_choice, params$method_tau))
    }

    list(
      outcome_name = if (is.null(outcome_name)) params$outcome_name else outcome_name,
      data = data_for_reg, meta = meta_object, subgroup = subgroup_object, metareg = metareg_object,
      metareg_null = metareg_null_object, metareg_df = metareg_df,
      subgroup_col = subgroup_col, metareg_col = metareg_col, model_choice = model_choice,
      prediction_flag = prediction_flag, sm = params$sm, label_e = params$label_e, label_c = params$label_c, label_left = params$label_left,
      label_right = params$label_right, ratio_scale = ratio_scale, forest_cols = forest_cols
    )
  }

  # Pre-calculated effect with both.
  analyze_precalc_te_sete_ci <- function(data, params, outcome_name = NULL) {
    data <- validate_precalc_te_sete_ci_data(data)
    subgroup_col <- params$subgroup_col
    metareg_col <- params$metareg_col
    forest_cols <- sanitize_forest_columns(params$forest_cols, data)
    model_choice <- params$model_choice
    prediction_flag <- params$prediction_flag
    model_flags <- resolve_meta_model_flags(model_choice)
    ratio_scale <- precalc_sm_is_ratio(params$sm)
    if (nzchar(metareg_col) && !requireNamespace("metafor", quietly = TRUE)) {
      stop("The metafor package is required. Install it with install.packages('metafor').")
    }
    data <- prepare_metareg_covariate(data, metareg_col)
    if (ratio_scale && any(data$TE <= 0 | data$lower <= 0 | data$upper <= 0)) {
      stop("For HR, RR and OR, TE, lower and upper must be positive values on the original ratio scale.")
    }

    meta_object <- meta::metagen(
      TE = data[["TE"]], seTE = data[["seTE"]], lower = data[["lower"]], upper = data[["upper"]],
      data = data, studlab = data[["study"]],
      random = model_flags$random, common = model_flags$common,
      sm = params$sm, method.tau = params$method_tau,
      method.random.ci = params$method_random_ci, method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
      prediction = prediction_flag, method.predict = params$method_predict,
      transf = FALSE
    )
    meta_object <- attach_forest_columns(meta_object, data, forest_cols)

    subgroup_object <- NULL
    if (nzchar(subgroup_col)) {
      subgroup_object <- meta::metagen(
        TE = data[["TE"]], seTE = data[["seTE"]], lower = data[["lower"]], upper = data[["upper"]],
        data = data, studlab = data[["study"]],
        random = model_flags$random, common = model_flags$common,
        sm = params$sm, method.tau = params$method_tau,
        method.random.ci = params$method_random_ci, method.I2 = if (is.null(params$method_i2)) "Q" else params$method_i2,
        prediction = prediction_flag, method.predict = params$method_predict,
        transf = FALSE, subgroup = data[[subgroup_col]]
      )
      subgroup_object <- attach_forest_columns(subgroup_object, data, forest_cols)
    }

    metareg_object <- NULL
    metareg_null_object <- NULL
    metareg_df <- NULL
    if (nzchar(metareg_col)) {
      metareg_df <- data.frame(
        yi = meta_object$TE,
        vi = meta_object$seTE^2,
        easy_meta_metareg_covariate = data$easy_meta_metareg_covariate
      )
      metareg_null_object <- metafor::rma(yi, vi, data = metareg_df, method = resolve_rma_method(model_choice, params$method_tau))
      metareg_object <- metafor::rma(yi, vi, mods = ~ easy_meta_metareg_covariate, data = metareg_df, method = resolve_rma_method(model_choice, params$method_tau))
    }

    list(
      outcome_name = if (is.null(outcome_name)) params$outcome_name else outcome_name,
      data = data, meta = meta_object, subgroup = subgroup_object, metareg = metareg_object,
      metareg_null = metareg_null_object, metareg_df = metareg_df,
      subgroup_col = subgroup_col, metareg_col = metareg_col, model_choice = model_choice,
      prediction_flag = prediction_flag, sm = params$sm, label_e = params$label_e, label_c = params$label_c, label_left = params$label_left,
      label_right = params$label_right, ratio_scale = ratio_scale, forest_cols = forest_cols
    )
  }

  # Runs one module's analyzer once per outcome with the same parameters. A
  # subgroup or moderator column missing from a given sheet, or not numeric in
  # it, is dropped for that outcome alone and the reason is recorded, so one
  # incomplete sheet does not take the whole batch down.
  analyze_outcome_batch <- function(outcomes, analyzer, params) {
    analyzed <- lapply(names(outcomes), function(outcome_name) {
      outcome_data <- outcomes[[outcome_name]]
      local_params <- params
      notes <- character(0)

      requested_subgroup <- if (!is.null(local_params$subgroup_col)) local_params$subgroup_col else ""
      if (nzchar(requested_subgroup)) {
        if (!requested_subgroup %in% names(outcome_data)) {
          local_params$subgroup_col <- ""
          notes <- c(notes, paste0("subgroup column '", requested_subgroup, "' not found"))
        }
      }

      requested_metareg <- if (!is.null(local_params$metareg_col)) local_params$metareg_col else ""
      if (nzchar(requested_metareg)) {
        if (!requested_metareg %in% names(outcome_data)) {
          local_params$metareg_col <- ""
          notes <- c(notes, paste0("metareg column '", requested_metareg, "' not found"))
        } else if (!is.numeric(outcome_data[[requested_metareg]])) {
          local_params$metareg_col <- ""
          notes <- c(notes, paste0("metareg column '", requested_metareg, "' is not numeric"))
        }
      }

      result <- analyzer(outcome_data, local_params, outcome_name = outcome_name)
      result$subgroup_requested <- requested_subgroup
      result$metareg_requested <- requested_metareg
      result$subgroup_used <- nzchar(local_params$subgroup_col)
      result$metareg_used <- nzchar(local_params$metareg_col)
      result$batch_note <- paste(notes, collapse = "; ")
      result
    })
    names(analyzed) <- names(outcomes)
    list(batch = TRUE, outcomes = analyzed)
  }

  # Shows and hides the controls that differ between the two modes. toggle_ids
  # disappear in batch, batch_only_ids appear only in batch, and the outcome name
  # field goes away because the sheet name replaces it.
  set_batch_mode_ui <- function(module_prefix, batch) {
    toggle_ids <- switch(
      module_prefix,
      single_prop = c("preview_single_prop_forest", "summary_single_prop_main", "preview_single_prop_loo", "preview_single_prop_funnel", "summary_single_prop_bias", "preview_single_prop_subgroup", "summary_single_prop_subgroup", "preview_single_prop_metareg", "summary_single_prop_metareg", "single_prop_subgroup_help_single", "single_prop_metareg_help_single"),
      single_mean = c("preview_single_mean_forest", "summary_single_mean_main", "preview_single_mean_loo", "preview_single_mean_funnel", "summary_single_mean_bias", "preview_single_mean_subgroup", "summary_single_mean_subgroup", "preview_single_mean_metareg", "summary_single_mean_metareg", "single_mean_subgroup_help_single", "single_mean_metareg_help_single"),
      binary = c("preview_binary_forest", "summary_binary_main", "preview_binary_loo", "preview_binary_funnel", "summary_binary_bias", "preview_binary_subgroup", "summary_binary_subgroup", "preview_binary_metareg", "summary_binary_metareg", "binary_subgroup_help_single", "binary_metareg_help_single"),
      cont_mean = c("preview_cont_mean_forest", "summary_cont_mean_main", "preview_cont_mean_loo", "preview_cont_mean_funnel", "summary_cont_mean_bias", "preview_cont_mean_subgroup", "summary_cont_mean_subgroup", "preview_cont_mean_metareg", "summary_cont_mean_metareg", "cont_mean_subgroup_help_single", "cont_mean_metareg_help_single"),
      precalc_te_ci = c("preview_precalc_te_ci_forest", "summary_precalc_te_ci_main", "preview_precalc_te_ci_loo", "preview_precalc_te_ci_funnel", "summary_precalc_te_ci_bias", "preview_precalc_te_ci_subgroup", "summary_precalc_te_ci_subgroup", "preview_precalc_te_ci_metareg", "summary_precalc_te_ci_metareg", "precalc_te_ci_subgroup_help_single", "precalc_te_ci_metareg_help_single"),
      precalc_te_sete = c("preview_precalc_te_sete_forest", "summary_precalc_te_sete_main", "preview_precalc_te_sete_loo", "preview_precalc_te_sete_funnel", "summary_precalc_te_sete_bias", "preview_precalc_te_sete_subgroup", "summary_precalc_te_sete_subgroup", "preview_precalc_te_sete_metareg", "summary_precalc_te_sete_metareg", "precalc_te_sete_subgroup_help_single", "precalc_te_sete_metareg_help_single"),
      precalc_te_sete_ci = c("preview_precalc_te_sete_ci_forest", "summary_precalc_te_sete_ci_main", "preview_precalc_te_sete_ci_loo", "preview_precalc_te_sete_ci_funnel", "summary_precalc_te_sete_ci_bias", "preview_precalc_te_sete_ci_subgroup", "summary_precalc_te_sete_ci_subgroup", "preview_precalc_te_sete_ci_metareg", "summary_precalc_te_sete_ci_metareg", "precalc_te_sete_ci_subgroup_help_single", "precalc_te_sete_ci_metareg_help_single"),
      character(0)
    )

    batch_only_ids <- switch(
      module_prefix,
      single_prop = c("prepare_single_prop_forest", "prepare_single_prop_loo", "prepare_single_prop_funnel", "prepare_single_prop_subgroup", "prepare_single_prop_metareg", "single_prop_subgroup_help_batch", "single_prop_metareg_help_batch"),
      single_mean = c("prepare_single_mean_forest", "prepare_single_mean_loo", "prepare_single_mean_funnel", "prepare_single_mean_subgroup", "prepare_single_mean_metareg", "single_mean_subgroup_help_batch", "single_mean_metareg_help_batch"),
      binary = c("prepare_binary_forest", "prepare_binary_loo", "prepare_binary_funnel", "prepare_binary_subgroup", "prepare_binary_metareg", "binary_subgroup_help_batch", "binary_metareg_help_batch"),
      cont_mean = c("prepare_cont_mean_forest", "prepare_cont_mean_loo", "prepare_cont_mean_funnel", "prepare_cont_mean_subgroup", "prepare_cont_mean_metareg", "cont_mean_subgroup_help_batch", "cont_mean_metareg_help_batch"),
      precalc_te_ci = c("prepare_precalc_te_ci_forest", "prepare_precalc_te_ci_loo", "prepare_precalc_te_ci_funnel", "prepare_precalc_te_ci_subgroup", "prepare_precalc_te_ci_metareg", "precalc_te_ci_subgroup_help_batch", "precalc_te_ci_metareg_help_batch"),
      precalc_te_sete = c("prepare_precalc_te_sete_forest", "prepare_precalc_te_sete_loo", "prepare_precalc_te_sete_funnel", "prepare_precalc_te_sete_subgroup", "prepare_precalc_te_sete_metareg", "precalc_te_sete_subgroup_help_batch", "precalc_te_sete_metareg_help_batch"),
      precalc_te_sete_ci = c("prepare_precalc_te_sete_ci_forest", "prepare_precalc_te_sete_ci_loo", "prepare_precalc_te_sete_ci_funnel", "prepare_precalc_te_sete_ci_subgroup", "prepare_precalc_te_sete_ci_metareg", "precalc_te_sete_ci_subgroup_help_batch", "precalc_te_sete_ci_metareg_help_batch"),
      character(0)
    )

    session$sendCustomMessage("toggleBatchModeUI", list(
      outcome_id = paste0(module_prefix, "_outcome_wrap"),
      toggle_ids = toggle_ids,
      batch_only_ids = batch_only_ids,
      batch = isTRUE(batch)
    ))
    if (isTRUE(batch)) {
      set_download_buttons_state(module_prefix, TRUE, batch = TRUE)
    } else {
      set_download_buttons_state(module_prefix, FALSE, batch = FALSE)
    }
  }

  # Every download this module owns.
  get_download_ids_for_module <- function(module_prefix) {
    switch(
      module_prefix,
      single_prop = c("download_single_prop_forest", "download_single_prop_loo", "download_single_prop_funnel", "download_single_prop_subgroup", "download_single_prop_metareg"),
      single_mean = c("download_single_mean_forest", "download_single_mean_loo", "download_single_mean_funnel", "download_single_mean_subgroup", "download_single_mean_metareg"),
      binary = c("download_binary_forest", "download_binary_loo", "download_binary_funnel", "download_binary_subgroup", "download_binary_metareg"),
      cont_mean = c("download_cont_mean_forest", "download_cont_mean_loo", "download_cont_mean_funnel", "download_cont_mean_subgroup", "download_cont_mean_metareg"),
      precalc_te_ci = c("download_precalc_te_ci_forest", "download_precalc_te_ci_loo", "download_precalc_te_ci_funnel", "download_precalc_te_ci_subgroup", "download_precalc_te_ci_metareg"),
      precalc_te_sete = c("download_precalc_te_sete_forest", "download_precalc_te_sete_loo", "download_precalc_te_sete_funnel", "download_precalc_te_sete_subgroup", "download_precalc_te_sete_metareg"),
      precalc_te_sete_ci = c("download_precalc_te_sete_ci_forest", "download_precalc_te_sete_ci_loo", "download_precalc_te_sete_ci_funnel", "download_precalc_te_sete_ci_subgroup", "download_precalc_te_sete_ci_metareg"),
      character(0)
    )
  }

  # Whether a download is still waiting for its files. The result card reads this
  # to decide between a real download button and a plain 2. Download locked.
  is_download_locked <- function(id) {
    locks <- batch_download_locks()
    isTRUE(locks[[id]])
  }

  # Records that decision for a set of downloads.
  set_download_lock_state <- function(ids, locked) {
    if (length(ids) == 0) return(invisible(NULL))
    locks <- batch_download_locks()
    for (id in ids) {
      locks[[id]] <- isTRUE(locked)
    }
    batch_download_locks(locks)
  }

  # Stamps a prepared download with the settings it was built for.
  set_download_ready_signature <- function(id, signature = NULL) {
    ready <- batch_download_ready_signatures()
    ready[[id]] <- signature
    batch_download_ready_signatures(ready)
  }

  # Reads that stamp back, so a change of settings can be detected.
  get_download_ready_signature <- function(id) {
    ready <- batch_download_ready_signatures()
    ready[[id]]
  }

  # Locks or unlocks a set of downloads and stamps them in one step, so the lock
  # and the signature can never drift apart.
  set_download_buttons_state <- function(module_prefix, disabled, ids = NULL, batch = TRUE, ready_signature = NULL) {
    target_ids <- if (is.null(ids)) get_download_ids_for_module(module_prefix) else ids
    if (length(target_ids) == 0) return(invisible(NULL))
    if (isTRUE(batch)) {
      for (id in target_ids) {
        set_download_ready_signature(id, if (isTRUE(disabled)) NULL else ready_signature)
      }
    } else {
      for (id in target_ids) {
        set_download_ready_signature(id, NULL)
      }
    }
    set_download_lock_state(target_ids, isTRUE(batch) && isTRUE(disabled))
  }

  # Shows the subgroup and meta-regression cards only when the data offers a
  # column they could use.
  set_optional_results_ui <- function(module_prefix, subgroup_visible, metareg_visible) {
    ids <- switch(
      module_prefix,
      single_prop = list(subgroup = "single_prop_subgroup_card", metareg = "single_prop_metareg_card"),
      single_mean = list(subgroup = "single_mean_subgroup_card", metareg = "single_mean_metareg_card"),
      binary = list(subgroup = "binary_subgroup_card", metareg = "binary_metareg_card"),
      cont_mean = list(subgroup = "cont_mean_subgroup_card", metareg = "cont_mean_metareg_card"),
      precalc_te_ci = list(subgroup = "precalc_te_ci_subgroup_card", metareg = "precalc_te_ci_metareg_card"),
      precalc_te_sete = list(subgroup = "precalc_te_sete_subgroup_card", metareg = "precalc_te_sete_metareg_card"),
      precalc_te_sete_ci = list(subgroup = "precalc_te_sete_ci_subgroup_card", metareg = "precalc_te_sete_ci_metareg_card"),
      NULL
    )
    if (is.null(ids)) return(invisible(NULL))

    session$sendCustomMessage("toggleOptionalResultCards", list(
      cards = list(
        list(id = ids$subgroup, visible = isTRUE(subgroup_visible)),
        list(id = ids$metareg, visible = isTRUE(metareg_visible))
      )
    ))
  }

  # The message shown while a file is being prepared, so a slow export does not
  # look like a dead button.
  register_download_notifications <- function(ids) {
    lapply(ids, function(id) {
      observeEvent(input[[id]], {
        showNotification("Preparing files... the save dialog will appear when the export is ready.", type = "message", duration = 6)
      }, ignoreInit = TRUE)
    })
  }

  # Renders each download as either a working button or the locked placeholder,
  # and answers a click on the placeholder with an explanation.
  register_download_gates <- function(ids) {
    lapply(ids, function(id) {
      output[[paste0(id, "_gate")]] <- renderUI({
        if (is_download_locked(id)) {
          actionButton(
            paste0(id, "_locked"),
            "2. Download locked",
            class = "result-action locked-download-action",
            title = "Click 1. Run analysis before downloading this batch export."
          )
        } else {
          downloadButton(id, "2. Download", class = "result-action")
        }
      })

      observeEvent(input[[paste0(id, "_locked")]], {
        showNotification("This batch export is not ready yet. Click 1. Run analysis in this card first.", type = "message", duration = 5)
      }, ignoreInit = TRUE)
    })
  }

  # Format, width and height as typed in one card.
  get_card_export_settings <- function(prefix) {
    list(
      file_format = input[[paste0(prefix, "_format")]],
      width = input[[paste0(prefix, "_width")]],
      height = input[[paste0(prefix, "_height")]]
    )
  }

  # Wires 1. Run analysis. It reads the dynamic result, so the archive follows the
  # column currently chosen in the card, builds the files, then unlocks the
  # download and stamps it with the settings used.
  register_batch_prepare_button <- function(button_id, download_id, result_fun, module_key, format_fun, width_fun, height_fun, spec) {
    observeEvent(input[[button_id]], {
      result <- result_fun()
      if (is.null(result)) {
        showNotification("Run the analysis before preparing this export.", type = "message", duration = 6)
        return()
      }
      if (!is_batch_result(result)) {
        showNotification("This prepare step is only needed when multiple outcomes are loaded.", type = "message", duration = 6)
        return()
      }
      prep_notification_id <- paste0("prep_", button_id)
      showNotification(
        ui = tags$div(
          class = "prepare-notification",
          tags$span(class = "prepare-notification-spinner"),
          tags$span(paste("Running analysis for", spec$label, "..."))
        ),
        duration = NULL,
        closeButton = FALSE,
        id = prep_notification_id,
        type = "message"
      )
      tryCatch({
        export_settings <- normalize_export_settings(format_fun(), width_fun(), height_fun())
        prepare_single_batch_export(module_key, result, export_settings$file_format, export_settings$width, export_settings$height, spec)
        picked <- picked_columns(module_key)
        ready_signature <- batch_export_signature(export_settings$file_format, export_settings$width, export_settings$height, picked[1], picked[2])
        set_download_buttons_state(module_key, FALSE, ids = download_id, batch = TRUE, ready_signature = ready_signature)
        removeNotification(prep_notification_id)
        showNotification(paste(spec$label, "is ready to download."), type = "message", duration = 6)
      }, error = function(error) {
        removeNotification(prep_notification_id)
        showNotification(paste("Batch export error:", error$message), type = "error", duration = 8)
      })
    }, ignoreInit = TRUE)
  }

  # Re-locks a download as soon as what it was built for stops matching: a changed
  # format, size, or picker column. It reads the raw result rather than the
  # dynamic one on purpose, because an observe() is eager and reading the dynamic
  # result here would refit the whole batch on every keystroke.
  register_batch_export_input_watchers <- function(prefix, result_fun, module_key, download_id) {
    observe({
      input[[paste0(prefix, "_format")]]
      input[[paste0(prefix, "_width")]]
      input[[paste0(prefix, "_height")]]
      result <- result_fun()
      if (!is.null(result) && is_batch_result(result)) {
        settings <- normalize_export_settings(
          input[[paste0(prefix, "_format")]],
          input[[paste0(prefix, "_width")]],
          input[[paste0(prefix, "_height")]]
        )
        picked <- picked_columns(module_key)
        current_signature <- batch_export_signature(settings$file_format, settings$width, settings$height, picked[1], picked[2])
        ready_signature <- get_download_ready_signature(download_id)
        if (is.null(ready_signature) || !identical(ready_signature, current_signature)) {
          set_download_buttons_state(module_key, TRUE, ids = download_id, batch = TRUE)
        }
      }
    })
  }

  batch_plot_download_ids <- c(
    "download_single_prop_forest", "download_single_prop_loo", "download_single_prop_funnel", "download_single_prop_subgroup", "download_single_prop_metareg",
    "download_single_mean_forest", "download_single_mean_loo", "download_single_mean_funnel", "download_single_mean_subgroup", "download_single_mean_metareg",
    "download_binary_forest", "download_binary_loo", "download_binary_funnel", "download_binary_subgroup", "download_binary_metareg",
    "download_cont_mean_forest", "download_cont_mean_loo", "download_cont_mean_funnel", "download_cont_mean_subgroup", "download_cont_mean_metareg",
    "download_precalc_te_ci_forest", "download_precalc_te_ci_loo", "download_precalc_te_ci_funnel", "download_precalc_te_ci_subgroup", "download_precalc_te_ci_metareg",
    "download_precalc_te_sete_forest", "download_precalc_te_sete_loo", "download_precalc_te_sete_funnel", "download_precalc_te_sete_subgroup", "download_precalc_te_sete_metareg",
    "download_precalc_te_sete_ci_forest", "download_precalc_te_sete_ci_loo", "download_precalc_te_sete_ci_funnel", "download_precalc_te_sete_ci_subgroup", "download_precalc_te_sete_ci_metareg"
  )

  register_download_gates(batch_plot_download_ids)
  register_download_notifications(batch_plot_download_ids)

  register_batch_prepare_button("prepare_single_prop_forest", "download_single_prop_forest", function() single_prop_result_dyn(), "single_prop", function() input$single_prop_forest_format, function() input$single_prop_forest_width, function() input$single_prop_forest_height, list(label = "Forestplot", plot_function = function(x) plot_single_prop_forest(x, input$single_prop_col_square, input$single_prop_col_square_lines, identical(input$single_prop_forest_sort, "yes")), require_component = NULL))
  register_batch_prepare_button("prepare_single_prop_loo", "download_single_prop_loo", function() single_prop_result_dyn(), "single_prop", function() input$single_prop_loo_format, function() input$single_prop_loo_width, function() input$single_prop_loo_height, list(label = "Leave-one-out", plot_function = function(x) plot_single_prop_loo(x, input$single_prop_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_single_prop_funnel", "download_single_prop_funnel", function() single_prop_result_dyn(), "single_prop", function() input$single_prop_funnel_format, function() input$single_prop_funnel_width, function() input$single_prop_funnel_height, list(label = "FunnelPlot", plot_function = function(x) plot_single_prop_funnel(x, input$single_prop_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_single_prop_subgroup", "download_single_prop_subgroup", function() single_prop_result_dyn(), "single_prop", function() input$single_prop_subgroup_format, function() input$single_prop_subgroup_width, function() input$single_prop_subgroup_height, list(label = "Subgroup", plot_function = function(x) plot_single_prop_subgroup(x, input$single_prop_col_square, input$single_prop_col_square_lines, identical(input$single_prop_forest_sort, "yes")), require_component = "subgroup"))
  register_batch_prepare_button("prepare_single_prop_metareg", "download_single_prop_metareg", function() single_prop_result_dyn(), "single_prop", function() input$single_prop_metareg_format, function() input$single_prop_metareg_width, function() input$single_prop_metareg_height, list(label = "Metarregression", plot_function = function(x) plot_single_prop_metareg(x, input$single_prop_col_square), require_component = "metareg"))

  register_batch_prepare_button("prepare_single_mean_forest", "download_single_mean_forest", function() single_mean_result_dyn(), "single_mean", function() input$single_mean_forest_format, function() input$single_mean_forest_width, function() input$single_mean_forest_height, list(label = "Forestplot", plot_function = function(x) plot_single_mean_forest(x, input$single_mean_col_square, input$single_mean_col_square_lines, identical(input$single_mean_forest_sort, "yes")), require_component = NULL))
  register_batch_prepare_button("prepare_single_mean_loo", "download_single_mean_loo", function() single_mean_result_dyn(), "single_mean", function() input$single_mean_loo_format, function() input$single_mean_loo_width, function() input$single_mean_loo_height, list(label = "Leave_one_out", plot_function = function(x) plot_single_mean_loo(x, input$single_mean_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_single_mean_funnel", "download_single_mean_funnel", function() single_mean_result_dyn(), "single_mean", function() input$single_mean_funnel_format, function() input$single_mean_funnel_width, function() input$single_mean_funnel_height, list(label = "FunnelPlot", plot_function = function(x) plot_single_mean_funnel(x, input$single_mean_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_single_mean_subgroup", "download_single_mean_subgroup", function() single_mean_result_dyn(), "single_mean", function() input$single_mean_subgroup_format, function() input$single_mean_subgroup_width, function() input$single_mean_subgroup_height, list(label = "Subgroup", plot_function = function(x) plot_single_mean_subgroup(x, input$single_mean_col_square, input$single_mean_col_square_lines, identical(input$single_mean_forest_sort, "yes")), require_component = "subgroup"))
  register_batch_prepare_button("prepare_single_mean_metareg", "download_single_mean_metareg", function() single_mean_result_dyn(), "single_mean", function() input$single_mean_metareg_format, function() input$single_mean_metareg_width, function() input$single_mean_metareg_height, list(label = "Metarregression", plot_function = function(x) plot_single_mean_metareg(x, input$single_mean_col_square), require_component = "metareg"))

  register_batch_prepare_button("prepare_binary_forest", "download_binary_forest", function() binary_result_dyn(), "binary", function() input$binary_forest_format, function() input$binary_forest_width, function() input$binary_forest_height, list(label = "Forestplot", plot_function = function(x) plot_binary_forest(x, input$binary_col_square, input$binary_col_square_lines, identical(input$binary_forest_sort, "yes")), require_component = NULL))
  register_batch_prepare_button("prepare_binary_loo", "download_binary_loo", function() binary_result_dyn(), "binary", function() input$binary_loo_format, function() input$binary_loo_width, function() input$binary_loo_height, list(label = "Leave_one_out", plot_function = function(x) plot_binary_loo(x, input$binary_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_binary_funnel", "download_binary_funnel", function() binary_result_dyn(), "binary", function() input$binary_funnel_format, function() input$binary_funnel_width, function() input$binary_funnel_height, list(label = "FunnelPlot", plot_function = function(x) plot_binary_funnel(x, input$binary_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_binary_subgroup", "download_binary_subgroup", function() binary_result_dyn(), "binary", function() input$binary_subgroup_format, function() input$binary_subgroup_width, function() input$binary_subgroup_height, list(label = "Subgroup", plot_function = function(x) plot_binary_subgroup(x, input$binary_col_square, input$binary_col_square_lines, identical(input$binary_forest_sort, "yes")), require_component = "subgroup"))
  register_batch_prepare_button("prepare_binary_metareg", "download_binary_metareg", function() binary_result_dyn(), "binary", function() input$binary_metareg_format, function() input$binary_metareg_width, function() input$binary_metareg_height, list(label = "Metarregression", plot_function = function(x) plot_binary_metareg(x, input$binary_col_square), require_component = "metareg"))

  register_batch_prepare_button("prepare_cont_mean_forest", "download_cont_mean_forest", function() cont_mean_result_dyn(), "cont_mean", function() input$cont_mean_forest_format, function() input$cont_mean_forest_width, function() input$cont_mean_forest_height, list(label = "Forestplot", plot_function = function(x) plot_cont_mean_forest(x, input$cont_mean_col_square, input$cont_mean_col_square_lines, identical(input$cont_mean_forest_sort, "yes")), require_component = NULL))
  register_batch_prepare_button("prepare_cont_mean_loo", "download_cont_mean_loo", function() cont_mean_result_dyn(), "cont_mean", function() input$cont_mean_loo_format, function() input$cont_mean_loo_width, function() input$cont_mean_loo_height, list(label = "Leave_one_out", plot_function = function(x) plot_cont_mean_loo(x, input$cont_mean_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_cont_mean_funnel", "download_cont_mean_funnel", function() cont_mean_result_dyn(), "cont_mean", function() input$cont_mean_funnel_format, function() input$cont_mean_funnel_width, function() input$cont_mean_funnel_height, list(label = "FunnelPlot", plot_function = function(x) plot_cont_mean_funnel(x, input$cont_mean_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_cont_mean_subgroup", "download_cont_mean_subgroup", function() cont_mean_result_dyn(), "cont_mean", function() input$cont_mean_subgroup_format, function() input$cont_mean_subgroup_width, function() input$cont_mean_subgroup_height, list(label = "Subgroup", plot_function = function(x) plot_cont_mean_subgroup(x, input$cont_mean_col_square, input$cont_mean_col_square_lines, identical(input$cont_mean_forest_sort, "yes")), require_component = "subgroup"))
  register_batch_prepare_button("prepare_cont_mean_metareg", "download_cont_mean_metareg", function() cont_mean_result_dyn(), "cont_mean", function() input$cont_mean_metareg_format, function() input$cont_mean_metareg_width, function() input$cont_mean_metareg_height, list(label = "Metarregression", plot_function = function(x) plot_cont_mean_metareg(x, input$cont_mean_col_square), require_component = "metareg"))

  register_batch_prepare_button("prepare_precalc_te_ci_forest", "download_precalc_te_ci_forest", function() precalc_te_ci_result_dyn(), "precalc_te_ci", function() input$precalc_te_ci_forest_format, function() input$precalc_te_ci_forest_width, function() input$precalc_te_ci_forest_height, list(label = "Forestplot", plot_function = function(x) plot_precalc_te_ci_forest(x, input$precalc_te_ci_col_square, input$precalc_te_ci_col_square_lines, identical(input$precalc_te_ci_forest_sort, "yes")), require_component = NULL))
  register_batch_prepare_button("prepare_precalc_te_ci_loo", "download_precalc_te_ci_loo", function() precalc_te_ci_result_dyn(), "precalc_te_ci", function() input$precalc_te_ci_loo_format, function() input$precalc_te_ci_loo_width, function() input$precalc_te_ci_loo_height, list(label = "Leave_one_out", plot_function = function(x) plot_precalc_te_ci_loo(x, input$precalc_te_ci_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_precalc_te_ci_funnel", "download_precalc_te_ci_funnel", function() precalc_te_ci_result_dyn(), "precalc_te_ci", function() input$precalc_te_ci_funnel_format, function() input$precalc_te_ci_funnel_width, function() input$precalc_te_ci_funnel_height, list(label = "FunnelPlot", plot_function = function(x) plot_precalc_te_ci_funnel(x, input$precalc_te_ci_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_precalc_te_ci_subgroup", "download_precalc_te_ci_subgroup", function() precalc_te_ci_result_dyn(), "precalc_te_ci", function() input$precalc_te_ci_subgroup_format, function() input$precalc_te_ci_subgroup_width, function() input$precalc_te_ci_subgroup_height, list(label = "Subgroup", plot_function = function(x) plot_precalc_te_ci_subgroup(x, input$precalc_te_ci_col_square, input$precalc_te_ci_col_square_lines, identical(input$precalc_te_ci_forest_sort, "yes")), require_component = "subgroup"))
  register_batch_prepare_button("prepare_precalc_te_ci_metareg", "download_precalc_te_ci_metareg", function() precalc_te_ci_result_dyn(), "precalc_te_ci", function() input$precalc_te_ci_metareg_format, function() input$precalc_te_ci_metareg_width, function() input$precalc_te_ci_metareg_height, list(label = "Metarregression", plot_function = function(x) plot_precalc_te_ci_metareg(x, input$precalc_te_ci_col_square), require_component = "metareg"))

  register_batch_prepare_button("prepare_precalc_te_sete_forest", "download_precalc_te_sete_forest", function() precalc_te_sete_result_dyn(), "precalc_te_sete", function() input$precalc_te_sete_forest_format, function() input$precalc_te_sete_forest_width, function() input$precalc_te_sete_forest_height, list(label = "Forestplot", plot_function = function(x) plot_precalc_te_ci_forest(x, input$precalc_te_sete_col_square, input$precalc_te_sete_col_square_lines, identical(input$precalc_te_sete_forest_sort, "yes")), require_component = NULL))
  register_batch_prepare_button("prepare_precalc_te_sete_loo", "download_precalc_te_sete_loo", function() precalc_te_sete_result_dyn(), "precalc_te_sete", function() input$precalc_te_sete_loo_format, function() input$precalc_te_sete_loo_width, function() input$precalc_te_sete_loo_height, list(label = "Leave_one_out", plot_function = function(x) plot_precalc_te_ci_loo(x, input$precalc_te_sete_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_precalc_te_sete_funnel", "download_precalc_te_sete_funnel", function() precalc_te_sete_result_dyn(), "precalc_te_sete", function() input$precalc_te_sete_funnel_format, function() input$precalc_te_sete_funnel_width, function() input$precalc_te_sete_funnel_height, list(label = "FunnelPlot", plot_function = function(x) plot_precalc_te_ci_funnel(x, input$precalc_te_sete_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_precalc_te_sete_subgroup", "download_precalc_te_sete_subgroup", function() precalc_te_sete_result_dyn(), "precalc_te_sete", function() input$precalc_te_sete_subgroup_format, function() input$precalc_te_sete_subgroup_width, function() input$precalc_te_sete_subgroup_height, list(label = "Subgroup", plot_function = function(x) plot_precalc_te_ci_subgroup(x, input$precalc_te_sete_col_square, input$precalc_te_sete_col_square_lines, identical(input$precalc_te_sete_forest_sort, "yes")), require_component = "subgroup"))
  register_batch_prepare_button("prepare_precalc_te_sete_metareg", "download_precalc_te_sete_metareg", function() precalc_te_sete_result_dyn(), "precalc_te_sete", function() input$precalc_te_sete_metareg_format, function() input$precalc_te_sete_metareg_width, function() input$precalc_te_sete_metareg_height, list(label = "Metarregression", plot_function = function(x) plot_precalc_te_ci_metareg(x, input$precalc_te_sete_col_square), require_component = "metareg"))

  register_batch_prepare_button("prepare_precalc_te_sete_ci_forest", "download_precalc_te_sete_ci_forest", function() precalc_te_sete_ci_result_dyn(), "precalc_te_sete_ci", function() input$precalc_te_sete_ci_forest_format, function() input$precalc_te_sete_ci_forest_width, function() input$precalc_te_sete_ci_forest_height, list(label = "Forestplot", plot_function = function(x) plot_precalc_te_ci_forest(x, input$precalc_te_sete_ci_col_square, input$precalc_te_sete_ci_col_square_lines, identical(input$precalc_te_sete_ci_forest_sort, "yes")), require_component = NULL))
  register_batch_prepare_button("prepare_precalc_te_sete_ci_loo", "download_precalc_te_sete_ci_loo", function() precalc_te_sete_ci_result_dyn(), "precalc_te_sete_ci", function() input$precalc_te_sete_ci_loo_format, function() input$precalc_te_sete_ci_loo_width, function() input$precalc_te_sete_ci_loo_height, list(label = "Leave_one_out", plot_function = function(x) plot_precalc_te_ci_loo(x, input$precalc_te_sete_ci_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_precalc_te_sete_ci_funnel", "download_precalc_te_sete_ci_funnel", function() precalc_te_sete_ci_result_dyn(), "precalc_te_sete_ci", function() input$precalc_te_sete_ci_funnel_format, function() input$precalc_te_sete_ci_funnel_width, function() input$precalc_te_sete_ci_funnel_height, list(label = "FunnelPlot", plot_function = function(x) plot_precalc_te_ci_funnel(x, input$precalc_te_sete_ci_col_square), require_component = NULL))
  register_batch_prepare_button("prepare_precalc_te_sete_ci_subgroup", "download_precalc_te_sete_ci_subgroup", function() precalc_te_sete_ci_result_dyn(), "precalc_te_sete_ci", function() input$precalc_te_sete_ci_subgroup_format, function() input$precalc_te_sete_ci_subgroup_width, function() input$precalc_te_sete_ci_subgroup_height, list(label = "Subgroup", plot_function = function(x) plot_precalc_te_ci_subgroup(x, input$precalc_te_sete_ci_col_square, input$precalc_te_sete_ci_col_square_lines, identical(input$precalc_te_sete_ci_forest_sort, "yes")), require_component = "subgroup"))
  register_batch_prepare_button("prepare_precalc_te_sete_ci_metareg", "download_precalc_te_sete_ci_metareg", function() precalc_te_sete_ci_result_dyn(), "precalc_te_sete_ci", function() input$precalc_te_sete_ci_metareg_format, function() input$precalc_te_sete_ci_metareg_width, function() input$precalc_te_sete_ci_metareg_height, list(label = "Metarregression", plot_function = function(x) plot_precalc_te_ci_metareg(x, input$precalc_te_sete_ci_col_square), require_component = "metareg"))

  register_batch_export_input_watchers("single_prop_forest", single_prop_result, "single_prop", "download_single_prop_forest")
  register_batch_export_input_watchers("single_prop_loo", single_prop_result, "single_prop", "download_single_prop_loo")
  register_batch_export_input_watchers("single_prop_funnel", single_prop_result, "single_prop", "download_single_prop_funnel")
  register_batch_export_input_watchers("single_prop_subgroup", single_prop_result, "single_prop", "download_single_prop_subgroup")
  register_batch_export_input_watchers("single_prop_metareg", single_prop_result, "single_prop", "download_single_prop_metareg")
  register_batch_export_input_watchers("single_mean_forest", single_mean_result, "single_mean", "download_single_mean_forest")
  register_batch_export_input_watchers("single_mean_loo", single_mean_result, "single_mean", "download_single_mean_loo")
  register_batch_export_input_watchers("single_mean_funnel", single_mean_result, "single_mean", "download_single_mean_funnel")
  register_batch_export_input_watchers("single_mean_subgroup", single_mean_result, "single_mean", "download_single_mean_subgroup")
  register_batch_export_input_watchers("single_mean_metareg", single_mean_result, "single_mean", "download_single_mean_metareg")
  register_batch_export_input_watchers("binary_forest", binary_result, "binary", "download_binary_forest")
  register_batch_export_input_watchers("binary_loo", binary_result, "binary", "download_binary_loo")
  register_batch_export_input_watchers("binary_funnel", binary_result, "binary", "download_binary_funnel")
  register_batch_export_input_watchers("binary_subgroup", binary_result, "binary", "download_binary_subgroup")
  register_batch_export_input_watchers("binary_metareg", binary_result, "binary", "download_binary_metareg")
  register_batch_export_input_watchers("cont_mean_forest", cont_mean_result, "cont_mean", "download_cont_mean_forest")
  register_batch_export_input_watchers("cont_mean_loo", cont_mean_result, "cont_mean", "download_cont_mean_loo")
  register_batch_export_input_watchers("cont_mean_funnel", cont_mean_result, "cont_mean", "download_cont_mean_funnel")
  register_batch_export_input_watchers("cont_mean_subgroup", cont_mean_result, "cont_mean", "download_cont_mean_subgroup")
  register_batch_export_input_watchers("cont_mean_metareg", cont_mean_result, "cont_mean", "download_cont_mean_metareg")
  register_batch_export_input_watchers("precalc_te_ci_forest", precalc_te_ci_result, "precalc_te_ci", "download_precalc_te_ci_forest")
  register_batch_export_input_watchers("precalc_te_ci_loo", precalc_te_ci_result, "precalc_te_ci", "download_precalc_te_ci_loo")
  register_batch_export_input_watchers("precalc_te_ci_funnel", precalc_te_ci_result, "precalc_te_ci", "download_precalc_te_ci_funnel")
  register_batch_export_input_watchers("precalc_te_ci_subgroup", precalc_te_ci_result, "precalc_te_ci", "download_precalc_te_ci_subgroup")
  register_batch_export_input_watchers("precalc_te_ci_metareg", precalc_te_ci_result, "precalc_te_ci", "download_precalc_te_ci_metareg")
  register_batch_export_input_watchers("precalc_te_sete_forest", precalc_te_sete_result, "precalc_te_sete", "download_precalc_te_sete_forest")
  register_batch_export_input_watchers("precalc_te_sete_loo", precalc_te_sete_result, "precalc_te_sete", "download_precalc_te_sete_loo")
  register_batch_export_input_watchers("precalc_te_sete_funnel", precalc_te_sete_result, "precalc_te_sete", "download_precalc_te_sete_funnel")
  register_batch_export_input_watchers("precalc_te_sete_subgroup", precalc_te_sete_result, "precalc_te_sete", "download_precalc_te_sete_subgroup")
  register_batch_export_input_watchers("precalc_te_sete_metareg", precalc_te_sete_result, "precalc_te_sete", "download_precalc_te_sete_metareg")
  register_batch_export_input_watchers("precalc_te_sete_ci_forest", precalc_te_sete_ci_result, "precalc_te_sete_ci", "download_precalc_te_sete_ci_forest")
  register_batch_export_input_watchers("precalc_te_sete_ci_loo", precalc_te_sete_ci_result, "precalc_te_sete_ci", "download_precalc_te_sete_ci_loo")
  register_batch_export_input_watchers("precalc_te_sete_ci_funnel", precalc_te_sete_ci_result, "precalc_te_sete_ci", "download_precalc_te_sete_ci_funnel")
  register_batch_export_input_watchers("precalc_te_sete_ci_subgroup", precalc_te_sete_ci_result, "precalc_te_sete_ci", "download_precalc_te_sete_ci_subgroup")
  register_batch_export_input_watchers("precalc_te_sete_ci_metareg", precalc_te_sete_ci_result, "precalc_te_sete_ci", "download_precalc_te_sete_ci_metareg")

  observe({
    # Same rule in both modes: the card shows when the spreadsheet offers an
    # extra column, and the picker inside it drives the analysis.
    set_optional_results_ui(
      "single_prop",
      picker_has_choices("single_prop_subgroup_pick"),
      picker_has_choices("single_prop_metareg_pick")
    )
  })

  observe({
    # Same rule in both modes: the card shows when the spreadsheet offers an
    # extra column, and the picker inside it drives the analysis.
    set_optional_results_ui(
      "single_mean",
      picker_has_choices("single_mean_subgroup_pick"),
      picker_has_choices("single_mean_metareg_pick")
    )
  })

  observe({
    # Same rule in both modes: the card shows when the spreadsheet offers an
    # extra column, and the picker inside it drives the analysis.
    set_optional_results_ui(
      "binary",
      picker_has_choices("binary_subgroup_pick"),
      picker_has_choices("binary_metareg_pick")
    )
  })

  observe({
    # Same rule in both modes: the card shows when the spreadsheet offers an
    # extra column, and the picker inside it drives the analysis.
    set_optional_results_ui(
      "cont_mean",
      picker_has_choices("cont_mean_subgroup_pick"),
      picker_has_choices("cont_mean_metareg_pick")
    )
  })

  observe({
    # Same rule in both modes: the card shows when the spreadsheet offers an
    # extra column, and the picker inside it drives the analysis.
    set_optional_results_ui(
      "precalc_te_ci",
      picker_has_choices("precalc_te_ci_subgroup_pick"),
      picker_has_choices("precalc_te_ci_metareg_pick")
    )
  })

  observe({
    # Same rule in both modes: the card shows when the spreadsheet offers an
    # extra column, and the picker inside it drives the analysis.
    set_optional_results_ui(
      "precalc_te_sete",
      picker_has_choices("precalc_te_sete_subgroup_pick"),
      picker_has_choices("precalc_te_sete_metareg_pick")
    )
  })

  observe({
    # Same rule in both modes: the card shows when the spreadsheet offers an
    # extra column, and the picker inside it drives the analysis.
    set_optional_results_ui(
      "precalc_te_sete_ci",
      picker_has_choices("precalc_te_sete_ci_subgroup_pick"),
      picker_has_choices("precalc_te_sete_ci_metareg_pick")
    )
  })

  lapply(names(module_choices), function(id) {
    observeEvent(input[[paste0("go_", id)]], {
      updateTabsetPanel(session, "pages", selected = id)
      if (identical(id, "binary")) {
        updateTabsetPanel(session, "binary_steps", selected = "data")
      }
    })
  })

  lapply(names(module_choices), function(id) {
    observeEvent(input[[paste0("back_home_", id)]], {
      updateTabsetPanel(session, "pages", selected = "home")
    })
  })

  observeEvent(input$go_single_proportions, {
    updateTabsetPanel(session, "pages", selected = "single_proportions")
    updateTabsetPanel(session, "single_prop_steps", selected = "data")
  })

  observeEvent(input$go_single_mean, {
    updateTabsetPanel(session, "pages", selected = "single_mean")
    updateTabsetPanel(session, "single_mean_steps", selected = "data")
  })

  observeEvent(input$go_precalc_te_ci, {
    updateTabsetPanel(session, "pages", selected = "precalc_te_ci")
    updateTabsetPanel(session, "precalc_te_ci_steps", selected = "data")
  })

  observeEvent(input$go_precalc_te_sete, {
    updateTabsetPanel(session, "pages", selected = "precalc_te_sete")
    updateTabsetPanel(session, "precalc_te_sete_steps", selected = "data")
  })

  observeEvent(input$go_precalc_te_sete_ci, {
    updateTabsetPanel(session, "pages", selected = "precalc_te_sete_ci")
    updateTabsetPanel(session, "precalc_te_sete_ci_steps", selected = "data")
  })

  observeEvent(input$go_network_binary, {
    updateTabsetPanel(session, "pages", selected = "network_binary")
    updateTabsetPanel(session, "network_binary_steps", selected = "data")
  })

  observeEvent(input$go_network_continuous, {
    updateTabsetPanel(session, "pages", selected = "network_continuous")
    updateTabsetPanel(session, "network_cont_steps", selected = "data")
  })

  observeEvent(input$go_network_precalculated, {
    updateTabsetPanel(session, "pages", selected = "network_precalc_ci")
    updateTabsetPanel(session, "network_precalc_ci_steps", selected = "data")
  })

  observeEvent(input$go_diagnostic_single, {
    updateTabsetPanel(session, "pages", selected = "diagnostic_single")
    updateTabsetPanel(session, "diagnostic_single_steps", selected = "data")
  })

  observeEvent(input$go_diagnostic_comparative, {
    updateTabsetPanel(session, "pages", selected = "diagnostic_comparative")
    updateTabsetPanel(session, "diagnostic_comparative_steps", selected = "data")
  })

  observeEvent(input$back_single_arm, {
    updateTabsetPanel(session, "pages", selected = "single_arm")
  })

  observeEvent(input$back_single_arm_mean, {
    updateTabsetPanel(session, "pages", selected = "single_arm")
  })

  observeEvent(input$back_network_from_binary, {
    updateTabsetPanel(session, "pages", selected = "network")
  })

  observeEvent(input$back_home_from_network_binary, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_network_from_continuous, {
    updateTabsetPanel(session, "pages", selected = "network")
  })

  observeEvent(input$back_home_from_network_continuous, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_network_from_precalc_ci, {
    updateTabsetPanel(session, "pages", selected = "network")
  })

  observeEvent(input$back_home_from_network_precalc_ci, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_diagnostic_from_single, {
    updateTabsetPanel(session, "pages", selected = "diagnostic")
  })

  observeEvent(input$back_home_from_diagnostic_single, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_diagnostic_from_comparative, {
    updateTabsetPanel(session, "pages", selected = "diagnostic")
  })

  observeEvent(input$back_home_from_diagnostic_comparative, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_home_from_single_prop, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_home_single_mean, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_home_from_single_mean, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_home_from_cont_mean, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_precalculated, {
    updateTabsetPanel(session, "pages", selected = "precalculated")
  })

  observeEvent(input$back_home_from_precalc_te_ci, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_precalculated_sete, {
    updateTabsetPanel(session, "pages", selected = "precalculated")
  })

  observeEvent(input$back_home_from_precalc_te_sete, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_precalculated_sete_ci, {
    updateTabsetPanel(session, "pages", selected = "precalculated")
  })

  observeEvent(input$back_home_from_precalc_te_sete_ci, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  observeEvent(input$back_home_from_binary, {
    updateTabsetPanel(session, "pages", selected = "home")
  })

  output$binary_step_label <- renderText({
    step <- input$binary_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  output$diagnostic_single_step_label <- renderText({
    step <- input$diagnostic_single_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  observeEvent(input$load_diagnostic_single_example, {
    data <- diagnostic_single_example_data()
    diagnostic_single_data(data)
    diagnostic_single_result(NULL)
    diagnostic_single_status("Example diagnostic dataset loaded: 12 studies with study, TP, FP, FN and TN.")
  })

  observeEvent(input$diagnostic_single_file, {
    req(input$diagnostic_single_file)
    tryCatch({
      data <- read_single_prop_table(input$diagnostic_single_file$datapath)
      data <- validate_diagnostic_single_data(data)
      diagnostic_single_data(data)
      diagnostic_single_result(NULL)
      diagnostic_single_status(paste0("Imported diagnostic dataset loaded: ", nrow(data), " studies and ", ncol(data), " columns."))
    }, error = function(error) {
      diagnostic_single_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_diagnostic_single_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$diagnostic_single_paste)
      data <- validate_diagnostic_single_data(data)
      diagnostic_single_data(data)
      diagnostic_single_result(NULL)
      diagnostic_single_status(paste0("Pasted diagnostic dataset loaded: ", nrow(data), " studies and ", ncol(data), " columns."))
    }, error = function(error) {
      diagnostic_single_status(paste("Paste error:", error$message))
    })
  })

  output$diagnostic_single_data_status <- renderText({ diagnostic_single_status() })
  output$diagnostic_single_run_status <- renderText({ diagnostic_single_status() })
  output$diagnostic_single_run_status_results <- renderText({ diagnostic_single_status() })

  observeEvent(input$diagnostic_single_to_params, {
    if (is.null(diagnostic_single_data())) {
      diagnostic_single_status("Please load, import, or paste a diagnostic dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "diagnostic_single_steps", selected = "parameters")
  })

  observeEvent(input$diagnostic_single_back_to_data, {
    updateTabsetPanel(session, "diagnostic_single_steps", selected = "data")
  })

  observeEvent(input$diagnostic_single_back_to_params, {
    updateTabsetPanel(session, "diagnostic_single_steps", selected = "parameters")
  })

  ## The step 3 subgroup selector is refilled with every new dataset. Required
  ## columns (the 2x2 table and the study label) are never candidates.
  observeEvent(diagnostic_single_data(), {
    data <- diagnostic_single_data()
    req(!is.null(data))
    reservadas <- c(module_data_specs$diagnostic_single$required)
    candidatas <- setdiff(names(data), reservadas)
  })

  observeEvent(input$run_diagnostic_single, {
    diagnostic_single_status("Running diagnostic analysis...")
    tryCatch({
      data <- diagnostic_single_data()
      if (is.null(data)) stop("Please load, import, or paste a diagnostic dataset before running the analysis.")
      params <- list(
        outcome_name = input$diagnostic_single_outcome,
        model_choice = if (is.null(input$diagnostic_single_model) || !nzchar(input$diagnostic_single_model)) "random" else input$diagnostic_single_model,
        sm = "PLOGIT",
        method_ci = "CP",
        method_tau = "ML"
      )
      single_run <- analyze_diagnostic_single(data, params)
      single_run$dyn <- list(analyzer = analyze_diagnostic_single, data = data, params = params)
      diagnostic_single_result(single_run)
      diagnostic_single_status("Diagnostic analysis completed. Continue below to preview or export the results you want.")
      updateTabsetPanel(session, "diagnostic_single_steps", selected = "results")
    }, error = function(error) {
      diagnostic_single_result(NULL)
      diagnostic_single_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 10)
    })
  })

  output$diagnostic_single_sensitivity_plot <- renderPlot({
    result <- diagnostic_single_result_dyn()
    validate(need(!is.null(result), "Run the diagnostic analysis to create the sensitivity forest plot."))
    plot_diagnostic_single_sensitivity(result, input$diagnostic_single_col_square, input$diagnostic_single_col_square_lines)
  })

  output$diagnostic_single_specificity_plot <- renderPlot({
    result <- diagnostic_single_result_dyn()
    validate(need(!is.null(result), "Run the diagnostic analysis to create the specificity forest plot."))
    plot_diagnostic_single_specificity(result, input$diagnostic_single_col_square, input$diagnostic_single_col_square_lines)
  })

  output$diagnostic_single_dor_plot <- renderPlot({
    result <- diagnostic_single_result_dyn()
    validate(need(!is.null(result), "Run the diagnostic analysis to create the DOR forest plot."))
    plot_diagnostic_single_dor(result, input$diagnostic_single_col_square, input$diagnostic_single_col_square_lines)
  })

  output$diagnostic_single_sens_summary <- renderPrint({
    result <- diagnostic_single_result_dyn()
    if (is.null(result)) {
      cat("Run the diagnostic analysis to see sensitivity results.")
      return()
    }
    print(summary(result$sensitivity))
  })

  output$diagnostic_single_spec_summary <- renderPrint({
    result <- diagnostic_single_result_dyn()
    if (is.null(result)) {
      cat("Run the diagnostic analysis to see specificity results.")
      return()
    }
    print(summary(result$specificity))
  })

  output$diagnostic_single_dor_summary <- renderPrint({
    result <- diagnostic_single_result_dyn()
    if (is.null(result)) {
      cat("Run the diagnostic analysis to see DOR results.")
      return()
    }
    print(summary(result$dor))
  })

  output$diagnostic_single_bivariate_summary <- renderPrint({
    result <- diagnostic_single_result_dyn()
    if (is.null(result)) {
      cat("Run the diagnostic analysis to see the bivariate model results.")
      return()
    }
    if (is.null(result$bivariate)) {
      cat(result$bivariate_error %||% "Bivariate model unavailable.")
      return()
    }
    cat(paste(diagnostic_bivariate_lines(result$bivariate), collapse = "\n"), "\n\n")
    if (!is.null(result$few_studies_warning)) cat(result$few_studies_warning, "\n\n")
    cat("Model: bivariate binomial GLMM (lme4::glmer), as specified in the",
        "Cochrane Handbook for DTA Reviews v2.0, chapter 10, appendix 5.\n\n")
    print(summary(result$bivariate$fit))
  })

  output$diagnostic_single_threshold_summary <- renderPrint({
    result <- diagnostic_single_result_dyn()
    if (is.null(result)) {
      cat("Run the diagnostic analysis to see threshold-effect results.")
      return()
    }
    cat(paste(diagnostic_single_threshold_text(result), collapse = "\n"))
  })

  observeEvent(input$preview_diagnostic_single_sens, {
    req(diagnostic_single_result_dyn())
    open_single_prop_plot_modal("Sensitivity forest plot preview", "diagnostic_single_sensitivity_plot", "72vh")
  })

  observeEvent(input$preview_diagnostic_single_spec, {
    req(diagnostic_single_result_dyn())
    open_single_prop_plot_modal("Specificity forest plot preview", "diagnostic_single_specificity_plot", "72vh")
  })

  observeEvent(input$preview_diagnostic_single_dor, {
    req(diagnostic_single_result_dyn())
    open_single_prop_plot_modal("Diagnostic odds ratio preview", "diagnostic_single_dor_plot", "72vh")
  })

  observeEvent(input$summary_diagnostic_single_sens, {
    req(diagnostic_single_result_dyn())
    open_single_prop_text_modal("Sensitivity summary", "diagnostic_single_sens_summary")
  })

  observeEvent(input$summary_diagnostic_single_spec, {
    req(diagnostic_single_result_dyn())
    open_single_prop_text_modal("Specificity summary", "diagnostic_single_spec_summary")
  })

  observeEvent(input$summary_diagnostic_single_bivariate, {
    req(diagnostic_single_result_dyn())
    open_single_prop_text_modal("Bivariate model summary", "diagnostic_single_bivariate_summary")
  })

  observeEvent(input$summary_diagnostic_single_dor, {
    req(diagnostic_single_result_dyn())
    open_single_prop_text_modal("Diagnostic odds ratio summary", "diagnostic_single_dor_summary")
  })

  observeEvent(input$summary_diagnostic_single_threshold, {
    req(diagnostic_single_result_dyn())
    open_single_prop_text_modal("Threshold effect and Deeks test", "diagnostic_single_threshold_summary")
  })

  diagnostic_single_download_handler <- function(plot_label, plot_function, settings_prefix) {
    downloadHandler(
      filename = function() {
        result <- diagnostic_single_result_dyn()
        outcome <- if (!is.null(result) && !is.null(result$outcome_name)) result$outcome_name else input$diagnostic_single_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- diagnostic_single_result_dyn()
        if (is.null(result)) stop("Run the diagnostic analysis before downloading plots.")
        settings <- get_card_export_settings(settings_prefix)
        write_plot_export(file, result, plot_label, settings$file_format, settings$width, settings$height, plot_function)
      }
    )
  }

  output$download_diagnostic_single_sens <- diagnostic_single_download_handler(
    "Sensitivity",
    function(result) plot_diagnostic_single_sensitivity(result, input$diagnostic_single_col_square, input$diagnostic_single_col_square_lines),
    "diagnostic_single_sens"
  )
  output$download_diagnostic_single_spec <- diagnostic_single_download_handler(
    "Specificity",
    function(result) plot_diagnostic_single_specificity(result, input$diagnostic_single_col_square, input$diagnostic_single_col_square_lines),
    "diagnostic_single_spec"
  )

  output$download_diagnostic_single_sroc <- diagnostic_single_download_handler("SROC", plot_diagnostic_summary_point, "diagnostic_single_sroc")
  output$download_diagnostic_single_dor <- diagnostic_single_download_handler(
    "Diagnostic_Odds_Ratio",
    function(result) plot_diagnostic_single_dor(result, input$diagnostic_single_col_square, input$diagnostic_single_col_square_lines),
    "diagnostic_single_dor"
  )

  output$diagnostic_comparative_step_label <- renderText({
    step <- input$diagnostic_comparative_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  observeEvent(input$load_diagnostic_comparative_example, {
    data <- diagnostic_comparative_example_data()
    diagnostic_comparative_data(data)
    diagnostic_comparative_result(NULL)
    diagnostic_comparative_status("Example comparative diagnostic dataset loaded: 12 paired studies and 2 tests.")
  })

  observeEvent(input$diagnostic_comparative_file, {
    req(input$diagnostic_comparative_file)
    tryCatch({
      data <- read_single_prop_table(input$diagnostic_comparative_file$datapath)
      data <- validate_diagnostic_comparative_data(data)
      diagnostic_comparative_data(data)
      diagnostic_comparative_result(NULL)
      diagnostic_comparative_status(paste0("Imported comparative diagnostic dataset loaded: ", nrow(data), " rows and ", length(unique(data$test)), " tests."))
    }, error = function(error) {
      diagnostic_comparative_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_diagnostic_comparative_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$diagnostic_comparative_paste)
      data <- validate_diagnostic_comparative_data(data)
      diagnostic_comparative_data(data)
      diagnostic_comparative_result(NULL)
      diagnostic_comparative_status(paste0("Pasted comparative diagnostic dataset loaded: ", nrow(data), " rows and ", length(unique(data$test)), " tests."))
    }, error = function(error) {
      diagnostic_comparative_status(paste("Paste error:", error$message))
    })
  })

  observeEvent(diagnostic_comparative_data(), {
    data <- diagnostic_comparative_data()
    if (is.null(data)) return()
    tests <- unique(as.character(data$test))
    updateSelectInput(session, "diagnostic_comparative_test_a", choices = tests, selected = tests[1])
    updateSelectInput(session, "diagnostic_comparative_test_b", choices = tests, selected = tests[min(2, length(tests))])
  })

  output$diagnostic_comparative_data_status <- renderText({ diagnostic_comparative_status() })
  output$diagnostic_comparative_run_status <- renderText({ diagnostic_comparative_status() })
  output$diagnostic_comparative_run_status_results <- renderText({ diagnostic_comparative_status() })

  observeEvent(input$diagnostic_comparative_to_params, {
    if (is.null(diagnostic_comparative_data())) {
      diagnostic_comparative_status("Please load, import, or paste a comparative diagnostic dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "diagnostic_comparative_steps", selected = "parameters")
  })

  observeEvent(input$diagnostic_comparative_back_to_data, {
    updateTabsetPanel(session, "diagnostic_comparative_steps", selected = "data")
  })

  observeEvent(input$diagnostic_comparative_back_to_params, {
    updateTabsetPanel(session, "diagnostic_comparative_steps", selected = "parameters")
  })

  observeEvent(input$run_diagnostic_comparative, {
    diagnostic_comparative_status("Running comparative diagnostic analysis...")
    tryCatch({
      data <- diagnostic_comparative_data()
      if (is.null(data)) stop("Please load, import, or paste a comparative diagnostic dataset before running the analysis.")
      params <- list(
        outcome_name = input$diagnostic_comparative_outcome,
        test_a = input$diagnostic_comparative_test_a,
        test_b = input$diagnostic_comparative_test_b
      )
      diagnostic_comparative_result(analyze_diagnostic_comparative(data, params))
      diagnostic_comparative_status("Comparative diagnostic analysis completed. Continue below to preview or export the results you want.")
      updateTabsetPanel(session, "diagnostic_comparative_steps", selected = "results")
    }, error = function(error) {
      diagnostic_comparative_result(NULL)
      diagnostic_comparative_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 10)
    })
  })

  output$diagnostic_comparative_sensitivity_plot <- renderPlot({
    result <- diagnostic_comparative_result()
    validate(need(!is.null(result), "Run the comparative diagnostic analysis to create the sensitivity plot."))
    plot_diagnostic_comparative_sensitivity(result, input$diagnostic_comparative_col_square, input$diagnostic_comparative_col_square_lines)
  })

  output$diagnostic_comparative_specificity_plot <- renderPlot({
    result <- diagnostic_comparative_result()
    validate(need(!is.null(result), "Run the comparative diagnostic analysis to create the specificity plot."))
    plot_diagnostic_comparative_specificity(result, input$diagnostic_comparative_col_square, input$diagnostic_comparative_col_square_lines)
  })

  output$diagnostic_comparative_sens_summary <- renderPrint({
    result <- diagnostic_comparative_result()
    if (is.null(result)) {
      cat("Run the comparative diagnostic analysis to see sensitivity results.")
      return()
    }
    print(summary(result$sensitivity))
  })

  output$diagnostic_comparative_spec_summary <- renderPrint({
    result <- diagnostic_comparative_result()
    if (is.null(result)) {
      cat("Run the comparative diagnostic analysis to see specificity results.")
      return()
    }
    print(summary(result$specificity))
  })

  output$diagnostic_comparative_bivariate_summary <- renderPrint({
    result <- diagnostic_comparative_result()
    if (is.null(result)) {
      cat("Run the comparative diagnostic analysis to see the comparative results.")
      return()
    }
    g <- result$groups
    if (is.null(g) || !isTRUE(g$available)) {
      cat(g$error %||% "Comparative bivariate model unavailable.")
      return()
    }
    cat(paste(unlist(lapply(g$levels, function(n)
      diagnostic_bivariate_lines(g$by_level[[n]], n))), collapse = "\n"), "\n\n")
    if (!is.null(result$few_studies_warning)) cat(result$few_studies_warning, "\n\n")
    cat("Both points come from one joint bivariate model with the test as a",
        "covariate, grouped by study, so a paired design keeps its pairing.\n")
  })

  output$diagnostic_comparative_overall_summary <- renderPrint({
    result <- diagnostic_comparative_result()
    if (is.null(result)) {
      cat("Run the comparative diagnostic analysis to see the overall comparison.")
      return()
    }
    cat(paste(diagnostic_comparative_lr_text(result, "overall"), collapse = "\n"))
  })

  output$diagnostic_comparative_sens_test_summary <- renderPrint({
    result <- diagnostic_comparative_result()
    if (is.null(result)) {
      cat("Run the comparative diagnostic analysis to see the sensitivity comparison.")
      return()
    }
    cat(paste(diagnostic_comparative_lr_text(result, "sensitivity"), collapse = "\n"))
  })

  output$diagnostic_comparative_spec_test_summary <- renderPrint({
    result <- diagnostic_comparative_result()
    if (is.null(result)) {
      cat("Run the comparative diagnostic analysis to see the specificity comparison.")
      return()
    }
    cat(paste(diagnostic_comparative_lr_text(result, "specificity"), collapse = "\n"))
  })

  observeEvent(input$preview_diagnostic_comparative_sens, {
    req(diagnostic_comparative_result())
    open_single_prop_plot_modal("Comparative sensitivity preview", "diagnostic_comparative_sensitivity_plot", "72vh")
  })

  observeEvent(input$preview_diagnostic_comparative_spec, {
    req(diagnostic_comparative_result())
    open_single_prop_plot_modal("Comparative specificity preview", "diagnostic_comparative_specificity_plot", "72vh")
  })

  observeEvent(input$summary_diagnostic_comparative_sens, {
    req(diagnostic_comparative_result())
    open_single_prop_text_modal("Comparative sensitivity summary", "diagnostic_comparative_sens_summary")
  })

  observeEvent(input$summary_diagnostic_comparative_spec, {
    req(diagnostic_comparative_result())
    open_single_prop_text_modal("Comparative specificity summary", "diagnostic_comparative_spec_summary")
  })

  observeEvent(input$summary_diagnostic_comparative_bivariate, {
    req(diagnostic_comparative_result())
    open_single_prop_text_modal("Comparative bivariate summary", "diagnostic_comparative_bivariate_summary")
  })

  observeEvent(input$summary_diagnostic_comparative_overall, {
    req(diagnostic_comparative_result())
    open_single_prop_text_modal("Overall comparison", "diagnostic_comparative_overall_summary")
  })

  observeEvent(input$summary_diagnostic_comparative_sens_test, {
    req(diagnostic_comparative_result())
    open_single_prop_text_modal("Sensitivity comparison", "diagnostic_comparative_sens_test_summary")
  })

  observeEvent(input$summary_diagnostic_comparative_spec_test, {
    req(diagnostic_comparative_result())
    open_single_prop_text_modal("Specificity comparison", "diagnostic_comparative_spec_test_summary")
  })

  diagnostic_comparative_download_handler <- function(plot_label, plot_function, settings_prefix) {
    downloadHandler(
      filename = function() {
        result <- diagnostic_comparative_result()
        outcome <- if (!is.null(result) && !is.null(result$outcome_name)) result$outcome_name else input$diagnostic_comparative_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- diagnostic_comparative_result()
        if (is.null(result)) stop("Run the comparative diagnostic analysis before downloading plots.")
        settings <- get_card_export_settings(settings_prefix)
        write_plot_export(file, result, plot_label, settings$file_format, settings$width, settings$height, plot_function)
      }
    )
  }

  output$download_diagnostic_comparative_sens <- diagnostic_comparative_download_handler(
    "Comparative_Sensitivity",
    function(result) plot_diagnostic_comparative_sensitivity(result, input$diagnostic_comparative_col_square, input$diagnostic_comparative_col_square_lines),
    "diagnostic_comparative_sens"
  )
  output$download_diagnostic_comparative_spec <- diagnostic_comparative_download_handler(
    "Comparative_Specificity",
    function(result) plot_diagnostic_comparative_specificity(result, input$diagnostic_comparative_col_square, input$diagnostic_comparative_col_square_lines),
    "diagnostic_comparative_spec"
  )
  output$download_diagnostic_comparative_sroc <- diagnostic_comparative_download_handler(
    "Comparative_SROC",
    function(result) plot_diagnostic_comparative_points(result, input$diagnostic_comparative_col_a, input$diagnostic_comparative_col_b),
    "diagnostic_comparative_sroc"
  )

  output$cont_mean_step_label <- renderText({
    step <- input$cont_mean_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  output$precalc_te_ci_step_label <- renderText({
    step <- input$precalc_te_ci_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  output$precalc_te_sete_step_label <- renderText({
    step <- input$precalc_te_sete_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  output$precalc_te_sete_ci_step_label <- renderText({
    step <- input$precalc_te_sete_ci_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  observeEvent(input$load_cont_mean_example, {
    data <- continuous_mean_example_data()
    cont_mean_data(data)
    cont_mean_multi_data(NULL)
    cont_mean_status(example_loaded_message(data))
  })

  observeEvent(input$cont_mean_file, {
    tryCatch({
      data <- read_single_prop_table(path = input$cont_mean_file$datapath)
      data <- validate_cont_mean_data(data)
      cont_mean_data(data)
      cont_mean_multi_data(NULL)
      cont_mean_status(paste0("Imported dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      cont_mean_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_cont_mean_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$cont_mean_paste)
      data <- validate_cont_mean_data(data)
      cont_mean_data(data)
      cont_mean_multi_data(NULL)
      cont_mean_status(paste0("Pasted dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      cont_mean_status(paste("Paste error:", error$message))
    })
  })

  observeEvent(input$cont_mean_multi_file, {
    tryCatch({
      outcomes <- read_outcome_workbook(input$cont_mean_multi_file$datapath, validate_cont_mean_data)
      cont_mean_multi_data(outcomes)
      cont_mean_data(NULL)
      cont_mean_status(paste0("Multi-outcome workbook loaded: ", length(outcomes), " outcomes detected."))
    }, error = function(error) {
      cont_mean_multi_data(NULL)
      cont_mean_status(paste("Workbook import error:", error$message))
    })
  })

  observeEvent(cont_mean_data(), {
    data <- cont_mean_data()
    req(!is.null(data))
    optional_cols <- setdiff(names(data), CONT_OUTCOME_COLS)
    numeric_optional_cols <- optional_cols[vapply(data[optional_cols], is.numeric, logical(1))]
    remember_picker_choices(session, "cont_mean_subgroup_pick", optional_cols)
    remember_picker_choices(session, "cont_mean_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "cont_mean_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "cont_mean_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "cont_mean_forest_cols", optional_cols)
    cont_mean_result(NULL)
    set_batch_mode_ui("cont_mean", FALSE)
  })

  observeEvent(cont_mean_multi_data(), {
    outcomes <- cont_mean_multi_data()
    req(!is.null(outcomes))
    optional_cols <- common_optional_columns(outcomes, CONT_OUTCOME_COLS)
    numeric_optional_cols <- common_numeric_optional_columns(outcomes, CONT_OUTCOME_COLS)
    remember_picker_choices(session, "cont_mean_subgroup_pick", optional_cols)
    remember_picker_choices(session, "cont_mean_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "cont_mean_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "cont_mean_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "cont_mean_forest_cols", optional_cols)
    cont_mean_result(NULL)
    set_batch_mode_ui("cont_mean", TRUE)
  })

  output$cont_mean_data_status <- renderText({ cont_mean_status() })
  output$cont_mean_multi_status <- renderText({ cont_mean_status() })
  output$cont_mean_run_status <- renderText({ cont_mean_status() })
  output$cont_mean_run_status_results <- renderText({ cont_mean_status() })

  output$cont_mean_preview <- renderTable({
    data <- cont_mean_data()
    if (is.null(data)) return(data.frame(Message = "Load the example dataset, import a file, or paste data to preview it."))
    head(data, 10)
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  observeEvent(input$cont_mean_to_params, {
    if (is.null(cont_mean_data()) && is.null(cont_mean_multi_data())) {
      cont_mean_status("Please load, import, or paste a dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "cont_mean_steps", selected = "parameters")
  })

  observeEvent(input$cont_mean_back_to_data, {
    updateTabsetPanel(session, "cont_mean_steps", selected = "data")
  })

  observeEvent(input$cont_mean_back_to_params, {
    updateTabsetPanel(session, "cont_mean_steps", selected = "parameters")
  })

  observeEvent(input$run_cont_mean, {
    clear_batch_export_cache("cont_mean")
    if (!is.null(cont_mean_multi_data())) set_download_buttons_state("cont_mean", TRUE, batch = TRUE)
    cont_mean_status("Running analysis...")
    tryCatch({
      if (!requireNamespace("meta", quietly = TRUE)) {
        stop("The meta package is required. Install it with install.packages('meta').")
      }
      comparison_labels <- resolve_comparison_labels(input$cont_mean_label_e, input$cont_mean_label_c, input$cont_mean_outcome_direction)
      params <- list(
        outcome_name = input$cont_mean_outcome,
        # Both columns come from the step-3 pickers, applied by dynamic_result_reactive().
        subgroup_col = "",
        metareg_col = "",
        model_choice = if (is.null(input$cont_mean_model) || !nzchar(input$cont_mean_model)) "random" else input$cont_mean_model,
        prediction_flag = identical(input$cont_mean_prediction, "yes"),
        sm = input$cont_mean_sm,
        method_tau = input$cont_mean_method_tau,
        method_i2 = if (is.null(input$cont_mean_method_i2)) "Q" else input$cont_mean_method_i2,
        method_random_ci = input$cont_mean_method_random_ci,
        method_predict = input$cont_mean_method_predict,
        label_e = comparison_labels$label_e,
        label_c = comparison_labels$label_c,
        label_left = comparison_labels$label_left,
        label_right = comparison_labels$label_right,
        forest_cols = sanitize_forest_columns(input$cont_mean_forest_cols)
      )

      if (!is.null(cont_mean_multi_data())) {
        result <- analyze_outcome_batch(cont_mean_multi_data(), analyze_cont_mean, params)
        # Same dyn contract as the single-outcome path, so the step-3 pickers can
        # re-run every outcome with a different column.
        result$dyn <- list(analyzer = analyze_cont_mean, outcomes = cont_mean_multi_data(), params = params)
        cont_mean_result(result)
        cont_mean_status("Continue below to preview or export the results you want.")
      } else {
        data <- cont_mean_data()
        if (is.null(data)) stop("Please load, import, or paste a dataset before running the analysis.")
        single_run <- analyze_cont_mean(data, params)
        single_run$dyn <- list(analyzer = analyze_cont_mean, data = data, params = params)
        cont_mean_result(single_run)
        cont_mean_status("Continue below to preview or export the results you want.")
      }
      updateTabsetPanel(session, "cont_mean_steps", selected = "results")
    }, error = function(error) {
      cont_mean_result(NULL)
      cont_mean_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 10)
    })
  })

  observeEvent(input$load_binary_example, {
    data <- binary_example_data()
    binary_data(data)
    binary_multi_data(NULL)
    binary_status(example_loaded_message(data))
  })

  observeEvent(input$binary_file, {
    tryCatch({
      data <- read_single_prop_table(path = input$binary_file$datapath)
      data <- validate_binary_data(data)
      binary_data(data)
      binary_multi_data(NULL)
      binary_status(paste0("Imported dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      binary_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_binary_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$binary_paste)
      data <- validate_binary_data(data)
      binary_data(data)
      binary_multi_data(NULL)
      binary_status(paste0("Pasted dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      binary_status(paste("Paste error:", error$message))
    })
  })

  observeEvent(input$binary_multi_file, {
    tryCatch({
      outcomes <- read_outcome_workbook(input$binary_multi_file$datapath, validate_binary_data)
      binary_multi_data(outcomes)
      binary_data(NULL)
      binary_status(paste0("Multi-outcome workbook loaded: ", length(outcomes), " outcomes detected."))
    }, error = function(error) {
      binary_multi_data(NULL)
      binary_status(paste("Workbook import error:", error$message))
    })
  })

  observeEvent(binary_data(), {
    data <- binary_data()
    req(!is.null(data))
    optional_cols <- setdiff(names(data), c("study", "event.e", "n.e", "event.c", "n.c"))
    numeric_optional_cols <- optional_cols[vapply(data[optional_cols], is.numeric, logical(1))]
    remember_picker_choices(session, "binary_subgroup_pick", optional_cols)
    remember_picker_choices(session, "binary_metareg_pick", numeric_optional_cols)
    update_forest_column_selector(session, "binary_forest_cols", optional_cols)
    updateSelectizeInput(session, "binary_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "binary_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    binary_result(NULL)
    set_batch_mode_ui("binary", FALSE)
  })

  observeEvent(binary_multi_data(), {
    outcomes <- binary_multi_data()
    req(!is.null(outcomes))
    optional_cols <- common_optional_columns(outcomes, c("study", "event.e", "n.e", "event.c", "n.c"))
    numeric_optional_cols <- common_numeric_optional_columns(outcomes, c("study", "event.e", "n.e", "event.c", "n.c"))
    remember_picker_choices(session, "binary_subgroup_pick", optional_cols)
    remember_picker_choices(session, "binary_metareg_pick", numeric_optional_cols)
    update_forest_column_selector(session, "binary_forest_cols", optional_cols)
    updateSelectizeInput(session, "binary_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "binary_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    binary_result(NULL)
    set_batch_mode_ui("binary", TRUE)
  })

  output$binary_data_status <- renderText({ binary_status() })
  output$binary_multi_status <- renderText({ binary_status() })
  output$binary_run_status <- renderText({ binary_status() })
  output$binary_run_status_results <- renderText({ binary_status() })

  output$binary_preview <- renderTable({
    data <- binary_data()
    if (is.null(data)) return(data.frame(Message = "Load the example dataset, import a file, or paste data to preview it."))
    head(data, 10)
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  observeEvent(input$binary_to_params, {
    if (is.null(binary_data()) && is.null(binary_multi_data())) {
      binary_status("Please load, import, or paste a dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "binary_steps", selected = "parameters")
  })

  observeEvent(input$binary_back_to_data, {
    updateTabsetPanel(session, "binary_steps", selected = "data")
  })

  observeEvent(input$binary_back_to_params, {
    updateTabsetPanel(session, "binary_steps", selected = "parameters")
  })

  observeEvent(input$run_binary, {
    clear_batch_export_cache("binary")
    if (!is.null(binary_multi_data())) set_download_buttons_state("binary", TRUE, batch = TRUE)
    binary_status("Running analysis...")
    tryCatch({
      if (!requireNamespace("meta", quietly = TRUE)) {
        stop("The meta package is required. Install it with install.packages('meta').")
      }
      comparison_labels <- resolve_comparison_labels(input$binary_label_e, input$binary_label_c, input$binary_outcome_direction)
      params <- list(
        outcome_name = input$binary_outcome,
        # Both columns come from the step-3 pickers, applied by dynamic_result_reactive().
        subgroup_col = "",
        metareg_col = "",
        model_choice = if (is.null(input$binary_model) || !nzchar(input$binary_model)) "random" else input$binary_model,
        prediction_flag = identical(input$binary_prediction, "yes"),
        sm = input$binary_sm,
        method = input$binary_method,
        method_tau = input$binary_method_tau,
        method_i2 = if (is.null(input$binary_method_i2)) "Q" else input$binary_method_i2,
        method_random_ci = input$binary_method_random_ci,
        method_predict = input$binary_method_predict,
        label_e = comparison_labels$label_e,
        label_c = comparison_labels$label_c,
        label_left = comparison_labels$label_left,
        label_right = comparison_labels$label_right,
        forest_cols = sanitize_forest_columns(input$binary_forest_cols)
      )

      if (!is.null(binary_multi_data())) {
        result <- analyze_outcome_batch(binary_multi_data(), analyze_binary, params)
        # Same dyn contract as the single-outcome path, so the step-3 pickers can
        # re-run every outcome with a different column.
        result$dyn <- list(analyzer = analyze_binary, outcomes = binary_multi_data(), params = params)
        binary_result(result)
        binary_status("Continue below to preview or export the results you want.")
      } else {
        data <- binary_data()
        if (is.null(data)) stop("Please load, import, or paste a dataset before running the analysis.")
        single_run <- analyze_binary(data, params)
        single_run$dyn <- list(analyzer = analyze_binary, data = data, params = params)
        binary_result(single_run)
        binary_status("Continue below to preview or export the results you want.")
      }
      updateTabsetPanel(session, "binary_steps", selected = "results")
    }, error = function(error) {
      binary_result(NULL)
      binary_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 10)
    })
  })

  output$network_binary_step_label <- renderText({
    step <- input$network_binary_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  observeEvent(input$load_network_binary_example, {
    data <- network_binary_example_data()
    network_binary_data(data)
    network_binary_result(NULL)
    network_binary_status("Example dataset loaded: 12 studies, 5 treatments and 26 treatment arms.")
  })

  observeEvent(input$network_binary_file, {
    tryCatch({
      data <- read_single_prop_table(path = input$network_binary_file$datapath)
      data <- validate_network_binary_data(data)
      network_binary_data(data)
      network_binary_result(NULL)
      network_binary_status(paste0("Imported dataset loaded: ", nrow(data), " arms, ", length(unique(data$study)), " studies and ", length(unique(data$treatment)), " treatments."))
    }, error = function(error) {
      network_binary_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_network_binary_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$network_binary_paste)
      data <- validate_network_binary_data(data)
      network_binary_data(data)
      network_binary_result(NULL)
      network_binary_status(paste0("Pasted dataset loaded: ", nrow(data), " arms, ", length(unique(data$study)), " studies and ", length(unique(data$treatment)), " treatments."))
    }, error = function(error) {
      network_binary_status(paste("Paste error:", error$message))
    })
  })

  output$network_binary_data_status <- renderText({ network_binary_status() })
  output$network_binary_run_status <- renderText({ network_binary_status() })
  output$network_binary_run_status_results <- renderText({ network_binary_status() })

  observeEvent(input$network_binary_to_params, {
    if (is.null(network_binary_data())) {
      network_binary_status("Please load, import, or paste a dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "network_binary_steps", selected = "parameters")
  })

  observeEvent(input$network_binary_back_to_data, {
    updateTabsetPanel(session, "network_binary_steps", selected = "data")
  })

  observeEvent(input$network_binary_back_to_params, {
    updateTabsetPanel(session, "network_binary_steps", selected = "parameters")
  })

  observeEvent(input$run_network_binary, {
    network_binary_status("Running network meta-analysis...")
    tryCatch({
      data <- network_binary_data()
      if (is.null(data)) stop("Please load, import, or paste a dataset before running the analysis.")
      params <- list(
        outcome_name = input$network_binary_outcome,
        sm = input$network_binary_sm,
        model_choice = if (is.null(input$network_binary_model) || !nzchar(input$network_binary_model)) "random" else input$network_binary_model,
        reference_group = input$network_binary_reference,
        method_tau = input$network_binary_method_tau,
        small_values = input$network_binary_small_values
      )
      network_binary_result(analyze_network_binary(data, params))
      network_binary_status("Network analysis completed. Continue below to preview or export the results you want.")
      updateTabsetPanel(session, "network_binary_steps", selected = "results")
    }, error = function(error) {
      network_binary_result(NULL)
      network_binary_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 10)
    })
  })

  output$network_cont_step_label <- renderText({
    step <- input$network_cont_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  observeEvent(input$load_network_cont_example, {
    data <- network_continuous_example_data()
    network_cont_data(data)
    network_cont_result(NULL)
    network_cont_status("Example dataset loaded: 12 studies, 5 treatments and 26 treatment arms.")
  })

  observeEvent(input$network_cont_file, {
    tryCatch({
      data <- read_single_prop_table(path = input$network_cont_file$datapath)
      data <- validate_network_continuous_data(data)
      network_cont_data(data)
      network_cont_result(NULL)
      network_cont_status(paste0("Imported dataset loaded: ", nrow(data), " arms, ", length(unique(data$study)), " studies and ", length(unique(data$treatment)), " treatments."))
    }, error = function(error) {
      network_cont_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_network_cont_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$network_cont_paste)
      data <- validate_network_continuous_data(data)
      network_cont_data(data)
      network_cont_result(NULL)
      network_cont_status(paste0("Pasted dataset loaded: ", nrow(data), " arms, ", length(unique(data$study)), " studies and ", length(unique(data$treatment)), " treatments."))
    }, error = function(error) {
      network_cont_status(paste("Paste error:", error$message))
    })
  })

  output$network_cont_data_status <- renderText({ network_cont_status() })
  output$network_cont_run_status <- renderText({ network_cont_status() })
  output$network_cont_run_status_results <- renderText({ network_cont_status() })

  observeEvent(input$network_cont_to_params, {
    if (is.null(network_cont_data())) {
      network_cont_status("Please load, import, or paste a dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "network_cont_steps", selected = "parameters")
  })

  observeEvent(input$network_cont_back_to_data, {
    updateTabsetPanel(session, "network_cont_steps", selected = "data")
  })

  observeEvent(input$network_cont_back_to_params, {
    updateTabsetPanel(session, "network_cont_steps", selected = "parameters")
  })

  observeEvent(input$run_network_cont, {
    network_cont_status("Running network meta-analysis...")
    tryCatch({
      data <- network_cont_data()
      if (is.null(data)) stop("Please load, import, or paste a dataset before running the analysis.")
      params <- list(
        outcome_name = input$network_cont_outcome,
        sm = input$network_cont_sm,
        model_choice = if (is.null(input$network_cont_model) || !nzchar(input$network_cont_model)) "random" else input$network_cont_model,
        reference_group = input$network_cont_reference,
        method_tau = input$network_cont_method_tau,
        small_values = input$network_cont_small_values
      )
      network_cont_result(analyze_network_continuous(data, params))
      network_cont_status("Network analysis completed. Continue below to preview or export the results you want.")
      updateTabsetPanel(session, "network_cont_steps", selected = "results")
    }, error = function(error) {
      network_cont_result(NULL)
      network_cont_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 10)
    })
  })

  output$network_precalc_ci_step_label <- renderText({
    step <- input$network_precalc_ci_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  observeEvent(input$load_network_precalc_ci_example, {
    data <- network_precalc_ci_example_data()
    network_precalc_ci_data(data)
    network_precalc_ci_result(NULL)
    network_precalc_ci_status("Example dataset loaded: 10 direct comparisons and 5 treatments.")
  })

  observeEvent(input$network_precalc_ci_file, {
    tryCatch({
      data <- read_single_prop_table(path = input$network_precalc_ci_file$datapath)
      data <- validate_network_precalc_ci_data(data)
      network_precalc_ci_data(data)
      network_precalc_ci_result(NULL)
      network_precalc_ci_status(paste0("Imported dataset loaded: ", nrow(data), " direct comparisons and ", length(unique(c(data$treat1, data$treat2))), " treatments."))
    }, error = function(error) {
      network_precalc_ci_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_network_precalc_ci_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$network_precalc_ci_paste)
      data <- validate_network_precalc_ci_data(data)
      network_precalc_ci_data(data)
      network_precalc_ci_result(NULL)
      network_precalc_ci_status(paste0("Pasted dataset loaded: ", nrow(data), " direct comparisons and ", length(unique(c(data$treat1, data$treat2))), " treatments."))
    }, error = function(error) {
      network_precalc_ci_status(paste("Paste error:", error$message))
    })
  })

  output$network_precalc_ci_data_status <- renderText({ network_precalc_ci_status() })
  output$network_precalc_ci_run_status <- renderText({ network_precalc_ci_status() })
  output$network_precalc_ci_run_status_results <- renderText({ network_precalc_ci_status() })

  observeEvent(input$network_precalc_ci_to_params, {
    if (is.null(network_precalc_ci_data())) {
      network_precalc_ci_status("Please load, import, or paste a dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "network_precalc_ci_steps", selected = "parameters")
  })

  observeEvent(input$network_precalc_ci_back_to_data, {
    updateTabsetPanel(session, "network_precalc_ci_steps", selected = "data")
  })

  observeEvent(input$network_precalc_ci_back_to_params, {
    updateTabsetPanel(session, "network_precalc_ci_steps", selected = "parameters")
  })

  observeEvent(input$run_network_precalc_ci, {
    network_precalc_ci_status("Running network meta-analysis...")
    tryCatch({
      data <- network_precalc_ci_data()
      if (is.null(data)) stop("Please load, import, or paste a dataset before running the analysis.")
      params <- list(
        outcome_name = input$network_precalc_ci_outcome,
        sm = input$network_precalc_ci_sm,
        model_choice = if (is.null(input$network_precalc_ci_model) || !nzchar(input$network_precalc_ci_model)) "random" else input$network_precalc_ci_model,
        reference_group = input$network_precalc_ci_reference,
        method_tau = input$network_precalc_ci_method_tau,
        small_values = input$network_precalc_ci_small_values
      )
      network_precalc_ci_result(analyze_network_precalc_ci(data, params))
      network_precalc_ci_status("Network analysis completed. Continue below to preview or export the results you want.")
      updateTabsetPanel(session, "network_precalc_ci_steps", selected = "results")
    }, error = function(error) {
      network_precalc_ci_result(NULL)
      network_precalc_ci_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 10)
    })
  })

  output$single_mean_step_label <- renderText({
    step <- input$single_mean_steps
    if (identical(step, "parameters")) return("Step 2 of 3: Parameters")
    if (identical(step, "results")) return("Step 3 of 3: Results and export")
    "Step 1 of 3: Data"
  })

  observeEvent(input$load_single_mean_example, {
    data <- single_mean_example_data()
    single_mean_data(data)
    single_mean_multi_data(NULL)
    single_mean_status(example_loaded_message(data))
  })

  observeEvent(input$single_mean_file, {
    tryCatch({
      data <- read_single_prop_table(path = input$single_mean_file$datapath)
      data <- validate_single_mean_data(data)
      single_mean_data(data)
      single_mean_multi_data(NULL)
      single_mean_status(paste0("Imported dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      single_mean_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_single_mean_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$single_mean_paste)
      data <- validate_single_mean_data(data)
      single_mean_data(data)
      single_mean_multi_data(NULL)
      single_mean_status(paste0("Pasted dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      single_mean_status(paste("Paste error:", error$message))
    })
  })

  observeEvent(input$single_mean_multi_file, {
    tryCatch({
      outcomes <- read_outcome_workbook(input$single_mean_multi_file$datapath, validate_single_mean_data)
      single_mean_multi_data(outcomes)
      single_mean_data(NULL)
      single_mean_status(paste0("Multi-outcome workbook loaded: ", length(outcomes), " outcomes detected."))
    }, error = function(error) {
      single_mean_multi_data(NULL)
      single_mean_status(paste("Workbook import error:", error$message))
    })
  })

  observeEvent(single_mean_data(), {
    data <- single_mean_data()
    req(!is.null(data))
    optional_cols <- setdiff(names(data), SINGLE_MEAN_OUTCOME_COLS)
    numeric_optional_cols <- optional_cols[vapply(data[optional_cols], is.numeric, logical(1))]
    remember_picker_choices(session, "single_mean_subgroup_pick", optional_cols)
    remember_picker_choices(session, "single_mean_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "single_mean_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "single_mean_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "single_mean_forest_cols", optional_cols)
    single_mean_result(NULL)
    set_batch_mode_ui("single_mean", FALSE)
  })

  observeEvent(single_mean_multi_data(), {
    outcomes <- single_mean_multi_data()
    req(!is.null(outcomes))
    optional_cols <- common_optional_columns(outcomes, SINGLE_MEAN_OUTCOME_COLS)
    numeric_optional_cols <- common_numeric_optional_columns(outcomes, SINGLE_MEAN_OUTCOME_COLS)
    remember_picker_choices(session, "single_mean_subgroup_pick", optional_cols)
    remember_picker_choices(session, "single_mean_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "single_mean_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "single_mean_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "single_mean_forest_cols", optional_cols)
    single_mean_result(NULL)
    set_batch_mode_ui("single_mean", TRUE)
  })

  output$single_mean_data_status <- renderText({ single_mean_status() })
  output$single_mean_multi_status <- renderText({ single_mean_status() })
  output$single_mean_run_status <- renderText({ single_mean_status() })
  output$single_mean_run_status_results <- renderText({ single_mean_status() })

  output$single_mean_preview <- renderTable({
    data <- single_mean_data()
    if (is.null(data)) return(data.frame(Message = "Load the example dataset, import a file, or paste data to preview it."))
    head(data, 10)
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  observeEvent(input$single_mean_to_params, {
    if (is.null(single_mean_data()) && is.null(single_mean_multi_data())) {
      single_mean_status("Please load, import, or paste a dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "single_mean_steps", selected = "parameters")
  })

  observeEvent(input$single_mean_back_to_data, {
    updateTabsetPanel(session, "single_mean_steps", selected = "data")
  })

  observeEvent(input$single_mean_back_to_params, {
    updateTabsetPanel(session, "single_mean_steps", selected = "parameters")
  })

  observeEvent(input$run_single_mean, {
    clear_batch_export_cache("single_mean")
    if (!is.null(single_mean_multi_data())) set_download_buttons_state("single_mean", TRUE, batch = TRUE)
    single_mean_status("Running analysis...")
    tryCatch({
      if (!requireNamespace("meta", quietly = TRUE)) {
        stop("The meta package is required. Install it with install.packages('meta').")
      }
      params <- list(
        outcome_name = input$single_mean_outcome,
        # Both columns come from the step-3 pickers, applied by dynamic_result_reactive().
        subgroup_col = "",
        metareg_col = "",
        model_choice = if (is.null(input$single_mean_model) || !nzchar(input$single_mean_model)) "random" else input$single_mean_model,
        prediction_flag = identical(input$single_mean_prediction, "yes"),
        sm = input$single_mean_sm,
        method_tau = input$single_mean_method_tau,
        method_i2 = if (is.null(input$single_mean_method_i2)) "Q" else input$single_mean_method_i2,
        method_random_ci = input$single_mean_method_random_ci %||% "classic",
        forest_cols = sanitize_forest_columns(input$single_mean_forest_cols)
      )

      if (!is.null(single_mean_multi_data())) {
        result <- analyze_outcome_batch(single_mean_multi_data(), analyze_single_mean, params)
        # Same dyn contract as the single-outcome path, so the step-3 pickers can
        # re-run every outcome with a different column.
        result$dyn <- list(analyzer = analyze_single_mean, outcomes = single_mean_multi_data(), params = params)
        single_mean_result(result)
        single_mean_status("Continue below to preview or export the results you want.")
      } else {
        data <- single_mean_data()
        if (is.null(data)) stop("Please load, import, or paste a dataset before running the analysis.")
        single_run <- analyze_single_mean(data, params)
        single_run$dyn <- list(analyzer = analyze_single_mean, data = data, params = params)
        single_mean_result(single_run)
        single_mean_status("Continue below to preview or export the results you want.")
      }
      updateTabsetPanel(session, "single_mean_steps", selected = "results")
    }, error = function(error) {
      single_mean_result(NULL)
      single_mean_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 10)
    })
  })

  output$single_prop_step_label <- renderText({
    step <- input$single_prop_steps
    if (identical(step, "parameters")) {
      return("Step 2 of 3: Parameters")
    }
    if (identical(step, "results")) {
      return("Step 3 of 3: Results and export")
    }
    "Step 1 of 3: Data"
  })

  observeEvent(input$single_prop_to_params, {
    if (is.null(single_prop_data()) && is.null(single_prop_multi_data())) {
      single_prop_status("Please load, import, or paste a dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "single_prop_steps", selected = "parameters")
  })

  observeEvent(input$single_prop_back_to_data, {
    updateTabsetPanel(session, "single_prop_steps", selected = "data")
  })

  observeEvent(input$single_prop_back_to_params, {
    updateTabsetPanel(session, "single_prop_steps", selected = "parameters")
  })

  observeEvent(input$load_single_prop_example, {
    data <- single_prop_example_data()
    single_prop_data(data)
    single_prop_multi_data(NULL)
    single_prop_status(example_loaded_message(data))
  })

  observeEvent(input$single_prop_file, {
    tryCatch({
      data <- read_single_prop_table(path = input$single_prop_file$datapath)
      data <- validate_single_prop_data(data)
      single_prop_data(data)
      single_prop_multi_data(NULL)
      single_prop_status(paste0("Imported dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      single_prop_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_single_prop_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$single_prop_paste)
      data <- validate_single_prop_data(data)
      single_prop_data(data)
      single_prop_multi_data(NULL)
      single_prop_status(paste0("Pasted dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      single_prop_status(paste("Paste error:", error$message))
    })
  })

  observeEvent(input$single_prop_multi_file, {
    tryCatch({
      outcomes <- read_outcome_workbook(input$single_prop_multi_file$datapath, validate_single_prop_data)
      single_prop_multi_data(outcomes)
      single_prop_data(NULL)
      single_prop_status(paste0("Multi-outcome workbook loaded: ", length(outcomes), " outcomes detected."))
    }, error = function(error) {
      single_prop_multi_data(NULL)
      single_prop_status(paste("Workbook import error:", error$message))
    })
  })

  observeEvent(single_prop_data(), {
    data <- single_prop_data()
    req(!is.null(data))
    optional_cols <- setdiff(names(data), c("study", "event", "n"))
    numeric_optional_cols <- optional_cols[vapply(data[optional_cols], is.numeric, logical(1))]
    remember_picker_choices(session, "single_prop_subgroup_pick", optional_cols)
    remember_picker_choices(session, "single_prop_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "single_prop_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "single_prop_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "single_prop_forest_cols", optional_cols)
    single_prop_result(NULL)
    set_batch_mode_ui("single_prop", FALSE)
  })

  observeEvent(single_prop_multi_data(), {
    outcomes <- single_prop_multi_data()
    req(!is.null(outcomes))
    optional_cols <- common_optional_columns(outcomes, c("study", "event", "n"))
    numeric_optional_cols <- common_numeric_optional_columns(outcomes, c("study", "event", "n"))
    remember_picker_choices(session, "single_prop_subgroup_pick", optional_cols)
    remember_picker_choices(session, "single_prop_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "single_prop_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "single_prop_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "single_prop_forest_cols", optional_cols)
    single_prop_result(NULL)
    set_batch_mode_ui("single_prop", TRUE)
  })

  output$single_prop_data_status <- renderText({
    single_prop_status()
  })
  output$single_prop_multi_status <- renderText({ single_prop_status() })

  output$single_prop_run_status <- renderText({
    single_prop_status()
  })

  output$single_prop_run_status_results <- renderText({
    single_prop_status()
  })

  output$single_prop_preview <- renderTable({
    data <- single_prop_data()
    if (is.null(data)) {
      return(data.frame(Message = "Load the example dataset, import a file, or paste data to preview it."))
    }
    head(data, 10)
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  observeEvent(input$run_single_prop, {
    clear_batch_export_cache("single_prop")
    if (!is.null(single_prop_multi_data())) set_download_buttons_state("single_prop", TRUE, batch = TRUE)
    single_prop_status("Running analysis...")
    tryCatch({
      if (!requireNamespace("meta", quietly = TRUE)) {
        stop("The meta package is required. Install it with install.packages('meta').")
      }
      params <- list(
        outcome_name = input$single_prop_outcome,
        # Both columns come from the step-3 pickers, applied by dynamic_result_reactive().
        subgroup_col = "",
        metareg_col = "",
        model_choice = if (is.null(input$single_prop_model) || !nzchar(input$single_prop_model)) "random" else input$single_prop_model,
        sm = input$single_prop_sm,
        method = input$single_prop_method,
        method_tau = input$single_prop_method_tau,
        method_i2 = if (is.null(input$single_prop_method_i2)) "Q" else input$single_prop_method_i2,
        method_random_ci = input$single_prop_method_random_ci %||% "classic",
        forest_cols = sanitize_forest_columns(input$single_prop_forest_cols)
      )

      if (!is.null(single_prop_multi_data())) {
        result <- analyze_outcome_batch(single_prop_multi_data(), analyze_single_prop, params)
        # Same dyn contract as the single-outcome path, so the step-3 pickers can
        # re-run every outcome with a different column.
        result$dyn <- list(analyzer = analyze_single_prop, outcomes = single_prop_multi_data(), params = params)
        single_prop_result(result)
        single_prop_status("Continue below to preview or export the results you want.")
      } else {
        data <- single_prop_data()
        if (is.null(data)) stop("Please load, import, or paste a dataset before running the analysis.")
        single_run <- analyze_single_prop(data, params)
        single_run$dyn <- list(analyzer = analyze_single_prop, data = data, params = params)
        single_prop_result(single_run)
        single_prop_status("Continue below to preview or export the results you want.")
      }

      updateTabsetPanel(session, "single_prop_steps", selected = "results")
    }, error = function(error) {
      single_prop_result(NULL)
      single_prop_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 10)
    })
  })

  observeEvent(input$load_precalc_te_ci_example, {
    data <- precalc_te_ci_example_data()
    precalc_te_ci_data(data)
    precalc_te_ci_multi_data(NULL)
    precalc_te_ci_status("Example dataset loaded: 12 studies with pre-calculated treatment effects and confidence intervals.")
  })

  observeEvent(input$precalc_te_ci_file, {
    tryCatch({
      data <- read_single_prop_table(path = input$precalc_te_ci_file$datapath)
      data <- validate_precalc_te_ci_data(data)
      precalc_te_ci_data(data)
      precalc_te_ci_multi_data(NULL)
      precalc_te_ci_status(paste0("Imported dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      precalc_te_ci_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_precalc_te_ci_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$precalc_te_ci_paste)
      data <- validate_precalc_te_ci_data(data)
      precalc_te_ci_data(data)
      precalc_te_ci_multi_data(NULL)
      precalc_te_ci_status(paste0("Pasted dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      precalc_te_ci_status(paste("Paste error:", error$message))
    })
  })

  observeEvent(input$precalc_te_ci_multi_file, {
    tryCatch({
      outcomes <- read_outcome_workbook(input$precalc_te_ci_multi_file$datapath, validate_precalc_te_ci_data)
      precalc_te_ci_multi_data(outcomes)
      precalc_te_ci_data(NULL)
      precalc_te_ci_status(paste0("Multi-outcome workbook loaded: ", length(outcomes), " outcomes detected."))
    }, error = function(error) {
      precalc_te_ci_multi_data(NULL)
      precalc_te_ci_status(paste("Workbook import error:", error$message))
    })
  })

  observeEvent(precalc_te_ci_data(), {
    data <- precalc_te_ci_data()
    req(!is.null(data))
    optional_cols <- setdiff(names(data), c("study", "TE", "lower", "upper", "n.e", "n.c"))
    numeric_optional_cols <- optional_cols[vapply(data[optional_cols], is.numeric, logical(1))]
    remember_picker_choices(session, "precalc_te_ci_subgroup_pick", optional_cols)
    remember_picker_choices(session, "precalc_te_ci_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "precalc_te_ci_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "precalc_te_ci_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "precalc_te_ci_forest_cols", optional_cols)
    precalc_te_ci_result(NULL)
    set_batch_mode_ui("precalc_te_ci", FALSE)
  })

  observeEvent(precalc_te_ci_multi_data(), {
    outcomes <- precalc_te_ci_multi_data()
    req(!is.null(outcomes))
    optional_cols <- common_optional_columns(outcomes, c("study", "TE", "lower", "upper", "n.e", "n.c"))
    numeric_optional_cols <- common_numeric_optional_columns(outcomes, c("study", "TE", "lower", "upper", "n.e", "n.c"))
    remember_picker_choices(session, "precalc_te_ci_subgroup_pick", optional_cols)
    remember_picker_choices(session, "precalc_te_ci_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "precalc_te_ci_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "precalc_te_ci_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "precalc_te_ci_forest_cols", optional_cols)
    precalc_te_ci_result(NULL)
    set_batch_mode_ui("precalc_te_ci", TRUE)
  })

  output$precalc_te_ci_data_status <- renderText({ precalc_te_ci_status() })
  output$precalc_te_ci_multi_status <- renderText({ precalc_te_ci_status() })
  output$precalc_te_ci_run_status <- renderText({ precalc_te_ci_status() })
  output$precalc_te_ci_run_status_results <- renderText({ precalc_te_ci_status() })

  observeEvent(input$precalc_te_ci_to_params, {
    if (is.null(precalc_te_ci_data()) && is.null(precalc_te_ci_multi_data())) {
      precalc_te_ci_status("Please load, import, or paste a dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "precalc_te_ci_steps", selected = "parameters")
  })

  observeEvent(input$precalc_te_ci_back_to_data, {
    updateTabsetPanel(session, "precalc_te_ci_steps", selected = "data")
  })

  observeEvent(input$precalc_te_ci_back_to_params, {
    updateTabsetPanel(session, "precalc_te_ci_steps", selected = "parameters")
  })

  observeEvent(input$run_precalc_te_ci, {
    clear_batch_export_cache("precalc_te_ci")
    if (!is.null(precalc_te_ci_multi_data())) set_download_buttons_state("precalc_te_ci", TRUE, batch = TRUE)
    precalc_te_ci_status("Running analysis...")
    tryCatch({
      if (!requireNamespace("meta", quietly = TRUE)) {
        stop("The meta package is required. Install it with install.packages('meta').")
      }
      comparison_labels <- resolve_comparison_labels(input$precalc_te_ci_label_e, input$precalc_te_ci_label_c, input$precalc_te_ci_outcome_direction)
      params <- list(
        outcome_name = input$precalc_te_ci_outcome,
        # Both columns come from the step-3 pickers, applied by dynamic_result_reactive().
        subgroup_col = "",
        metareg_col = "",
        model_choice = if (is.null(input$precalc_te_ci_model) || !nzchar(input$precalc_te_ci_model)) "random" else input$precalc_te_ci_model,
        prediction_flag = identical(input$precalc_te_ci_prediction, "yes"),
        sm = input$precalc_te_ci_sm,
        method_tau = input$precalc_te_ci_method_tau,
        method_i2 = if (is.null(input$precalc_te_ci_method_i2)) "Q" else input$precalc_te_ci_method_i2,
        method_random_ci = input$precalc_te_ci_method_random_ci,
        method_predict = input$precalc_te_ci_method_predict,
        label_e = comparison_labels$label_e,
        label_c = comparison_labels$label_c,
        label_left = comparison_labels$label_left,
        label_right = comparison_labels$label_right,
        forest_cols = sanitize_forest_columns(input$precalc_te_ci_forest_cols)
      )

      if (!is.null(precalc_te_ci_multi_data())) {
        result <- analyze_outcome_batch(precalc_te_ci_multi_data(), analyze_precalc_te_ci, params)
        # Same dyn contract as the single-outcome path, so the step-3 pickers can
        # re-run every outcome with a different column.
        result$dyn <- list(analyzer = analyze_precalc_te_ci, outcomes = precalc_te_ci_multi_data(), params = params)
        precalc_te_ci_result(result)
        precalc_te_ci_status("Continue below to preview or export the results you want.")
      } else {
        data <- precalc_te_ci_data()
        if (is.null(data)) stop("Please load, import, or paste a dataset before running the analysis.")
        single_run <- analyze_precalc_te_ci(data, params)
        single_run$dyn <- list(analyzer = analyze_precalc_te_ci, data = data, params = params)
        precalc_te_ci_result(single_run)
        precalc_te_ci_status("Continue below to preview or export the results you want.")
      }

      updateTabsetPanel(session, "precalc_te_ci_steps", selected = "results")
    }, error = function(error) {
      precalc_te_ci_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 8)
    })
  })

  output$precalc_te_ci_summary <- renderPrint({
    result <- precalc_te_ci_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to see the model summary here.")
      return()
    }
    if (is_batch_result(result)) {
      cat("Batch mode summary\n\n")
      print(batch_result_overview(result))
      return()
    }
    cat("Outcome:", input$precalc_te_ci_outcome, "\n\n")
    print(summary(result$meta))
  })

  output$precalc_te_ci_forest <- renderPlot({
    result <- precalc_te_ci_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the forest plot."))
    plot_precalc_te_ci_forest(result, input$precalc_te_ci_col_square, input$precalc_te_ci_col_square_lines, identical(input$precalc_te_ci_forest_sort, "yes"))
  })

  output$precalc_te_ci_loo <- renderPlot({
    result <- precalc_te_ci_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the leave-one-out plot."))
    plot_precalc_te_ci_loo(result, input$precalc_te_ci_col_square)
  })

  output$precalc_te_ci_funnel <- renderPlot({
    result <- precalc_te_ci_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the funnel plot."))
    plot_precalc_te_ci_funnel(result, input$precalc_te_ci_col_square)
  })

  output$precalc_te_ci_bias <- renderPrint({
    result <- precalc_te_ci_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to evaluate small-study effects.")
      return()
    }
    if (nrow(result$data) < 10) {
      cat("Egger test is usually recommended only when there are at least 10 studies.")
      return()
    }
    print(meta::metabias(result$meta, method.bias = "Egger", plotit = FALSE))
  })

  output$precalc_te_ci_subgroup_plot <- renderPlot({
    result <- precalc_te_ci_result_dyn()
    validate(need(!is.null(result) && !is.null(result$subgroup), if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot."))
    plot_precalc_te_ci_subgroup(result, input$precalc_te_ci_col_square, input$precalc_te_ci_col_square_lines, identical(input$precalc_te_ci_forest_sort, "yes"))
  })

  output$precalc_te_ci_subgroup_note <- renderPrint({
    result <- precalc_te_ci_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a subgroup column in this card.")
      return()
    }
    print(summary(result$subgroup))
  })

  output$precalc_te_ci_metareg_plot <- renderPlot({
    result <- precalc_te_ci_result_dyn()
    validate(need(!is.null(result) && !is.null(result$metareg), if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot."))
    plot_precalc_te_ci_metareg(result, input$precalc_te_ci_col_square)
  })

  output$precalc_te_ci_metareg_summary <- renderPrint({
    result <- precalc_te_ci_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a moderator column in this card.")
      return()
    }
    print(summary(result$metareg))
  })

  output$precalc_te_ci_metareg_table <- renderTable({
    extract_precalc_te_ci_metareg_table(precalc_te_ci_result_dyn())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  observeEvent(input$preview_precalc_te_ci_forest, {
    req(precalc_te_ci_result_dyn())
    open_single_prop_plot_modal("Forest plot preview", "precalc_te_ci_forest", "72vh")
  })

  observeEvent(input$preview_precalc_te_ci_loo, {
    req(precalc_te_ci_result_dyn())
    open_single_prop_plot_modal("Leave-one-out preview", "precalc_te_ci_loo", "72vh")
  })

  observeEvent(input$preview_precalc_te_ci_funnel, {
    req(precalc_te_ci_result_dyn())
    open_single_prop_plot_modal("Funnel plot preview", "precalc_te_ci_funnel", "72vh")
  })

  observeEvent(input$preview_precalc_te_ci_subgroup, {
    result <- precalc_te_ci_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_plot_modal("Subgroup analysis preview", "precalc_te_ci_subgroup_plot", "72vh")
  })

  observeEvent(input$preview_precalc_te_ci_metareg, {
    result <- precalc_te_ci_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    if (is_batch_result(result)) {
      showNotification("Preview is unavailable in batch mode. Use Download to export all outcomes.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression preview",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("precalc_te_ci_metareg_plot", height = "60vh"),
      tags$hr(),
      tableOutput("precalc_te_ci_metareg_table")
    ))
  })

  observeEvent(input$summary_precalc_te_ci_main, {
    req(precalc_te_ci_result_dyn())
    open_single_prop_text_modal("Main meta-analysis summary", "precalc_te_ci_summary")
  })

  observeEvent(input$summary_precalc_te_ci_bias, {
    req(precalc_te_ci_result_dyn())
    open_single_prop_text_modal("Small-study effects summary", "precalc_te_ci_bias")
  })

  observeEvent(input$summary_precalc_te_ci_subgroup, {
    result <- precalc_te_ci_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_text_modal("Subgroup analysis summary", "precalc_te_ci_subgroup_note")
  })

  observeEvent(input$summary_precalc_te_ci_metareg, {
    result <- precalc_te_ci_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression summary",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("precalc_te_ci_metareg_table"),
      tags$hr(),
      verbatimTextOutput("precalc_te_ci_metareg_summary")
    ))
  })

  precalc_te_ci_download_handler <- function(plot_label, plot_function, require_component = NULL, settings_prefix) {
    downloadHandler(
      filename = function() {
        outcome <- input$precalc_te_ci_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        result <- precalc_te_ci_result_dyn()
        if (is_batch_result(result)) {
          return(paste0(plot_label, "_Batch.zip"))
        }
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- precalc_te_ci_result_dyn()
        if (is.null(result)) stop("Run the analysis before downloading plots.")
        if (!is_batch_result(result) && !is.null(require_component) && is.null(result[[require_component]])) {
          stop(paste("This plot is unavailable because", require_component, "was not selected."))
        }
        settings <- get_card_export_settings(settings_prefix)
        width <- settings$width
        height <- settings$height
        file_format <- settings$file_format
        if (is_batch_result(result)) {
          copy_prepared_batch_export(file, "precalc_te_ci", result, plot_label, file_format, width, height, plot_function, require_component)
          return(invisible(NULL))
        }
        write_plot_export(file, result, plot_label, file_format, width, height, plot_function, require_component)
      }
    )
  }

  output$download_precalc_te_ci_forest <- precalc_te_ci_download_handler(
    "Forestplot",
    function(result) plot_precalc_te_ci_forest(result, input$precalc_te_ci_col_square, input$precalc_te_ci_col_square_lines, identical(input$precalc_te_ci_forest_sort, "yes")),
    settings_prefix = "precalc_te_ci_forest"
  )
  output$download_precalc_te_ci_loo <- precalc_te_ci_download_handler("Leave_one_out", function(result) plot_precalc_te_ci_loo(result, input$precalc_te_ci_col_square), settings_prefix = "precalc_te_ci_loo")
  output$download_precalc_te_ci_funnel <- precalc_te_ci_download_handler("FunnelPlot", function(result) plot_precalc_te_ci_funnel(result, input$precalc_te_ci_col_square), settings_prefix = "precalc_te_ci_funnel")
  output$download_precalc_te_ci_subgroup <- precalc_te_ci_download_handler(
    "Subgroup",
    function(result) plot_precalc_te_ci_subgroup(result, input$precalc_te_ci_col_square, input$precalc_te_ci_col_square_lines, identical(input$precalc_te_ci_forest_sort, "yes")),
    require_component = "subgroup",
    settings_prefix = "precalc_te_ci_subgroup"
  )
  output$download_precalc_te_ci_metareg <- precalc_te_ci_download_handler(
    "Metarregression",
    function(result) plot_precalc_te_ci_metareg(result, input$precalc_te_ci_col_square),
    require_component = "metareg",
    settings_prefix = "precalc_te_ci_metareg"
  )

  observeEvent(input$load_precalc_te_sete_example, {
    data <- precalc_te_sete_example_data()
    precalc_te_sete_data(data)
    precalc_te_sete_multi_data(NULL)
    precalc_te_sete_status(example_loaded_message(data))
  })

  observeEvent(input$precalc_te_sete_file, {
    tryCatch({
      data <- read_single_prop_table(path = input$precalc_te_sete_file$datapath)
      data <- validate_precalc_te_sete_data(data)
      precalc_te_sete_data(data)
      precalc_te_sete_multi_data(NULL)
      precalc_te_sete_status(paste0("Imported dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      precalc_te_sete_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_precalc_te_sete_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$precalc_te_sete_paste)
      data <- validate_precalc_te_sete_data(data)
      precalc_te_sete_data(data)
      precalc_te_sete_multi_data(NULL)
      precalc_te_sete_status(paste0("Pasted dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      precalc_te_sete_status(paste("Paste error:", error$message))
    })
  })

  observeEvent(input$precalc_te_sete_multi_file, {
    tryCatch({
      outcomes <- read_outcome_workbook(input$precalc_te_sete_multi_file$datapath, validate_precalc_te_sete_data)
      precalc_te_sete_multi_data(outcomes)
      precalc_te_sete_data(NULL)
      precalc_te_sete_status(paste0("Multi-outcome workbook loaded: ", length(outcomes), " outcomes detected."))
    }, error = function(error) {
      precalc_te_sete_multi_data(NULL)
      precalc_te_sete_status(paste("Workbook import error:", error$message))
    })
  })

  observeEvent(precalc_te_sete_data(), {
    data <- precalc_te_sete_data()
    req(!is.null(data))
    optional_cols <- setdiff(names(data), c("study", "TE", "seTE", "n.e", "n.c"))
    numeric_optional_cols <- optional_cols[vapply(data[optional_cols], is.numeric, logical(1))]
    remember_picker_choices(session, "precalc_te_sete_subgroup_pick", optional_cols)
    remember_picker_choices(session, "precalc_te_sete_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "precalc_te_sete_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "precalc_te_sete_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "precalc_te_sete_forest_cols", optional_cols)
    precalc_te_sete_result(NULL)
    set_batch_mode_ui("precalc_te_sete", FALSE)
  })

  observeEvent(precalc_te_sete_multi_data(), {
    outcomes <- precalc_te_sete_multi_data()
    req(!is.null(outcomes))
    optional_cols <- common_optional_columns(outcomes, c("study", "TE", "seTE", "n.e", "n.c"))
    numeric_optional_cols <- common_numeric_optional_columns(outcomes, c("study", "TE", "seTE", "n.e", "n.c"))
    remember_picker_choices(session, "precalc_te_sete_subgroup_pick", optional_cols)
    remember_picker_choices(session, "precalc_te_sete_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "precalc_te_sete_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "precalc_te_sete_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "precalc_te_sete_forest_cols", optional_cols)
    precalc_te_sete_result(NULL)
    set_batch_mode_ui("precalc_te_sete", TRUE)
  })

  output$precalc_te_sete_data_status <- renderText({ precalc_te_sete_status() })
  output$precalc_te_sete_multi_status <- renderText({ precalc_te_sete_status() })
  output$precalc_te_sete_run_status <- renderText({ precalc_te_sete_status() })
  output$precalc_te_sete_run_status_results <- renderText({ precalc_te_sete_status() })

  observeEvent(input$precalc_te_sete_to_params, {
    if (is.null(precalc_te_sete_data()) && is.null(precalc_te_sete_multi_data())) {
      precalc_te_sete_status("Please load, import, or paste a dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "precalc_te_sete_steps", selected = "parameters")
  })

  observeEvent(input$precalc_te_sete_back_to_data, {
    updateTabsetPanel(session, "precalc_te_sete_steps", selected = "data")
  })

  observeEvent(input$precalc_te_sete_back_to_params, {
    updateTabsetPanel(session, "precalc_te_sete_steps", selected = "parameters")
  })

  observeEvent(input$run_precalc_te_sete, {
    clear_batch_export_cache("precalc_te_sete")
    if (!is.null(precalc_te_sete_multi_data())) set_download_buttons_state("precalc_te_sete", TRUE, batch = TRUE)
    precalc_te_sete_status("Running analysis...")
    tryCatch({
      if (!requireNamespace("meta", quietly = TRUE)) {
        stop("The meta package is required. Install it with install.packages('meta').")
      }
      comparison_labels <- resolve_comparison_labels(input$precalc_te_sete_label_e, input$precalc_te_sete_label_c, input$precalc_te_sete_outcome_direction)
      params <- list(
        outcome_name = input$precalc_te_sete_outcome,
        # Both columns come from the step-3 pickers, applied by dynamic_result_reactive().
        subgroup_col = "",
        metareg_col = "",
        model_choice = if (is.null(input$precalc_te_sete_model) || !nzchar(input$precalc_te_sete_model)) "random" else input$precalc_te_sete_model,
        prediction_flag = identical(input$precalc_te_sete_prediction, "yes"),
        sm = input$precalc_te_sete_sm,
        method_tau = input$precalc_te_sete_method_tau,
        method_i2 = if (is.null(input$precalc_te_sete_method_i2)) "Q" else input$precalc_te_sete_method_i2,
        method_random_ci = input$precalc_te_sete_method_random_ci,
        method_predict = input$precalc_te_sete_method_predict,
        label_e = comparison_labels$label_e,
        label_c = comparison_labels$label_c,
        label_left = comparison_labels$label_left,
        label_right = comparison_labels$label_right,
        forest_cols = sanitize_forest_columns(input$precalc_te_sete_forest_cols)
      )

      if (!is.null(precalc_te_sete_multi_data())) {
        result <- analyze_outcome_batch(precalc_te_sete_multi_data(), analyze_precalc_te_sete, params)
        # Same dyn contract as the single-outcome path, so the step-3 pickers can
        # re-run every outcome with a different column.
        result$dyn <- list(analyzer = analyze_precalc_te_sete, outcomes = precalc_te_sete_multi_data(), params = params)
        precalc_te_sete_result(result)
        precalc_te_sete_status("Continue below to preview or export the results you want.")
      } else {
        data <- precalc_te_sete_data()
        if (is.null(data)) stop("Please load, import, or paste a dataset before running the analysis.")
        single_run <- analyze_precalc_te_sete(data, params)
        single_run$dyn <- list(analyzer = analyze_precalc_te_sete, data = data, params = params)
        precalc_te_sete_result(single_run)
        precalc_te_sete_status("Continue below to preview or export the results you want.")
      }

      updateTabsetPanel(session, "precalc_te_sete_steps", selected = "results")
    }, error = function(error) {
      precalc_te_sete_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 8)
    })
  })

  output$precalc_te_sete_summary <- renderPrint({
    result <- precalc_te_sete_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to see the model summary here.")
      return()
    }
    if (is_batch_result(result)) {
      cat("Batch mode summary\n\n")
      print(batch_result_overview(result))
      return()
    }
    cat("Outcome:", input$precalc_te_sete_outcome, "\n\n")
    print(summary(result$meta))
  })

  output$precalc_te_sete_forest <- renderPlot({
    result <- precalc_te_sete_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the forest plot."))
    plot_precalc_te_ci_forest(result, input$precalc_te_sete_col_square, input$precalc_te_sete_col_square_lines, identical(input$precalc_te_sete_forest_sort, "yes"))
  })

  output$precalc_te_sete_loo <- renderPlot({
    result <- precalc_te_sete_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the leave-one-out plot."))
    plot_precalc_te_ci_loo(result, input$precalc_te_sete_col_square)
  })

  output$precalc_te_sete_funnel <- renderPlot({
    result <- precalc_te_sete_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the funnel plot."))
    plot_precalc_te_ci_funnel(result, input$precalc_te_sete_col_square)
  })

  output$precalc_te_sete_bias <- renderPrint({
    result <- precalc_te_sete_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to evaluate small-study effects.")
      return()
    }
    if (nrow(result$data) < 10) {
      cat("Egger test is usually recommended only when there are at least 10 studies.")
      return()
    }
    print(meta::metabias(result$meta, method.bias = "Egger", plotit = FALSE))
  })

  output$precalc_te_sete_subgroup_plot <- renderPlot({
    result <- precalc_te_sete_result_dyn()
    validate(need(!is.null(result) && !is.null(result$subgroup), if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot."))
    plot_precalc_te_ci_subgroup(result, input$precalc_te_sete_col_square, input$precalc_te_sete_col_square_lines, identical(input$precalc_te_sete_forest_sort, "yes"))
  })

  output$precalc_te_sete_subgroup_note <- renderPrint({
    result <- precalc_te_sete_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a subgroup column in this card.")
      return()
    }
    print(summary(result$subgroup))
  })

  output$precalc_te_sete_metareg_plot <- renderPlot({
    result <- precalc_te_sete_result_dyn()
    validate(need(!is.null(result) && !is.null(result$metareg), if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot."))
    plot_precalc_te_ci_metareg(result, input$precalc_te_sete_col_square)
  })

  output$precalc_te_sete_metareg_summary <- renderPrint({
    result <- precalc_te_sete_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a moderator column in this card.")
      return()
    }
    print(summary(result$metareg))
  })

  output$precalc_te_sete_metareg_table <- renderTable({
    extract_precalc_te_ci_metareg_table(precalc_te_sete_result_dyn())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  observeEvent(input$preview_precalc_te_sete_forest, {
    req(precalc_te_sete_result_dyn())
    open_single_prop_plot_modal("Forest plot preview", "precalc_te_sete_forest", "72vh")
  })

  observeEvent(input$preview_precalc_te_sete_loo, {
    req(precalc_te_sete_result_dyn())
    open_single_prop_plot_modal("Leave-one-out preview", "precalc_te_sete_loo", "72vh")
  })

  observeEvent(input$preview_precalc_te_sete_funnel, {
    req(precalc_te_sete_result_dyn())
    open_single_prop_plot_modal("Funnel plot preview", "precalc_te_sete_funnel", "72vh")
  })

  observeEvent(input$preview_precalc_te_sete_subgroup, {
    result <- precalc_te_sete_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_plot_modal("Subgroup analysis preview", "precalc_te_sete_subgroup_plot", "72vh")
  })

  observeEvent(input$preview_precalc_te_sete_metareg, {
    result <- precalc_te_sete_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    if (is_batch_result(result)) {
      showNotification("Preview is unavailable in batch mode. Use Download to export all outcomes.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression preview",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("precalc_te_sete_metareg_plot", height = "60vh"),
      tags$hr(),
      tableOutput("precalc_te_sete_metareg_table")
    ))
  })

  observeEvent(input$summary_precalc_te_sete_main, {
    req(precalc_te_sete_result_dyn())
    open_single_prop_text_modal("Main meta-analysis summary", "precalc_te_sete_summary")
  })

  observeEvent(input$summary_precalc_te_sete_bias, {
    req(precalc_te_sete_result_dyn())
    open_single_prop_text_modal("Small-study effects summary", "precalc_te_sete_bias")
  })

  observeEvent(input$summary_precalc_te_sete_subgroup, {
    result <- precalc_te_sete_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_text_modal("Subgroup analysis summary", "precalc_te_sete_subgroup_note")
  })

  observeEvent(input$summary_precalc_te_sete_metareg, {
    result <- precalc_te_sete_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression summary",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("precalc_te_sete_metareg_table"),
      tags$hr(),
      verbatimTextOutput("precalc_te_sete_metareg_summary")
    ))
  })

  precalc_te_sete_download_handler <- function(plot_label, plot_function, require_component = NULL, settings_prefix) {
    downloadHandler(
      filename = function() {
        outcome <- input$precalc_te_sete_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        result <- precalc_te_sete_result_dyn()
        if (is_batch_result(result)) {
          return(paste0(plot_label, "_Batch.zip"))
        }
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- precalc_te_sete_result_dyn()
        if (is.null(result)) stop("Run the analysis before downloading plots.")
        if (!is_batch_result(result) && !is.null(require_component) && is.null(result[[require_component]])) {
          stop(paste("This plot is unavailable because", require_component, "was not selected."))
        }
        settings <- get_card_export_settings(settings_prefix)
        width <- settings$width
        height <- settings$height
        file_format <- settings$file_format
        if (is_batch_result(result)) {
          copy_prepared_batch_export(file, "precalc_te_sete", result, plot_label, file_format, width, height, plot_function, require_component)
          return(invisible(NULL))
        }
        write_plot_export(file, result, plot_label, file_format, width, height, plot_function, require_component)
      }
    )
  }

  output$download_precalc_te_sete_forest <- precalc_te_sete_download_handler(
    "Forestplot",
    function(result) plot_precalc_te_ci_forest(result, input$precalc_te_sete_col_square, input$precalc_te_sete_col_square_lines, identical(input$precalc_te_sete_forest_sort, "yes")),
    settings_prefix = "precalc_te_sete_forest"
  )
  output$download_precalc_te_sete_loo <- precalc_te_sete_download_handler("Leave_one_out", function(result) plot_precalc_te_ci_loo(result, input$precalc_te_sete_col_square), settings_prefix = "precalc_te_sete_loo")
  output$download_precalc_te_sete_funnel <- precalc_te_sete_download_handler("FunnelPlot", function(result) plot_precalc_te_ci_funnel(result, input$precalc_te_sete_col_square), settings_prefix = "precalc_te_sete_funnel")
  output$download_precalc_te_sete_subgroup <- precalc_te_sete_download_handler(
    "Subgroup",
    function(result) plot_precalc_te_ci_subgroup(result, input$precalc_te_sete_col_square, input$precalc_te_sete_col_square_lines, identical(input$precalc_te_sete_forest_sort, "yes")),
    require_component = "subgroup",
    settings_prefix = "precalc_te_sete_subgroup"
  )
  output$download_precalc_te_sete_metareg <- precalc_te_sete_download_handler(
    "Metarregression",
    function(result) plot_precalc_te_ci_metareg(result, input$precalc_te_sete_col_square),
    require_component = "metareg",
    settings_prefix = "precalc_te_sete_metareg"
  )

  observeEvent(input$load_precalc_te_sete_ci_example, {
    data <- precalc_te_sete_ci_example_data()
    precalc_te_sete_ci_data(data)
    precalc_te_sete_ci_multi_data(NULL)
    precalc_te_sete_ci_status(example_loaded_message(data))
  })

  observeEvent(input$precalc_te_sete_ci_file, {
    tryCatch({
      data <- read_single_prop_table(path = input$precalc_te_sete_ci_file$datapath)
      data <- validate_precalc_te_sete_ci_data(data)
      precalc_te_sete_ci_data(data)
      precalc_te_sete_ci_multi_data(NULL)
      precalc_te_sete_ci_status(paste0("Imported dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      precalc_te_sete_ci_status(paste("Import error:", error$message))
    })
  })

  observeEvent(input$use_precalc_te_sete_ci_paste, {
    tryCatch({
      data <- read_single_prop_table(pasted = input$precalc_te_sete_ci_paste)
      data <- validate_precalc_te_sete_ci_data(data)
      precalc_te_sete_ci_data(data)
      precalc_te_sete_ci_multi_data(NULL)
      precalc_te_sete_ci_status(paste0("Pasted dataset loaded: ", nrow(data), " rows and ", ncol(data), " columns."))
    }, error = function(error) {
      precalc_te_sete_ci_status(paste("Paste error:", error$message))
    })
  })

  observeEvent(input$precalc_te_sete_ci_multi_file, {
    tryCatch({
      outcomes <- read_outcome_workbook(input$precalc_te_sete_ci_multi_file$datapath, validate_precalc_te_sete_ci_data)
      precalc_te_sete_ci_multi_data(outcomes)
      precalc_te_sete_ci_data(NULL)
      precalc_te_sete_ci_status(paste0("Multi-outcome workbook loaded: ", length(outcomes), " outcomes detected."))
    }, error = function(error) {
      precalc_te_sete_ci_multi_data(NULL)
      precalc_te_sete_ci_status(paste("Workbook import error:", error$message))
    })
  })

  observeEvent(precalc_te_sete_ci_data(), {
    data <- precalc_te_sete_ci_data()
    req(!is.null(data))
    optional_cols <- setdiff(names(data), c("study", "TE", "seTE", "lower", "upper", "n.e", "n.c"))
    numeric_optional_cols <- optional_cols[vapply(data[optional_cols], is.numeric, logical(1))]
    remember_picker_choices(session, "precalc_te_sete_ci_subgroup_pick", optional_cols)
    remember_picker_choices(session, "precalc_te_sete_ci_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "precalc_te_sete_ci_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "precalc_te_sete_ci_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "precalc_te_sete_ci_forest_cols", optional_cols)
    precalc_te_sete_ci_result(NULL)
    set_batch_mode_ui("precalc_te_sete_ci", FALSE)
  })

  observeEvent(precalc_te_sete_ci_multi_data(), {
    outcomes <- precalc_te_sete_ci_multi_data()
    req(!is.null(outcomes))
    optional_cols <- common_optional_columns(outcomes, c("study", "TE", "seTE", "lower", "upper", "n.e", "n.c"))
    numeric_optional_cols <- common_numeric_optional_columns(outcomes, c("study", "TE", "seTE", "lower", "upper", "n.e", "n.c"))
    remember_picker_choices(session, "precalc_te_sete_ci_subgroup_pick", optional_cols)
    remember_picker_choices(session, "precalc_te_sete_ci_metareg_pick", numeric_optional_cols)
    updateSelectizeInput(session, "precalc_te_sete_ci_subgroup_table_cols", choices = optional_cols, selected = character(0))
    updateSelectizeInput(session, "precalc_te_sete_ci_metareg_table_cols", choices = numeric_optional_cols, selected = character(0))
    update_forest_column_selector(session, "precalc_te_sete_ci_forest_cols", optional_cols)
    precalc_te_sete_ci_result(NULL)
    set_batch_mode_ui("precalc_te_sete_ci", TRUE)
  })

  output$precalc_te_sete_ci_data_status <- renderText({ precalc_te_sete_ci_status() })
  output$precalc_te_sete_ci_multi_status <- renderText({ precalc_te_sete_ci_status() })
  output$precalc_te_sete_ci_run_status <- renderText({ precalc_te_sete_ci_status() })
  output$precalc_te_sete_ci_run_status_results <- renderText({ precalc_te_sete_ci_status() })

  observeEvent(input$precalc_te_sete_ci_to_params, {
    if (is.null(precalc_te_sete_ci_data()) && is.null(precalc_te_sete_ci_multi_data())) {
      precalc_te_sete_ci_status("Please load, import, or paste a dataset before going to parameters.")
      return()
    }
    updateTabsetPanel(session, "precalc_te_sete_ci_steps", selected = "parameters")
  })

  observeEvent(input$precalc_te_sete_ci_back_to_data, {
    updateTabsetPanel(session, "precalc_te_sete_ci_steps", selected = "data")
  })

  observeEvent(input$precalc_te_sete_ci_back_to_params, {
    updateTabsetPanel(session, "precalc_te_sete_ci_steps", selected = "parameters")
  })

  observeEvent(input$run_precalc_te_sete_ci, {
    clear_batch_export_cache("precalc_te_sete_ci")
    if (!is.null(precalc_te_sete_ci_multi_data())) set_download_buttons_state("precalc_te_sete_ci", TRUE, batch = TRUE)
    precalc_te_sete_ci_status("Running analysis...")
    tryCatch({
      if (!requireNamespace("meta", quietly = TRUE)) {
        stop("The meta package is required. Install it with install.packages('meta').")
      }
      comparison_labels <- resolve_comparison_labels(input$precalc_te_sete_ci_label_e, input$precalc_te_sete_ci_label_c, input$precalc_te_sete_ci_outcome_direction)
      params <- list(
        outcome_name = input$precalc_te_sete_ci_outcome,
        # Both columns come from the step-3 pickers, applied by dynamic_result_reactive().
        subgroup_col = "",
        metareg_col = "",
        model_choice = if (is.null(input$precalc_te_sete_ci_model) || !nzchar(input$precalc_te_sete_ci_model)) "random" else input$precalc_te_sete_ci_model,
        prediction_flag = identical(input$precalc_te_sete_ci_prediction, "yes"),
        sm = input$precalc_te_sete_ci_sm,
        method_tau = input$precalc_te_sete_ci_method_tau,
        method_i2 = if (is.null(input$precalc_te_sete_ci_method_i2)) "Q" else input$precalc_te_sete_ci_method_i2,
        method_random_ci = input$precalc_te_sete_ci_method_random_ci,
        method_predict = input$precalc_te_sete_ci_method_predict,
        label_e = comparison_labels$label_e,
        label_c = comparison_labels$label_c,
        label_left = comparison_labels$label_left,
        label_right = comparison_labels$label_right,
        forest_cols = sanitize_forest_columns(input$precalc_te_sete_ci_forest_cols)
      )

      if (!is.null(precalc_te_sete_ci_multi_data())) {
        result <- analyze_outcome_batch(precalc_te_sete_ci_multi_data(), analyze_precalc_te_sete_ci, params)
        # Same dyn contract as the single-outcome path, so the step-3 pickers can
        # re-run every outcome with a different column.
        result$dyn <- list(analyzer = analyze_precalc_te_sete_ci, outcomes = precalc_te_sete_ci_multi_data(), params = params)
        precalc_te_sete_ci_result(result)
        precalc_te_sete_ci_status("Continue below to preview or export the results you want.")
      } else {
        data <- precalc_te_sete_ci_data()
        if (is.null(data)) stop("Please load, import, or paste a dataset before running the analysis.")
        single_run <- analyze_precalc_te_sete_ci(data, params)
        single_run$dyn <- list(analyzer = analyze_precalc_te_sete_ci, data = data, params = params)
        precalc_te_sete_ci_result(single_run)
        precalc_te_sete_ci_status("Continue below to preview or export the results you want.")
      }

      updateTabsetPanel(session, "precalc_te_sete_ci_steps", selected = "results")
    }, error = function(error) {
      precalc_te_sete_ci_status(paste("Analysis error:", error$message))
      showNotification(paste("Analysis error:", error$message), type = "error", duration = 8)
    })
  })

  output$precalc_te_sete_ci_summary <- renderPrint({
    result <- precalc_te_sete_ci_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to see the model summary here.")
      return()
    }
    if (is_batch_result(result)) {
      cat("Batch mode summary\n\n")
      print(batch_result_overview(result))
      return()
    }
    cat("Outcome:", input$precalc_te_sete_ci_outcome, "\n\n")
    print(summary(result$meta))
  })

  output$precalc_te_sete_ci_forest <- renderPlot({
    result <- precalc_te_sete_ci_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the forest plot."))
    plot_precalc_te_ci_forest(result, input$precalc_te_sete_ci_col_square, input$precalc_te_sete_ci_col_square_lines, identical(input$precalc_te_sete_ci_forest_sort, "yes"))
  })

  output$precalc_te_sete_ci_loo <- renderPlot({
    result <- precalc_te_sete_ci_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the leave-one-out plot."))
    plot_precalc_te_ci_loo(result, input$precalc_te_sete_ci_col_square)
  })

  output$precalc_te_sete_ci_funnel <- renderPlot({
    result <- precalc_te_sete_ci_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the funnel plot."))
    plot_precalc_te_ci_funnel(result, input$precalc_te_sete_ci_col_square)
  })

  output$precalc_te_sete_ci_bias <- renderPrint({
    result <- precalc_te_sete_ci_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to evaluate small-study effects.")
      return()
    }
    if (nrow(result$data) < 10) {
      cat("Egger test is usually recommended only when there are at least 10 studies.")
      return()
    }
    print(meta::metabias(result$meta, method.bias = "Egger", plotit = FALSE))
  })

  output$precalc_te_sete_ci_subgroup_plot <- renderPlot({
    result <- precalc_te_sete_ci_result_dyn()
    validate(need(!is.null(result) && !is.null(result$subgroup), if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot."))
    plot_precalc_te_ci_subgroup(result, input$precalc_te_sete_ci_col_square, input$precalc_te_sete_ci_col_square_lines, identical(input$precalc_te_sete_ci_forest_sort, "yes"))
  })

  output$precalc_te_sete_ci_subgroup_note <- renderPrint({
    result <- precalc_te_sete_ci_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a subgroup column in this card.")
      return()
    }
    print(summary(result$subgroup))
  })

  output$precalc_te_sete_ci_metareg_plot <- renderPlot({
    result <- precalc_te_sete_ci_result_dyn()
    validate(need(!is.null(result) && !is.null(result$metareg), if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot."))
    plot_precalc_te_ci_metareg(result, input$precalc_te_sete_ci_col_square)
  })

  output$precalc_te_sete_ci_metareg_summary <- renderPrint({
    result <- precalc_te_sete_ci_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a moderator column in this card.")
      return()
    }
    print(summary(result$metareg))
  })

  output$precalc_te_sete_ci_metareg_table <- renderTable({
    extract_precalc_te_ci_metareg_table(precalc_te_sete_ci_result_dyn())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  observeEvent(input$preview_precalc_te_sete_ci_forest, {
    req(precalc_te_sete_ci_result_dyn())
    open_single_prop_plot_modal("Forest plot preview", "precalc_te_sete_ci_forest", "72vh")
  })

  observeEvent(input$preview_precalc_te_sete_ci_loo, {
    req(precalc_te_sete_ci_result_dyn())
    open_single_prop_plot_modal("Leave-one-out preview", "precalc_te_sete_ci_loo", "72vh")
  })

  observeEvent(input$preview_precalc_te_sete_ci_funnel, {
    req(precalc_te_sete_ci_result_dyn())
    open_single_prop_plot_modal("Funnel plot preview", "precalc_te_sete_ci_funnel", "72vh")
  })

  observeEvent(input$preview_precalc_te_sete_ci_subgroup, {
    result <- precalc_te_sete_ci_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_plot_modal("Subgroup analysis preview", "precalc_te_sete_ci_subgroup_plot", "72vh")
  })

  observeEvent(input$preview_precalc_te_sete_ci_metareg, {
    result <- precalc_te_sete_ci_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    if (is_batch_result(result)) {
      showNotification("Preview is unavailable in batch mode. Use Download to export all outcomes.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression preview",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("precalc_te_sete_ci_metareg_plot", height = "60vh"),
      tags$hr(),
      tableOutput("precalc_te_sete_ci_metareg_table")
    ))
  })

  observeEvent(input$summary_precalc_te_sete_ci_main, {
    req(precalc_te_sete_ci_result_dyn())
    open_single_prop_text_modal("Main meta-analysis summary", "precalc_te_sete_ci_summary")
  })

  observeEvent(input$summary_precalc_te_sete_ci_bias, {
    req(precalc_te_sete_ci_result_dyn())
    open_single_prop_text_modal("Small-study effects summary", "precalc_te_sete_ci_bias")
  })

  observeEvent(input$summary_precalc_te_sete_ci_subgroup, {
    result <- precalc_te_sete_ci_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_text_modal("Subgroup analysis summary", "precalc_te_sete_ci_subgroup_note")
  })

  observeEvent(input$summary_precalc_te_sete_ci_metareg, {
    result <- precalc_te_sete_ci_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression summary",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("precalc_te_sete_ci_metareg_table"),
      tags$hr(),
      verbatimTextOutput("precalc_te_sete_ci_metareg_summary")
    ))
  })

  precalc_te_sete_ci_download_handler <- function(plot_label, plot_function, require_component = NULL, settings_prefix) {
    downloadHandler(
      filename = function() {
        outcome <- input$precalc_te_sete_ci_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        result <- precalc_te_sete_ci_result_dyn()
        if (is_batch_result(result)) {
          return(paste0(plot_label, "_Batch.zip"))
        }
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- precalc_te_sete_ci_result_dyn()
        if (is.null(result)) stop("Run the analysis before downloading plots.")
        if (!is_batch_result(result) && !is.null(require_component) && is.null(result[[require_component]])) {
          stop(paste("This plot is unavailable because", require_component, "was not selected."))
        }
        settings <- get_card_export_settings(settings_prefix)
        width <- settings$width
        height <- settings$height
        file_format <- settings$file_format
        if (is_batch_result(result)) {
          copy_prepared_batch_export(file, "precalc_te_sete_ci", result, plot_label, file_format, width, height, plot_function, require_component)
          return(invisible(NULL))
        }
        write_plot_export(file, result, plot_label, file_format, width, height, plot_function, require_component)
      }
    )
  }

  output$download_precalc_te_sete_ci_forest <- precalc_te_sete_ci_download_handler(
    "Forestplot",
    function(result) plot_precalc_te_ci_forest(result, input$precalc_te_sete_ci_col_square, input$precalc_te_sete_ci_col_square_lines, identical(input$precalc_te_sete_ci_forest_sort, "yes")),
    settings_prefix = "precalc_te_sete_ci_forest"
  )
  output$download_precalc_te_sete_ci_loo <- precalc_te_sete_ci_download_handler("Leave_one_out", function(result) plot_precalc_te_ci_loo(result, input$precalc_te_sete_ci_col_square), settings_prefix = "precalc_te_sete_ci_loo")
  output$download_precalc_te_sete_ci_funnel <- precalc_te_sete_ci_download_handler("FunnelPlot", function(result) plot_precalc_te_ci_funnel(result, input$precalc_te_sete_ci_col_square), settings_prefix = "precalc_te_sete_ci_funnel")
  output$download_precalc_te_sete_ci_subgroup <- precalc_te_sete_ci_download_handler(
    "Subgroup",
    function(result) plot_precalc_te_ci_subgroup(result, input$precalc_te_sete_ci_col_square, input$precalc_te_sete_ci_col_square_lines, identical(input$precalc_te_sete_ci_forest_sort, "yes")),
    require_component = "subgroup",
    settings_prefix = "precalc_te_sete_ci_subgroup"
  )
  output$download_precalc_te_sete_ci_metareg <- precalc_te_sete_ci_download_handler(
    "Metarregression",
    function(result) plot_precalc_te_ci_metareg(result, input$precalc_te_sete_ci_col_square),
    require_component = "metareg",
    settings_prefix = "precalc_te_sete_ci_metareg"
  )

  output$single_prop_summary <- renderPrint({
    result <- single_prop_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to see the model summary here.")
      return()
    }
    if (is_batch_result(result)) {
      cat("Batch mode summary\n\n")
      print(batch_result_overview(result))
      return()
    }
    cat("Outcome:", input$single_prop_outcome, "\n\n")
    print(summary(result$meta))
  })

  output$single_prop_forest <- renderPlot({
    result <- single_prop_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the forest plot."))
    validate(need(!is_batch_result(result), "Preview is available only for a single outcome. Use Download to export all outcomes."))

    plot_single_prop_forest(result, input$single_prop_col_square, input$single_prop_col_square_lines, identical(input$single_prop_forest_sort, "yes"))
  })

  output$single_prop_loo <- renderPlot({
    result <- single_prop_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the leave-one-out plot."))
    validate(need(!is_batch_result(result), "Preview is available only for a single outcome. Use Download to export all outcomes."))

    plot_single_prop_loo(result, input$single_prop_col_square)
  })

  output$single_prop_funnel <- renderPlot({
    result <- single_prop_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the funnel plot."))
    validate(need(!is_batch_result(result), "Preview is available only for a single outcome. Use Download to export all outcomes."))

    plot_single_prop_funnel(result, input$single_prop_col_square)
  })

  output$single_prop_bias <- renderPrint({
    result <- single_prop_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to evaluate small-study effects.")
      return()
    }
    if (is_batch_result(result)) {
      cat("Batch mode does not show a combined preview here. Use the plot downloads to export all outcomes.")
      return()
    }

    if (nrow(result$data) < 10) {
      cat("Egger test is usually recommended only when there are at least 10 studies.")
      return()
    }

    print(meta::metabias(
      result$meta,
      method.bias = "Egger",
      plotit = FALSE
    ))
  })

  output$single_prop_subgroup_plot <- renderPlot({
    result <- single_prop_result_dyn()
    validate(need(!is.null(result) && !is.null(result$subgroup), if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot."))
    validate(need(!is_batch_result(result), "Preview is available only for a single outcome. Use Download to export all outcomes."))

    plot_single_prop_subgroup(result, input$single_prop_col_square, input$single_prop_col_square_lines, identical(input$single_prop_forest_sort, "yes"))
  })

  output$single_prop_subgroup_note <- renderPrint({
    result <- single_prop_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a subgroup column in this card.")
      return()
    }
    if (is_batch_result(result)) {
      cat("Batch mode does not show a combined subgroup summary here. Use the plot downloads to export all outcomes.")
      return()
    }
    print(summary(result$subgroup))
  })

  output$single_prop_metareg_plot <- renderPlot({
    result <- single_prop_result_dyn()
    validate(need(!is.null(result) && !is.null(result$metareg), if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot."))
    validate(need(!is_batch_result(result), "Preview is available only for a single outcome. Use Download to export all outcomes."))

    plot_single_prop_metareg(result, input$single_prop_col_square)
  })

  output$single_prop_metareg_summary <- renderPrint({
    result <- single_prop_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a moderator column in this card.")
      return()
    }
    if (is_batch_result(result)) {
      cat("Batch mode does not show a combined meta-regression summary here. Use the plot downloads to export all outcomes.")
      return()
    }
    print(summary(result$metareg))
  })

  output$single_prop_metareg_table <- renderTable({
    req(!is_batch_result(single_prop_result_dyn()))
    extract_single_prop_metareg_table(single_prop_result_dyn())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  # Opens a plot in a modal. Shared by every module despite the name.
  #
  # height = "auto" lets the image keep whatever ratio renderPlot gave it. The
  # summary-point previews use that, because a ROC panel stretched to the modal
  # width turns a round confidence region into a flat sliver.
  open_single_prop_plot_modal <- function(title, output_id, height = "70vh") {
    showModal(modalDialog(
      title = title,
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput(output_id, height = height)
    ))
  }

  # The same for a text summary.
  open_single_prop_text_modal <- function(title, output_id) {
    showModal(modalDialog(
      title = title,
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      verbatimTextOutput(output_id)
    ))
  }

  observeEvent(input$preview_single_prop_forest, {
    req(single_prop_result_dyn())
    if (is_batch_result(single_prop_result_dyn())) {
      showNotification("Preview is unavailable in batch mode. Use Download to export all outcomes.", type = "message", duration = 6)
      return()
    }
    open_single_prop_plot_modal("Forest plot preview", "single_prop_forest", "72vh")
  })

  observeEvent(input$preview_single_prop_loo, {
    req(single_prop_result_dyn())
    if (is_batch_result(single_prop_result_dyn())) {
      showNotification("Preview is unavailable in batch mode. Use Download to export all outcomes.", type = "message", duration = 6)
      return()
    }
    open_single_prop_plot_modal("Leave-one-out preview", "single_prop_loo", "72vh")
  })

  observeEvent(input$preview_single_prop_funnel, {
    req(single_prop_result_dyn())
    if (is_batch_result(single_prop_result_dyn())) {
      showNotification("Preview is unavailable in batch mode. Use Download to export all outcomes.", type = "message", duration = 6)
      return()
    }
    open_single_prop_plot_modal("Funnel plot preview", "single_prop_funnel", "72vh")
  })

  observeEvent(input$preview_single_prop_subgroup, {
    result <- single_prop_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    if (is_batch_result(result)) {
      showNotification("Preview is unavailable in batch mode. Use Download to export all outcomes.", type = "message", duration = 6)
      return()
    }
    open_single_prop_plot_modal("Subgroup analysis preview", "single_prop_subgroup_plot", "72vh")
  })

  observeEvent(input$preview_single_prop_metareg, {
    result <- single_prop_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    if (is_batch_result(result)) {
      showNotification("Preview is unavailable in batch mode. Use Download to export all outcomes.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression preview",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("single_prop_metareg_plot", height = "60vh"),
      tags$hr(),
      tableOutput("single_prop_metareg_table")
    ))
  })

  observeEvent(input$summary_single_prop_main, {
    req(single_prop_result_dyn())
    open_single_prop_text_modal("Main meta-analysis summary", "single_prop_summary")
  })

  observeEvent(input$summary_single_prop_bias, {
    req(single_prop_result_dyn())
    open_single_prop_text_modal("Small-study effects summary", "single_prop_bias")
  })

  observeEvent(input$summary_single_prop_subgroup, {
    result <- single_prop_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_text_modal("Subgroup analysis summary", "single_prop_subgroup_note")
  })

  observeEvent(input$summary_single_prop_metareg, {
    result <- single_prop_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression summary",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("single_prop_metareg_table"),
      tags$hr(),
      verbatimTextOutput("single_prop_metareg_summary")
    ))
  })

  # One *_download_handler() per module, all the same shape: work out the file
  # name from the outcome, then either write one plot or, for a batch, hand over
  # the prepared archive. They stay separate because each reads its own module's
  # export settings and colour inputs.
  #
  # Every one reads the dynamic result, so a file always matches the column
  # selected in the card at the moment it is downloaded.
  single_prop_download_handler <- function(plot_id, plot_label, plot_function, require_component = NULL, settings_prefix) {
    downloadHandler(
      filename = function() {
        outcome <- input$single_prop_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        result <- single_prop_result_dyn()
        if (is_batch_result(result)) {
          return(paste0(plot_label, "_Batch.zip"))
        }
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- single_prop_result_dyn()
        if (is.null(result)) {
          stop("Run the analysis before downloading plots.")
        }
        if (!is_batch_result(result) && !is.null(require_component) && is.null(result[[require_component]])) {
          stop(paste("This plot is unavailable because", require_component, "was not selected."))
        }

        settings <- get_card_export_settings(settings_prefix)
        width <- settings$width
        height <- settings$height
        file_format <- settings$file_format
        if (is_batch_result(result)) {
          copy_prepared_batch_export(file, "single_prop", result, plot_label, file_format, width, height, plot_function, require_component)
          return(invisible(NULL))
        }
        write_plot_export(file, result, plot_label, file_format, width, height, plot_function, require_component)
      }
    )
  }

  output$download_single_prop_forest <- single_prop_download_handler(
    "forest",
    "Forestplot",
    function(result) plot_single_prop_forest(result, input$single_prop_col_square, input$single_prop_col_square_lines, identical(input$single_prop_forest_sort, "yes")),
    settings_prefix = "single_prop_forest"
  )

  output$download_single_prop_loo <- single_prop_download_handler(
    "loo",
    "Leave-one-out",
    function(result) plot_single_prop_loo(result, input$single_prop_col_square), settings_prefix = "single_prop_loo"
  )

  output$download_single_prop_funnel <- single_prop_download_handler(
    "funnel",
    "FunnelPlot",
    function(result) plot_single_prop_funnel(result, input$single_prop_col_square), settings_prefix = "single_prop_funnel"
  )

  output$download_single_prop_subgroup <- single_prop_download_handler(
    "subgroup",
    "Subgroup",
    function(result) plot_single_prop_subgroup(result, input$single_prop_col_square, input$single_prop_col_square_lines, identical(input$single_prop_forest_sort, "yes")),
    require_component = "subgroup",
    settings_prefix = "single_prop_subgroup"
  )

  output$download_single_prop_metareg <- single_prop_download_handler(
    "metareg",
    "Metarregression",
    function(result) plot_single_prop_metareg(result, input$single_prop_col_square),
    require_component = "metareg",
    settings_prefix = "single_prop_metareg"
  )

  output$single_mean_summary <- renderPrint({
    result <- single_mean_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to see the model summary here.")
      return()
    }
    if (is_batch_result(result)) {
      cat("Batch mode summary\n\n")
      print(batch_result_overview(result))
      return()
    }
    cat("Outcome:", input$single_mean_outcome, "\n\n")
    print(summary(result$meta))
  })

  output$single_mean_forest <- renderPlot({
    result <- single_mean_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the forest plot."))
    plot_single_mean_forest(result, input$single_mean_col_square, input$single_mean_col_square_lines, identical(input$single_mean_forest_sort, "yes"))
  })

  output$single_mean_loo <- renderPlot({
    result <- single_mean_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the leave-one-out plot."))
    plot_single_mean_loo(result, input$single_mean_col_square)
  })

  output$single_mean_funnel <- renderPlot({
    result <- single_mean_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the funnel plot."))
    plot_single_mean_funnel(result, input$single_mean_col_square)
  })

  output$single_mean_bias <- renderPrint({
    result <- single_mean_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to evaluate small-study effects.")
      return()
    }
    if (nrow(result$data) < 10) {
      cat("Egger test is usually recommended only when there are at least 10 studies.")
      return()
    }
    print(meta::metabias(result$meta, method.bias = "Egger", plotit = FALSE))
  })

  output$single_mean_subgroup_plot <- renderPlot({
    result <- single_mean_result_dyn()
    validate(need(!is.null(result) && !is.null(result$subgroup), if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot."))
    plot_single_mean_subgroup(result, input$single_mean_col_square, input$single_mean_col_square_lines, identical(input$single_mean_forest_sort, "yes"))
  })

  output$single_mean_subgroup_note <- renderPrint({
    result <- single_mean_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a subgroup column in this card.")
      return()
    }
    print(summary(result$subgroup))
  })

  output$single_mean_metareg_plot <- renderPlot({
    result <- single_mean_result_dyn()
    validate(need(!is.null(result) && !is.null(result$metareg), if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot."))
    plot_single_mean_metareg(result, input$single_mean_col_square)
  })

  output$single_mean_metareg_summary <- renderPrint({
    result <- single_mean_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a moderator column in this card.")
      return()
    }
    print(summary(result$metareg))
  })

  output$single_mean_metareg_table <- renderTable({
    extract_single_mean_metareg_table(single_mean_result_dyn())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  observeEvent(input$preview_single_mean_forest, {
    req(single_mean_result_dyn())
    open_single_prop_plot_modal("Forest plot preview", "single_mean_forest", "72vh")
  })

  observeEvent(input$preview_single_mean_loo, {
    req(single_mean_result_dyn())
    open_single_prop_plot_modal("Leave-one-out preview", "single_mean_loo", "72vh")
  })

  observeEvent(input$preview_single_mean_funnel, {
    req(single_mean_result_dyn())
    open_single_prop_plot_modal("Funnel plot preview", "single_mean_funnel", "72vh")
  })

  observeEvent(input$preview_single_mean_subgroup, {
    result <- single_mean_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_plot_modal("Subgroup analysis preview", "single_mean_subgroup_plot", "72vh")
  })

  observeEvent(input$preview_single_mean_metareg, {
    result <- single_mean_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression preview",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("single_mean_metareg_plot", height = "60vh"),
      tags$hr(),
      tableOutput("single_mean_metareg_table")
    ))
  })

  observeEvent(input$summary_single_mean_main, {
    req(single_mean_result_dyn())
    open_single_prop_text_modal("Main meta-analysis summary", "single_mean_summary")
  })

  observeEvent(input$summary_single_mean_bias, {
    req(single_mean_result_dyn())
    open_single_prop_text_modal("Small-study effects summary", "single_mean_bias")
  })

  observeEvent(input$summary_single_mean_subgroup, {
    result <- single_mean_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_text_modal("Subgroup analysis summary", "single_mean_subgroup_note")
  })

  observeEvent(input$summary_single_mean_metareg, {
    result <- single_mean_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression summary",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("single_mean_metareg_table"),
      tags$hr(),
      verbatimTextOutput("single_mean_metareg_summary")
    ))
  })

  single_mean_download_handler <- function(plot_label, plot_function, require_component = NULL, settings_prefix) {
    downloadHandler(
      filename = function() {
        outcome <- input$single_mean_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        result <- single_mean_result_dyn()
        if (is_batch_result(result)) {
          return(paste0(plot_label, "_Batch.zip"))
        }
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- single_mean_result_dyn()
        if (is.null(result)) stop("Run the analysis before downloading plots.")
        if (!is_batch_result(result) && !is.null(require_component) && is.null(result[[require_component]])) {
          stop(paste("This plot is unavailable because", require_component, "was not selected."))
        }
        settings <- get_card_export_settings(settings_prefix)
        width <- settings$width
        height <- settings$height
        file_format <- settings$file_format
        if (is_batch_result(result)) {
          copy_prepared_batch_export(file, "single_mean", result, plot_label, file_format, width, height, plot_function, require_component)
          return(invisible(NULL))
        }
        write_plot_export(file, result, plot_label, file_format, width, height, plot_function, require_component)
      }
    )
  }

  output$download_single_mean_forest <- single_mean_download_handler(
    "Forestplot",
    function(result) plot_single_mean_forest(result, input$single_mean_col_square, input$single_mean_col_square_lines, identical(input$single_mean_forest_sort, "yes")),
    settings_prefix = "single_mean_forest"
  )
  output$download_single_mean_loo <- single_mean_download_handler("Leave_one_out", function(result) plot_single_mean_loo(result, input$single_mean_col_square), settings_prefix = "single_mean_loo")
  output$download_single_mean_funnel <- single_mean_download_handler("FunnelPlot", function(result) plot_single_mean_funnel(result, input$single_mean_col_square), settings_prefix = "single_mean_funnel")
  output$download_single_mean_subgroup <- single_mean_download_handler(
    "Subgroup",
    function(result) plot_single_mean_subgroup(result, input$single_mean_col_square, input$single_mean_col_square_lines, identical(input$single_mean_forest_sort, "yes")),
    require_component = "subgroup",
    settings_prefix = "single_mean_subgroup"
  )
  output$download_single_mean_metareg <- single_mean_download_handler(
    "Metarregression",
    function(result) plot_single_mean_metareg(result, input$single_mean_col_square),
    require_component = "metareg",
    settings_prefix = "single_mean_metareg"
  )

  output$binary_summary <- renderPrint({
    result <- binary_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to see the model summary here.")
      return()
    }
    if (is_batch_result(result)) {
      cat("Batch mode summary\n\n")
      print(batch_result_overview(result))
      return()
    }
    cat("Outcome:", input$binary_outcome, "\n\n")
    print(summary(result$meta))
  })

  output$binary_forest <- renderPlot({
    result <- binary_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the forest plot."))
    plot_binary_forest(result, input$binary_col_square, input$binary_col_square_lines, identical(input$binary_forest_sort, "yes"))
  })

  output$binary_loo <- renderPlot({
    result <- binary_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the leave-one-out plot."))
    plot_binary_loo(result, input$binary_col_square)
  })

  output$binary_funnel <- renderPlot({
    result <- binary_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the funnel plot."))
    plot_binary_funnel(result, input$binary_col_square)
  })

  output$binary_bias <- renderPrint({
    result <- binary_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to evaluate small-study effects.")
      return()
    }
    if (nrow(result$data) < 10) {
      cat("Egger test is usually recommended only when there are at least 10 studies.")
      return()
    }
    print(meta::metabias(result$meta, method.bias = "Egger", plotit = FALSE))
  })

  output$binary_subgroup_plot <- renderPlot({
    result <- binary_result_dyn()
    validate(need(!is.null(result) && !is.null(result$subgroup), if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot."))
    plot_binary_subgroup(result, input$binary_col_square, input$binary_col_square_lines, identical(input$binary_forest_sort, "yes"))
  })

  output$binary_subgroup_note <- renderPrint({
    result <- binary_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a subgroup column in this card.")
      return()
    }
    print(summary(result$subgroup))
  })

  output$binary_metareg_plot <- renderPlot({
    result <- binary_result_dyn()
    validate(need(!is.null(result) && !is.null(result$metareg), if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot."))
    plot_binary_metareg(result, input$binary_col_square)
  })

  output$binary_metareg_summary <- renderPrint({
    result <- binary_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a moderator column in this card.")
      return()
    }
    print(summary(result$metareg))
  })

  output$binary_metareg_table <- renderTable({
    extract_binary_metareg_table(binary_result_dyn())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  observeEvent(input$preview_binary_forest, {
    req(binary_result_dyn())
    open_single_prop_plot_modal("Forest plot preview", "binary_forest", "72vh")
  })

  observeEvent(input$preview_binary_loo, {
    req(binary_result_dyn())
    open_single_prop_plot_modal("Leave-one-out preview", "binary_loo", "72vh")
  })

  observeEvent(input$preview_binary_funnel, {
    req(binary_result_dyn())
    open_single_prop_plot_modal("Funnel plot preview", "binary_funnel", "72vh")
  })

  observeEvent(input$preview_binary_subgroup, {
    result <- binary_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_plot_modal("Subgroup analysis preview", "binary_subgroup_plot", "72vh")
  })

  observeEvent(input$preview_binary_metareg, {
    result <- binary_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression preview",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("binary_metareg_plot", height = "60vh"),
      tags$hr(),
      tableOutput("binary_metareg_table")
    ))
  })

  observeEvent(input$summary_binary_main, {
    req(binary_result_dyn())
    open_single_prop_text_modal("Main meta-analysis summary", "binary_summary")
  })

  observeEvent(input$summary_binary_bias, {
    req(binary_result_dyn())
    open_single_prop_text_modal("Small-study effects summary", "binary_bias")
  })

  # In single-outcome mode the subgroup and moderator columns are chosen in step
  # 3, inside the result card itself. Here the analysis is RE-RUN by the same
  # analyze_*() with the same parameters, swapping only the column: it is
  # literally the step 2 path, so the numbers cannot diverge. Preview, summary
  # and download all read this same reactive, which is what stops the screen
  # showing one plot while the file carries another. Batch results are returned
  # untouched.
  #
  # The step 3 selectors start with "None" only; the options are remembered as
  # they are populated so a card with nothing to offer can be hidden.
  picker_choices <- reactiveValues()
  # Whether a step-3 picker has anything to offer, which is what decides if its
  # card is worth showing.
  picker_has_choices <- function(input_id) {
    isTRUE(picker_choices[[input_id]])
  }
  # Fills a picker and records whether it ended up with any options.
  remember_picker_choices <- function(session, input_id, choices) {
    cols <- setdiff(choices, "")
    picker_choices[[input_id]] <- length(cols) > 0
    updateSelectInput(session, input_id, choices = c("None" = "", cols))
  }

  # The result the cards actually read. It re-runs the analysis with the columns
  # chosen in step 3, for one outcome and for a batch alike, and falls back to the
  # untouched result when the column cannot be used. Preview, summary and download
  # all read this same reactive, which is what stops the screen showing one thing
  # while the file carries another.
  dynamic_result_reactive <- function(prefix, result_fun) {
    reactive({
      result <- result_fun()
      if (is.null(result)) return(result)
      dyn <- result$dyn
      if (is.null(dyn) || is.null(dyn$analyzer)) return(result)

      batch <- is_batch_result(result)
      frames <- if (batch) dyn$outcomes else list(dyn$data)
      # A column is usable when it exists and actually splits the studies. In
      # batch that only has to hold for one outcome: analyze_outcome_batch drops
      # the column, with a note, for the outcomes where it does not.
      usable <- function(col) {
        !is.null(col) && nzchar(col) && any(vapply(frames, function(d) {
          col %in% names(d) && length(unique(d[[col]][!is.na(d[[col]])])) > 1
        }, logical(1)))
      }
      sub_col <- if (usable(input[[paste0(prefix, "_subgroup_pick")]]))
        input[[paste0(prefix, "_subgroup_pick")]] else ""
      reg_col <- if (usable(input[[paste0(prefix, "_metareg_pick")]]))
        input[[paste0(prefix, "_metareg_pick")]] else ""

      if (!nzchar(sub_col) && !nzchar(reg_col)) return(result)

      params <- modifyList(dyn$params, list(subgroup_col = sub_col, metareg_col = reg_col))
      rerun <- tryCatch(
        if (batch) analyze_outcome_batch(dyn$outcomes, dyn$analyzer, params)
        else dyn$analyzer(dyn$data, params),
        error = function(e) NULL
      )
      if (is.null(rerun)) {
        # impossible column (a single study per level, say): keep the base
        # result instead of taking the card down
        return(result)
      }
      rerun$dyn <- dyn
      rerun
    })
  }


  diagnostic_single_result_dyn <- dynamic_result_reactive("diagnostic_single", diagnostic_single_result)
  single_prop_result_dyn <- dynamic_result_reactive("single_prop", single_prop_result)
  single_mean_result_dyn <- dynamic_result_reactive("single_mean", single_mean_result)
  binary_result_dyn <- dynamic_result_reactive("binary", binary_result)
  cont_mean_result_dyn <- dynamic_result_reactive("cont_mean", cont_mean_result)
  precalc_te_ci_result_dyn <- dynamic_result_reactive("precalc_te_ci", precalc_te_ci_result)
  precalc_te_sete_result_dyn <- dynamic_result_reactive("precalc_te_sete", precalc_te_sete_result)
  precalc_te_sete_ci_result_dyn <- dynamic_result_reactive("precalc_te_sete_ci", precalc_te_sete_ci_result)

  # Wires the two summary-table cards for a module: the preview modal and the
  # XLSX download, both built from the columns selected in the card itself.
  register_summary_tables <- function(prefix, result_fun) {
    subgroup_data <- reactive({
      result <- result_fun()
      if (is.null(result)) stop("Run the analysis before building the table.")
      build_subgroup_summary_table(result, input[[paste0(prefix, "_subgroup_table_cols")]])
    })

    metareg_data <- reactive({
      result <- result_fun()
      if (is.null(result)) stop("Run the analysis before building the table.")
      build_metareg_summary_table(result, input[[paste0(prefix, "_metareg_table_cols")]])
    })

    output[[paste0(prefix, "_subgroup_table_output")]] <- renderTable(
      subgroup_data(), striped = TRUE, bordered = TRUE, spacing = "m", align = "l", na = ""
    )
    output[[paste0(prefix, "_metareg_table_output")]] <- renderTable(
      metareg_data(), striped = TRUE, bordered = TRUE, spacing = "m", align = "l", na = ""
    )

    open_table_modal <- function(kind, title, note) {
      result <- result_fun()
      if (is.null(result)) {
        showNotification("Run the analysis before building the table.", type = "message", duration = 6)
        return()
      }
      if (length(input[[paste0(prefix, "_", kind, "_table_cols")]]) == 0) {
        showNotification("Select at least one column to include in the table.", type = "message", duration = 6)
        return()
      }
      table_data <- tryCatch(
        if (identical(kind, "subgroup")) subgroup_data() else metareg_data(),
        error = function(error) error
      )
      if (inherits(table_data, "error")) {
        showNotification(conditionMessage(table_data), type = "error", duration = 8)
        return()
      }
      showModal(modalDialog(
        title = title,
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tableOutput(paste0(prefix, "_", kind, "_table_output")),
        tags$p(class = "help-text", note)
      ))
    }

    observeEvent(input[[paste0("preview_", prefix, "_subgroup_table")]], {
      open_table_modal(
        "subgroup",
        "Subgroup summary table",
        "Estimates come from the same model used in the forest plot. The p value tests whether the effect differs across the levels of each variable."
      )
    })

    observeEvent(input[[paste0("preview_", prefix, "_metareg_table")]], {
      open_table_modal(
        "metareg",
        "Meta-regression summary table",
        "Each moderator is fitted in its own model, on the same effect sizes as the forest plot. Only numeric moderators are listed; to compare categories, use the subgroup summary table."
      )
    })

    output[[paste0("download_", prefix, "_subgroup_table")]] <- downloadHandler(
      filename = function() "Subgroup_summary_table.xlsx",
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) write_xlsx_workbook(file, list(Subgroups = subgroup_data()))
    )

    output[[paste0("download_", prefix, "_metareg_table")]] <- downloadHandler(
      filename = function() "Metaregression_summary_table.xlsx",
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) write_xlsx_workbook(file, list(Metaregression = metareg_data()))
    )
  }

  register_summary_tables("binary", binary_result)
  register_summary_tables("cont_mean", cont_mean_result)
  register_summary_tables("precalc_te_ci", precalc_te_ci_result)
  register_summary_tables("precalc_te_sete", precalc_te_sete_result)
  register_summary_tables("precalc_te_sete_ci", precalc_te_sete_ci_result)
  register_summary_tables("single_prop", single_prop_result)
  register_summary_tables("single_mean", single_mean_result)


  observeEvent(input$summary_binary_subgroup, {
    result <- binary_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_text_modal("Subgroup analysis summary", "binary_subgroup_note")
  })

  observeEvent(input$summary_binary_metareg, {
    result <- binary_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression summary",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("binary_metareg_table"),
      tags$hr(),
      verbatimTextOutput("binary_metareg_summary")
    ))
  })

  binary_download_handler <- function(plot_label, plot_function, require_component = NULL, settings_prefix) {
    downloadHandler(
      filename = function() {
        outcome <- input$binary_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        result <- binary_result_dyn()
        if (is_batch_result(result)) {
          return(paste0(plot_label, "_Batch.zip"))
        }
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- binary_result_dyn()
        if (is.null(result)) stop("Run the analysis before downloading plots.")
        if (!is_batch_result(result) && !is.null(require_component) && is.null(result[[require_component]])) {
          stop(paste("This plot is unavailable because", require_component, "was not selected."))
        }
        settings <- get_card_export_settings(settings_prefix)
        width <- settings$width
        height <- settings$height
        file_format <- settings$file_format
        if (is_batch_result(result)) {
          copy_prepared_batch_export(file, "binary", result, plot_label, file_format, width, height, plot_function, require_component)
          return(invisible(NULL))
        }
        write_plot_export(file, result, plot_label, file_format, width, height, plot_function, require_component)
      }
    )
  }

  output$download_binary_forest <- binary_download_handler(
    "Forestplot",
    function(result) plot_binary_forest(result, input$binary_col_square, input$binary_col_square_lines, identical(input$binary_forest_sort, "yes")),
    settings_prefix = "binary_forest"
  )
  output$download_binary_loo <- binary_download_handler("Leave_one_out", function(result) plot_binary_loo(result, input$binary_col_square), settings_prefix = "binary_loo")
  output$download_binary_funnel <- binary_download_handler("FunnelPlot", function(result) plot_binary_funnel(result, input$binary_col_square), settings_prefix = "binary_funnel")
  output$download_binary_subgroup <- binary_download_handler(
    "Subgroup",
    function(result) plot_binary_subgroup(result, input$binary_col_square, input$binary_col_square_lines, identical(input$binary_forest_sort, "yes")),
    require_component = "subgroup",
    settings_prefix = "binary_subgroup"
  )
  output$download_binary_metareg <- binary_download_handler(
    "Metarregression",
    function(result) plot_binary_metareg(result, input$binary_col_square),
    require_component = "metareg",
    settings_prefix = "binary_metareg"
  )

  output$network_binary_summary <- renderPrint({
    result <- network_binary_result()
    if (is.null(result)) {
      cat("Run the analysis to see the network meta-analysis summary here.")
      return()
    }
    cat("Outcome:", result$outcome_name, "\n\n")
    print(summary(result$nma))
  })

  output$network_binary_graph <- renderPlot({
    result <- network_binary_result()
    validate(need(!is.null(result), "Run the network analysis to create the network graph."))
    plot_network_binary_graph(result, input$network_binary_col_points)
  })

  output$network_binary_forest <- renderPlot({
    result <- network_binary_result()
    validate(need(!is.null(result), "Run the network analysis to create the forest plot."))
    plot_network_binary_forest(result)
  })

  output$network_binary_rankogram <- renderPlot({
    result <- network_binary_result()
    validate(need(!is.null(result), "Run the network analysis to create the rankogram."))
    plot_network_binary_rankogram(result, input$network_binary_col_points)
  })

  output$network_binary_funnel <- renderPlot({
    result <- network_binary_result()
    validate(need(!is.null(result), "Run the network analysis to create the funnel plot."))
    plot_network_binary_funnel(result)
  })

  output$network_binary_league_table <- renderTable({
    network_binary_league_table(network_binary_result())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$network_binary_qtest <- renderPrint({
    result <- network_binary_result()
    if (is.null(result)) {
      cat("Run the analysis to inspect between-study heterogeneity.")
      return()
    }
    cat(paste(network_binary_qtest_text(result), collapse = "\n"))
  })

  output$network_binary_split_summary <- renderPrint({
    result <- network_binary_result()
    if (is.null(result)) {
      cat("Run the analysis to inspect node splitting.")
      return()
    }
    cat(paste(network_binary_split_text(result), collapse = "\n"))
  })

  output$network_binary_rank_summary <- renderPrint({
    result <- network_binary_result()
    if (is.null(result)) {
      cat("Run the analysis to inspect treatment ranking.")
      return()
    }
    cat(paste(network_binary_rank_text(result), collapse = "\n"))
  })

  observeEvent(input$preview_network_binary_graph, {
    req(network_binary_result())
    open_single_prop_plot_modal("Network graph preview", "network_binary_graph", "72vh")
  })

  observeEvent(input$preview_network_binary_forest, {
    req(network_binary_result())
    open_single_prop_plot_modal("Network forest plot preview", "network_binary_forest", "72vh")
  })

  observeEvent(input$preview_network_binary_rankogram, {
    req(network_binary_result())
    open_single_prop_plot_modal("P-score ranking preview", "network_binary_rankogram", "72vh")
  })

  observeEvent(input$preview_network_binary_funnel, {
    req(network_binary_result())
    open_single_prop_plot_modal("Comparison-adjusted funnel plot preview", "network_binary_funnel", "72vh")
  })

  observeEvent(input$summary_network_binary_main, {
    req(network_binary_result())
    open_single_prop_text_modal("Network meta-analysis summary", "network_binary_summary")
  })

  observeEvent(input$summary_network_binary_graph, {
    req(network_binary_result())
    open_single_prop_text_modal("Network meta-analysis summary", "network_binary_summary")
  })

  observeEvent(input$summary_network_binary_league, {
    req(network_binary_result())
    showModal(modalDialog(
      title = "League table",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("network_binary_league_table")
    ))
  })

  observeEvent(input$summary_network_binary_qtest, {
    req(network_binary_result())
    open_single_prop_text_modal("Between-study heterogeneity", "network_binary_qtest")
  })

  observeEvent(input$summary_network_binary_split, {
    req(network_binary_result())
    open_single_prop_text_modal("Node-splitting summary", "network_binary_split_summary")
  })

  observeEvent(input$summary_network_binary_rank, {
    req(network_binary_result())
    open_single_prop_text_modal("Treatment ranking summary", "network_binary_rank_summary")
  })

  network_binary_download_handler <- function(plot_label, plot_function, settings_prefix) {
    downloadHandler(
      filename = function() {
        outcome <- input$network_binary_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- network_binary_result()
        if (is.null(result)) stop("Run the network analysis before downloading plots.")
        settings <- get_card_export_settings(settings_prefix)
        write_plot_export(file, result, plot_label, settings$file_format, settings$width, settings$height, plot_function)
      }
    )
  }

  output$download_network_binary_graph <- network_binary_download_handler(
    "Network_Graph",
    function(result) plot_network_binary_graph(result, input$network_binary_col_points),
    "network_binary_graph"
  )
  output$download_network_binary_forest <- network_binary_download_handler("Forestplot", plot_network_binary_forest, "network_binary_forest")
  output$download_network_binary_split <- network_binary_download_handler("SplitEvidence", function(result) plot_network_binary_split(result, input$network_binary_col_points), "network_binary_split")
  output$download_network_binary_rankogram <- network_binary_download_handler("P_score_ranking", function(result) plot_network_binary_rankogram(result, input$network_binary_col_points), "network_binary_rankogram")
  output$download_network_binary_funnel <- network_binary_download_handler("Comparison_Adjusted_Funnel", plot_network_binary_funnel, "network_binary_funnel")

  output$download_network_binary_pairwise <- downloadHandler(
    filename = function() {
      outcome <- input$network_binary_outcome
      if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
      paste0("Network_Docs_", gsub("[^A-Za-z0-9]+", "_", outcome), ".csv")
    },
    content = function(file) {
      result <- network_binary_result()
      if (is.null(result)) stop("Run the network analysis before downloading the pairwise object.")
      utils::write.csv(as.data.frame(result$pairwise), file, row.names = FALSE)
    }
  )

  output$download_network_binary_league <- downloadHandler(
    filename = function() {
      outcome <- input$network_binary_outcome
      if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
      paste0("nma_results_", gsub("[^A-Za-z0-9]+", "_", outcome), ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      result <- network_binary_result()
      if (is.null(result)) stop("Run the network analysis before downloading the league table.")
      write_xlsx_workbook(file, list(League_table = network_binary_league_table(result)))
    }
  )

  output$download_network_binary_qtest <- downloadHandler(
    filename = function() {
      outcome <- input$network_binary_outcome
      if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
      paste0("network_QTest_", gsub("[^A-Za-z0-9]+", "_", outcome), ".txt")
    },
    content = function(file) {
      result <- network_binary_result()
      if (is.null(result)) stop("Run the network analysis before downloading heterogeneity results.")
      writeLines(network_binary_qtest_text(result), file)
    }
  )

  output$network_cont_summary <- renderPrint({
    result <- network_cont_result()
    if (is.null(result)) {
      cat("Run the analysis to see the network meta-analysis summary here.")
      return()
    }
    cat("Outcome:", result$outcome_name, "\n\n")
    print(summary(result$nma))
  })

  output$network_cont_graph <- renderPlot({
    result <- network_cont_result()
    validate(need(!is.null(result), "Run the network analysis to create the network graph."))
    plot_network_binary_graph(result, input$network_cont_col_points)
  })

  output$network_cont_forest <- renderPlot({
    result <- network_cont_result()
    validate(need(!is.null(result), "Run the network analysis to create the forest plot."))
    plot_network_binary_forest(result)
  })

  output$network_cont_rankogram <- renderPlot({
    result <- network_cont_result()
    validate(need(!is.null(result), "Run the network analysis to create the rankogram."))
    plot_network_binary_rankogram(result, input$network_cont_col_points)
  })

  output$network_cont_funnel <- renderPlot({
    result <- network_cont_result()
    validate(need(!is.null(result), "Run the network analysis to create the funnel plot."))
    plot_network_binary_funnel(result)
  })

  output$network_cont_league_table <- renderTable({
    network_binary_league_table(network_cont_result())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$network_cont_qtest <- renderPrint({
    result <- network_cont_result()
    if (is.null(result)) {
      cat("Run the analysis to inspect between-study heterogeneity.")
      return()
    }
    cat(paste(network_binary_qtest_text(result), collapse = "\n"))
  })

  output$network_cont_split_summary <- renderPrint({
    result <- network_cont_result()
    if (is.null(result)) {
      cat("Run the analysis to inspect node splitting.")
      return()
    }
    cat(paste(network_binary_split_text(result), collapse = "\n"))
  })

  output$network_cont_rank_summary <- renderPrint({
    result <- network_cont_result()
    if (is.null(result)) {
      cat("Run the analysis to inspect treatment ranking.")
      return()
    }
    cat(paste(network_binary_rank_text(result), collapse = "\n"))
  })

  observeEvent(input$preview_network_cont_graph, {
    req(network_cont_result())
    open_single_prop_plot_modal("Network graph preview", "network_cont_graph", "72vh")
  })

  observeEvent(input$preview_network_cont_forest, {
    req(network_cont_result())
    open_single_prop_plot_modal("Network forest plot preview", "network_cont_forest", "72vh")
  })

  observeEvent(input$preview_network_cont_rankogram, {
    req(network_cont_result())
    open_single_prop_plot_modal("P-score ranking preview", "network_cont_rankogram", "72vh")
  })

  observeEvent(input$preview_network_cont_funnel, {
    req(network_cont_result())
    open_single_prop_plot_modal("Comparison-adjusted funnel plot preview", "network_cont_funnel", "72vh")
  })

  observeEvent(input$summary_network_cont_main, {
    req(network_cont_result())
    open_single_prop_text_modal("Network meta-analysis summary", "network_cont_summary")
  })

  observeEvent(input$summary_network_cont_graph, {
    req(network_cont_result())
    open_single_prop_text_modal("Network meta-analysis summary", "network_cont_summary")
  })

  observeEvent(input$summary_network_cont_league, {
    req(network_cont_result())
    showModal(modalDialog(
      title = "League table",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("network_cont_league_table")
    ))
  })

  observeEvent(input$summary_network_cont_qtest, {
    req(network_cont_result())
    open_single_prop_text_modal("Between-study heterogeneity", "network_cont_qtest")
  })

  observeEvent(input$summary_network_cont_split, {
    req(network_cont_result())
    open_single_prop_text_modal("Node-splitting summary", "network_cont_split_summary")
  })

  observeEvent(input$summary_network_cont_rank, {
    req(network_cont_result())
    open_single_prop_text_modal("Treatment ranking summary", "network_cont_rank_summary")
  })

  network_cont_download_handler <- function(plot_label, plot_function, settings_prefix) {
    downloadHandler(
      filename = function() {
        outcome <- input$network_cont_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- network_cont_result()
        if (is.null(result)) stop("Run the network analysis before downloading plots.")
        settings <- get_card_export_settings(settings_prefix)
        write_plot_export(file, result, plot_label, settings$file_format, settings$width, settings$height, plot_function)
      }
    )
  }

  output$download_network_cont_graph <- network_cont_download_handler(
    "Network_Graph",
    function(result) plot_network_binary_graph(result, input$network_cont_col_points),
    "network_cont_graph"
  )
  output$download_network_cont_forest <- network_cont_download_handler("Forestplot", plot_network_binary_forest, "network_cont_forest")
  output$download_network_cont_split <- network_cont_download_handler("SplitEvidence", function(result) plot_network_binary_split(result, input$network_cont_col_points), "network_cont_split")
  output$download_network_cont_rankogram <- network_cont_download_handler("P_score_ranking", function(result) plot_network_binary_rankogram(result, input$network_cont_col_points), "network_cont_rankogram")
  output$download_network_cont_funnel <- network_cont_download_handler("Comparison_Adjusted_Funnel", plot_network_binary_funnel, "network_cont_funnel")

  output$download_network_cont_pairwise <- downloadHandler(
    filename = function() {
      outcome <- input$network_cont_outcome
      if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
      paste0("Network_Docs_", gsub("[^A-Za-z0-9]+", "_", outcome), ".csv")
    },
    content = function(file) {
      result <- network_cont_result()
      if (is.null(result)) stop("Run the network analysis before downloading the pairwise object.")
      utils::write.csv(as.data.frame(result$pairwise), file, row.names = FALSE)
    }
  )

  output$download_network_cont_league <- downloadHandler(
    filename = function() {
      outcome <- input$network_cont_outcome
      if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
      paste0("nma_results_", gsub("[^A-Za-z0-9]+", "_", outcome), ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      result <- network_cont_result()
      if (is.null(result)) stop("Run the network analysis before downloading the league table.")
      write_xlsx_workbook(file, list(League_table = network_binary_league_table(result)))
    }
  )

  output$download_network_cont_qtest <- downloadHandler(
    filename = function() {
      outcome <- input$network_cont_outcome
      if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
      paste0("network_QTest_", gsub("[^A-Za-z0-9]+", "_", outcome), ".txt")
    },
    content = function(file) {
      result <- network_cont_result()
      if (is.null(result)) stop("Run the network analysis before downloading heterogeneity results.")
      writeLines(network_binary_qtest_text(result), file)
    }
  )

  output$network_precalc_ci_summary <- renderPrint({
    result <- network_precalc_ci_result()
    if (is.null(result)) {
      cat("Run the analysis to see the network meta-analysis summary here.")
      return()
    }
    cat("Outcome:", result$outcome_name, "\n\n")
    if (isTRUE(result$ratio_scale)) {
      cat("Note: HR/RR/OR were transformed to log scale internally for the model and back-transformed in tables/plots when supported.\n\n")
    }
    print(summary(result$nma))
  })

  output$network_precalc_ci_graph <- renderPlot({
    result <- network_precalc_ci_result()
    validate(need(!is.null(result), "Run the network analysis to create the network graph."))
    plot_network_binary_graph(result, input$network_precalc_ci_col_points)
  })

  output$network_precalc_ci_forest <- renderPlot({
    result <- network_precalc_ci_result()
    validate(need(!is.null(result), "Run the network analysis to create the forest plot."))
    plot_network_binary_forest(result)
  })

  output$network_precalc_ci_rankogram <- renderPlot({
    result <- network_precalc_ci_result()
    validate(need(!is.null(result), "Run the network analysis to create the rankogram."))
    plot_network_binary_rankogram(result, input$network_precalc_ci_col_points)
  })

  output$network_precalc_ci_funnel <- renderPlot({
    result <- network_precalc_ci_result()
    validate(need(!is.null(result), "Run the network analysis to create the funnel plot."))
    plot_network_binary_funnel(result)
  })

  output$network_precalc_ci_league_table <- renderTable({
    network_binary_league_table(network_precalc_ci_result())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$network_precalc_ci_qtest <- renderPrint({
    result <- network_precalc_ci_result()
    if (is.null(result)) {
      cat("Run the analysis to inspect between-study heterogeneity.")
      return()
    }
    cat(paste(network_binary_qtest_text(result), collapse = "\n"))
  })

  output$network_precalc_ci_split_summary <- renderPrint({
    result <- network_precalc_ci_result()
    if (is.null(result)) {
      cat("Run the analysis to inspect node splitting.")
      return()
    }
    cat(paste(network_binary_split_text(result), collapse = "\n"))
  })

  output$network_precalc_ci_rank_summary <- renderPrint({
    result <- network_precalc_ci_result()
    if (is.null(result)) {
      cat("Run the analysis to inspect treatment ranking.")
      return()
    }
    cat(paste(network_binary_rank_text(result), collapse = "\n"))
  })

  observeEvent(input$preview_network_precalc_ci_graph, {
    req(network_precalc_ci_result())
    open_single_prop_plot_modal("Network graph preview", "network_precalc_ci_graph", "72vh")
  })

  observeEvent(input$preview_network_precalc_ci_forest, {
    req(network_precalc_ci_result())
    open_single_prop_plot_modal("Network forest plot preview", "network_precalc_ci_forest", "72vh")
  })

  observeEvent(input$preview_network_precalc_ci_rankogram, {
    req(network_precalc_ci_result())
    open_single_prop_plot_modal("P-score ranking preview", "network_precalc_ci_rankogram", "72vh")
  })

  observeEvent(input$preview_network_precalc_ci_funnel, {
    req(network_precalc_ci_result())
    open_single_prop_plot_modal("Comparison-adjusted funnel plot preview", "network_precalc_ci_funnel", "72vh")
  })

  observeEvent(input$summary_network_precalc_ci_main, {
    req(network_precalc_ci_result())
    open_single_prop_text_modal("Network meta-analysis summary", "network_precalc_ci_summary")
  })

  observeEvent(input$summary_network_precalc_ci_graph, {
    req(network_precalc_ci_result())
    open_single_prop_text_modal("Network meta-analysis summary", "network_precalc_ci_summary")
  })

  observeEvent(input$summary_network_precalc_ci_league, {
    req(network_precalc_ci_result())
    showModal(modalDialog(
      title = "League table",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("network_precalc_ci_league_table")
    ))
  })

  observeEvent(input$summary_network_precalc_ci_qtest, {
    req(network_precalc_ci_result())
    open_single_prop_text_modal("Between-study heterogeneity", "network_precalc_ci_qtest")
  })

  observeEvent(input$summary_network_precalc_ci_split, {
    req(network_precalc_ci_result())
    open_single_prop_text_modal("Node-splitting summary", "network_precalc_ci_split_summary")
  })

  observeEvent(input$summary_network_precalc_ci_rank, {
    req(network_precalc_ci_result())
    open_single_prop_text_modal("Treatment ranking summary", "network_precalc_ci_rank_summary")
  })

  network_precalc_ci_download_handler <- function(plot_label, plot_function, settings_prefix) {
    downloadHandler(
      filename = function() {
        outcome <- input$network_precalc_ci_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- network_precalc_ci_result()
        if (is.null(result)) stop("Run the network analysis before downloading plots.")
        settings <- get_card_export_settings(settings_prefix)
        write_plot_export(file, result, plot_label, settings$file_format, settings$width, settings$height, plot_function)
      }
    )
  }

  output$download_network_precalc_ci_graph <- network_precalc_ci_download_handler(
    "Network_Graph",
    function(result) plot_network_binary_graph(result, input$network_precalc_ci_col_points),
    "network_precalc_ci_graph"
  )
  output$download_network_precalc_ci_forest <- network_precalc_ci_download_handler("Forestplot", plot_network_binary_forest, "network_precalc_ci_forest")
  output$download_network_precalc_ci_split <- network_precalc_ci_download_handler("SplitEvidence", function(result) plot_network_binary_split(result, input$network_precalc_ci_col_points), "network_precalc_ci_split")
  output$download_network_precalc_ci_rankogram <- network_precalc_ci_download_handler("P_score_ranking", function(result) plot_network_binary_rankogram(result, input$network_precalc_ci_col_points), "network_precalc_ci_rankogram")
  output$download_network_precalc_ci_funnel <- network_precalc_ci_download_handler("Comparison_Adjusted_Funnel", plot_network_binary_funnel, "network_precalc_ci_funnel")

  output$download_network_precalc_ci_pairwise <- downloadHandler(
    filename = function() {
      outcome <- input$network_precalc_ci_outcome
      if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
      paste0("Network_Docs_", gsub("[^A-Za-z0-9]+", "_", outcome), ".csv")
    },
    content = function(file) {
      result <- network_precalc_ci_result()
      if (is.null(result)) stop("Run the network analysis before downloading the network input object.")
      utils::write.csv(as.data.frame(result$pairwise), file, row.names = FALSE)
    }
  )

  output$download_network_precalc_ci_league <- downloadHandler(
    filename = function() {
      outcome <- input$network_precalc_ci_outcome
      if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
      paste0("nma_results_", gsub("[^A-Za-z0-9]+", "_", outcome), ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      result <- network_precalc_ci_result()
      if (is.null(result)) stop("Run the network analysis before downloading the league table.")
      write_xlsx_workbook(file, list(League_table = network_binary_league_table(result)))
    }
  )

  output$download_network_precalc_ci_qtest <- downloadHandler(
    filename = function() {
      outcome <- input$network_precalc_ci_outcome
      if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
      paste0("network_QTest_", gsub("[^A-Za-z0-9]+", "_", outcome), ".txt")
    },
    content = function(file) {
      result <- network_precalc_ci_result()
      if (is.null(result)) stop("Run the network analysis before downloading heterogeneity results.")
      writeLines(network_binary_qtest_text(result), file)
    }
  )

  output$cont_mean_summary <- renderPrint({
    result <- cont_mean_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to see the model summary here.")
      return()
    }
    if (is_batch_result(result)) {
      cat("Batch mode summary\n\n")
      print(batch_result_overview(result))
      return()
    }
    cat("Outcome:", input$cont_mean_outcome, "\n\n")
    print(summary(result$meta))
  })

  output$cont_mean_forest <- renderPlot({
    result <- cont_mean_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the forest plot."))
    plot_cont_mean_forest(result, input$cont_mean_col_square, input$cont_mean_col_square_lines, identical(input$cont_mean_forest_sort, "yes"))
  })

  output$cont_mean_loo <- renderPlot({
    result <- cont_mean_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the leave-one-out plot."))
    plot_cont_mean_loo(result, input$cont_mean_col_square)
  })

  output$cont_mean_funnel <- renderPlot({
    result <- cont_mean_result_dyn()
    validate(need(!is.null(result), "Run the analysis to create the funnel plot."))
    plot_cont_mean_funnel(result, input$cont_mean_col_square)
  })

  output$cont_mean_bias <- renderPrint({
    result <- cont_mean_result_dyn()
    if (is.null(result)) {
      cat("Run the analysis to evaluate small-study effects.")
      return()
    }
    if (nrow(result$data) < 10) {
      cat("Egger test is usually recommended only when there are at least 10 studies.")
      return()
    }
    print(meta::metabias(result$meta, method.bias = "Egger", plotit = FALSE))
  })

  output$cont_mean_subgroup_plot <- renderPlot({
    result <- cont_mean_result_dyn()
    validate(need(!is.null(result) && !is.null(result$subgroup), if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot."))
    plot_cont_mean_subgroup(result, input$cont_mean_col_square, input$cont_mean_col_square_lines, identical(input$cont_mean_forest_sort, "yes"))
  })

  output$cont_mean_subgroup_note <- renderPrint({
    result <- cont_mean_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a subgroup column in this card.")
      return()
    }
    print(summary(result$subgroup))
  })

  output$cont_mean_metareg_plot <- renderPlot({
    result <- cont_mean_result_dyn()
    validate(need(!is.null(result) && !is.null(result$metareg), if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot."))
    plot_cont_mean_metareg(result, input$cont_mean_col_square)
  })

  output$cont_mean_metareg_summary <- renderPrint({
    result <- cont_mean_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      cat(if (is.null(result)) "Run the analysis first." else "Optional: pick a moderator column in this card.")
      return()
    }
    print(summary(result$metareg))
  })

  output$cont_mean_metareg_table <- renderTable({
    extract_cont_mean_metareg_table(cont_mean_result_dyn())
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  observeEvent(input$preview_cont_mean_forest, {
    req(cont_mean_result_dyn())
    open_single_prop_plot_modal("Forest plot preview", "cont_mean_forest", "72vh")
  })

  observeEvent(input$preview_cont_mean_loo, {
    req(cont_mean_result_dyn())
    open_single_prop_plot_modal("Leave-one-out preview", "cont_mean_loo", "72vh")
  })

  observeEvent(input$preview_cont_mean_funnel, {
    req(cont_mean_result_dyn())
    open_single_prop_plot_modal("Funnel plot preview", "cont_mean_funnel", "72vh")
  })

  observeEvent(input$preview_cont_mean_subgroup, {
    result <- cont_mean_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_plot_modal("Subgroup analysis preview", "cont_mean_subgroup_plot", "72vh")
  })

  observeEvent(input$preview_cont_mean_metareg, {
    result <- cont_mean_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression preview",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      plotOutput("cont_mean_metareg_plot", height = "60vh"),
      tags$hr(),
      tableOutput("cont_mean_metareg_table")
    ))
  })

  observeEvent(input$summary_cont_mean_main, {
    req(cont_mean_result_dyn())
    open_single_prop_text_modal("Main meta-analysis summary", "cont_mean_summary")
  })

  observeEvent(input$summary_cont_mean_bias, {
    req(cont_mean_result_dyn())
    open_single_prop_text_modal("Small-study effects summary", "cont_mean_bias")
  })

  observeEvent(input$summary_cont_mean_subgroup, {
    result <- cont_mean_result_dyn()
    if (is.null(result) || is.null(result$subgroup)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a subgroup column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    open_single_prop_text_modal("Subgroup analysis summary", "cont_mean_subgroup_note")
  })

  observeEvent(input$summary_cont_mean_metareg, {
    result <- cont_mean_result_dyn()
    if (is.null(result) || is.null(result$metareg)) {
      showNotification(if (is.null(result)) "Run the analysis first." else "Pick a moderator column in this card to create this plot.", type = "message", duration = 6)
      return()
    }
    showModal(modalDialog(
      title = "Meta-regression summary",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tableOutput("cont_mean_metareg_table"),
      tags$hr(),
      verbatimTextOutput("cont_mean_metareg_summary")
    ))
  })

  cont_mean_download_handler <- function(plot_label, plot_function, require_component = NULL, settings_prefix) {
    downloadHandler(
      filename = function() {
        outcome <- input$cont_mean_outcome
        settings <- get_card_export_settings(settings_prefix)
        file_format <- settings$file_format
        if (is.null(outcome) || !nzchar(trimws(outcome))) outcome <- "Outcome"
        if (is.null(file_format) || !nzchar(file_format)) file_format <- "png"
        result <- cont_mean_result_dyn()
        if (is_batch_result(result)) {
          return(paste0(plot_label, "_Batch.zip"))
        }
        paste0(plot_label, "_", gsub("[^A-Za-z0-9]+", "_", outcome), ".", file_format)
      },
      content = function(file) {
        result <- cont_mean_result_dyn()
        if (is.null(result)) stop("Run the analysis before downloading plots.")
        if (!is_batch_result(result) && !is.null(require_component) && is.null(result[[require_component]])) {
          stop(paste("This plot is unavailable because", require_component, "was not selected."))
        }
        settings <- get_card_export_settings(settings_prefix)
        width <- settings$width
        height <- settings$height
        file_format <- settings$file_format
        if (is_batch_result(result)) {
          copy_prepared_batch_export(file, "cont_mean", result, plot_label, file_format, width, height, plot_function, require_component)
          return(invisible(NULL))
        }
        write_plot_export(file, result, plot_label, file_format, width, height, plot_function, require_component)
      }
    )
  }

  output$download_cont_mean_forest <- cont_mean_download_handler(
    "Forestplot",
    function(result) plot_cont_mean_forest(result, input$cont_mean_col_square, input$cont_mean_col_square_lines, identical(input$cont_mean_forest_sort, "yes")),
    settings_prefix = "cont_mean_forest"
  )
  output$download_cont_mean_loo <- cont_mean_download_handler("Leave_one_out", function(result) plot_cont_mean_loo(result, input$cont_mean_col_square), settings_prefix = "cont_mean_loo")
  output$download_cont_mean_funnel <- cont_mean_download_handler("FunnelPlot", function(result) plot_cont_mean_funnel(result, input$cont_mean_col_square), settings_prefix = "cont_mean_funnel")
  output$download_cont_mean_subgroup <- cont_mean_download_handler(
    "Subgroup",
    function(result) plot_cont_mean_subgroup(result, input$cont_mean_col_square, input$cont_mean_col_square_lines, identical(input$cont_mean_forest_sort, "yes")),
    require_component = "subgroup",
    settings_prefix = "cont_mean_subgroup"
  )
  output$download_cont_mean_metareg <- cont_mean_download_handler(
    "Metarregression",
    function(result) plot_cont_mean_metareg(result, input$cont_mean_col_square),
    require_component = "metareg",
    settings_prefix = "cont_mean_metareg"
  )
}

shinyApp(ui, server)

