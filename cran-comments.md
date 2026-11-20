# cran-comments

## Submission

This is a new submission.

## Test environments

* local Windows 11, R 4.4.2, `R CMD check --as-cran`
* win-builder, R-devel (2026-11-17 r90926 ucrt)
* win-builder, R-release (R 4.6.2)

## R CMD check results

0 errors | 0 warnings | 1 note, identical on R-devel and R-release

The note is `New submission`, together with a list of possibly misspelled
words in DESCRIPTION, which we believe are false positives:

* `Chu`, `Reitsma` and `Rucker` are surnames of the authors of the methods the
  package relies on, and `Costa` and `Filho` are the surname of the author of
  the paper that describes the package. All are cited with their DOIs.
* `Cochrane` refers to the Cochrane Handbook for Systematic Reviews of
  Diagnostic Test Accuracy.
* `et` and `al` come from "et al.", and `pre` is the first half of the
  hyphenated word "pre-calculated".

## What the package does

`metavidence` ships a 'shiny' application for meta-analysis. All statistical
work is delegated to `meta`, `metafor`, `netmeta` and `lme4` (with `lmtest` for
the likelihood-ratio tests); the package provides the guided interface, the
input validation and the publication-ready exports.

`run_app()` is the only exported function. Its example is wrapped in
`if (interactive())` because the application blocks the console while running.

The application writes only to `tempdir()`, when the user asks for a plot or a
spreadsheet to be exported. It opens no network connections and reads no files
other than the ones the user selects.
