# MetaVidence

Meta-analysis without code. A Shiny application that walks the researcher
through three steps (data, parameters, results) up to publication-ready plots
and tables.

## How to use

Take a look in our [tutorial](https://metavidence.com/tutorials/en/get-started.html)

### Online, nothing to install

**https://metavidence.com**

It runs entirely in your browser, compiled to WebAssembly. No account, no
upload: the spreadsheet is read in the tab itself and no data is sent to any
server. The first visit downloads R and the statistical packages, which takes a
few minutes; after that it starts much faster.

### Locally in R, faster

Worth it if you use it often. It skips the ~130 MB WebAssembly download and runs
on native R, without the 2 to 3 times penalty of WASM.

MetaVidence has been submitted to CRAN. Until it is accepted, install it from
this repository:

```r
install.packages("remotes")
remotes::install_github("ValbertOCF/MetaVidence", subdir = "pkg")
metavidence::run_app()
```

Once it is on CRAN this becomes `install.packages("metavidence")`.

Or from a clone of this repository:

```r
install.packages(c("shiny", "meta", "metafor", "netmeta", "mada",
                   "lme4", "lmtest", "ggplot2", "readxl", "openxlsx", "zip"))
shiny::runApp()
```

Step-by-step instructions, including how to install R and RStudio from scratch,
are in the [Get started](https://metavidence.com/tutorials/en/get-started.html)
tutorial.

### The three steps

1. Pick the kind of data you have.
2. Load the spreadsheet (Excel or CSV), paste straight from Google Sheets, or
   start from the example dataset. Each screen lists the required columns and
   has a button to download the spreadsheet template.
3. Set the parameters. Every field has a "?" explaining what it does.
4. Preview, then download the plots and the statistical summaries.

For several outcomes in one run, upload an Excel file with **one sheet per
outcome**; the sheet name becomes the outcome name in the plots and files.

Full documentation, in English and Portuguese, is at
[metavidence.com/tutorials](https://metavidence.com/tutorials/).

## Available analyses

| Module | What it does |
|---|---|
| **Binary outcomes** | RR, OR and RD from event counts and sample sizes |
| **Continuous outcomes** | MD and SMD, from mean + SD, from median + IQR, or from both mixed in one file |
| **Pre-calculated effects** | TE with 95% CI, TE with standard error, or the full set: HR, RR, OR, RD, MD or SMD |
| **Single-arm meta-analysis** | Single proportions and single means |
| **Network meta-analysis** | Frequentist NMA: binary, continuous or pre-calculated TE |
| **Diagnostic test accuracy** | Sensitivity, specificity, DOR, SROC, subgroups and comparison of two tests |

Every module offers fixed-effect and random-effects models, a forest plot and
export to PNG (600 DPI) and vector PDF. What varies:

## How to cite

If MetaVidence produced results you are publishing, cite it like this:

> Costa Filho VO (2026). MetaVidence: no-code meta-analysis in the browser and in R.
> Version 0.1.0. https://metavidence.com

```bibtex
@Manual{metavidence,
  title   = {MetaVidence: no-code meta-analysis in the browser and in R},
  author  = {Valbert Oliveira Costa Filho},
  year    = {2026},
  note    = {R package version 0.1.0},
  url     = {https://metavidence.com}
}
```

## Project structure

| File | Role |
|---|---|
| `app.R` | The whole application: interface, server and statistics |
| `easymeta.css` | Design system (colours, typography, components) |
| `easymeta.js` | Usability layer: stepper, import tabs, parameter sections, tooltips and the download bridge |
| `docs/` | WebAssembly build published by GitHub Pages |
| `tutorials/` | Quarto sources for the tutorial pages, in `pt/` and `en/` |
| `build_shinylive.R` | Rebuilds `docs/`, restores the CNAME, injects the loading screen and renders the tutorials |
| `build_package.R` | Assembles the R package in `pkg/` from the root `app.R` |
| `pkg/` | Package sources (DESCRIPTION, R/, man/); `inst/` is generated |
| `loading_screen.html` | Screen shown while R loads in the browser |
| `DEPLOY.md` | How to rebuild and publish |

`docs/` is build output, not source. After editing `app.R`, `easymeta.css` or
`easymeta.js`, run `build_shinylive.R` before committing, or the site goes up
with the previous version.

## Credits

The analyses use the packages
[meta](https://cran.r-project.org/package=meta),
[metafor](https://cran.r-project.org/package=metafor),
[netmeta](https://cran.r-project.org/package=netmeta) and
[mada](https://cran.r-project.org/package=mada). When publishing results, cite
those packages and record the version you used.

## Licence

GPL-3.
