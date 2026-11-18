# cran-comments

## Submission

This is a new submission.

## Test environments

* local Windows 11, R 4.4.2 (no LaTeX installed, so the PDF manual was not
  built locally; both win-builder runs built it without error)
* win-builder, R-devel (R Under development, 2026-08-31 r90457 ucrt)
* win-builder, R-release (R 4.6.1)

## R CMD check results

0 errors | 0 warnings | 1 note, identical on R-devel and R-release

The note is `New submission`, plus two items inside it that we believe are
false positives:

**Possibly misspelled words in DESCRIPTION.** `Reitsma` and `Rucker` are the
surnames of the authors of the two methods the package relies on, cited with
their DOIs; `et` and `al` come from "et al."; `pre` is the first half of the
hyphenated word "pre-calculated".

**Possibly invalid URL: https://metavidence.com.** The site is live and serves
the application. win-builder reports `SSL connect error: Recv failure:
Connection was reset`, which we have not been able to reproduce from any other
network. From here the address answers 200 over HTTPS, including through the
same libcurl path the check itself uses; the certificate is a current Let's
Encrypt one covering both `metavidence.com` and `www.metavidence.com`, served
with the full chain, over TLS 1.2 and 1.3. The domain resolves straight to the
GitHub Pages addresses, with no proxy in front of it, and the two GitHub URLs
listed in the same DESCRIPTION are reached without trouble by the check machine.

The same page is also reachable at <https://valbertocf.github.io/MetaVidence/>.

## What the package does

`metavidence` ships a Shiny application for meta-analysis. All statistical work
is delegated to `meta`, `metafor`, `netmeta` and `mada`; the package provides
the guided interface, the input validation and the publication-ready exports.

`run_app()` is the only exported function. Its example is wrapped in
`if (interactive())` because the application blocks the console while running.

The application writes only to `tempdir()`, when the user asks for a plot or a
spreadsheet to be exported. It opens no network connections and reads no files
other than the ones the user selects.
