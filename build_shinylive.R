## Builds the static shinylive (WebAssembly) version of MetaVidence.
## Output: ./docs, ready to publish on GitHub Pages / Netlify.
##
## shinylive::export() WIPES docs/ on every run, so anything that is not part of
## the export has to be re-applied here. Two things depend on that:
##   1. docs/CNAME       : without it the metavidence.com domain goes down
##   2. the loading screen : the generated index.html has none, and the visitor
##                           would stare at a blank page for 30 to 60 seconds
##   3. docs/favicon.svg   : the tab icon, which the app cannot set from inside
##                           the iframe
##   4. docs/tutorials/    : the rendered Quarto pages
## Doing it by hand after each build is how it got lost before.

app_dir <- "shinylive_app"
out_dir <- "docs"
domain <- "metavidence.com"
loading_file <- "loading_screen.html"
favicon_file <- "favicon.svg"

## shinylive_app/ is disposable: it exists only because the export needs a folder
## with the app inside it. It is recreated here, so it stays out of the
## repository, and there is never a second copy of app.R to fall out of sync.
if (!dir.exists(app_dir)) dir.create(app_dir, recursive = TRUE)
for (f in c("app.R", "easymeta.css", "easymeta.js")) {
  file.copy(f, file.path(app_dir, f), overwrite = TRUE)
}

cat("shinylive version:", as.character(packageVersion("shinylive")), "\n")
cat("exporting", app_dir, "->", out_dir, "\n\n")

start <- Sys.time()
shinylive::export(app_dir, out_dir, quiet = FALSE)
cat("\nelapsed:", round(as.numeric(difftime(Sys.time(), start, units = "mins")), 1), "min\n")

## ---- 1. dominio personalizado -------------------------------------------
writeLines(domain, file.path(out_dir, "CNAME"))

## ---- 2. loading screen, tab title and tab icon --------------------------
index_path <- file.path(out_dir, "index.html")
index <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

index <- sub("<title>[^<]*</title>", "<title>MetaVidence</title>", index)

## The tab icon comes from the OUTER document. The app declares its own in
## app.R, but on the website shinylive mounts the app inside an iframe, and an
## iframe favicon never reaches the tab: that is why the site showed the generic
## globe while the locally running app already showed the logo. Here the same
## drawing is linked from index.html, which is the page the tab actually loads.
if (!file.exists(favicon_file)) {
  stop("Missing ", favicon_file, ". The tab icon would silently fall back to the browser globe.")
}
file.copy(favicon_file, file.path(out_dir, "favicon.svg"), overwrite = TRUE)
index <- sub("</title>",
             "</title>
    <link rel=\"icon\" type=\"image/svg+xml\" href=\"favicon.svg\">",
             index, fixed = TRUE)

if (!grepl("rel=\"icon\"", index, fixed = TRUE)) {
  stop("Favicon link was not injected. Check the </title> anchor in index.html.")
}

if (!file.exists(loading_file)) {
  stop("Missing ", loading_file, ". The loading screen would be silently dropped.")
}
overlay <- paste(readLines(loading_file, warn = FALSE), collapse = "\n")

## Injected as the first child of <body>, BEFORE #root, so it paints on the
## first frame without waiting for shinylive.js to load.
index <- sub("<body>", paste0("<body>\n", overlay), index, fixed = TRUE)

if (!grepl("mv-boot", index, fixed = TRUE)) {
  stop("Loading screen was not injected. Check the <body> anchor in index.html.")
}
writeLines(index, index_path)

## ---- 3. tutorial pages ---------------------------------------------------
## Same reason as the CNAME: the export wipes docs/ entirely, so the tutorials
## have to be put back here rather than by hand.
achar_quarto <- function() {
  no_path <- Sys.which("quarto")
  if (nzchar(no_path)) return(unname(no_path))
  candidatos <- c(
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
    "/usr/local/bin/quarto", "/usr/bin/quarto")
  for (cand in candidatos) if (file.exists(cand)) return(cand)
  ""
}

tutorial_src <- "tutorials"
tutorial_site <- file.path(tutorial_src, "_site")
quarto <- achar_quarto()

## One Quarto project per language, so each navbar comes out in its own
## language rather than being translated at render time.
idiomas <- c("pt", "en")

if (nzchar(quarto)) {
  cat("
renderizando os tutoriais
")
  for (idioma in idiomas) {
    st <- system2(quarto, c("render", shQuote(normalizePath(file.path(tutorial_src, idioma)))),
                  stdout = TRUE, stderr = TRUE)
    if (!dir.exists(file.path(tutorial_site, idioma))) {
      stop("Quarto did not produce ", file.path(tutorial_site, idioma), ":
",
           paste(utils::tail(st, 10), collapse = "
"))
    }
    cat("  ", idioma, "ok
")
  }
  ## The images are shared by both languages: they sit one level up and the
  ## pages reference them as ../img/. Without this copy they all break.
  file.copy(file.path(tutorial_src, "img"), tutorial_site, recursive = TRUE, overwrite = TRUE)
  file.copy(file.path(tutorial_src, "index.html"), tutorial_site, overwrite = TRUE)
} else if (!dir.exists(tutorial_site)) {
  ## Fail loudly: publishing without tutorials would leave the app's buttons
  ## pointing at a 404.
  stop("Quarto not found and ", tutorial_site, " does not exist. ",
       "Instale o Quarto ou renderize os tutoriais antes de reconstruir.")
} else {
  cat("
Quarto nao encontrado; usando ", tutorial_site, " ja renderizado
")
}

destino_tutoriais <- file.path(out_dir, "tutorials")
unlink(destino_tutoriais, recursive = TRUE)
dir.create(destino_tutoriais, recursive = TRUE, showWarnings = FALSE)
file.copy(list.files(tutorial_site, full.names = TRUE), destino_tutoriais,
          recursive = TRUE, overwrite = TRUE)

## ---- verificacao --------------------------------------------------------
final <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
cat("\npost-export checks\n")
cat("  CNAME          :", readLines(file.path(out_dir, "CNAME"), warn = FALSE), "\n")
cat("  <title>        :", sub('.*<title>([^<]*)</title>.*', '\\1', final), "\n")
cat("  favicon        :",
    if (file.exists(file.path(out_dir, "favicon.svg")) &&
        grepl("rel=\"icon\"", paste(readLines(index_path, warn = FALSE), collapse = ""), fixed = TRUE))
      "linked" else "MISSING", "
")
cat("  loading screen :", if (grepl("id=\"mv-boot\"", final, fixed = TRUE)) "injected" else "MISSING", "\n")
cat("  #root present  :", if (grepl('id="root"', final, fixed = TRUE)) "yes" else "NO", "\n")
n_tut <- vapply(idiomas, function(i)
  length(list.files(file.path(destino_tutoriais, i), pattern = "[.]html$")), integer(1))
cat("  tutorials      :", if (all(n_tut > 0))
      paste(paste0(idiomas, " ", n_tut), collapse = ", ") else "MISSING", "
")

files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
total_mb <- round(sum(file.info(files)$size) / 1024^2, 1)
cat("files:", length(files), "| total size:", total_mb, "MB\n")
