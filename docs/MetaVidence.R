# MetaVidence
#
# Click "Source" (top right of this panel) to open MetaVidence.
# The first time it installs the app, which takes a few minutes.
# After that it opens in a few seconds, and updates itself when a new
# version comes out.
#
# Clique em "Source" (canto superior direito deste painel) para abrir o
# MetaVidence. Na primeira vez ele instala o app, o que leva alguns minutos.
# Depois disso abre em poucos segundos, e se atualiza sozinho quando sai
# uma versao nova.

tryCatch(
  source("https://metavidence.com/start.R"),
  error = function(e) {
    # No internet: open the version already installed, if there is one.
    # Sem internet: abre a versao ja instalada, se houver.
    if (requireNamespace("metavidence", quietly = TRUE)) metavidence::run_app() else stop(e)
  }
)
