# MetaVidence, deploy estático (shinylive / WebAssembly)

A pasta `docs/` contém o MetaVidence compilado para WebAssembly. Ela roda o R
inteiro **dentro do navegador do usuário**. Não existe servidor R, não há
custo de hospedagem e nenhum dado do usuário sai da máquina dele.

## Como reconstruir depois de mexer no app

```bash
Rscript build_shinylive.R
```

Um comando só. O script já faz tudo:

1. copia `app.R`, `easymeta.css` e `easymeta.js` para `shinylive_app/`;
2. exporta `shinylive_app/` para `docs/`;
3. **recria o `docs/CNAME`** (`metavidence.com`);
4. **injeta a tela de carregamento** e troca o `<title>` para `MetaVidence`;
5. confere os quatro itens e imprime o resultado.

A primeira execução baixa ~130 MB de pacotes WebAssembly (~2 min). As
seguintes usam o cache e levam segundos.

> **Por que os passos 3 e 4 existem.** O `shinylive::export()` **limpa a pasta
> `docs/` antes de escrever**. Tudo que não vem do export é apagado: o `CNAME`
> (e o domínio sai do ar no deploy seguinte) e qualquer edição no `index.html`.
> Já aconteceu com o CNAME. Por isso essas etapas moram no script, e não numa
> instrução manual. Se você mexer no `index.html` gerado, mexa aqui.

Ao final o script imprime:

```
post-export checks
  CNAME          : metavidence.com
  <title>        : MetaVidence
  loading screen : injected
  #root present  : yes
```

Se algum item vier diferente disso, **não publique**: o `index.html` gerado
pelo shinylive mudou de forma e a injeção precisa ser reajustada. O script
aborta sozinho se a âncora `<body>` sumir ou se o `loading_screen.html` não
existir.

## A tela de carregamento

O `shinylive` baixa o R inteiro antes de mostrar qualquer coisa: 30–60 s de
página em branco no primeiro acesso. `loading_screen.html` cobre esse tempo
com a marca, o progresso e dois botões (tutoriais e versão local).

O progresso é **medido, não simulado**:

| Etapa | Sinal real |
|---|---|
| Starting up | a página começou a carregar |
| Downloading R and the statistical packages | > 2 MB somados via `PerformanceObserver` |
| Starting R | o banner do R aparece no console do webR |
| Opening MetaVidence | a interface do app aparece dentro do `#root` |

Nenhuma barra anda por temporizador. Se o navegador travar, a barra trava
junto, que é a informação honesta. O contador mostra os bytes que realmente
passaram pela rede; numa revisita servida do cache ele diz "Loading from
cache" em vez de "0.0 MB".

Três proteções importam:

- **Falha aberta.** Se a detecção não disparar, a cortina se remove sozinha
  depois de 5 minutos. O visitante nunca fica preso atrás dela.
- **Service worker bloqueado.** O shinylive não roda sem service worker
  (janela anônima em alguns navegadores, extensões de privacidade). A tela
  detecta o erro e explica o que houve, em vez de progredir para sempre.
- **Aba em segundo plano.** O navegador estrangula `setInterval` para ~1 s
  (medi 3 s). Por isso a detecção usa `MutationObserver` como caminho rápido,
  com a sondagem só de piso.

## Como testar localmente

```bash
python -m http.server 8123 --directory docs
```

Depois abra <http://127.0.0.1:8123>.

## Como publicar

### GitHub Pages (grátis, recomendado)

1. `git init && git add . && git commit -m "MetaVidence"`
2. Crie um repositório no GitHub e faça o push.
3. Em *Settings → Pages*, escolha a branch `main` e a pasta `/docs`.
4. O app fica em `https://<usuario>.github.io/<repositorio>/`.

O arquivo `docs/.nojekyll` já está lá. Sem ele o GitHub Pages ignoraria
parte dos arquivos do shinylive.

### Netlify / Cloudflare Pages

Arraste a pasta `docs/` para o painel do serviço. Nenhuma configuração de
build é necessária: é um site estático puro.

## O que o usuário final vai sentir

- **Primeiro acesso:** ~130 MB de download e 30–60 s até a tela aparecer.
  Depois disso o navegador guarda tudo em cache e a abertura é rápida.
- **Análises:** rodam mais devagar que no R nativo (WebAssembly é ~2–3×
  mais lento). Meta-análises comuns levam poucos segundos; network
  meta-analysis é a mais demorada.
- **Privacidade:** as planilhas nunca são enviadas para lugar nenhum.

## Verificado nesta build

Testado no navegador, com o app rodando em WebAssembly:

- módulo binário: análise + forest plot renderizado;
- network meta-analysis: rodou com `netmeta` 3.4-0 (a versão do webR é mais
  nova que a instalada localmente, sem quebra de compatibilidade);
- diagnóstico: análise + curva SROC (`mada`);
- geração de template `.xlsx` (`openxlsx` + `zip`): arquivo válido;
- camada de interface: stepper, abas de importação, seções de parâmetros,
  118 tooltips e ícones do data check.

## Downloads: por que existe um "download bridge"

Ao clicar num link de download do Shiny, o navegador faz a requisição pelo
processo dele, **contornando o service worker** do shinylive. A requisição
chega ao servidor estático, que não conhece a rota `session/<token>/download/…`,
responde 404 e o navegador mostra *"arquivo não disponível"*.

`easymeta.js` intercepta o clique, busca os bytes com `fetch()` (que passa
pelo service worker) e entrega o arquivo como Blob. Sem isso, **nenhum
download funciona na versão web**: nem gráficos, nem templates, nem os ZIPs
em lote. No app servido normalmente pelo R o comportamento é idêntico.

> Se um download falhar depois de você editar os arquivos, recarregue com
> **Ctrl+Shift+R**: o shinylive guarda o pacote do app no service worker e
> um F5 comum pode continuar rodando a versão antiga.

## Diferenças de versão de pacotes

O webR usa versões próprias, que podem não ser idênticas às da sua máquina
(ex.: `netmeta` 3.4-0 no webR vs 3.2-0 local). Se você publicar resultados
gerados pela versão web, vale registrar no artigo qual versão foi usada.
a tela de análise não muda, mas números podem variar entre versões de
pacote, como em qualquer atualização.

## O pacote R

O mesmo `app.R` alimenta três artefatos. A raiz é a **única fonte de verdade**:

```
app.R + easymeta.css + easymeta.js
        │
        ├── build_shinylive.R ──> docs/   (site WebAssembly)
        ├── build_package.R   ──> pkg/    (pacote R)
        └── shiny::runApp()   ──> raiz    (desenvolvimento)
```

Nunca edite `pkg/inst/app/` nem `shinylive_app/`: os dois são regenerados e
suas mudanças seriam apagadas. Edite a raiz e rode o build.

### Como montar

```bash
Rscript build_package.R
```

Ele copia os fontes para `pkg/inst/app/`, gera `NAMESPACE` e `man/` com o
roxygen2, e confere que o pacote está sincronizado com a raiz.

### Como checar antes de publicar

```bash
R CMD build pkg
R CMD check --as-cran metavidence_0.1.0.tar.gz
```

Esperado: **2 NOTEs**, ambas ambientais:

- *CRAN incoming feasibility*: "New submission" mais os URLs do DESCRIPTION.
  Enquanto o repositório e o domínio estiverem fora do ar eles retornam 404.
  **Precisam estar no ar antes de submeter ao CRAN**, que checa URLs.
- *unable to verify current time*: a máquina não alcançou um servidor de hora.
  Não aparece nas máquinas do CRAN.

Qualquer NOTE além dessas duas é regressão de verdade.

> **Por que os `@importFrom` em `R/run_app.R`.** O `R CMD check` só lê `R/`,
> nunca `inst/`. Como o app inteiro vive em `inst/app/app.R`, os 14 pacotes que
> ele usa apareceriam como declarados e não usados. Os `@importFrom` declaram
> uma função real de cada um. Se você adicionar uma dependência nova ao app,
> **adicione também um `@importFrom`**, senão a NOTE volta.

### A resolução do CSS/JS

O `app.R` roda em três lugares, e o `shiny::runApp()` **não** muda o diretório
de trabalho para o diretório do app (conferido no fonte: não há `setwd` nele).
Por isso `metavidence_assets()` procura o `easymeta.css` primeiro no diretório
de trabalho (repositório e WebAssembly) e depois em `system.file("app", ...)`
(pacote instalado). Se essa função quebrar, o app abre **sem estilo nenhum** e
nada acusa erro. É o modo de falha a vigiar ao mexer nela.


## As páginas de tutorial

A fonte fica em `tutorials/*.qmd` (Quarto); o `build_shinylive.R` renderiza e
copia o resultado para `docs/tutorials/`, servido em
`metavidence.com/tutorials/`.

**Elas não deixam o app mais lento.** Os 133 MB do WebAssembly só são baixados
por quem abre a raiz; uma página de tutorial é HTML comum. E o service worker
do shinylive **não interfere**: ele é criado com `useCaching = false`, e mesmo
com o cache ligado só gravaria caminhos sob `/shinylive/` e o favicon. Então
`/tutorials/` passa direto para a rede, sem interceptação e sem risco de
servir versão velha depois de um deploy.

### Reconstruir

```bash
Rscript build_shinylive.R
```

Ele acha o Quarto sozinho (no PATH ou no que vem com o RStudio), renderiza e
recoloca em `docs/`. O bloco de verificação ganhou uma linha:

```
  tutorials      : 2 paginas
```

Se vier `MISSING`, **não publique**: os botões "Read the tutorials" na home e na
tela de carregamento cairiam em 404. O script aborta se o Quarto não for
encontrado **e** não houver um `tutorials/_site/` já renderizado.

### As capturas de tela

```bash
Rscript design/make_shots.R
```

Sobe o app numa porta própria, percorre o fluxo com o `chromote` e salva os
PNGs em `tutorials/img/`. Os gráficos passam por `magick::image_trim` para
tirar a moldura branca. Sem isso o forest plot fica minúsculo no meio da
página.

Rode de novo sempre que a interface mudar. Prints tirados à mão envelhecem: em
duas semanas mostram uma tela que não existe mais, e ninguém percebe.
