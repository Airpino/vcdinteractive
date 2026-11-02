# vcdinteractive

Interactive displays for categorical data, built on the ideas of
[vcd](https://CRAN.R-project.org/package=vcd) and
[vcdExtra](https://CRAN.R-project.org/package=vcdExtra) and rendered as
htmlwidgets (they work in the RStudio viewer, R Markdown, Quarto, Shiny and in
stand-alone HTML pages). Prototype, version 0.1.0.

## Installation

The package is not on CRAN. It requires R (>= 4.1.0) and the packages
`htmlwidgets` and `jsonlite`, which are installed automatically.

### From GitHub

```r
# install.packages("remotes")
remotes::install_github("Airpino/vcdinteractive")

# with the vignettes (they need knitr, rmarkdown, vcd and vcdExtra)
remotes::install_github("Airpino/vcdinteractive", build_vignettes = TRUE,
                        dependencies = TRUE)
```

or, with `pak`:

```r
# install.packages("pak")
pak::pak("Airpino/vcdinteractive")
```

### From the source archive

Download `vcdinteractive_0.1.0.tar.gz` (from the *Releases* page of the
repository, or build it with `R CMD build vcdinteractive`) and run

```r
install.packages("vcdinteractive_0.1.0.tar.gz", repos = NULL, type = "source")
```

On Windows no compiler (Rtools) is needed: the package contains only R and
JavaScript code.

### Suggested packages

```r
install.packages(c("robustbase",          # robust residuals
                   "vcd", "vcdExtra",     # data sets used in the examples and vignettes
                   "ca", "gnm",           # ca/mjca objects and gnm models as input
                   "knitr", "rmarkdown")) # to build the vignettes
```

Without `robustbase` the robust residuals of the mutual independence model
are computed by median polish, and are not available for other models.

### Check the installation

```r
library(vcdinteractive)
imosaic(HairEyeColor)
browseVignettes("vcdinteractive")   # if installed with the vignettes
```

## Features

1. **Dynamic variable order**: drag the variables in the toolbar (or use the
   ◀ ▶ buttons) and toggle the split direction of each position (rows /
   columns). The mosaic is recomputed and animated in the browser.
   Each variable has a check box to **add it to or remove it from** the plot:
   the mosaic can be built up one variable at a time (the table is collapsed
   over the removed variables and the model is projected on the remaining
   ones; all sub-tables are precomputed, argument `subsets`).
2. **Tooltips**: each tile shows its combination of categories, the
   observed and expected counts, O − E, O/E, all the residual types with their
   p-values, and the multiplicity-adjusted p-value. Hovering a category label
   highlights all its tiles.
3. **Several residual types** for shading (`residual_types`):
   Pearson, adjusted standardized (Haberman), deviance, Freeman–Tukey,
   Anscombe, and *robust* residuals measured against a robust fit of the
   model (`robustbase::glmrob`, or a median polish of the log-counts).
   One click (or keys 1–6) recolours the tiles; the formula of the selected
   residual and when to use it are shown in the panel. See
   `vignette("residuals", package = "vcdinteractive")`.
4. **Significant cells**: a side panel lists the cells that depart
   significantly from the model (Holm, Bonferroni, BH or no correction;
   chosen α), sorted by |residual|, and exports them to CSV. The same list is
   available in R with `significant_cells()`.

5. **Four views** of the same table in `imosaic()`: mosaic, association plot
   (`vcd::assoc`), sieve diagram (`vcd::sieve`) and doubledecker plot
   (`vcd::doubledecker`), with animated transitions; a **models panel** to
   compare log-linear models (`models = c("mutual", "joint", ...)` or
   formulas); **fitted `glm`/`gnm` models** as input.

## Other interactive displays

| Function | Counterpart in vcd / vcdExtra | Interactions |
|---|---|---|
| `ipairs()` | `pairs.table()` | linked highlighting across panels, zoom on a panel, significant cells |
| `ifourfold()` | `fourfold()`, `plot(loddsratio)`, `woolf_test()` | standardization, confidence rings and level, linked forest plot of log odds ratios |
| `irootogram()` | `rootogram(goodfit())` | distribution, hanging/standing/deviation, scale |
| `ispine()`, `icdplot()` | `spine()`, `cd_plot()` | slider for bins or bandwidth, quantile bins |
| `imcaplot()` | `mcaplot()`, `plot(ca)` | dimensions, show/hide variables, zoom and pan, labels on hover |
| `iassoc_graph()` | `assoc_graph()` | model comparison, edge strength, draggable nodes, marginal associations |

## Vignettes

* `vignette("residuals")`: the residuals, their formulas and when to use them
* `vignette("ucb-admissions")`: Simpson's paradox in the UC Berkeley data
* `vignette("strucplots")`: association, sieve and doubledecker plots; comparing models
* `vignette("pairs-fourfold")`: pairwise mosaic matrices and fourfold displays
* `vignette("discrete-distributions")`: rootograms
* `vignette("categorical-response")`: spinograms and conditional densities
* `vignette("correspondence-graphs")`: correspondence analysis and association graphs

## Usage

```r
library(vcdinteractive)

imosaic(HairEyeColor)
imosaic(Titanic, residuals = "robust")
imosaic(~ Class + Survived, data = Titanic)
imosaic(Titanic, expected = ~ Class * Sex * Age + Survived)   # [CSA][S]

cells <- mosaic_cells(Titanic)          # all cell statistics
significant_cells(cells, type = "standardized", adjust = "BH")
```

## How it works

Expected counts and residuals depend on the cells, not on the order in which
the variables are split, so they are computed once in R
(`mosaic_cells()`), using a Poisson log-linear model fitted with `glm()`.
The recursive layout of the mosaic (with vcd's `spacing_increase` gaps and
`shading_hcl` colours) is recomputed in JavaScript whenever the order or the
directions change.

P-values compare the leverage-adjusted residuals, r / sqrt(1 − h), with the
standard normal distribution. For robust residuals no leverage adjustment is
applied, and the p-values are only indicative: they are meaningful when the
model fits most of the cells.
