# Two-Factors Design: Split-Plot in RCBD

Planning an experiment follows a reproducible routine:

1.  **Load required libraries:** Load `inti`, `knitr`, and `dplyr`
    packages.
2.  **Define factor levels:** Set up lists with genotypes, treatments,
    and management factors.
3.  **Dispatch design generator:** Choose between CRD, RCBD, Split-plot,
    or Augmented designs.
4.  **Plot the field sketch:** Verify spatial layouts and
    serpentine/zigzag sequences.
5.  **Label design:** Design the experimental labels to facilitate the
    data collection.
6.  **Export to Field Book app:** Generate field-ready sheets with trait
    parameters.

\
`# Install packages and dependencies`\
\
[`library`](https://rdrr.io/r/base/library.html)`(`[`inti`](https://inkaverse.com/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`huito`](https://huito.inkaverse.com/)`)`

## Designs with Two Factors

When evaluating two or more factors, four designs become available:
**CRD**, **RCBD**, **Split-plot RCBD**, and **Augmented**.

### Split-Plot Design in RCBD

The Split-plot Design is recommended when one factor requires larger
experimental units due to management constraints (such as irrigation)
assigned to main plots, while a second factor (such as commercial quinoa
varieties) is assigned to sub-plots within each main plot.

\
`# 1. Define factors: Irrigation regimes (main plots) and commercial quinoa varieties (sub-plots)`\
`factors_split`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  Irrigation ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Full"``, ``"Deficit"``)``,`\
`  Variety    ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Var_1"``, ``"Var_2"``, ``"Var_3"``)`\
`)`\
\
`# 2. Generate Split-plot layout: 2 main levels x 3 sub levels x 4 blocks = 24 plots`\
`split_exp`` ``<-`` `[`design_split`](https://inkaverse.com/reference/design_split.md)`(`\
`  factors ``=`` ``factors_split``,`\
`  type ``=`` ``"split_rcbd"``,`\
`  rep ``=`` ``4``,`\
`  zigzag ``=`` ``TRUE``,`\
`  seed ``=`` ``2026`\
`)`\
\
`# Fieldbook preview`\
`split_exp``$``fieldbook`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``10``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``caption ``=`` ``"Split-plot Fieldbook preview"``)`

| qrcode | plots | ntreat | Irrigation | Variety | wp_sp | block | sort | rows | cols | design |
|:---|---:|---:|:---|:---|:---|---:|---:|---:|---:|:---|
| inkaverse_1001_Full_Var_1 | 1001 | 1 | Full | Var_1 | Full_Var_1 | 1 | 1 | 1 | 1 | split-rcbd |
| inkaverse_1002_Full_Var_2 | 1002 | 3 | Full | Var_2 | Full_Var_2 | 1 | 2 | 2 | 1 | split-rcbd |
| inkaverse_1003_Full_Var_3 | 1003 | 5 | Full | Var_3 | Full_Var_3 | 1 | 3 | 3 | 1 | split-rcbd |
| inkaverse_1004_Deficit_Var_2 | 1004 | 4 | Deficit | Var_2 | Deficit_Var_2 | 1 | 4 | 3 | 2 | split-rcbd |
| inkaverse_1005_Deficit_Var_1 | 1005 | 2 | Deficit | Var_1 | Deficit_Var_1 | 1 | 5 | 2 | 2 | split-rcbd |
| inkaverse_1006_Deficit_Var_3 | 1006 | 6 | Deficit | Var_3 | Deficit_Var_3 | 1 | 6 | 1 | 2 | split-rcbd |
| inkaverse_2001_Deficit_Var_1 | 2001 | 2 | Deficit | Var_1 | Deficit_Var_1 | 2 | 1 | 4 | 1 | split-rcbd |
| inkaverse_2002_Deficit_Var_3 | 2002 | 6 | Deficit | Var_3 | Deficit_Var_3 | 2 | 2 | 5 | 1 | split-rcbd |
| inkaverse_2003_Deficit_Var_2 | 2003 | 4 | Deficit | Var_2 | Deficit_Var_2 | 2 | 3 | 6 | 1 | split-rcbd |
| inkaverse_2004_Full_Var_1 | 2004 | 1 | Full | Var_1 | Full_Var_1 | 2 | 4 | 6 | 2 | split-rcbd |

Split-plot Fieldbook preview {.table .caption-top}

\
\
`# Field layout visualization`\
[`tarpuy_plotdesign`](https://inkaverse.com/reference/tarpuy_plotdesign.md)`(`\
`  data ``=`` ``split_exp``,`\
`  factor ``=`` ``"Irrigation"``,`\
`  fill ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"plots"``, ``"Variety"``)`\
`)`

![](DoE-2_SPLIT_files/figure-html/unnamed-chunk-2-1.png)
