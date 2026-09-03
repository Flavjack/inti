# Two-Factors Design: CRD

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

When evaluating two factors, four designs become available: **CRD**,
**RCBD**, **Split-plot RCBD**, and **Augmented**.

## Factorial Completely Randomized Design (Factorial CRD)

Recommended for multi-factor experiments under homogeneous conditions,
such as temperature- and salinity-controlled germination assays in
growth chambers.

\
`# 1. Define factors: Salinity levels and incubation temperatures`\
`factors_crd_2f`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  NaCl ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"0"``, ``"50"``, ``"100"``)``,`\
`  Temp ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"20"``, ``"25"``)`\
`)`\
\
`# 2. Generate factorial CRD layout`\
`crd_exp_2f`` ``<-`` `[`design_repblock`](https://inkaverse.com/reference/design_repblock.md)`(`\
`  nfactors ``=`` ``2``,`\
`  factors ``=`` ``factors_crd_2f``,`\
`  type ``=`` ``"crd"``,`\
`  rep ``=`` ``4``,`\
`  zigzag ``=`` ``TRUE``,`\
`  seed ``=`` ``2026`\
`)`\
\
`# Fieldbook preview`\
`crd_exp_2f``$``fieldbook`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``10``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``caption ``=`` ``"Factorial CRD Fieldbook preview"``)`

| qrcode         | plots | ntreat | NaCl | Temp | sort | rep | rows | cols | design |
|:---------------|------:|-------:|:-----|:-----|-----:|----:|-----:|-----:|:-------|
| inkaverse_1001 |  1001 |      1 | 0    | 20   |    1 |   1 |    1 |    1 | crd    |
| inkaverse_1002 |  1002 |      6 | 100  | 25   |    2 |   2 |    1 |    2 | crd    |
| inkaverse_1003 |  1003 |      2 | 50   | 20   |    3 |   3 |    1 |    3 | crd    |
| inkaverse_1004 |  1004 |      6 | 100  | 25   |    4 |   1 |    1 |    4 | crd    |
| inkaverse_1005 |  1005 |      2 | 50   | 20   |    5 |   2 |    1 |    5 | crd    |
| inkaverse_1006 |  1006 |      2 | 50   | 20   |    6 |   1 |    1 |    6 | crd    |
| inkaverse_1007 |  1007 |      5 | 50   | 25   |    7 |   4 |    2 |    6 | crd    |
| inkaverse_1008 |  1008 |      1 | 0    | 20   |    8 |   3 |    2 |    5 | crd    |
| inkaverse_1009 |  1009 |      6 | 100  | 25   |    9 |   3 |    2 |    4 | crd    |
| inkaverse_1010 |  1010 |      5 | 50   | 25   |   10 |   2 |    2 |    3 | crd    |

Factorial CRD Fieldbook preview {.table .caption-top}

\
\
`# Spatial layout visualization`\
[`tarpuy_plotdesign`](https://inkaverse.com/reference/tarpuy_plotdesign.md)`(`\
`  data ``=`` ``crd_exp_2f``,`\
`  factor ``=`` ``"NaCl"``,`\
`  fill ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"plots"``, ``"Temp"``)`\
`)`

![](DoE-2_DCA_files/figure-html/unnamed-chunk-2-1.png)
