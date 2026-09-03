# Single-Factor Design: CRD

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

## Completely Randomized Design (CRD)

The Completely Randomized Design is recommended when experimental units
are homogeneous, such as germination chambers, lab assays, or controlled
greenhouse benches.

\
`# 1. Define salinity levels (NaCl concentrations in mM)`\
`factors_crd`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  NaCl``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"0"``, ``"50"``, ``"100"``, ``"150"``, ``"200"``)`\
`)`\
\
`# 2. Generate CRD layout (5 treatments x 4 replications = 20 petri dishes/units)`\
`crd_exp`` ``<-`` `[`design_repblock`](https://inkaverse.com/reference/design_repblock.md)`(`\
`  factors ``=`` ``factors_crd``,`\
`  type ``=`` ``"crd"``,`\
`  rep ``=`` ``4``,`\
`  zigzag ``=`` ``TRUE``,`\
`  seed ``=`` ``2026`\
`)`\
\
`# Fieldbook preview`\
`crd_exp``$``fieldbook`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``10``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``caption ``=`` ``"CRD Fieldbook preview"``)`

| qrcode         | plots | ntreat | NaCl | sort | rep | rows | cols | design |
|:---------------|------:|-------:|:-----|-----:|----:|-----:|-----:|:-------|
| inkaverse_1001 |  1001 |      1 | 0    |    1 |   1 |    1 |    1 | crd    |
| inkaverse_1002 |  1002 |      1 | 0    |    2 |   3 |    1 |    2 | crd    |
| inkaverse_1003 |  1003 |      3 | 100  |    3 |   3 |    1 |    3 | crd    |
| inkaverse_1004 |  1004 |      2 | 50   |    4 |   2 |    1 |    4 | crd    |
| inkaverse_1005 |  1005 |      3 | 100  |    5 |   2 |    1 |    5 | crd    |
| inkaverse_1006 |  1006 |      2 | 50   |    6 |   1 |    2 |    5 | crd    |
| inkaverse_1007 |  1007 |      4 | 150  |    7 |   3 |    2 |    4 | crd    |
| inkaverse_1008 |  1008 |      2 | 50   |    8 |   3 |    2 |    3 | crd    |
| inkaverse_1009 |  1009 |      4 | 150  |    9 |   4 |    2 |    2 | crd    |
| inkaverse_1010 |  1010 |      5 | 200  |   10 |   2 |    2 |    1 | crd    |

CRD Fieldbook preview {.table .caption-top}

\
\
`# Layout on germination chamber shelves`\
\
[`tarpuy_plotdesign`](https://inkaverse.com/reference/tarpuy_plotdesign.md)`(`\
`  data ``=`` ``crd_exp``,`\
`  factor ``=`` ``"NaCl"``,`\
`  fill ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"plots"``, ``"NaCl"``)`\
`)`

![](DoE-1_DCA_files/figure-html/unnamed-chunk-2-1.png)
