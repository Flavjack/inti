# Single-Factor Design: RCBD

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

## Randomized Complete Block Design (RCBD)

The Randomized Complete Block Design is recommended when an
environmental gradient is present in the field, grouping experimental
units into homogeneous blocks to control spatial variability.

\
`# 1. Define factors: Bean genotypes and fertilization doses`\
`factors_rcbd`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  Genotype ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Bean_01"``, ``"Bean_02"``, ``"Bean_03"``)``,`\
`  Fertilization ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"0"``, ``"50"``, ``"100"``)`\
`)`\
\
`# 2. Generate RCBD layout (3 genotypes x 3 doses = 9 treatments, 4 blocks = 36 )`\
`rcbd_exp`` ``<-`` `[`design_repblock`](https://inkaverse.com/reference/design_repblock.md)`(`\
`  nfactors ``=`` ``2``,`\
`  factors ``=`` ``factors_rcbd``,`\
`  type ``=`` ``"rcbd"``,`\
`  rep ``=`` ``4``,`\
`  zigzag ``=`` ``TRUE``,`\
`  seed ``=`` ``2026`\
`)`\
\
`# Fieldbook preview`\
`rcbd_exp``$``fieldbook`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``10``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``caption ``=`` ``"RCBD Fieldbook preview"``)`

| qrcode         | plots | ntreat | Genotype | Fertilization | sort | block | rows | cols | design |
|:---------------|------:|-------:|:---------|:--------------|-----:|------:|-----:|-----:|:-------|
| inkaverse_1001 |  1001 |      2 | Bean_02  | 0             |    1 |     1 |    1 |    1 | rcbd   |
| inkaverse_1002 |  1002 |      9 | Bean_03  | 100           |    2 |     1 |    1 |    2 | rcbd   |
| inkaverse_1003 |  1003 |      5 | Bean_02  | 50            |    3 |     1 |    1 |    3 | rcbd   |
| inkaverse_1004 |  1004 |      6 | Bean_03  | 50            |    4 |     1 |    1 |    4 | rcbd   |
| inkaverse_1005 |  1005 |      4 | Bean_01  | 50            |    5 |     1 |    1 |    5 | rcbd   |
| inkaverse_1006 |  1006 |      3 | Bean_03  | 0             |    6 |     1 |    1 |    6 | rcbd   |
| inkaverse_1007 |  1007 |      8 | Bean_02  | 100           |    7 |     1 |    1 |    7 | rcbd   |
| inkaverse_1008 |  1008 |      7 | Bean_01  | 100           |    8 |     1 |    1 |    8 | rcbd   |
| inkaverse_1009 |  1009 |      1 | Bean_01  | 0             |    9 |     1 |    1 |    9 | rcbd   |
| inkaverse_2001 |  2001 |      5 | Bean_02  | 50            |    1 |     2 |    2 |    9 | rcbd   |

RCBD Fieldbook preview {.table .caption-top}

\
\
`# Field layout visualization`\
[`tarpuy_plotdesign`](https://inkaverse.com/reference/tarpuy_plotdesign.md)`(`\
`  data ``=`` ``rcbd_exp``,`\
`  factor ``=`` ``"Genotype"``,`\
`  fill ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"plots"``, ``"Fertilization"``)`\
`)`

![](DoE-1_RCBD_files/figure-html/unnamed-chunk-2-1.png)
