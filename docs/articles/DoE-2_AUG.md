# Two-Factors Design: Augmented Design in RCBD

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

## Augmented Design in RCBD (Augmented RCBD)

The Augmented Design is recommended for screening large collections of
entries (e.g., accessions or candidate clones) when seed or space is
limited, repeating check varieties in each block while evaluating new
entries only once.

\
`# 1. Define checks (commercial controls) and new accessions`\
`checks`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"INIA_415"``, ``"INIA_420"``)`\
`entries`` ``<-`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"Geno_"``, ``1``:``50``)`\
\
`# 2. Generate Augmented layout: 18 entries + (2 checks x 3 blocks) = 24 plots`\
`aug_exp`` ``<-`` `[`design_augmented`](https://inkaverse.com/reference/design_augmented.md)`(`\
`  checks ``=`` ``checks``,`\
`  entries ``=`` ``entries``,`\
`  blocks ``=`` ``5``,`\
`  zigzag ``=`` ``TRUE``,`\
`  seed ``=`` ``2026`\
`)`\
\
`# Fieldbook preview`\
`aug_exp``$``fieldbook`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``10``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``caption ``=`` ``"Augmented RCBD Fieldbook preview"``)`

| qrcode | plots | ntreat | entry | type | checks | block | sort | rows | cols | design |
|:---|---:|---:|:---|:---|---:|---:|---:|---:|---:|:---|
| inkaverse_1001_INIA_415 | 1001 | 1 | INIA_415 | check | 1 | 1 | 1 | 1 | 1 | augmented |
| inkaverse_1002_Geno_38 | 1002 | 40 | Geno_38 | test | 0 | 1 | 2 | 1 | 2 | augmented |
| inkaverse_1003_Geno_31 | 1003 | 33 | Geno_31 | test | 0 | 1 | 3 | 1 | 3 | augmented |
| inkaverse_1004_Geno_36 | 1004 | 38 | Geno_36 | test | 0 | 1 | 4 | 1 | 4 | augmented |
| inkaverse_1005_Geno_45 | 1005 | 47 | Geno_45 | test | 0 | 1 | 5 | 1 | 5 | augmented |
| inkaverse_1006_Geno_29 | 1006 | 31 | Geno_29 | test | 0 | 1 | 6 | 1 | 6 | augmented |
| inkaverse_1007_Geno_5 | 1007 | 7 | Geno_5 | test | 0 | 1 | 7 | 1 | 7 | augmented |
| inkaverse_1008_Geno_44 | 1008 | 46 | Geno_44 | test | 0 | 1 | 8 | 1 | 8 | augmented |
| inkaverse_1009_INIA_420 | 1009 | 2 | INIA_420 | check | 1 | 1 | 9 | 1 | 9 | augmented |
| inkaverse_1010_Geno_34 | 1010 | 36 | Geno_34 | test | 0 | 1 | 10 | 1 | 10 | augmented |

Augmented RCBD Fieldbook preview {.table .caption-top}

\
\
`# Field layout visualization`\
[`tarpuy_plotdesign`](https://inkaverse.com/reference/tarpuy_plotdesign.md)`(`\
`  data ``=`` ``aug_exp``,`\
`  factor ``=`` ``"type"``,          `\
`  fill ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"plots"``, ``"entry"``)`\
`)`

![](DoE-2_AUG_files/figure-html/unnamed-chunk-2-1.png)
