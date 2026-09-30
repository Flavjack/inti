# Two-Factors Design: RCBD

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
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`huito`](https://huito.inkaverse.com/)`)`

## Designs with Two Factors

When evaluating two factors, four designs become available: **CRD**,
**RCBD**, **Split-plot RCBD**, and **Augmented**.

## Factorial Randomized Complete Block Design (RCBD)

Recommended for multi-factor trials where field spatial variability or
environmental gradients require blocking to control experimental error.

\
`# 1. Define factors: Bean genotypes and fertilization levels`\
`factors`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  Genotype ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Bean_01"``, ``"Bean_02"``, ``"Bean_03"``)``,`\
`  Fertilization ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"0"``, ``"50"``, ``"100"``)`\
`)`\
\
`# 2. Generate factorial RCBD layout`\
`design`` ``<-`` `[`design_repblock`](https://inkaverse.com/reference/design_repblock.md)`(`\
`  nfactors ``=`` ``2``,`\
`  factors ``=`` ``factors``,`\
`  type ``=`` ``"rcbd"``,`\
`  rep ``=`` ``4``,`\
`  zigzag ``=`` ``TRUE``,`\
`  seed ``=`` ``2026`\
`)`\
\
`# Fieldbook preview`\
`fb`` ``<-`` ``design``$``fieldbook`\
\
`fb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``caption ``=`` ``"Fieldbook preview"``)`

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
| inkaverse_2002 |  2002 |      1 | Bean_01  | 0             |    2 |     2 |    2 |    8 | rcbd   |
| inkaverse_2003 |  2003 |      3 | Bean_03  | 0             |    3 |     2 |    2 |    7 | rcbd   |
| inkaverse_2004 |  2004 |      6 | Bean_03  | 50            |    4 |     2 |    2 |    6 | rcbd   |
| inkaverse_2005 |  2005 |      4 | Bean_01  | 50            |    5 |     2 |    2 |    5 | rcbd   |
| inkaverse_2006 |  2006 |      9 | Bean_03  | 100           |    6 |     2 |    2 |    4 | rcbd   |
| inkaverse_2007 |  2007 |      8 | Bean_02  | 100           |    7 |     2 |    2 |    3 | rcbd   |
| inkaverse_2008 |  2008 |      2 | Bean_02  | 0             |    8 |     2 |    2 |    2 | rcbd   |
| inkaverse_2009 |  2009 |      7 | Bean_01  | 100           |    9 |     2 |    2 |    1 | rcbd   |
| inkaverse_3001 |  3001 |      9 | Bean_03  | 100           |    1 |     3 |    3 |    1 | rcbd   |
| inkaverse_3002 |  3002 |      1 | Bean_01  | 0             |    2 |     3 |    3 |    2 | rcbd   |
| inkaverse_3003 |  3003 |      7 | Bean_01  | 100           |    3 |     3 |    3 |    3 | rcbd   |
| inkaverse_3004 |  3004 |      4 | Bean_01  | 50            |    4 |     3 |    3 |    4 | rcbd   |
| inkaverse_3005 |  3005 |      2 | Bean_02  | 0             |    5 |     3 |    3 |    5 | rcbd   |
| inkaverse_3006 |  3006 |      6 | Bean_03  | 50            |    6 |     3 |    3 |    6 | rcbd   |
| inkaverse_3007 |  3007 |      5 | Bean_02  | 50            |    7 |     3 |    3 |    7 | rcbd   |
| inkaverse_3008 |  3008 |      3 | Bean_03  | 0             |    8 |     3 |    3 |    8 | rcbd   |
| inkaverse_3009 |  3009 |      8 | Bean_02  | 100           |    9 |     3 |    3 |    9 | rcbd   |
| inkaverse_4001 |  4001 |      7 | Bean_01  | 100           |    1 |     4 |    4 |    9 | rcbd   |
| inkaverse_4002 |  4002 |      2 | Bean_02  | 0             |    2 |     4 |    4 |    8 | rcbd   |
| inkaverse_4003 |  4003 |      1 | Bean_01  | 0             |    3 |     4 |    4 |    7 | rcbd   |
| inkaverse_4004 |  4004 |      4 | Bean_01  | 50            |    4 |     4 |    4 |    6 | rcbd   |
| inkaverse_4005 |  4005 |      3 | Bean_03  | 0             |    5 |     4 |    4 |    5 | rcbd   |
| inkaverse_4006 |  4006 |      8 | Bean_02  | 100           |    6 |     4 |    4 |    4 | rcbd   |
| inkaverse_4007 |  4007 |      6 | Bean_03  | 50            |    7 |     4 |    4 |    3 | rcbd   |
| inkaverse_4008 |  4008 |      5 | Bean_02  | 50            |    8 |     4 |    4 |    2 | rcbd   |
| inkaverse_4009 |  4009 |      9 | Bean_03  | 100           |    9 |     4 |    4 |    1 | rcbd   |

Fieldbook preview {.table .caption-top}

\
\
`# Spatial layout visualization`\
[`tarpuy_plotdesign`](https://inkaverse.com/reference/tarpuy_plotdesign.md)`(`\
`  data ``=`` ``design``,`\
`  factor ``=`` ``"Genotype"``,`\
`  fill ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"plots"``, ``"Fertilization"``)`\
`)`

![](DoE-2_RCBD_files/figure-html/unnamed-chunk-2-1.png)

## Label

The experimental field book generated by the design is used as the input
data for label creation. Each row represents an experimental unit,
allowing the automatic generation of individualized labels.

## Customize the label layout

The label layout can be customized by combining text, images and QR
codes. Each layer can use values from the experimental field book,
allowing automatic generation of labels for every experimental plot.

Load package and import fonts.

\
`font`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Permanent Marker"``, ``"Tillana"``, ``"Courgette"``)`\
\
[`huito_fonts`](http://huito.inkaverse.com/reference/huito_fonts.md)`(``font``)`

> You can find more fonts in <https://fonts.google.com/>

## Label design

\
`label`` ``<-`` ``fb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`label_layout`](http://huito.inkaverse.com/reference/label_layout.md)`(`\
`    size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``5.2``, ``10``)`\
`    , border_color ``=`` ``"#5C0000"`\
`    , border_width ``=`` ``1.5`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_image`](http://huito.inkaverse.com/reference/include_image.md)`(`\
`    value ``=`` ``"https://inkaverse.com/img/inkaverse.png"`\
`    , size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1.3``, ``1.5``)`\
`    , position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.8``, ``9.1``)`\
`  ``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(`\
`    value ``=`` ``"plots"`\
`    , position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``4.2``, ``9.1``)`\
`    , size ``=`` ``20`\
`    , color ``=`` ``"black"`\
`    , fontface ``=`` ``"bold"`\
`    , font ``=`` ``font``[``1``]`\
`  ``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_image`](http://huito.inkaverse.com/reference/include_image.md)`(``value ``=`` ``"https://huito.inkaverse.com/img/scale.pdf"`\
`                , size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``1``)`\
`                , position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``7.7``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_barcode`](http://huito.inkaverse.com/reference/include_barcode.md)`(``value ``=`` ``"qrcode"`\
`                  , size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``5``)`\
`                  , position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``4.7``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(`\
`    value ``=`` ``"Genotype"`\
`    ,`\
`    position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``2``)`\
`    ,`\
`    size ``=`` ``12`\
`    ,`\
`    prefix ``=`` ``"Genotype: "`\
`    ,`\
`    color ``=`` ``"blue"`\
`    ,`\
`    font ``=`` ``font``[``2``]`\
`    , `\
`    fontface ``=`` ``"bold"`\
`  ``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(`\
`    value ``=`` ``"Fertilization"`\
`    ,`\
`    position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``1.5``)`\
`    ,`\
`    size ``=`` ``12`\
`    ,`\
`    prefix ``=`` ``"Fertilization: "`\
`    ,`\
`    color ``=`` ``"red"`\
`    ,`\
`    font ``=`` ``font``[``2``]`\
`    , `\
`    fontface ``=`` ``"bold"`\
`  ``)`` ``|>`` `\
`  `[`include_image`](http://huito.inkaverse.com/reference/include_image.md)`(``value ``=`` ``"https://huito.inkaverse.com/img/scale.pdf"`\
`                ,`\
`                size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``1``)`\
`                ,`\
`                position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``0.6``)``)`` `

### Label preview

The preview mode `label_print(mode = "preview")` generate a example of
the label design from a random row of the data set.

\
`label`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`label_print`](http://huito.inkaverse.com/reference/label_print.md)`(``mode ``=`` ``"preview"``)`

![](DoE-2_RCBD_files/figure-html/unnamed-chunk-5-1.png)

### Generate the complete labels

If you want generate the complete labels list, change:
`label_print(mode = "complete")`.

\
`label`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`label_print`](http://huito.inkaverse.com/reference/label_print.md)`(``mode ``=`` ``"complete"`\
`              , filename ``=`` ``"vertical-DBCA-2"`\
`              , nlabels ``=`` ``12``)`
