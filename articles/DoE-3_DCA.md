# Three-Factors Design: CRD

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

## Designs with Three Factors

When evaluating three factors, factorial experiments can be implemented
using **CRD** and **RCBD** designs.

## Factorial Completely Randomized Design (Factorial CRD)

Recommended for multi-factor experiments under homogeneous conditions,
such as temperature-, salinity-, and genotype-controlled germination
assays in growth chambers.

\
`# 1. Define factors: Salinity levels, incubation temperatures, and genotypes`\
`factors`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  NaCl ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"0"``, ``"50"``)``,`\
`  Temp ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"20"``, ``"25"``)``,`\
`  Genotype ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"G1"``, ``"G2"``)`\
`)`\
\
`# 2. Generate factorial CRD layout`\
`design`` ``<-`` `[`design_repblock`](https://inkaverse.com/reference/design_repblock.md)`(`\
`  nfactors ``=`` ``3``,`\
`  factors ``=`` ``factors``,`\
`  type ``=`` ``"crd"``,`\
`  rep ``=`` ``4``,`\
`  zigzag ``=`` ``TRUE``,`\
`  seed ``=`` ``2026`\
`)`\
\
`# Fieldbook preview`\
`fb`` ``<-`` ``design``$``fieldbook`` `\
\
`fb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``caption ``=`` ``"Factorial CRD Fieldbook preview"``)`

| qrcode         | plots | ntreat | NaCl | Temp | Genotype | sort | rep | rows | cols | design |
|:---------------|------:|-------:|:-----|:-----|:---------|-----:|----:|-----:|-----:|:-------|
| inkaverse_1001 |  1001 |      3 | 0    | 25   | G1       |    1 |   1 |    1 |    1 | crd    |
| inkaverse_1002 |  1002 |      4 | 50   | 25   | G1       |    2 |   2 |    1 |    2 | crd    |
| inkaverse_1003 |  1003 |      2 | 50   | 20   | G1       |    3 |   4 |    1 |    3 | crd    |
| inkaverse_1004 |  1004 |      8 | 50   | 25   | G2       |    4 |   1 |    1 |    4 | crd    |
| inkaverse_1005 |  1005 |      2 | 50   | 20   | G1       |    5 |   2 |    1 |    5 | crd    |
| inkaverse_1006 |  1006 |      4 | 50   | 25   | G1       |    6 |   1 |    1 |    6 | crd    |
| inkaverse_1007 |  1007 |      4 | 50   | 25   | G1       |    7 |   4 |    1 |    7 | crd    |
| inkaverse_1008 |  1008 |      7 | 0    | 25   | G2       |    8 |   3 |    1 |    8 | crd    |
| inkaverse_1009 |  1009 |      5 | 0    | 20   | G2       |    9 |   4 |    2 |    8 | crd    |
| inkaverse_1010 |  1010 |      2 | 50   | 20   | G1       |   10 |   3 |    2 |    7 | crd    |
| inkaverse_1011 |  1011 |      8 | 50   | 25   | G2       |   11 |   3 |    2 |    6 | crd    |
| inkaverse_1012 |  1012 |      7 | 0    | 25   | G2       |   12 |   1 |    2 |    5 | crd    |
| inkaverse_1013 |  1013 |      5 | 0    | 20   | G2       |   13 |   1 |    2 |    4 | crd    |
| inkaverse_1014 |  1014 |      7 | 0    | 25   | G2       |   14 |   2 |    2 |    3 | crd    |
| inkaverse_1015 |  1015 |      6 | 50   | 20   | G2       |   15 |   1 |    2 |    2 | crd    |
| inkaverse_1016 |  1016 |      1 | 0    | 20   | G1       |   16 |   2 |    2 |    1 | crd    |
| inkaverse_1017 |  1017 |      8 | 50   | 25   | G2       |   17 |   2 |    3 |    1 | crd    |
| inkaverse_1018 |  1018 |      8 | 50   | 25   | G2       |   18 |   4 |    3 |    2 | crd    |
| inkaverse_1019 |  1019 |      5 | 0    | 20   | G2       |   19 |   2 |    3 |    3 | crd    |
| inkaverse_1020 |  1020 |      1 | 0    | 20   | G1       |   20 |   4 |    3 |    4 | crd    |
| inkaverse_1021 |  1021 |      4 | 50   | 25   | G1       |   21 |   3 |    3 |    5 | crd    |
| inkaverse_1022 |  1022 |      1 | 0    | 20   | G1       |   22 |   3 |    3 |    6 | crd    |
| inkaverse_1023 |  1023 |      6 | 50   | 20   | G2       |   23 |   3 |    3 |    7 | crd    |
| inkaverse_1024 |  1024 |      6 | 50   | 20   | G2       |   24 |   4 |    3 |    8 | crd    |
| inkaverse_1025 |  1025 |      2 | 50   | 20   | G1       |   25 |   1 |    4 |    8 | crd    |
| inkaverse_1026 |  1026 |      3 | 0    | 25   | G1       |   26 |   2 |    4 |    7 | crd    |
| inkaverse_1027 |  1027 |      6 | 50   | 20   | G2       |   27 |   2 |    4 |    6 | crd    |
| inkaverse_1028 |  1028 |      5 | 0    | 20   | G2       |   28 |   3 |    4 |    5 | crd    |
| inkaverse_1029 |  1029 |      1 | 0    | 20   | G1       |   29 |   1 |    4 |    4 | crd    |
| inkaverse_1030 |  1030 |      7 | 0    | 25   | G2       |   30 |   4 |    4 |    3 | crd    |
| inkaverse_1031 |  1031 |      3 | 0    | 25   | G1       |   31 |   4 |    4 |    2 | crd    |
| inkaverse_1032 |  1032 |      3 | 0    | 25   | G1       |   32 |   3 |    4 |    1 | crd    |

Factorial CRD Fieldbook preview {.table .caption-top}

\
\
`# Spatial layout visualization`\
[`tarpuy_plotdesign`](https://inkaverse.com/reference/tarpuy_plotdesign.md)`(`\
`  data ``=`` ``design``,`\
`  factor ``=`` ``"NaCl"``,`\
`  fill ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"plots"``, ``"Temp"``, ``"Genotype"``)`\
`)`

![](DoE-3_DCA_files/figure-html/unnamed-chunk-2-1.png)

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
`    ,`\
`    border_color ``=`` ``"#5C0000"`\
`    ,`\
`    border_width ``=`` ``1.5`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_image`](http://huito.inkaverse.com/reference/include_image.md)`(`\
`    value ``=`` ``"https://inkaverse.com/img/inkaverse.png"`\
`    ,`\
`    size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1.3``, ``1.5``)`\
`    ,`\
`    position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.8``, ``9.1``)`\
`  ``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(`\
`    value ``=`` ``"plots"`\
`    ,`\
`    position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``4.2``, ``9.1``)`\
`    ,`\
`    size ``=`` ``20`\
`    ,`\
`    color ``=`` ``"black"`\
`    ,`\
`    fontface ``=`` ``"bold"`\
`    ,`\
`    font ``=`` ``font``[``1``]`\
`  ``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_image`](http://huito.inkaverse.com/reference/include_image.md)`(``value ``=`` ``"https://huito.inkaverse.com/img/scale.pdf"`\
`                ,`\
`                size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``1``)`\
`                ,`\
`                position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``7.7``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_barcode`](http://huito.inkaverse.com/reference/include_barcode.md)`(``value ``=`` ``"qrcode"`\
`                  ,`\
`                  size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``5``)`\
`                  ,`\
`                  position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``4.7``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
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
`    value ``=`` ``"NaCl"`\
`    ,`\
`    position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``1.5``)`\
`    ,`\
`    size ``=`` ``12`\
`    ,`\
`    prefix ``=`` ``"salinity: "`\
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

![](DoE-3_DCA_files/figure-html/unnamed-chunk-5-1.png)

### Generate the complete labels

If you want generate the complete labels list, change:
`label_print(mode = "complete")`.

\
`label`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`label_print`](http://huito.inkaverse.com/reference/label_print.md)`(``mode ``=`` ``"complete"`\
`              , filename ``=`` ``"vertical-DCA-3"`\
`              , nlabels ``=`` ``12``)`
