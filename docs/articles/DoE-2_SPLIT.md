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
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
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
`factors`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  Irrigation ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Full"``, ``"Deficit"``)``,`\
`  Variety    ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"chulpi"``, ``"kancolla"``, ``"choclito"``)`\
`)`\
\
`# 2. Generate Split-plot layout: 2 main levels x 3 sub levels x 4 blocks = 24 plots`\
`design`` ``<-`` `[`design_split`](https://inkaverse.com/reference/design_split.md)`(`\
`  factors ``=`` ``factors``,`\
`  type ``=`` ``"split_rcbd"``,`\
`  rep ``=`` ``4``,`\
`  zigzag ``=`` ``TRUE``,`\
`  seed ``=`` ``2026`\
`)`\
\
`# Fieldbook preview`\
`fb`` ``<-`` ``design``$``fieldbook`` `\
\
`fb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``caption ``=`` ``"Split-plot Fieldbook preview"``)`

| qrcode | plots | ntreat | Irrigation | Variety | wp_sp | block | sort | rows | cols | design |
|:---|---:|---:|:---|:---|:---|---:|---:|---:|---:|:---|
| inkaverse_1001_Full_chulpi | 1001 | 1 | Full | chulpi | Full_chulpi | 1 | 1 | 1 | 1 | split-rcbd |
| inkaverse_1002_Full_kancolla | 1002 | 3 | Full | kancolla | Full_kancolla | 1 | 2 | 2 | 1 | split-rcbd |
| inkaverse_1003_Full_choclito | 1003 | 5 | Full | choclito | Full_choclito | 1 | 3 | 3 | 1 | split-rcbd |
| inkaverse_1004_Deficit_kancolla | 1004 | 4 | Deficit | kancolla | Deficit_kancolla | 1 | 4 | 3 | 2 | split-rcbd |
| inkaverse_1005_Deficit_chulpi | 1005 | 2 | Deficit | chulpi | Deficit_chulpi | 1 | 5 | 2 | 2 | split-rcbd |
| inkaverse_1006_Deficit_choclito | 1006 | 6 | Deficit | choclito | Deficit_choclito | 1 | 6 | 1 | 2 | split-rcbd |
| inkaverse_2001_Deficit_chulpi | 2001 | 2 | Deficit | chulpi | Deficit_chulpi | 2 | 1 | 4 | 1 | split-rcbd |
| inkaverse_2002_Deficit_choclito | 2002 | 6 | Deficit | choclito | Deficit_choclito | 2 | 2 | 5 | 1 | split-rcbd |
| inkaverse_2003_Deficit_kancolla | 2003 | 4 | Deficit | kancolla | Deficit_kancolla | 2 | 3 | 6 | 1 | split-rcbd |
| inkaverse_2004_Full_chulpi | 2004 | 1 | Full | chulpi | Full_chulpi | 2 | 4 | 6 | 2 | split-rcbd |
| inkaverse_2005_Full_choclito | 2005 | 5 | Full | choclito | Full_choclito | 2 | 5 | 5 | 2 | split-rcbd |
| inkaverse_2006_Full_kancolla | 2006 | 3 | Full | kancolla | Full_kancolla | 2 | 6 | 4 | 2 | split-rcbd |
| inkaverse_3001_Full_chulpi | 3001 | 1 | Full | chulpi | Full_chulpi | 3 | 1 | 7 | 1 | split-rcbd |
| inkaverse_3002_Full_kancolla | 3002 | 3 | Full | kancolla | Full_kancolla | 3 | 2 | 8 | 1 | split-rcbd |
| inkaverse_3003_Full_choclito | 3003 | 5 | Full | choclito | Full_choclito | 3 | 3 | 9 | 1 | split-rcbd |
| inkaverse_3004_Deficit_chulpi | 3004 | 2 | Deficit | chulpi | Deficit_chulpi | 3 | 4 | 9 | 2 | split-rcbd |
| inkaverse_3005_Deficit_choclito | 3005 | 6 | Deficit | choclito | Deficit_choclito | 3 | 5 | 8 | 2 | split-rcbd |
| inkaverse_3006_Deficit_kancolla | 3006 | 4 | Deficit | kancolla | Deficit_kancolla | 3 | 6 | 7 | 2 | split-rcbd |
| inkaverse_4001_Deficit_chulpi | 4001 | 2 | Deficit | chulpi | Deficit_chulpi | 4 | 1 | 10 | 1 | split-rcbd |
| inkaverse_4002_Deficit_choclito | 4002 | 6 | Deficit | choclito | Deficit_choclito | 4 | 2 | 11 | 1 | split-rcbd |
| inkaverse_4003_Deficit_kancolla | 4003 | 4 | Deficit | kancolla | Deficit_kancolla | 4 | 3 | 12 | 1 | split-rcbd |
| inkaverse_4004_Full_choclito | 4004 | 5 | Full | choclito | Full_choclito | 4 | 4 | 12 | 2 | split-rcbd |
| inkaverse_4005_Full_kancolla | 4005 | 3 | Full | kancolla | Full_kancolla | 4 | 5 | 11 | 2 | split-rcbd |
| inkaverse_4006_Full_chulpi | 4006 | 1 | Full | chulpi | Full_chulpi | 4 | 6 | 10 | 2 | split-rcbd |

Split-plot Fieldbook preview {.table .caption-top}

\
\
`# Field layout visualization`\
[`tarpuy_plotdesign`](https://inkaverse.com/reference/tarpuy_plotdesign.md)`(`\
`  data ``=`` ``design``,`\
`  factor ``=`` ``"Irrigation"``,`\
`  fill ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"plots"``, ``"Variety"``)`\
`)`

![](DoE-2_SPLIT_files/figure-html/unnamed-chunk-2-1.png)

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
`label`` ``<-`` ``fb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)`  `\
`  `[`label_layout`](http://huito.inkaverse.com/reference/label_layout.md)`(``size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``2.5``)`\
`               , border_color ``=`` ``"blue"`\
`               ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_image`](http://huito.inkaverse.com/reference/include_image.md)`(`\
`    value ``=`` ``"https://flavjack.github.io/inti/img/inkaverse.png"`\
`    , size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.1``, ``2.4``)`\
`    , position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1.2``, ``1.25``)`\
`    ``# , opts = list("image_scale(200)", "image_noise()")`\
`    ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_barcode`](http://huito.inkaverse.com/reference/include_barcode.md)`(`\
`     value ``=`` ``"qrcode"`\
`     , size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.5``, ``2.5``)`\
`     , position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``8.2``, ``1.25``)`\
`     ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(``value ``=`` ``"INKAVERSE"`\
`               , position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``4.6``, ``2``)`\
`               , size ``=`` ``20`\
`               , font ``=`` ``font``[``1``]`\
`               , fontface ``=`` ``"bold"`\
`               , color ``=`` ``"red"`\
`               ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(``value ``=`` ``"Irrigation"`\
`               , position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.4``, ``1.2``)`\
`               , size ``=`` ``12`\
`               , opts ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``hjust ``=`` ``0.0``, vjust ``=`` ``0.0``)`\
`               , font ``=`` ``font``[``2``]`\
`               , color ``=`` ``"black"`\
`               , prefix ``=`` ``"Irrigation: "`\
`               , fontface ``=`` ``"bold"`\
`               ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(``value ``=`` ``"Variety"`\
`               , position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.4``, ``0.5``)`\
`               , opts ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``hjust ``=`` ``0.0``, vjust ``=`` ``0.0``)`` `\
`               , size ``=`` ``12`\
`               , color ``=`` ``"#009966"`\
`               , font ``=`` ``font``[``2``]`\
`               , prefix ``=`` ``"Variety: "`\
`               , fontface ``=`` ``"bold"`\
`               ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(``value ``=`` ``"plots"`\
`               , position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``9.7``, ``1.25``)`\
`               , angle ``=`` ``90`\
`               , size ``=`` ``12`\
`               , color ``=`` ``"brown"`\
`               , font ``=`` ``font``[``3``]`\
`               , prefix ``=`` ``"Plot: "`\
`               ``)`` `

### Label preview

The preview mode `label_print(mode = "preview")` generate a example of
the label design from a random row of the data set.

\
`label`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`label_print`](http://huito.inkaverse.com/reference/label_print.md)`(``mode ``=`` ``"preview"``)`

![](DoE-2_SPLIT_files/figure-html/unnamed-chunk-5-1.png)

### Generate the complete labels

If you want generate the complete labels list, change:
`label_print(mode = "complete")`.

\
`label`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`label_print`](http://huito.inkaverse.com/reference/label_print.md)`(``mode ``=`` ``"complete"`\
`              , filename ``=`` ``"horizontal-split"`\
`              , nlabels ``=`` ``12``)`
