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

``` r

# Install packages and dependencies

library(inti)
library(dplyr)
library(huito)
```

## Designs with Two Factors

When evaluating two or more factors, four designs become available:
**CRD**, **RCBD**, **Split-plot RCBD**, and **Augmented**.

## Augmented Design in RCBD (Augmented RCBD)

The Augmented Design is recommended for screening large collections of
entries (e.g., accessions or candidate clones) when seed or space is
limited, repeating check varieties in each block while evaluating new
entries only once.

``` r

# 1. Define checks (commercial controls) and new accessions
checks <- c("INIA_415", "INIA_420")
entries <- paste0("Geno_", 1:50)

# 2. Generate Augmented layout: 18 entries + (2 checks x 3 blocks) = 24 plots
design <- design_augmented(
  checks = checks,
  entries = entries,
  blocks = 5,
  zigzag = FALSE,
  seed = 2026
)

# Fieldbook preview

fb <- design$fieldbook 

fb %>% 
  knitr::kable(caption = "Augmented RCBD Fieldbook preview")
```

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
| inkaverse_1011_Geno_27 | 1011 | 29 | Geno_27 | test | 0 | 1 | 11 | 1 | 11 | augmented |
| inkaverse_1012_Geno_33 | 1012 | 35 | Geno_33 | test | 0 | 1 | 12 | 1 | 12 | augmented |
| inkaverse_2001_INIA_420 | 2001 | 2 | INIA_420 | check | 1 | 2 | 1 | 2 | 1 | augmented |
| inkaverse_2002_Geno_50 | 2002 | 52 | Geno_50 | test | 0 | 2 | 2 | 2 | 2 | augmented |
| inkaverse_2003_Geno_37 | 2003 | 39 | Geno_37 | test | 0 | 2 | 3 | 2 | 3 | augmented |
| inkaverse_2004_Geno_15 | 2004 | 17 | Geno_15 | test | 0 | 2 | 4 | 2 | 4 | augmented |
| inkaverse_2005_INIA_415 | 2005 | 1 | INIA_415 | check | 1 | 2 | 5 | 2 | 5 | augmented |
| inkaverse_2006_Geno_18 | 2006 | 20 | Geno_18 | test | 0 | 2 | 6 | 2 | 6 | augmented |
| inkaverse_2007_Geno_41 | 2007 | 43 | Geno_41 | test | 0 | 2 | 7 | 2 | 7 | augmented |
| inkaverse_2008_Geno_24 | 2008 | 26 | Geno_24 | test | 0 | 2 | 8 | 2 | 8 | augmented |
| inkaverse_2009_Geno_12 | 2009 | 14 | Geno_12 | test | 0 | 2 | 9 | 2 | 9 | augmented |
| inkaverse_2010_Geno_43 | 2010 | 45 | Geno_43 | test | 0 | 2 | 10 | 2 | 10 | augmented |
| inkaverse_2011_Geno_10 | 2011 | 12 | Geno_10 | test | 0 | 2 | 11 | 2 | 11 | augmented |
| inkaverse_2012_Geno_19 | 2012 | 21 | Geno_19 | test | 0 | 2 | 12 | 2 | 12 | augmented |
| inkaverse_3001_Geno_40 | 3001 | 42 | Geno_40 | test | 0 | 3 | 1 | 3 | 1 | augmented |
| inkaverse_3002_Geno_16 | 3002 | 18 | Geno_16 | test | 0 | 3 | 2 | 3 | 2 | augmented |
| inkaverse_3003_INIA_420 | 3003 | 2 | INIA_420 | check | 1 | 3 | 3 | 3 | 3 | augmented |
| inkaverse_3004_Geno_3 | 3004 | 5 | Geno_3 | test | 0 | 3 | 4 | 3 | 4 | augmented |
| inkaverse_3005_Geno_25 | 3005 | 27 | Geno_25 | test | 0 | 3 | 5 | 3 | 5 | augmented |
| inkaverse_3006_Geno_8 | 3006 | 10 | Geno_8 | test | 0 | 3 | 6 | 3 | 6 | augmented |
| inkaverse_3007_Geno_42 | 3007 | 44 | Geno_42 | test | 0 | 3 | 7 | 3 | 7 | augmented |
| inkaverse_3008_Geno_46 | 3008 | 48 | Geno_46 | test | 0 | 3 | 8 | 3 | 8 | augmented |
| inkaverse_3009_Geno_47 | 3009 | 49 | Geno_47 | test | 0 | 3 | 9 | 3 | 9 | augmented |
| inkaverse_3010_Geno_4 | 3010 | 6 | Geno_4 | test | 0 | 3 | 10 | 3 | 10 | augmented |
| inkaverse_3011_INIA_415 | 3011 | 1 | INIA_415 | check | 1 | 3 | 11 | 3 | 11 | augmented |
| inkaverse_3012_Geno_32 | 3012 | 34 | Geno_32 | test | 0 | 3 | 12 | 3 | 12 | augmented |
| inkaverse_4001_Geno_13 | 4001 | 15 | Geno_13 | test | 0 | 4 | 1 | 4 | 1 | augmented |
| inkaverse_4002_Geno_35 | 4002 | 37 | Geno_35 | test | 0 | 4 | 2 | 4 | 2 | augmented |
| inkaverse_4003_Geno_14 | 4003 | 16 | Geno_14 | test | 0 | 4 | 3 | 4 | 3 | augmented |
| inkaverse_4004_Geno_49 | 4004 | 51 | Geno_49 | test | 0 | 4 | 4 | 4 | 4 | augmented |
| inkaverse_4005_INIA_420 | 4005 | 2 | INIA_420 | check | 1 | 4 | 5 | 4 | 5 | augmented |
| inkaverse_4006_Geno_26 | 4006 | 28 | Geno_26 | test | 0 | 4 | 6 | 4 | 6 | augmented |
| inkaverse_4007_Geno_2 | 4007 | 4 | Geno_2 | test | 0 | 4 | 7 | 4 | 7 | augmented |
| inkaverse_4008_Geno_9 | 4008 | 11 | Geno_9 | test | 0 | 4 | 8 | 4 | 8 | augmented |
| inkaverse_4009_Geno_17 | 4009 | 19 | Geno_17 | test | 0 | 4 | 9 | 4 | 9 | augmented |
| inkaverse_4010_Geno_39 | 4010 | 41 | Geno_39 | test | 0 | 4 | 10 | 4 | 10 | augmented |
| inkaverse_4011_INIA_415 | 4011 | 1 | INIA_415 | check | 1 | 4 | 11 | 4 | 11 | augmented |
| inkaverse_4012_Geno_20 | 4012 | 22 | Geno_20 | test | 0 | 4 | 12 | 4 | 12 | augmented |
| inkaverse_5001_Geno_23 | 5001 | 25 | Geno_23 | test | 0 | 5 | 1 | 5 | 1 | augmented |
| inkaverse_5002_Geno_22 | 5002 | 24 | Geno_22 | test | 0 | 5 | 2 | 5 | 2 | augmented |
| inkaverse_5003_Geno_7 | 5003 | 9 | Geno_7 | test | 0 | 5 | 3 | 5 | 3 | augmented |
| inkaverse_5004_Geno_21 | 5004 | 23 | Geno_21 | test | 0 | 5 | 4 | 5 | 4 | augmented |
| inkaverse_5005_Geno_1 | 5005 | 3 | Geno_1 | test | 0 | 5 | 5 | 5 | 5 | augmented |
| inkaverse_5006_Geno_30 | 5006 | 32 | Geno_30 | test | 0 | 5 | 6 | 5 | 6 | augmented |
| inkaverse_5007_Geno_11 | 5007 | 13 | Geno_11 | test | 0 | 5 | 7 | 5 | 7 | augmented |
| inkaverse_5008_INIA_420 | 5008 | 2 | INIA_420 | check | 1 | 5 | 8 | 5 | 8 | augmented |
| inkaverse_5009_Geno_6 | 5009 | 8 | Geno_6 | test | 0 | 5 | 9 | 5 | 9 | augmented |
| inkaverse_5010_INIA_415 | 5010 | 1 | INIA_415 | check | 1 | 5 | 10 | 5 | 10 | augmented |
| inkaverse_5011_Geno_48 | 5011 | 50 | Geno_48 | test | 0 | 5 | 11 | 5 | 11 | augmented |
| inkaverse_5012_Geno_28 | 5012 | 30 | Geno_28 | test | 0 | 5 | 12 | 5 | 12 | augmented |

Augmented RCBD Fieldbook preview {.table .caption-top}

``` r


# Field layout visualization
tarpuy_plotdesign(
  data = design,
  factor = "type",          
  fill = c("plots", "entry")
)
```

![](DoE-2_AUG_files/figure-html/unnamed-chunk-2-1.png)

## Label

The experimental field book generated by the design is used as the input
data for label creation. Each row represents an experimental unit,
allowing the automatic generation of individualized labels.

## Customize the label layout

The label layout can be customized by combining text, images and QR
codes. Each layer can use values from the experimental field book,
allowing automatic generation of labels for every experimental plot.

Load package and import fonts.

``` r

font <- c("Permanent Marker", "Tillana", "Courgette")

huito_fonts(font)
```

> You can find more fonts in <https://fonts.google.com/>

## Label design

``` r

label <- fb %>%
  label_layout(
    size = c(5.2, 10)
    ,
    border_color = "#5C0000"
    ,
    border_width = 1.5
  ) %>%
  include_image(
    value = "files/inkaverse.png"
    ,
    size = c(1.3, 1.5)
    ,
    position = c(0.8, 9.1)
  )  %>%
  include_text(
    value = "plots"
    ,
    position = c(4.2, 9.1)
    ,
    size = 20
    ,
    color = "black"
    ,
    fontface = "bold"
    ,
    font = font[1]
  )  %>%
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                ,
                size = c(5, 1)
                ,
                position = c(2.6, 7.7)) %>%
  include_barcode(value = "qrcode"
                  ,
                  size = c(5, 5)
                  ,
                  position = c(2.6, 4.7)) %>%
  include_text(
    value = "checks"
    ,
    position = c(2.6, 2)
    ,
    size = 12
    ,
    prefix = "Checks: "
    ,
    color = "blue"
    ,
    font = font[2]
    , 
    fontface = "bold"
  )  %>%
    include_text(
    value = "entry"
    ,
    position = c(2.6, 1.5)
    ,
    size = 12
    ,
    prefix = "Entry: "
    ,
    color = "red"
    ,
    font = font[2]
    , 
    fontface = "bold"
  )  %>%
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                ,
                size = c(5, 1)
                ,
                position = c(2.6, 0.6)) 
```

### Label preview

The preview mode `label_print(mode = "preview")` generate a example of
the label design from a random row of the data set.

``` r

label %>% 
  label_print(mode = "preview")
```

![](DoE-2_AUG_files/figure-html/unnamed-chunk-5-1.png)

### Generate the complete labels

If you want generate the complete labels list, change:
`label_print(mode = "complete")`.

``` r

label %>% 
  label_print(mode = "complete"
              , filename = "vertical-aug")
```
