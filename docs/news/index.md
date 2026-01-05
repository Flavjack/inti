# Changelog

## inti 0.7.0

- Improve templates for `scihub()` for works from bash
- Update color pallet to Brewer

## inti 0.6.9

CRAN release: 2025-10-17

- Improve templates for `scihub()`
- [`H2cal()`](https://inkaverse.com/reference/H2cal.md) was updated to
  include factors as formula
- Fix error in gdocs2qmd

## inti 0.6.8

CRAN release: 2025-08-30

- Avoid label overlapping in the significance
- Update pkgdown web page
- Fix table export in `gdocs2qmd()`
- Update ANOVA table for kable()
- Exclude rmarkdown template ==\> update to scihub research repository
  - Include Addins: `SciHub`

## inti 0.6.7

CRAN release: 2025-02-26

- Rticles
- Fix table conversion when only is present one table
- Yupana
- Avoid different bar widths
- New project template info
- Increase y limits for better error visualization
- Re arrange statistic information
- Repare agricolae table summary anova
- Tarpuy
  - Update budget template
  - Update experimental plan information

## inti 0.6.6

CRAN release: 2024-09-03

- Package
  - New function related
    [`outliers_remove()`](https://inkaverse.com/reference/outliers_remove.md)
    =\> “`remove_outliers`” to work with formula
  - New function related
    [`plot_diag()`](https://inkaverse.com/reference/plot_diag.md) =\>
    “`plot_diagnostic`” to work with formula
- Rticles
  - Fix Tables and Figures order in final document
- Tarpuy
  - Change name of the trait tab from `abbreviation` to `trait`
  - Update traits tab for include two formats: `date` and `mcategorical`
  - Fix sort of the traits in field book app
  - New option for generate the qr-code for each plot

## inti 0.6.5

CRAN release: 2024-05-16

- Package
  - Include Addins for Google authentication and renew process
- Rticles
  - Fix title position in article
  - Fix figure caption and cross reference

## inti 0.6.4

CRAN release: 2024-02-05

- Update bootstrap in apps
- Tarpuy:
  - Alows to exclude or delete [when](https://josesamos.github.io/when/)
    or `{sample}` colums
  - Allow `defaultValue` for traits
- Rticles
  - Include images using markdown syntax `![]()`

=\> Fix if “defaultvalue” is in the trait table

## inti 0.6.3

CRAN release: 2023-10-27

- `gdocs2qmd()`
  - change params: template ==\> theme
  - reference-doc: style_rticle.docx
- Tarpuy
  - Field book design allows different number of rows  
  - Design without replication (observation plots) ==\>
    [`design_noreps()`](https://inkaverse.com/reference/design_noreps.md)
  - Fix traits name order

## inti 0.6.2

CRAN release: 2023-09-02

- `gdocs2qmd()`
  - Bug with: “`Unknown element type at this position: UNSUPPORTED`”
  - The function works for articles and thesis
  - Include the cover page using a table
  - Include R markdown templates from RStudio
  - Rticles vignettes updated

## inti 0.6.1

CRAN release: 2023-05-30

- Tarpuy:
  - Include google sheet and docs in PLEX
  - Allow empty rows and without filling
  - Drop values in sheet traits with “X”
  - Only generate traits sheets
  - Seed is set by default
- Deprecated:
  - include_figure()
- New function
  - include_pdf()
- gdocs2qmd()
  - Word document with different output structure

## inti 0.6.0

CRAN release: 2023-01-24

- Fix for dev dplyr (Thanks [@hadley](https://github.com/hadley))
- [`tarpuy_plotdesign()`](https://inkaverse.com/reference/tarpuy_plotdesign.md)
  - Autoconvert as factor for plot design
  - Default names in plot to “row” and “columns”
- New function: `design_repblock` for “rcbd”, “crd” for any factor
  number
- Yupana create by default the sheet `locale = "en_US"` to use the
  decimal point
- `yupana_mvr` allow to select specific numeric variables
- `tarpuy_varlist` adapted to field book app
- Tarpuy: new module for use the information in Field Book app
  <https://play.google.com/store/apps/details?id=com.fieldbook.tracker>
- Rename function: `tarpuy_varlist` ==\> `tarpuy_traits`

## inti 0.5.8

CRAN release: 2022-11-16

`gdocs2qmd(format)` allow transform to quarto or Rmarkdown format

Update RStudio download link to posit

Yupana - fieldbook module:

Use “\_” or “.” to separate the traits factors:
[`yupana_reshape()`](https://inkaverse.com/reference/yupana_reshape.md)

Load and Save in a specific sheet

## inti 0.5.7

CRAN release: 2022-08-09

- Update functions bookdown to quarto:
  - [`figure2rmd()`](https://inkaverse.com/reference/figure2rmd.md) ==\>
    [`figure2qmd()`](https://inkaverse.com/reference/figure2qmd.md)
  - [`table2rmd()`](https://inkaverse.com/reference/table2rmd.md) ==\>
    [`table2qmd()`](https://inkaverse.com/reference/table2qmd.md)
  - `gdocs2rmd()` ==\> `gdocs2qmd()`
- Fix [`plot_raw()`](https://inkaverse.com/reference/plot_raw.md):
  “length(x) = 2 \> 1’ in coercion to ’logical(1)”
- Update [`jc_tombola()`](https://inkaverse.com/reference/jc_tombola.md)
- `outliers_remove(drop.na = FALSE)` avoid drop NA values by default
- [`H2cal()`](https://inkaverse.com/reference/H2cal.md) outliers are
  changed to NA in the data.frame
- [`yupana_mvr()`](https://inkaverse.com/reference/yupana_mvr.md):
  update function for correlation and PCA
- Yupana: update multivariate analysis

## inti 0.5.6

CRAN release: 2022-05-19

- Package: [`web_table()`](https://inkaverse.com/reference/web_table.md)
  - `autoWidth = TRUE`
  - `columnwidth` argument
  - `width` argument
- Fix [`plot_smr()`](https://inkaverse.com/reference/plot_smr.md):
  “length(x) = 2 \> 1’ in coercion to ’logical(1)”
- New function:
  [`split_folder()`](https://inkaverse.com/reference/split_folder.md)
- Yupana: add `scale` and `method` in correlation plot

## inti 0.5.5

CRAN release: 2022-04-01

- Yupana: update
  [`yupana_import()`](https://inkaverse.com/reference/yupana_import.md)
  using [`if_any()`](https://dplyr.tidyverse.org/reference/across.html)
  instead
  [`across()`](https://dplyr.tidyverse.org/reference/across.html)
- Tarpuy: in `dsg` the column `qr` ==\> `barcode`
- Tarpuy: update sheets names in `intro` section
- Tarypu: export field-book in specific sheet
- Tarpuy: select sheet for field-book sketch
- Tarpuy: create field-book only with the factor list
- Tarpuy: column with \[\] in `design` are omitted in the field-book
  generation
- CRAN comments: if (class(model) == “lmerMod”) =\> if ( is(model,
  “lmerMod”)

## inti 0.5.4

CRAN release: 2022-02-22

- [`outliers_remove()`](https://inkaverse.com/reference/outliers_remove.md):
  change [`cbind()`](https://rdrr.io/r/base/cbind.html) by
  [`cbind.data.frame()`](https://rdrr.io/r/base/cbind.html).
- Fix apps auth.
- Thanks to Uwe Ligges to allow consecutive CRAN updates.

## inti 0.5.3

CRAN release: 2022-02-18

- Complete location name in experimental information.
- Avoid labels in axis and legend using `""`.
- Update vignettes using bookdown.
- Fix table summary in
  [`H2cal()`](https://inkaverse.com/reference/H2cal.md).
- Update diagnostic plot in
  [`plot_diag()`](https://inkaverse.com/reference/plot_diag.md) to lm
  and lmerMod.
- Update code for logIn modules in apps.
- Update correlation graph in yupana.

## inti 0.5.2

CRAN release: 2021-12-19

- *Package*
  - Fix CRAN comments
  - Fix path to install Tarpuy dependencies
  - Include huito logo in apps
  - Fix factors in Tarpuy field-book export
  - Update code from
    [`tarpuy_design()`](https://inkaverse.com/reference/tarpuy_design.md)
  - Update barcode column for split using “\_”
  - Update function
    [`tarpuy_plex()`](https://inkaverse.com/reference/tarpuy_plex.md)

## inti 0.5.1

CRAN release: 2021-12-10

- *Package*
  - [`H2cal()`](https://inkaverse.com/reference/H2cal.md): BLUEs from
    H2cal may be wrong if other fixed effects in the model
    ([\#10](https://github.com/flavjack/inti/issues/10)).
    - Thanks Jim Holland ([@ncsumaize](https://github.com/ncsumaize))
      for the suggestion to improve the function.
  - New functions: `gdoc2rmd()`,
    [`table2rmd()`](https://inkaverse.com/reference/table2rmd.md),
    [`figure2rmd()`](https://inkaverse.com/reference/figure2rmd.md)
    - Use Articul8 Add-ons from Google docs to build Rticles
  - Update pkgdown

## inti 0.5.0

CRAN release: 2021-11-07

> Changes are incompatible with the old versions.

- *Package*
  - `yupana_export_smr` ==\>
    [`yupana_export()`](https://inkaverse.com/reference/yupana_export.md)
    - Extract all the information from `yupana_analysis`
  - `yupana_import_smr` ==\>
    [`yupana_import()`](https://inkaverse.com/reference/yupana_import.md)
    - Import information from the web and `yupana_analysis`
  - Update function
    [`H2cal()`](https://inkaverse.com/reference/H2cal.md)
- *Yupana*
  - Include statistics and anova table in export results
  - Clean headers in export data, exclude “{}”
  - Update load/save interface
- *Tarpuy*
  - Variable list only include arguments with {}
    - You can exclude: {evaluation} or {sampling}

## inti 0.4.4

CRAN release: 2021-10-01

- *Package*
  - [`jc_tombola()`](https://inkaverse.com/reference/jc_tombola.md)
    - Update function and selection paper by meeting
  - [`yupana_analysis()`](https://inkaverse.com/reference/yupana_analysis.md)
    - Include last_factor selection
  - [`yupana_mvr()`](https://inkaverse.com/reference/yupana_mvr.md)
    - Function not need last_factor
  - Include package version in the apps
  - Fixed navigation bar in the apps
- *Yupana*
  - PCA for individual in the bottom
  - Include version in the output table
  - Dimension for plots in multivariate analysis

## inti 0.4.3

CRAN release: 2021-09-08

- *Yupana*
  - Show equation with adjusted R in scatter plot graph
  - `sig` include all the variables from the summary table
  - In plots if the number of reps is 1 the `sig` and `error` is “none”

## inti 0.4.2

CRAN release: 2021-08-15

- *Package*
  - Include info
    [`plot_smr()`](https://inkaverse.com/reference/plot_smr.md) and
    `plot_raw`
  - [`plot_smr()`](https://inkaverse.com/reference/plot_smr.md)
    - Delete legend border
  - Transparent logos background
  - New vignette for coding with yupana
  - Update Rticles and Books template
  - Fix [`web_table()`](https://inkaverse.com/reference/web_table.md) to
    export in xlsx
  - [`plot_raw()`](https://inkaverse.com/reference/plot_raw.md) with
    scientific notation in labels
  - Include new data set `potato`
- *Yupana*
  - Legend position load correct
  - Headers with \[\] are excluded in the analysis

> Agradecimiento a Pedro Barriga por sus sugerencias para mejorar
> [`yupana()`](https://inkaverse.com/reference/yupana.md)

## inti 0.4.1

CRAN release: 2021-06-25

- *Package*
  - [`plot_smr()`](https://inkaverse.com/reference/plot_smr.md)
    - Add significance font size
  - [`plot_raw()`](https://inkaverse.com/reference/plot_raw.md)
    - Allows vector with colors for plots
    - Include “scatter plot”
  - [`H2cal()`](https://inkaverse.com/reference/H2cal.md) include trial
    option for MET
  - New video for the version \> 0.4.1
- *Yupana*
  - Add equations in regressions plot
  - Include scatter plot in “Exploratory” module

## inti 0.4.0

CRAN release: 2021-05-25

> Changes incompatible with the old versions.

### Major changes

- *Rticles*
  - Deprecated: `create_rticles()` & `rticles()`
  - Deprecated shiny app: `rticles`
  - Now you can create your docs using Rmarkdown templates
    - Rticles
    - Books
  - Vignette for explain the dependencies to use rticles
- *Yupana*
  - Styled messages
  - New module: `Exploratory`
  - Update in the modules
    - No need `fbsm`
    - Reactivity in the analysis
  - Export model information
  - Overwrite graph info
  - Design with 3 factor use
    [`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
  - Allow import/export information for plots
  - Reduce font size in the significance
- *Tarpuy*
  - Styled messages
  - Vignette for explain the modules in the app
  - Overwrite fieldbook info
- *Package*
  - New function:
    [`plot_raw()`](https://inkaverse.com/reference/plot_raw.md)
    - Box plot graph
  - Update function: `fieldbook_mvr` –\>
    [`yupana_mvr()`](https://inkaverse.com/reference/yupana_mvr.md)
    - Can be used independently
  - symbols to internal data
    - Table to create footnotes
  - rename functions
  - Include new logo
  - Vignettes: comparison between
    [`H2cal()`](https://inkaverse.com/reference/H2cal.md) and `asreml`
  - Add data base for MET
  - Logo for the package and apps

> Agradecimiento a Khaterine por la idea en el diseño de los logos

## inti 0.3.0

CRAN release: 2021-04-24

- *Yupana*
  - Fix `{arguments}` from `xlimits` to `ylimits`
  - Update tables style
- *Rticles*
  - Update template files
- *Package*
  - Vignette for describe the arguments and options in Yupana
  - Delete redundant functions `info_figure()` & `info_grahics()`
  - Update functions: `include_figure()` & `include_figure()`
  - Update function
    [`plot_smr()`](https://inkaverse.com/reference/plot_smr.md) include:
    - `xtext`: labels for the x level
    - `gtext`: labels for the group levels

## inti 0.2.0

CRAN release: 2021-04-14

> Changes incompatible with the old versions.

### Major changes

- *Package*
  - Arguments changed in the syntax for `fbsm` and `graphics`.
- *Yupana*
  - Delete error messages in console when run the app
  - Change dependency: `ggpubr` –\> `cowplot`
  - Multivariate analysis need factor levels n\>2
  - Allows copy the `Statistics` table
- *Yupana*
  - Delete error messages in console when run the app
  - fix dates in experiments
- *Rticles*
  - update code for unzip `Articul8` files
- *Package*
  - ´mean_comparison´ update
    - No remove the treatments column
    - Allows plot 3 factors comparison
      [`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
    - New arguments for plot: xlimits, xrotation, dimension, opt
    - Delete redundant arguments; limits, brakes
    - Suggest use “\*” instead of “:”
    - Include additional layers to the plot.
      e.g. [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
    - Save plot dimensions in the exported sheet
  - `web_table` fix resize table in web

### Bug fixes

- *Yupana & Tarpuy*
  - add `pkgs.R` file to load dependencies in apps
- *Tarpuy*
  - fix auto-install packages in `inti::tarpuy(T)`

## inti 0.1.3

CRAN release: 2021-03-20

### Major changes

- *Rticles*
  - update bootstrap
  - include code section
- *Yupana & Tarpuy*
  - google auth verification
- *Tarpuy*
  - Include QR code in fieldbook
- *Apps*
  - bslib dependence install from CRAN
  - Include video for local installation
  - Suppress messages when load apps

## inti 0.1.2

CRAN release: 2020-11-25

### Major changes

- *Package*
  - Exclude package `multtest` from the depends
  - CRAN error: `include_table`
  - Search engine in the web page

## inti 0.1.1

CRAN release: 2020-11-17

### Major changes

- *Yupana & Tarpuy*
  - now apps work locally
  - update bootstrap
  - update packages dependencies for apps
- *Yupana*
  - Graphs: button for generate and refresh graphs
  - Fieldbook: `plot_label` in fieldbook summary for label axis in plots
  - Analysis: export analysis with sheet name
  - Analysis: round digits in export table
- *Package*
  - new functions: `info_figure()` & `info_table()`
  - update pkgdown documentation

### Bug fixes

- *Yupana & Tarpuy*
  - fix problem with ‘cloud.json’
- *Yupana*
  - Multivariate: exclude variables without variation in PCA
  - Multivariate: exclude columns with all NA values
  - Graphs: app not stop if graph arguments are wrong
  - update from observeEvent() –\> reactive()
- *rticles*
  - update app with the new bookdown release

## inti 0.1.0

CRAN release: 2020-10-22

- First package release
