# Versión 0.3.1

- upcoming updates
  - Exploratory module []
    - Boxplot
    - Distributions
    - emmeans [?]
  - Comparison using vectors []
  - PCA analysis
    - Dimension contributions []
    - Results to download []
    - Styled message for apps []

# Version 0.3.0

- *Yupana*
  - Fix `{arguments}` from `xlimits` to `ylimits`
  - Update tables style
  
- *Rticles*
  - Update template files
  
- *Package* 
  - Vignette for describe the arguments and options in Yupana
  - Delete redundant functions `info_figure()` & `info_grahics()`
  - Update functions: `include_figure()` & `include_figure()`
  - Update function `plot_smr()` include:
    - `xtext`: labels for the x level
    - `gtext`: labels for the group levels

# Version 0.2.0

Changes incompatible with the old versions.
Arguments changed in the syntax for `fbsm` and `graphics`.

## Major changes

- *Yupana*
  - Delete error messages in console when run the app
  - Change dependency: `ggpubr` --> `cowplot`
  - Multivariate analysis need factor levels n>2 
  - Allows copy the `Statistics` table

- *Yupana*
  - Delete error messages in console when run the app
  - fix dates in experiments

- *Rticles*
  - update code for unzip `Articul8` files
  
- *Package* 
  - ´mean_comparison´ update
    - No remove the treatments column
    - Allows plot 3 factors comparison `facet_grid()`
    - New arguments for plot: xlimits, xrotation, dimension, opt
    - Delete redundant arguments; limits, brakes
    - Suggest use "*" instead of ":"
    - Include additional layers to the plot. e.g. `coord_flip()`
    - Save plot dimensions in the exported sheet
  - `web_table` fix resize table in web
  
## Bug fixes

- *Yupana & Tarpuy*
  - add `pkgs.R` file to load dependencies in apps

- *Tarpuy*
  - fix auto-install packages in `inti::tarpuy(T)`

# Version 0.1.3

## Major changes

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

# Version 0.1.2

## Major changes

- *Package* 
  - Exclude package `multtest` from the depends
  - CRAN error: `include_table`
  - Search engine in the web page

# Version 0.1.1

## Major changes

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

## Bug fixes

- *Yupana & Tarpuy*
  - fix problem with 'cloud.json'
  
- *Yupana* 
  - Multivariate: exclude variables without variation in PCA 
  - Multivariate: exclude columns with all NA values
  - Graphs: app not stop if graph arguments are wrong
  - update from observeEvent() --> reactive()
  
- *rticles*
  - update app with the new bookdown release

# Version 0.1.0

- First package release

