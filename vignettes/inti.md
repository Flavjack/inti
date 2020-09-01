---
title: "inti package"
author: "Flavio Lozano-Isla"
date: "2020-09-01"
output: 
      rmarkdown::html_vignette:
        toc: true
        toc_depth: 4
        keep_md: true
vignette: >
  %\VignetteIndexEntry{inti package}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# Inti package

The package `inti` has been developed to provide tools used in plant science and experimental designs. The package also include shiny apps for design experiments, interactive analysis and technical writing.


```r
library(inti)
```

# Install from Github

You can install the last modifications and updates from the source repository.


```r
install.packages("devtools")
devtools::install_github("flavjack/inti")
```

# Shiny apps

The shiny apps are based in the function developed in the package. 

**Yupana:** data analysis and graphics for experimental designs.

**Tarpuy:** ease way to deploy fieldbook experimental plans.

**Rticles:** Template for technical documents using Rmarkdown.

## Using local 

The apps are included into the `Addins` list or you can run the following code.

### Yupana


```r
inti::yupana()
```

### Tarpuy


```r
inti::tarpuy()
```

### Rticles


```r
inti::rticles_addin()
```
