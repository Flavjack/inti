---
title: "inti package"
author: "Flavio Lozano-Isla"
date: "2020-08-30"
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

<!--html_preserve-->

<div id=footer style="width:100%; margin:auto;">

<div style="display:inline-block; width:32%">
<p style="text-align:center">
<a target="_blank" href="https://flavjack.shinyapps.io/tarpuy/"><img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/icons/favicon_tarpuy/android-chrome-512x512.png" style="height:80px" title="tarpuy"></a>
<span style="display:block;"><small>Tarpuy</small></span>
</p></div>

<div style="display:inline-block; width:32%">
<p style="text-align:center">
<a target="_blank" href="https://flavjack.shinyapps.io/rticles/"><img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/icons/favicon_rticles/android-chrome-512x512.png" style="height:80px" title="rticles"></a>
<span style="display:block;"><small>Rticles</small></span>
</p></div>


<div style="display:inline-block; width:32%">
<p style="text-align:center">
<a target="_blank" href="https://flavjack.shinyapps.io/yupanapro/"><img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/icons/favicon_yupana/android-chrome-512x512.png" style="height:80px" title="yupana"></a>
<span style="display:block;"><small>Yupana</small></span>
</p></div>

</div>

<!-- *** -->

<div id=footer style="width:100%; margin:auto;">

<div style="display:inline-block; width:100%">
<p style="text-align:center">
<a target="_blank" href="https://github.com/Flavjack/inti"><img src="https://image.flaticon.com/icons/svg/25/25231.svg" style="height:75px" title="Github" alt="Github"></a>
<span style="display:block;"><small>Github</small></span>
</p></div>

</div>

<!--/html_preserve-->

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
