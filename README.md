[![Build status](https://ci.appveyor.com/api/projects/status/7x7cr66jq4pb83eu?svg=true)](https://ci.appveyor.com/project/omarbenites/fieldbook)

[![Build Status](https://travis-ci.org/Flavjack/fieldbook.svg?branch=master)](https://travis-ci.org/Flavjack/fieldbook)


# fieldbook
Data anlysis application for statistical industry: fertilizer, crop production, etc.

# Instalacion

## In shiny-server

Life is not easier, and configure shiny apps in servers is cumbersome.
During fieldbook installation you should be aware of RcppEigen (and other c++ packages) tha compile C++ code.

Solution: Run the code below in the Ubuntu server
Note: If the package's name have uppercase letters, write them in lowercase RcppEigen
--> rcppeigen

rm -rf 


```{r eval=F}

apt-get update     # refresh
apt-get install software-properties-common
add-apt-repository -y "ppa:marutter/rrutter"
add-apt-repository -y "ppa:marutter/c2d4u"
apt-get update     # now with new repos
apt-get install r-cran-readr
apt-get install r-cran-rcppeigen
apt-get install r-cran-dplyr



```

## Errors 

ERROR: An error has occurred. Check your logs or contact the app author for clarification.

# Solution:

Apparently sanitize error messages is true by default for my configuration. Add
options(shiny.sanitize.errors = FALSE) to your app.

In the configuration file, place

sanitize_errors false;
preserve_logs true;

within server to resolve permanently.





# Proverbios

(14) “Envió desde lo alto, me tomó, me sacó de las muchas aguas. Me libró de mi poderoso enemigo, y de los que me aborrecían; pues eran más fuertes que yo… pero me libró porque se agradó de mí” (Salmo 18:16,17,19b).
