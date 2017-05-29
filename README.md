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


paquetes <- c("broom","readxl","leaps", "scatterplot3d", "cellranger", "knitr","klaR","flashClust","shinyBS",
              "spdep", "AlgDesign", "DBI", , "assertthat","tibble", "gtable", "scales", "ggrepel", "cellranger", 
              "htmlwidgets", "httpuv", "xtable", "rhandsontable", "ggpubr", "htmltools", "ggplot2", "tidyr", "magrittr","agricolae")


sudo su - -c "R -e \"install.packages('agricolae', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('broom', repos='http://cran.rstudio.com/')\""


sudo su - -c "R -e \"install.packages('readxl', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('leaps', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('scatterplot3d', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('cellranger', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('knitr', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('klaR', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('flashClust', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('spdep', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('AlgDesign', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('DBI', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('assertthat', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('tibble', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('gtable', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('scales', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('ggrepel', repos='http://cran.rstudio.com/')\""


sudo su - -c "R -e \"install.packages('htmlwidgets', repos='http://cran.rstudio.com/')\""


sudo su - -c "R -e \"install.packages('httpuv', repos='http://cran.rstudio.com/')\""


sudo su - -c "R -e \"install.packages('xtable', repos='http://cran.rstudio.com/')\""


sudo su - -c "R -e \"install.packages('rhandsontable', repos='http://cran.rstudio.com/')\""


sudo su - -c "R -e \"install.packages('ggpubr', repos='http://cran.rstudio.com/')\""


sudo su - -c "R -e \"install.packages('htmltools', repos='http://cran.rstudio.com/')\""



sudo su - -c "R -e \"install.packages('ggplot2', repos='http://cran.rstudio.com/')\""



sudo su - -c "R -e \"install.packages('tidyr', repos='http://cran.rstudio.com/')\""



sudo su - -c "R -e \"install.packages('magrittr', repos='http://cran.rstudio.com/')\""


apt-get update     # refresh
apt-get install software-properties-common
add-apt-repository -y "ppa:marutter/rrutter"
add-apt-repository -y "ppa:marutter/c2d4u"
apt-get update     # now with new repos
apt-get install r-cran-readr
apt-get install r-cran-rcppeigen
apt-get install r-cran-dplyr


sudo su - -c "R -e \"devtools::install_github('flavjack/fieldbook')\""




#credenciales
#user:shiny
#clave: quipo2017


> .libPaths()
[1] "/usr/local/lib/R/site-library" "/usr/lib/R/site-library"
[3] "/usr/lib/R/library"


cp -R /usr/local/lib/R/site-library/fieldbook/fieldbook/ /srv/shiny-server/sample-apps

  
  
###sudo


# Instruct Shiny Server to run applications as the user "shiny"
run_as shiny;

# Define a server that listens on port 3838
server {
  listen 3838;
  
  # Define a location at the base URL
  location / {
    
    # Host the directory of Shiny Apps stored in this directory
    site_dir /srv/shiny-server;
    
    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;
    
    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
  }
}










# Proverbios

(14) “Envió desde lo alto, me tomó, me sacó de las muchas aguas. Me libró de mi poderoso enemigo, y de los que me aborrecían; pues eran más fuertes que yo… pero me libró porque se agradó de mí” (Salmo 18:16,17,19b).
