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


#Setting server

sudo apt-get update
# Add a trusted key
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
# Add the CRAN repo
sudo add-apt-repository 'deb [arch=amd64,i386] https://cran.rstudio.com/bin/linux/ubuntu xenial/'
# Update the list again
sudo apt-get update



Now, we’re ready to install R and other dependencies for shiny server:
# Install R
sudo apt-get -y install r-base r-base-dev
# Install shiny server dependencies
sudo apt-get -y install libapparmor1 gdebi-core
# These are used by R libraries
sudo apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev


# Download rstudio server:
wget https://download2.rstudio.org/rstudio-server-1.0.136-amd64.deb
# Download shiny server:
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
# Install
sudo apt install gdebi-core
sudo gdebi rstudio-server-1.0.136-amd64.deb
sudo gdebi shiny-server-1.5.1.834-amd64.deb




## Installation of R package with c++ code  

sudo apt-get update     # refresh
sudo apt-get install software-properties-common
sudo add-apt-repository -y "ppa:marutter/rrutter"
sudo add-apt-repository -y "ppa:marutter/c2d4u"
sudo apt-get update     # now with new repos
sudo apt-get install r-cran-readr
sudo apt-get install r-cran-rcppeigen
sudo apt-get install r-cran-dplyr


paquetes <- c("broom","readxl","leaps", "scatterplot3d", "cellranger", "knitr","klaR","flashClust","shinyBS",
              "spdep", "AlgDesign", "DBI", , "assertthat","tibble", "gtable", "scales", "ggrepel", "cellranger", 
              "htmlwidgets", "httpuv", "xtable", "rhandsontable", "ggpubr", "htmltools", "ggplot2", "tidyr", "magrittr","agricolae")


sudo su - -c "R -e \"install.packages('agricolae',repos='http://cran.rstudio.com/')\""


sudo su - -c "R -e \"install.packages('broom', repos='http://cran.rstudio.com/')\""


sudo su - -c "R -e \"install.packages('readxl', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('digest',repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('devtools',repos='http://cran.rstudio.com/')\""


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


sudo su - -c "R -e \"install.packages('devtools', repos='http://cran.rstudio.com/')\""

#Sin token
#sudo su - -c "R -e \"devtools::install_github('flavjack/fieldbook')\""

#Con token
sudo su - -c "R -e \"devtools::install_github('Flavjack/fieldbook', auth_token =  'fbb416c167892bfe43c26fe07052bd8ce162951f')\""     



#credenciales
#user:shiny
#clave: quipo2017


> .libPaths()
[1] "/usr/local/lib/R/site-library" "/usr/lib/R/site-library"
[3] "/usr/lib/R/library"


sudo cp -R /usr/local/lib/R/site-library/fieldbook/fieldbook/ /srv/shiny-server/sample-apps

  
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
