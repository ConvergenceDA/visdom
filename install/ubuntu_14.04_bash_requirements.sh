#!/bin/bash

# Download and install Microsoft R Open (MRO, formerly RRO):
# See https://mran.microsoft.com/documents/rro/installation/#revorinst-lin

# remove default version of R (if applicable)
sudo apt-get remove r-base-core

# get MRO
cd /usr/local/src
sudo wget https://mran.microsoft.com/install/mro/3.3.1/microsoft-r-open-3.3.1.tar.gz

# extract and install
sudo tar zxvf microsoft-r-open-3.3.1.tar.gz
cd microsoft-r-open/
sudo ./install.sh
#Choose to install MKL libraries and accept licenses
#For unattended install, ./install.sh -a -s

# back to /usr/local/src
cd /usr/local/src

#Install rstudio server for running browser-based R sessions:
sudo apt-get install gdebi-core
sudo wget https://download2.rstudio.org/rstudio-server-0.99.903-amd64.deb
sudo gdebi rstudio-server-0.99.903-amd64.deb

# knitr requires pandoc from more recent than the Ubuntu 14.04 packages
# if necessary sudo apt-get remove pandoc pandoc-citeproc
sudo wget https://github.com/jgm/pandoc/releases/download/1.17.2/pandoc-1.17.2-1-amd64.deb
sudo gdebi pandoc-1.17.2-1-amd64.deb

# install lib dependencies for R modules that will be used
# by devtools, vignettes, or visodm 
sudo apt-get install zlib1g-dev libssl-dev libcurl4-openssl-dev
sudo apt-get install gfortran
sudo apt-get install libmariadbclient-dev 


# install devtools if you don't have it.
# monitor this for errors related to missing dependencies
sudo R -e 'install.packages(c("devtools"))'

# install visdom from GitHub.
# the unzip='internal' option is a fix for Ubuntu's devtools support
# see discussion at https://github.com/RevolutionAnalytics/RRO/issues/37
sudo R -e 'options(unzip = "internal"); devtools::install_github("convergenceda/visdom", build_vignettes=F )'

# check if it works!
R -e 'library(visdom);?visdom'

# now get the vignettes up and running
sudo R -e 'install.packages(c("knitr","rmarkdown","DBI"))'

sudo R -e 'options(unzip = "internal"); devtools::install_github("convergenceda/visdom", build_vignettes=T, force=T)'

