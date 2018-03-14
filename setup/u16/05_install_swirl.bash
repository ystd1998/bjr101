#!/bin/bash

sudo sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu precise/" >> /etc/apt/sources.list.d/cran.list'
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo apt-get update
sudo apt-get install r-base-dev

# required by swirl
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libssl-dev

R
#> install.packages("swirl")
#> library(swirl)
#> swirl()
