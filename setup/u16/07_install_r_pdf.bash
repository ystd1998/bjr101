#!/bin/bash
# http://rprogramming.net/create-html-or-pdf-files-with-r-knitr-miktex-and-pandoc/

# Step 1: Install MiKTeX
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys D6BC243565B2087BC3F897C9277A7293F59E4889
echo "deb http://miktex.org/download/ubuntu xenial universe" | sudo tee /etc/apt/sources.list.d/miktex.list
sudo apt-get update

sudo apt-get install -y miktex pandoc

sudo apt-get install texlive-latex-base -y

# install gsl
sudo apt-get install -y libgsl0-dev gsl-bin
