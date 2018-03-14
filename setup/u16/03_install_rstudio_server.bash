#!/bin/bash

sudo apt-get install gdebi-core

cd /tmp

wget https://download2.rstudio.org/rstudio-server-1.1.442-amd64.deb
sudo gdebi rstudio-server-1.1.442-amd64.deb


# /etc/rstudio/rserver.conf
# /etc/rstudio/rsession.conf

sudo rstudio-server verify-installation
sudo rstudio-server restart
