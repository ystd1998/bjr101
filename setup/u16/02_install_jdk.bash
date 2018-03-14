#!/bin/bash

# install open-jdk
# sudo apt-get update
# sudo apt-get install default-jdk

# install Oracle JDK
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java8-installer

# Manage Java
sudo update-alternatives --config java

# ADD JAVA_HOME
sudo bash -c 'echo "JAVA_HOME=\"/usr/lib/jvm/java-8-oracle\"" >> /etc/environment'
source /etc/environment
echo $JAVA_HOME
