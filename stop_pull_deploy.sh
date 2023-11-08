#!/bin/sh

set -e

if [[ "$PWD" != "/home/ubuntu" ]]; then
  echo "Need to run script from home directory, not project folder!"
  exit
fi

rm master.zip
rm -r NIST-Decision-Tree-master

wget https://github.com/usnistgov/NIST-Decision-Tree/archive/refs/heads/master.zip
unzip master.zip

sudo docker build -t ndt ./NIST-Decision-Tree-master
sudo docker rm -f $(sudo docker ps -a -q)
sudo docker image prune -f
sudo docker run -d -p 8080:3838 ndt