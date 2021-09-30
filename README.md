# NIST Decision Tree (NDT) for Key Comparisons

This software describes a Decision Tree intended to guide the selection of statistical models and data reduction procedures in key comparisons (KCs). For more information, see: https://www.nist.gov/publications/decision-tree-key-comparisons

Reference:  
Possolo A, Koepke A, Newton D, Winchester MR (2021) *Decision Tree for Key Comparisons.*  J Res Natl Inst Stan 126:126007. https://doi.org/10.6028/jres.126.007.

## Using NDT

The easiest way to run and use NDT is by first installing Docker (https://docs.docker.com/get-docker/).

Once Docker is installed, download the NDT repository (at github.com/usnistgov/NIST-Decision-Tree, click 'Code' and then 'Download ZIP'), and navigate your terminal to the main directory of the project (the same level as Dockerfile). Then, run the following command to build the image:
```
docker build -t ndt .
```
To run the container, run the following command:
```
docker run -d -p 3838:3838 --name my_container ndt
```
(-d for 'detached', -p specifies the port mapping, '--name' gives a name to the running container, and 'ndt' tells docker which image to build the container from.) Then the app should be visible at localhost:3838 (accessed via your web browser).

To stop and remove the running container, run the following:
```
docker rm -f my_container
```

Alternatively, you can run, stop, and remove the container using the Docker Desktop application.
