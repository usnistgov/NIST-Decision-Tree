# COMET: Counting Method Evaluation Tool

COMET (COunting Method Evaluation Tool) is a Shiny App that accepts data in a specific template based on a specified experimental design and conducts statistical analysis, and outputs quality indicators for the cell counting methods as well as additional meta-data important for reporting of results.

COMET is designed to accept data from experimental designs that follow the principles outlined in the ISO standard, "ISO 20391-2:2019 Biotechnology — Cell counting — Part 2: Experimental design and statistical analysis to quantify counting method performance"  The outputs of this app are intended follow the reporting requirements and recommendations of the standard.

More information on the experimental design and statistical analysis approach for quantifying cell counting method performance can be found at: https://www.nist.gov/programs-projects/evaluating-quality-cell-counting-methods-experimental-design-and-statistical.


## Using COMET 

The easiest way to run and use COMET is by first installing Docker (https://docs.docker.com/get-docker/).

Once Docker is installed, download the COMET repository (at github.com/usnistgov/COMET, click 'Code' and then 'Download ZIP'), and navigate your terminal to the main directory of the project (the same level as Dockerfile). Then, run the following command to build the image:
```
docker build -t comet .
```
To run the container, run the following command:
```
docker run -d -p 3838:3838 --name my_container comet
```
(-d for 'detached', -p specifies the port mapping, '--name' gives a name to the running container, and 'comet' tells docker which image to build the container from.) Then the app should be visible at localhost:3838 (accessed via your web browser).

To stop and remove the running container, run the following:
```
docker rm -f my_container
```

Alternatively, you can run, stop, and remove the container using the Docker Desktop application.
