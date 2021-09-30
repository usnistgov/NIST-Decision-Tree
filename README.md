# NIST Decision Tree (NDT) for Key Comparisons

This contribution describes a Decision Tree intended to guide the selection of statistical models and data reduction procedures in key comparisons (KCs). The Decision Tree addresses a specific need of the Inorganic Analysis Working Group (IAWG) of the Consultative Committee (CC) for Amount of Substance, Metrology in Chemistry and Biology (CCQM), of the International Committee for Weights and Measures (CIPM), and it is likely to address similar needs of other working groups and consultative committees. Because the portfolio of KCs previously organized by the CCQM-IAWG affords a full range of opportunities to demonstrate the capabilities of the Decision Tree, the majority of the illustrative examples of application of the Decision Tree are from this working group. However, the Decision Tree is widely applicable in other areas of metrology, as illustrated in examples of application to measurements of radionuclides and of the efficiency of a thermistor power sensor. The Decision Tree is intended for use after choices will have been made about the measurement results that qualify for inclusion in the calculation of the key comparison reference value (KCRV), and about the measurement results for which degrees of equivalence should be produced. Both these choices should be based on substantive considerations, not on purely statistical criteria. However, the Decision Tree does not require that the measurement results selected for either purpose be mutually consistent. The Decision Tree should be used as a guide, not as the sole and autonomous determinant of the model that should be selected for the measurement results obtained in a KC, or of the procedure that should be employed to reduce these results. The scientists running the KCs ultimately have the freedom and responsibility to make the corresponding choices that they deem most appropriate and that best fit the purpose of each KC. The Decision Tree involves three statistical tests, and comprises five terminal leaves, which correspond to as many alternative ways in which the KCRV, its associated uncertainty, and the degrees of equivalence (DoEs) may be computed. This contribution does not purport to suggest that any of the KCRVs, associated uncertainties, or DoEs, presented in previously approved final reports issued by working groups of the CCs should be modified. Neither do the alternative results question existing, demonstrated calibration and measurement capabilities (CMCs), nor do they support any new CMCs.

For more information, see: https://www.nist.gov/publications/decision-tree-key-comparisons

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
