# Model of Situated Reasoning and Judgment (SiRJ) [![DOI](https://zenodo.org/badge/154691039.svg)](https://zenodo.org/badge/latestdoi/154691039)

This repository contains all code necessary to run the SiRJ computational model and replicate the simulations/results reported in the paper: 
> Grand, J.A. (2020). A general response process theory for situational judgment tests. *Journal of Applied Psychology, 105,* 819-862.

All code is provided under the GNU GPL and may be freely manipulated and distributed. Citing the original paper if any portions of the model and/or simulation is borrowed, adapted, or used for other purposes is appreciated. There is no expectation for the author to provide techical guidance or support with troubleshooting, running, or adapting any portions of the provided code.

# File structure
The material in this repository is organized into three sections:
1. The **base** folder contains all code for running and conducting simulations with the base SiRJ model. Base SiRJ corresponds directly to the original model as described and presented in Figure 1 of the paper. *Users wishing to adapt and extend the base functionality of SiRJ should use the code contained in this folder.*
2. The **paperSims** folder contains all code for replicating the virtual experiments and analyses reported in the paper.
3. The **shinyApp** folder contains all code used to construct the [interactive web application](https://grandjam.shinyapps.io/sirj) that accompanies the original paper. This code is provided for users wishing to understand and/or adapt the structure of this app for their own purposes.

Each folder contains its own README file explaining its content and instructions for using the code. All code for the SiRJ model and simulations was constructed in the open-source [R programming environment](https://cran.r-project.org/) and requires the user to have downloaded and installed the latest version of the software to use.
