# Organization of SiRJ model
The code for SiRJ is divided into 5 R script files:
1. **environInits.R** -- contains parameters and performs operations related to constructing the situational ecology of test takers
2. **personInits.R** -- contains parameters and performs operations related to constructing an individual test taker
3. **SJTInits.R** -- contains parameters and performs operations related to constructing the items included on an SJT
4. **modelFuncs.R** -- contains functions used throughout the other script files related to creating memory structures, searching memory structures, carrying out HyGene, and performing the conditional reasoning steps in SiRJ
5. **answerSJT.R** -- contains single overall function that creates and simulates a person to SJT items

# Running simulations
The script file, **FINAL_SJTsim.R**, provides a template and code for conducting simulations with SiRJ and is the file used to manipulate parameters, create experimental conditions, and generate data for simulated test takers.

The steps below provide an overview of how simulations are run using base SiRJ:
1. Download all 6 files in this repository to the same folder on your computer.
2. The current simulation setup uses four R packages to facilitate processing speed and data organization. If not already installed on your computer, these should be installed by running the following commands in the R console window:
```
install.packages("parallel")
install.packages("snowfall")
install.packages("rlecuyer")
install.packages("data.table")
```
3. The simulation is coded to utilize parallel processing. There are two options for running the model in parallel. These options are controlled on lines 10-15 of the code in the FINAL-SJTsim.R file. Note that only **one** of these options should be selected.
   - **Option #1:** Use this option to run the model in parallel on your local computer using multiple cores of your computer.  *Note that Option 1 is selected by default*
   - **Option #2:** Use this option to run the model in parallel that will be distributed across cores on multiple computers.
4. Line 18 creates a data frame called `conds` that specifies the parameters and values to be manipulated for a simulation. The provided code is set up to manipulate two person-level parameters -- expertise level (`exptLvl`) and experience strength (`expStrength`). Note that manipulating different parameters will require the user to ensure that values are not given to these parameters in the associated initialization file.
5. Line 25 determines the number of simulated test takers to run **_per condition_**. Under the provided default setup, the simulation will run 10,000 participants (`nPersons = 500` x 20 conditions). Note that running this many individuals may take a long time depending on the speed of your computer, number of cores avaialable, available system RAM, etc.
6. To run the simulation under the default settings, select and execute lines 9-37 in the FINAL_SJTSim.R script flie. Assuming the user selected Option 1 in Step #3 above, a new object called `condsDat` will be created once the simulation is complete that contains all the raw data from the simulation. Lines 42-61 formats the raw data into a data frame more useful for analyses. Lines 63-64 save the entire R enviroment and the final data file (respectively) to the current working directory.
