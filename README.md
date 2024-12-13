# ReadMe

This depository contains the code to run PASOM--An agent-based model simulating science attitudes based on Spiral of Silence Theory. Outputs from the model simulations were used for the paper, “Toxic behaviour facilitates echo chamber formation: An agent-based modelling simulation of science attitudes based on Spiral of Silence Theory” submitted to Plos One.

## Director Structure

Of the below directories, only the `scripts` and `run_sims` folders are included on GitHub with other directories created as needed within the code.

The directory structure is:
- `input`		  contains objects used to run the simulations, including 'graph' objects (starting with 'g') and objects used to create agents' priors (pers00.rds and pers20.rds)
- `output`	  contains the outputs from the model runs (files described below)
- `plots`		  contains plots produced from the simulations outputs
- `results`	  contains cleaned results from the simulations used to produce the plots and other results
- `run_sims`	contains scripts to run the simulations
- `scripts`	  contains all other scripts

Within the `scripts` folder:
- `0_fixed.R` creates objects used as inputs in the simulations and must be run prior to running any simulations.
- Files beginning with `2_` generate results for the manuscript
- Files beginning with `3_` create results for the supplement
- All other files are used with 'source()' in other scripts

Within the `run_sims` folder, each of the files runs an associated set of simulations.

## Run Simulations

All code is R code and was run on an RStudio Server. Code must be run with the base directory as the working directory. If set up in a project in RStudio, this can be done by clicking the down arrow next to `Source`, selecting `Source as Background Job...`, and setting the working directory as the base directory.

To run a set of simulations, first run `0_fixed.R` to create input files or create alternative versions via some other method. Then either open and run the desired script from the `run_sims` folder, or create a new file to run an alternative set of simulations with alternative parameter values or inputs.

Note that the code may not work in a Windows environment due to the use of `mclapply()`. This function parallellises the process of running multiple simulations and works 'out of the box' on Linux and Mac but may require setting up to work on Windows. Alternatively, the code can be changed to `lapply()` but this will make running the code take substantially longer as simulations will run consecutively.

If running alternative sets of simulations from those included, the file should include:
- Parameter values (or load them from `default_params.R` in `scripts` and then optionally change selected ones)
- A network (labelled, `g0`, optionally loaded from `default_objects.R`)
- A data.frame used to set priors in the model function (labelled, `pers0`, also optionally loaded from `default_objects.R`)
- A simulation name (labelled, `model`).

These are used in the downstream script, `modelsetup.R`, so must be set.

To run the simulations, the `igraph`, `fastmatch`, and `openssl` packages are required. To produce figures and other results from the simulation outputs, the `ggplot2`, `ggnewscale`, `cowplot`, `igraph`, `paletteer`, and `reshape` packages are required. The `usethis` and `gitcreds` packages were used to set up the GitHub repository from RStudio Server.

## Model Outputs

Running models will create the `output` folder and and a folder for the simulation (named as the string assigned to `model`). The code will populate the folder with files for each of the simulation outputs. A file of the parameter values used for each of the simulations will be placed in the `output` folder. The text of functions used will also be created, which is used to check that the functions have not changed between runs. All files are saved with the `.rds` file type. The files can be opened in any program that can interpret R objects, such as R or RStudio.

% The main simulation output files (indicated by the simulation name followed by `_np`) are structured identically with the following structure. In all cases, matrices are arranged with agents separated by rows (i.e., agent 1 is row 1) and rounds separated by columns (i.e., column 1 is round 1). In the below schematic, `model_output` is the model output file and `iteration_number` is the number of the simulation for the model (from 1 to 200 in the default case).

File structure:
```
- [model_output]
   |- iter[iteration_number]
       |- opinion1      a matrix of opinions
       |- K1            a matrix of number of connections
       |- a11           a matrix of constructive shares supporting the pro-science position
       |- a10           a matrix of constructive shares supporting the anti-science position
       |- tx11          a matrix of toxic shares opposing the pro-science position
       |- tx10          a matrix of toxic shares opposing the anti-science position
       |- echo          a matrix of echo chamber membership (-1 = anti-science, 1 = pro-science, 0 = neither)
       |- pub11         a vector of the number of originator agents who shared in support of the pro-science position each round
       |- pub10         a vector of the number of originator agents who shared in support of the anti-science position each round
       |- lmda1         a matrix of values of \lambda_{ij} (i.e., agents' interest in the topic, see Equation (11) in the supplement).
       |- g0            the original network
       |- g             the network at the end of the simulation
       |- agents        a dataframe of final values by agent
           |- id        the ID value of each agent (from "001" to "500" for 500 simulations with 500 agents)
           |- bs        \beta_{Sij} values after the final round of the simulation (see Equation (16) from the documentation)
           |- as        \alpha_{Sij} values after the final round of the simulation (see Equation (15) from the documentation)
           |- at1       \alpha_{Tij1} values after the final round of the simulation (see Equation (2) from the documentation)
           |- at0       \alpha_{Tij0} values after the final round of the simulation (see Equation (2) from the documentation)
           |- aa1       \alpha_{Aij1} values after the final round of the simulation (see Equation (6) from the documentation)
           |- aa0       \alpha_{Aij0} values after the final round of the simulation (see Equation (6) from the documentation)
           |- Kt1       K_{Tij1} values after the final round of the simulation (see Equation (3) from the documentation)
           |- Kt0       K_{Tij0} values after the final round of the simulation (see Equation (3) from the documentation)
           |- Ka1       K_{Aij1} values after the final round of the simulation (see Equation (7) from the documentation)
           |- Ka0       K_{Aij0} values after the final round of the simulation (see Equation (7) from the documentation)
           |- ps        \pi_{Sij}^* values after the final round of the simulation (see Equation (17) from the documentation)
           |- lmda      \lambda_{ij} values after the final round of the simulation (see "lmda1" above and Equation (11) from the documentation)
       |- final_round   the number of the final round (i.e., 200 in every case here)
```

The files for the simulations reported in the paper are as follows:

- `base_np.rds`: The baseline simulations.
- `cx15_np.rds`: The simulations changing the c_x parameter to 1.5 (from 2 in the baseline model).
- `cx1_np.rds`: The simulations changing the c_x parameter to 1.
- `cx5_np.rds`: The simulations changing the c_x parameter to 0.5.
- `cx0_np.rds`: The simulations changing the c_x parameter to 0.

And the files for the simulations reported in the supplement are as follows:

- `g1_np.rds`: The "C1" set of simulations from the supplement, using a different cluster network at the start of the simulations.
- `g2_np.rds`: The "C2" set of simulations from the supplement, using a different cluster network at the start of the simulations.
- `lattice_np`: The "L" set of simulations from the supplement, using a lattice or grid network at the start of the simulations.
- `g1000_np`: The "A1000" set of simulations from the supplement, using a cluster network with twice as many agents at the start of the simulations.
- `stcon_high_np.rds`: The "HSP" set of simulations from the supplement, changing starting priors to be four times as strong.
- `bt1_np.rds`: The "BT1" set of simulations from the supplement, changing the c_T and c_{TG} parameters from 10 to 1 (see Equations (12) and (23))
- `sd5_np.rds`: The "SD.5" set of simulations from the supplement, halving the standard devation of the e_{jI} and f_{ijI} variables in the model.
- `sdswap.rds`: The "SDSwap" set of simulations from the supplement, swapping the standard devation of the e_{jI} and f_{ijI} variables in the model (to exaggerate individual variance).
- `ck8_np.rds`: The "CK.08" set of simulations from the supplement, changing the value of the c_{KG} parameter from 0.04 to 0.08 (see Equation (23))
- `cb1_np.rds`: The "CB1" set of simulations from the supplement, changing the value of the c_{BG} parameter from 0.2 to 0.1 (see Equation (23))
- `d0_np.rds`: The "D0" set of simulations from the supplement, changing the value of d_T and d_A from 0.9 to 0 (see Equations (2)-(3) and (6)-(7))

# Copyright

All content within the repository is the Copyright of the Commonwealth Scientific and Industrial Research Organisation (CSIRO) 2024 and is distributed under the terms of the GNU General Public License (GPL) version 3 or later. See the LICENSE file for the standard GPL license terms.

PASOM is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

PASOM is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with PASOM. If not, see <https://www.gnu.org/licenses/>.
