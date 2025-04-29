# ReadMe

This depository contains the code to run PASOM--An agent-based model simulating science attitudes based on Spiral of Silence Theory. Outputs from the model simulations were used for the paper, “Toxic behaviour facilitates echo chamber formation: An agent-based modelling simulation of science attitudes based on Spiral of Silence Theory” accepted by Plos One. Outputs can be found at https://doi.org/10.25919/hj39-0229.

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

The files to run the simulations reported in the paper are as follows:

- `base.R`: The baseline simulations.
- `bt_stc.R`: The simulations changing the $c_T$ parameter and starting priors.
- `cx_stc.R`: The simulations changing the $c_X$ parameter and starting priors.
- `bt_cx.rds`: The simulations changing the $c_T$ and $c_X$ parameters.

All other files in the `run_sims` folder run simulations reported in the Supplement.

Note that the code may not work in a Windows environment due to the use of `mclapply()`. This function parallellises the process of running multiple simulations and works 'out of the box' on Linux and Mac but may require setting up to work on Windows. Alternatively, the code can be changed to `lapply()` but this will make running the code take substantially longer as simulations will run consecutively.

If running alternative sets of simulations from those included, the file should include:
- Parameter values
- A network
- A data.frame used to set priors in the model function
- A simulation name

If running a model based on the base parameter value set, these can be loaded from `default_params.R` in `scripts` and then selected parameters can be optionally changed. Similarly, the base network (labelled, `g0`) and the default set of values to create priors (labelled, `pers0`) can be loaded from `default_objects.R` in `scripts`. The simulation name should be assigned to an object labelled, `model` (e.g., `model <- "base"` for the base model). Each of these objects is used in the downstream script, `modelsetup.R`, so must be set.

To run the simulations, the `igraph`, `fastmatch`, and `openssl` packages are required. To produce figures and other results from the simulation outputs, the `ggplot2`, `ggnewscale`, `cowplot`, `igraph`, `paletteer`, and `reshape` packages are required. The `usethis` and `gitcreds` packages were used to set up the GitHub repository from RStudio Server but should not be required to use the model.

## Model Outputs

Running models will create the `output` folder and and a folder for the simulation (named as the string assigned to `model`). The code will populate the folder with files for each of the simulation outputs. A file of the parameter values used for each of the simulations will be placed in the `output` folder. The text of functions used will also be created, which is used to check that the functions have not changed between runs. All files are saved with the `.rds` file type (with the exception of plots). The files can be opened in any program that can interpret R objects, such as R or RStudio.

Alternatively, outputs can be found on CSIRO's Data Access Portal (Bainbridge, Tim; Ryan, Matt; Golley, Sinead; Kakoschke, Naomi; & Brindal, Emily (2025): PASOM: Toxic behaviour facilitates echo chamber formation. v1. CSIRO. Data Collection. https://doi.org/10.25919/hj39-0229).

Within each folder numerous files will be saved for each parameter set, representing a single tracked output per file. Almost all files are lists of length equal to the number of simulation run. Some of these contain matrices of the number of agents x the number of rounds; others are only for the final round.

Files with all rounds are:
- `opinion1`: matrices of opinions
- `K1`: matrices of number of connections
- `a11`: matrices of constructive shares supporting the pro-science position
- `a10`: matrices of constructive shares supporting the anti-science position
- `tx11`: matrices of toxic shares opposing the pro-science position
- `tx10`: matrices of toxic shares opposing the anti-science position
- `echo`: matrices of echo chamber membership (-1 = anti-science, 1 = pro-science, 0 = neither)
- `pub11`: vectors of the number of originator agents who shared in support of the pro-science position each round
- `pub10`: vectors of the number of originator agents who shared in support of the anti-science position each round
- `lmda1`: matrices of values of `\lambda_{ij}` (i.e., agents' interest in the topic, see Equation (11) in the supplement).

Files with only first or final round data are:
- `g0`: the original network
- `g`: networks at the end of the simulations
- `agents`: dataframes of final values for each agent

# Copyright

All content within the repository is the Copyright of the Commonwealth Scientific and Industrial Research Organisation (CSIRO) 2024 and is distributed under the terms of the GNU General Public License (GPL) version 3 or later. See the LICENSE file for the standard GPL license terms.

PASOM is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

PASOM is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with PASOM. If not, see <https://www.gnu.org/licenses/>.
