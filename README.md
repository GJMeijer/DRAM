# DRAM

Dundee Root Analytical Model (DRAM) - A three-dimensional analytical model for the mobilisation of root reinforcement in direct shear conditions

April 2021 - Gerrit Meijer (<gjm36@bath.ac.uk>)

This R package contains:

- an interactive app (using R Shiny), interactively showing how root reinforcement are affected by root properties, soil propeties and root orientations in three dimensions. 
The reinforcements can the compared to the predictions by existing reinforcement models.
- all functions required to make calculations in the above

The app can also be run online through Shinyapps.io: <https://gjmeijer.shinyapps.io/DRAM/>


## Installation

1. Open R. (If not installed, I recommend installing RStudio (free software) that can be downloaded from https://www.rstudio.com/products/rstudio/download/)
2. If not already installed, install the `devtools` package by typing `install.packages("devtools")` in the R console. This package allows you to interact with R packages hosted on GitHub (among many other things)
3. Install the `DRAM` package by typing `devtools::install_github("GJMeijer/DRAM")` in the R console


## Running the app

To run the included app, type `DRAM::run_app()` in the R console. This will open an interactive App in your default browser. This app relies on R continuing to run in the background for any computations.

This app shows an interactive version of the DRAM. Full documentation can be found within the app itself. The app is constructed using the R package `shiny`.


## Using calculations functions

A wrapper function (`dram_runanalysis_wrapper()`) is provided that takes all input parameters required for a DRAM calculation and outputs various dataframes with results. You can use this function in R to do all DRAM calculations that are performed within R using the R console rather than the app. For more information, type 
`?dram_runanalysis_wrapper` into your R console window.

When using this function, make sure that you use a consistent set of units (e.g. when using lenghts in meters and forces in kilonewstons, the correct unit for stress and stiffness is kPa, or when using lenghts in mm and forces in newtons, the correct unit for stress and stiffness is MPa, etc.). All angles should be specified in radians rather than degrees. All output is returned in the same unit system.


## Detailed function documentation

For each of the functions in the package, a help page can be opened by typing `?function_name` in the R console, where `function_name` is the name of the function you want to see the documentation of.


