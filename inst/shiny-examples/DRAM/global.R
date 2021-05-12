#load all DRAM functions
library('DRAM')
#load input parameters
dp <- read.csv('parameter_list.csv', stringsAsFactors = FALSE)
#dp <- read.csv('inst/shiny-examples/DRAM/parameter_list.csv', stringsAsFactors = FALSE)
row.names(dp) <- dp$parameter
