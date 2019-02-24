# Loading packages 
library(dplyr)
library(magrittr)
library(readr)
library(here)

# Sourcing scripts - in a perfect world I would make this a package
source(here("NTR4_assignment1/R/import_data.R"))
source(here("NTR4_assignment1/R/parse_args.R"))

# Importing arguments from command line call
# args = commandArgs(trailingOnly=TRUE) %>% 
#   parse_args()

args <- list(path = "/home/ntr4/ds_mdm/NTR4_assignment1/data/WTCCC/",
             ctrl = c("58C", "NBS"),
             disease = "T2D",
             chr_n = "22")

import_data(args)
  