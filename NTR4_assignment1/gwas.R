# Loading packages 
library(dplyr)
library(magrittr)
library(readr)
library(here)
library(tidyr)

# Sourcing scripts - in a perfect world I would make this a package
list.files(path = here("NTR4_assignment1/R"), full.names = TRUE) %>% lapply(., source)

# Importing arguments from command line call
# args = commandArgs(trailingOnly=TRUE) %>% 
#   parse_args()

args <- list(path = "/home/ntr4/ds_mdm/NTR4_assignment1/data/WTCCC/",
             ctrl = c("58C", "NBS"),
             disease = "T2D",
             chr_n = "22")

data <- import_data(args)

data_count <- data %>% 
  #sample_frac(0.01) %>%
  nest(-wtccc_id) %>% 
  mutate(genotype = purrr::map(data, ~ get_genotype),
         allele = purrr::map(genotype, ~ get_allele),
         hw = purrr::map2(allele, genotype, ~ test_hardy_weinberg),
         cd = purrr::map(test_ctrl_vs_disease, ~ allele),
         format = purrr::map(add_description, ~ allele)
         )



  



  

#NP id (rsid), chromosome number, minor allele, major allele, minor allele frequency in disease, minor allele frequency in controls, odds ratio (major vs. minor allele), p-value, Hardy-Weinberg deviation p-value
