# Loading packages 
library(dplyr)
library(magrittr)
library(readr)
library(here)
library(tidyr)

# Sourcing scripts - in a perfect world I would make this a package
source(here("ds_mdm/NTR4_assignment1/R/import_data.R"))
source(here("ds_mdm/NTR4_assignment1/R/parse_args.R"))

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
  mutate(genotype_freq = purrr::map(data, ~ .x %>% group_by(group, genotype) %>% 
                                 count() %>% ungroup() %>% 
                              drop_na),
         allele_freq = ,
         wildtype = purrr::map(count, ~.x %>%
                                 group_by(genotype) %>% 
                                 mutate(n_sum = sum(n)) %>%
                                 arrange(desc(n_sum)) %>% 
                                 dplyr::select(wildtype = genotype) %>% 
                                 head(1)),
         samples
  )

hardy_weinberg <- function(df){
  df %>% filter(group == "ctrl") %>% 
    
}

data_count %>% unnest(genotype_freq) %>% separate(genotype, c("a1", "a2"), sep = " ", remove = FALSE) %>% mutate(status = if_else(a1 == a2, "homo", "hetero"))

  