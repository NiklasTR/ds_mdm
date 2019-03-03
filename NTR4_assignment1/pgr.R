# Loading packages 
library(dplyr)
library(magrittr)
library(readr)
library(here)
library(tidyr)
library(naniar)
library(broom)
#library(parallel)

# Sourcing scripts - in a perfect world I would make this a package
list.files(path = here("NTR4_assignment1/R"), full.names = TRUE) %>% lapply(., source)

# Importing arguments from command line call
args = commandArgs(trailingOnly=TRUE) %>%
  parse_args()

# Rscript gwas.R "/home/ntr4/ds_mdm/NTR4_assignment1/data/WTCCC/" "c('58C', 'NBS')" "T2D" "22"
# args <- list(path = "/home/ntr4/ds_mdm/NTR4_assignment1/data/WTCCC/",
#              ctrl = c("58C", "NBS"),
#              disease = "T2D",
#              chr_n = "X")

print(str(args))

data <- import_data(args)

t2d_fc_r <- read_csv(here("NTR4_assignment1/data/T2D_pgr.csv"))

path = paste0(paste0(args$path, args$disease, "_", args$chr_n, "_pgr.csv"))

tmp <- data %>%
  mutate(chr_n = as.character(chr_n)) %>% 
  filter(wtccc_id %in% t2d_fc_r$wtccc_id) %>%
  separate(genotype, c("a1", "a2"), sep = " ", remove = FALSE) %>%
  write.csv(path, row.names=FALSE)

print(paste0("wrote ", path))