# Loading packages 
library(dplyr)
library(magrittr)
library(readr)
library(here)
library(tidyr)
library(naniar)
library(broom)

# Sourcing scripts - in a perfect world I would make this a package
list.files(path = here("NTR4_assignment1/R"), full.names = TRUE) %>% lapply(., source)

# Importing arguments from command line call
args = commandArgs(trailingOnly=TRUE) %>%
  parse_args()

# Rscript gwas.R "/home/ntr4/ds_mdm/NTR4_assignment1/data/WTCCC/" "c('58C', 'NBS')" "T2D" "22"
# args <- list(path = "/home/ntr4/ds_mdm/NTR4_assignment1/data/WTCCC/",
#              ctrl = c("58C", "NBS"),
#              disease = "T2D",
#              chr_n = "22")

print(str(args))

data <- import_data(args)

# annotation <- read_delim(paste0(args$path, args$disease, "/snps_info.tar.gz"),
#                          "\t", escape_double = FALSE, col_names = FALSE,
#                          trim_ws = TRUE)[,c(3:5)] %>%
#   magrittr::set_colnames(c("egav_id", "wtccc_id", "rs_id"))

path = paste0(paste0(args$path, args$disease, "_", args$chr_n, ".csv"))

tmp <- data %>% 
  nest(-wtccc_id) %>% 
  #sample_frac(0.001) %>%
  mutate(genotype = purrr::map(data, get_genotype),
         allele = purrr::map(genotype, get_allele),
         hw = purrr::map2(allele, genotype, test_hardy_weinberg),
         cd = purrr::map(allele, test_ctrl_vs_disease),
         format = purrr::map(allele, add_description)
         ) %>% 
  unnest(hw, cd, format) %>% 
  mutate(chr_n = args$chr_n) %>%
  dplyr::select(-genotype, -allele, -data) %>%
  # left_join(annotation) %>% 
  write.csv(path, row.names=FALSE)

print(paste0("wrote ", path))