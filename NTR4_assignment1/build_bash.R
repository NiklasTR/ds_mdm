library(here)
library(dplyr)
library(stringr)
library(tidyverse)

line <- read_lines(here("NTR4_assignment1/bash/template"))

disease <- c("RA", "T2D", "T1D", "CAD", "CD", "BD")
chr <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
         "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
         "21", "22", "X")


line[length(line)] %>% 
  str_replace(pattern = "T2D", replacement = disease) %>% 
  lapply(., str_replace, pattern = "22", replacement = chr) %>% 
  unlist() %>%
  lapply(., function(x) c(line[1:length(line)-1], x)) %>% 
  lapply(., function(x) write_lines(path = paste0(here("NTR4_assignment1/bash/"), "call", x[length(line)] %>% str_split(pattern = "'") %>% unlist() %>% .[c(6, 8)] %>% paste(collapse = "_"), ".run"), x))

