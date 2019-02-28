import_data <- function(args){
  # creating paths to data
  path_ctrl = paste(args$path, 
                  args$ctrl,
                  paste0("Affx_gt_", args$ctrl, "_Chiamo_", args$chr_n, ".tped.gz"),
                  sep = "/")
  
  path_disease = paste(args$path, 
                       args$disease,
                       paste0("Affx_gt_", args$disease, "_Chiamo_", args$chr_n, ".tped.gz"),
                       sep = "/")
  
  # reading files
   lapply(path_ctrl[1], function(x) read_delim(x, delim="\t", col_names=F) %>% .[,c(1:4)]) %>% 
     # clunky way of generating SNP descriptors
    append(lapply(path_ctrl, function(x) read_delim(x, delim="\t", col_names=F) %>% .[,-c(1:4)])) %>%
     # only binding the measurment data. I had a big in this code, that was row_binding and not column_binding the information
    bind_cols() %>% 
    magrittr::set_colnames(c("chr_n", "wtccc_id", "start", "end", colnames(.)[-c(1:4)])) %>%
    gather(subject, genotype, -chr_n, -wtccc_id, -start, -end)%>%
    mutate(group = "ctrl") %>%
    rbind(.,
          lapply(path_disease, read_delim, delim="\t", col_names=F) %>% 
            bind_rows() %>% 
            magrittr::set_colnames(c("chr_n", "wtccc_id", "start", "end", colnames(.)[-c(1:4)])) %>%
            gather(subject, genotype, -chr_n, -wtccc_id, -start, -end)%>%
            mutate(group = "disease")) %>% 
    return()
}