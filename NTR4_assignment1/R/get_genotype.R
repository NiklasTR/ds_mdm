get_genotype <- function(df){
  df_sm <- df %>% group_by(group, genotype) %>% 
    count() %>% ungroup() 
  
  # expanding to allele configurations that are rare, padding with 0s
  df_exp <- df_sm %>% 
    expand(group, genotype) %>% left_join(df_sm, by = c("group", "genotype")) %>% 
    replace_na(list(n = 0)) 
  
  df_com <- df_exp %>% 
    drop_na %>% 
    group_by(group) %>% 
    mutate(sum = sum(n)) %>%
    ungroup() %>%
    separate(genotype, c("a1", "a2"), sep = " ", remove = FALSE) %>% 
    mutate(status = if_else(a1 == a2, "homo", "hetero"))
  
  # adding a clunky function to avoid rare events, in which both ctrl and disease group do not have all 3 expected genotype configurations
  
  ## case 1 - no heterozygous configuration
  if(df_com$status %>% unique() %>% length() < 2 & df_com$status %>% unique() %>% .[1]== "homo"){
    df_com <- df_com %>% group_by(group) %>% 
      do(rbind(., tibble(status = "hetero", 
                         
                         a1 = .$a1[1], 
                         a2 = .$a2[2], 
                         genotype = paste0(a1, " ", a2), 
                         n = NA, 
                         sum = .$sum[1], 
                         group = .$group %>% unique()))) %>% 
      replace_na(list(n = 0, sum = 0)) 
  }
  
  ## case 2 - no homozygous configuration for one allele
  if(df_com$status %>% length() < 6 & ("hetero" %in% (df_com$status %>% unique()))){
    df_com <- df_com %>% arrange(status) %>% 
      group_by(group) %>% 
      do(rbind(., tibble(status = "homo", 
                         a1 = "N", 
                         a2 = "N", 
                         genotype = paste0(a1, " ", a2), 
                         n = NA, 
                         sum = .$sum[1], 
                         group = .$group %>% unique()))) %>% 
      replace_na(list(n = 0, sum = 0)) 
  }
  return(df_com)
  #print("getting genotype")
}
