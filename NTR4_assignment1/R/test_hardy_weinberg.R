test_hardy_weinberg <- function(allele, genotype){
  
  if(n_miss(allele) > 0 | nrow(allele) != 4){return(tibble(hw_p.value = NA))}
  
  allele <- allele %>% filter(group == "ctrl")
  genotype <- genotype %>% filter(group == "ctrl")
  
  n_pred <- tibble(predicted = c(allele$ratio[1]^2,
                                 2*allele$ratio[1]*allele$ratio[2],
                                 allele$ratio[2]^2) * unique(genotype$sum)) %>% 
    cbind(genotype,.)
  
  matrix(c(n_pred$n, n_pred$predicted), nrow = 2, byrow = TRUE) %>%
    chisq.test() %>%
    broom::tidy() %>% 
    dplyr::select(p.value) %>%
    magrittr::set_colnames(paste0("hw_", colnames(.))) %>% 
    return()
  
  print("getting hardy weinberg")
}
