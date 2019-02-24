test_ctrl_vs_disease <- function(allele){
  allele <- allele %>% arrange(group, type)
  m <- matrix(c(allele$n), nrow = 2, byrow = TRUE)
  
  m %>% chisq.test() %>% 
    broom::tidy() %>% 
    dplyr::select(p.value) %>%
    magrittr::set_colnames(paste0("cd_", colnames(.))) %>% 
    mutate(cd_or = (m[2,2]/m[2,1])/(m[1,2]/m[1,1])) %>% return()
}