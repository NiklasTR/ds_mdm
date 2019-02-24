test_ctrl_vs_disease <- function(allele){
  
  if(n_miss(allele) > 0 | nrow(allele) != 4){return(tibble(cd_p.value = NA,
                                                           cd_or = NA))}
  
  allele <- allele %>% arrange(group, type)
  m <- matrix(c(allele$n), nrow = 2, byrow = TRUE)
  
  #if(is.na(m)){return(NA)}
  if(min(m) < 0){return(NA)}
  
  #return(m)
  m %>% chisq.test() %>%
    broom::tidy() %>%
    dplyr::select(p.value) %>%
    magrittr::set_colnames(paste0("cd_", colnames(.))) %>%
    mutate(cd_or = (m[2,2]/m[2,1])/(m[1,2]/m[1,1])) %>% return()
  
  #print("getting ctrl vs. disease")
}
