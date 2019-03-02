get_allele <- function(genotype){
  allele_hetero <- genotype %>% dplyr::filter(status == "hetero") %>%
    dplyr::select(-a1, -a2, -genotype, -status, n_hetero = n)
  
  allele <- genotype %>% 
    dplyr::filter(status == "homo") %>% 
    mutate(n_a = ifelse(status == "homo", n*2, n)) %>%
    left_join(., allele_hetero, by = c("group")) %>% 
    mutate(n = n_a + n_hetero) %>% 
    dplyr::select(group, a1, n) %>% 
    group_by(group) %>%
    mutate(sum = sum(n),
           ratio = n/sum) %>%
    group_by(a1) %>% 
    mutate(sum_allele = sum(n)) %>% 
    ungroup() %>%
    # in cases in which the allele frequency is equal, I need to break the tie
    mutate(type = if_else(sum_allele == max(sum_allele), "major", "minor"))
  
  if(allele$type %>% unique() %>% length() < 2){
    allele$type <- rep(c("major", "minor"), times = 2)
  }
  
  return(allele)
  
  #print("getting allele")
}