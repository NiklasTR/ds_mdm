run_ewas <- function(variable, 
                     modification_variable = "scale(x)",
                     target = "LBXGLU",
                     modification_target = "scale(log(x))",
                     nhd = NHData.train, 
                     description = ExposureDescription){
  
  # define covariates
  covariates <- c("female",
                  "black", 
                  "mexican",
                  "other_hispanic",
                  "other_eth",
                  "scale(RIDAGEYR)",
                  "scale(INDFMPIR)")
  
  # defining target part of formula
  target_mod = str_replace(modification_target, pattern = "x", target)
  variable_mod = str_replace(modification_variable, pattern = "x", variable)
  
  input <- c(covariates, variable_mod)
  
  # selecting variables
  # nhd_f <- nhd %>% dplyr::select_(input)
  
  # Formatting the dataset
  dsn <- svydesign(ids=~SDMVPSU,
                   strata=~SDMVSTRA,
                   weights=~WTMEC2YR,
                   nest=T, 
                   data=nhd)
  
  # define formula
  f <- as.formula(
    paste(
      paste0(target_mod," ~"), 
      paste(input, collapse="+")
      )
     )
  
  
  # fit model
  mod <-svyglm(formula = f, dsn)
  
  # extracting model result and adding metadata
  summary(mod)$coefficients %>%
    as.data.frame() %>%
    janitor::clean_names() %>%
    rownames_to_column("exposure_id") %>%
    dplyr::select(-t_value, pvalue = pr_t) %>% 
    filter(!(exposure_id %in% covariates)) %>% 
    filter(exposure_id != "(Intercept)") %>% 
    filter(exposure_id == variable_mod) %>%
    mutate(exposure_id = variable) %>%
    left_join(description %>% dplyr::select(exposure_id = var, exposure_name = var_desc) %>% distinct() , by = "exposure_id") %>%
    head(1) %>%
    return()
  
}
