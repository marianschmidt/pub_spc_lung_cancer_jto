# project:      SPN - Data Analysis  
# script:       01.x1.cr_impute.missing.rates.R  
# author:       Marian Eberl  
# recent date:  2020-09-18  
# versions:     2020-09-18 first version  
# req packages: tidyverse, msSPChelpR, knitr
# content:		  helper script for 01.an_analyses.spc.lung.Rmd to impute missing rates for Bavaria and Lower Saxony
# used in:  
# dependend on:  
# comments:  
# execution time:  
# quality check: (version 2017-07-28, YYYY-MM-DD, Name Programmer)  


#replace factor with character
rates <-  rates %>% 
  mutate(across(where(is.factor), as.character))

rates_dco <-  rates_dco %>% 
  mutate(across(where(is.factor), as.character))

#impute rates for Bavaria and Lower Saxony 2000-2004
rates_imp <- rates %>%
  tidyr::complete(t_site,
                  region = c("DE2 Bavaria", "DE9 Lower Saxony"),
                  year = c("2000 - 2004", "2002", "2003", "2004",
                           "2005 - 2009", "2005", "2006", "2007", "2008", "2009",
                           "2010 - 2014", "2010", "2011", "2012", "2013", "2014"),
                  sex,
                  age
  ) %>%
  filter(is.na(incidence_crude_rate))

#for Bavaria fill up empty years with rates after 2004

rates_imp2 <- rates %>%
  filter(region == "DE2 Bavaria" & year == "2005 - 2009") %>%
  right_join(rates_imp, by = c("t_site", "region", "sex", "age")) %>%
  select(t_site,
         region,
         year = "year.y",
         sex,
         age,
         comment = "comment.y",
         incidence_cases = "incidence_cases.x",
         incidence_crude_rate = "incidence_crude_rate.x",
         population_pyar = "population_pyar.x",
         population_n_per_year = "population_n_per_year.x"
  ) %>%
  mutate(comment = case_when(region == "DE2 Bavaria" ~ "values imputed from rates DE2 Bavaria for the period 2005-2009",
                             TRUE ~ comment))

#for Lower Saxony fill up empty years with rates after 2004

rates_imp3 <- rates %>%
  filter(region == "DE9 Lower Saxony" & year == "2005 - 2009") %>%
  right_join(rates_imp, by = c("t_site", "region", "sex", "age")) %>%
  select(t_site,
         region,
         year = "year.y",
         sex,
         age,
         comment = "comment.y",
         incidence_cases = "incidence_cases.x",
         incidence_crude_rate = "incidence_crude_rate.x",
         population_pyar = "population_pyar.x",
         population_n_per_year = "population_n_per_year.x"
  ) %>%
  mutate(comment = case_when(region == "DE9 Lower Saxony" ~ "values imputed from rates DE9 Lower Saxony for the period 2005-2009",
                             TRUE ~ comment))

rates_imp2 <- rates_imp2 %>%
  filter(region == "DE2 Bavaria")

rates_imp3 <- rates_imp3 %>%
  filter(region == "DE9 Lower Saxony")

rates_imp <- rbind(rates, rates_imp2, rates_imp3)

rm(rates, rates_imp2, rates_imp3)


#impute rates incl. DCO for Bavaria and Lower Saxony 2000-2004
rates_dco_imp <- rates_dco %>%
  tidyr::complete(t_site,
                  region = c("DE2 Bavaria", "DE9 Lower Saxony"),
                  year = c("2000 - 2004", "2002", "2003", "2004",
                           "2005 - 2009", "2005", "2006", "2007", "2008", "2009",
                           "2010 - 2014", "2010", "2011", "2012", "2013", "2014"),
                  sex,
                  age
  ) %>%
  filter(is.na(incidence_crude_rate))

#for Bavaria fill up empty years with rates after 2004

rates_imp2 <- rates_dco %>%
  filter(region == "DE2 Bavaria" & year == "2005 - 2009") %>%
  right_join(rates_dco_imp, by = c("t_site", "region", "sex", "age")) %>%
  select(t_site,
         region,
         year = "year.y",
         sex,
         age,
         comment = "comment.y",
         incidence_cases = "incidence_cases.x",
         incidence_crude_rate = "incidence_crude_rate.x",
         population_pyar = "population_pyar.x",
         population_n_per_year = "population_n_per_year.x"
  ) %>%
  mutate(comment = case_when(region == "DE2 Bavaria" ~ "values imputed from rates DE2 Bavaria for the period 2005-2009",
                             TRUE ~ comment))

#for Lower Saxony fill up empty years with rates after 2004

rates_imp3 <- rates_dco %>%
  filter(region == "DE9 Lower Saxony" & year == "2005 - 2009") %>%
  right_join(rates_dco_imp, by = c("t_site", "region", "sex", "age")) %>%
  select(t_site,
         region,
         year = "year.y",
         sex,
         age,
         comment = "comment.y",
         incidence_cases = "incidence_cases.x",
         incidence_crude_rate = "incidence_crude_rate.x",
         population_pyar = "population_pyar.x",
         population_n_per_year = "population_n_per_year.x"
  ) %>%
  mutate(comment = case_when(region == "DE9 Lower Saxony" ~ "values imputed from rates DE9 Lower Saxony for the period 2005-2009",
                             TRUE ~ comment))

rates_imp2 <- rates_imp2 %>%
  filter(region == "DE2 Bavaria")

rates_imp3 <- rates_imp3 %>%
  filter(region == "DE9 Lower Saxony")

rates_dco_imp <- rbind(rates_dco, rates_imp2, rates_imp3)

rm(rates_dco, rates_imp2, rates_imp3)