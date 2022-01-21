### loading survey and register data
uvreg <- haven::read_spss(here("data", "original", "regf_UV.sav"), # using here package for creating project-relative path
                          col_select = c(volda1_2007:sexe13kg, # selects only relevant columns
                                         kjoenn,
                                         fodeaar,
                                         invkat, 
                                         famtype_2003:famtype_2005,
                                         eduF2016,
                                         SOSBAK)
                          )

# change all variables to factors
uvreg <- as_factor(uvreg)

# delete respondents older than 19
uvreg <- uvreg %>% 
  filter(fodeaar >= 1987)

### loading complete survey data set
uvcompl <- haven::read_spss(here("data", "original", "uv_complete.sav"),
                            col_select = c(kjonn, 
                                           alder, 
                                           io_fode, 
                                           studretn_2007, 
                                           reg)
                            )

# change all variables to factors
uvcompl <- as_factor(uvcompl)

# delete respondents older than 19
uvcompl <- uvcompl %>% 
  filter(as.numeric(alder) < 4)