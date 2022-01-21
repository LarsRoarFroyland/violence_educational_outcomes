### default setting for flextable package
set_flextable_defaults(
  font.family = "Times New Roman", 
  font.size = 12,
  font.color = "black",
  text.align = "center",
  digits = 3,
  theme_fun = function(x) x,
)

### functions to fit flextables to word pages

# vertical page
FitFlextableToVertPage <- function(ft, pgwidth = 6.5){
  ft_out <- ft %>% autofit()
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}

# horizontal page
FitFlextableToHorPage <- function(ft, pgwidth = 9.5){
  ft_out <- ft %>% autofit()
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}