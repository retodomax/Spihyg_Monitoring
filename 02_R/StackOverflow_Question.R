### Stack overflow question
tibble(y = 1:10,
       Col1 = rep(c("yes", "no"), 5))

filter_crit = "Col1"



dat %>% 
  {if(is.na(filter_crit)) . else filter(.data[[filter_crit]] == "Ja")} %>% 
  summarize(sum(y))

## R Conditional evaluation when using the pipe operator %>%
## https://stackoverflow.com/a/30604360/6152316


## Within the if statement, I want to use the .data[[variable]] as described here
## https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html


## How should this be done with dplyr?
