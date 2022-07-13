library(tidyverse)


# Problem 1 ---------------------------------------------------------------

## Find a way to specify a grouping character vector which can also be NULL
# function(grouping){
#   dat %>% 
#     group_by(grouping)
# }
# where grouping = NULL or grouping = c("col1", "col2")


fun_dplyr <- function(dat, n_min){
  dat %>% 
    group_by(Species) %>% 
    summarise(n = n(),
              mu = mean(Sepal.Length)) %>% 
    filter(n > n_min) %>% 
    mutate(a = ifelse(mu > 5.2, "large", "small"))
}

ggplot2::mpg

mpg %>% 
  group_by(drv, cyl) %>% 
  summarise(hwy = mean(hwy))

myfun <- function(group){
  mpg %>% 
    group_by({{group}}) %>% 
    summarise(hwy = mean(hwy))
}

myfun(group = NULL)
myfun(group = drv)
myfun(group = "drv")
myfun(group = c(drv, cyl))
myfun(group = c("drv", "cyl"))


myfun <- function(...){
  mpg %>% 
    group_by(...) %>% 
    summarise(hwy = mean(hwy))
}

myfun(NULL)
myfun(drv)
myfun(drv, cyl)
myfun("drv", "cyl")


myfun <- function(group){
  mpg %>% 
    group_by(drv, across(any_of(group))) %>% 
    summarise(hwy = mean(hwy))
}

myfun(group = NULL)
myfun(group = drv)
myfun(group = "drv")
myfun(group = c(drv, cyl))
myfun(group = c("drv", "cyl"))
myfun(group = "cyl")

## Best option!
mpg %>% 
  group_by(across(any_of("drv")))



# second problem ----------------------------------------------------------
vari

## Find a way to specify a arrange character vector which can also be NULL
# function(vari){
#   dat %>% 
#     arrange(vari)
# }
# where vari = NULL or vari = c("col1", "col2")
myfun <- function(vari){
  mpg %>% 
    arrange(across(any_of(vari)))
}

myfun(vari = NULL)
myfun(vari = "drv")
myfun(vari = c("drv", "class"))

### Solution
myfun <- function(vari){
  mpg %>% 
    arrange_at(vari)
}

myfun(vari = NULL)
myfun(vari = "drv")
myfun(vari = c("drv", "class"))
