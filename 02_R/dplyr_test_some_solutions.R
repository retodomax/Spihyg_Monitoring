#### First problem: how to programmically include serveral variables in group_by()
#### Second problem: how to allow for NULL in group_by()

mtcars

mtcars %>% 
  group_by(cyl, gear) %>% 
  summarise(mpg = mean(mpg))

mtcars %>% 
  group_by(cyl) %>% 
  summarise(mpg = mean(mpg))

mtcars %>% 
  group_by() %>% 
  summarise(mpg = mean(mpg))



# function ----------------------------------------------------------------

myfun <- function(group){
  mtcars %>% 
    group_by(.data[[group]]) %>% 
    summarise(mpg = mean(mpg))
}
myfun(group = "cyl") ## works
myfun(group = "gear")  ## works
myfun(group = c("cyl", "gear"))  ## does not work!
# see here: https://search.r-project.org/CRAN/refmans/rlang/html/topic-data-mask-programming.html
# "The .data pronoun can only be subsetted with single column names."


## use across()

myfun <- function(group){
  mtcars %>% 
    group_by(across(.data[[group]])) %>% 
    summarise(mpg = mean(mpg))
}

myfun(group = "cyl") ## works
myfun(group = "gear")  ## works
myfun(group = c("cyl", "gear"))  ## does not work!


## use across() and no character

myfun <- function(group){
  mtcars %>% 
    group_by(across({{ group }})) %>% 
    summarise(mpg = mean(mpg))
}

myfun(group = "cyl") ## works
myfun(group = "gear")  ## works
myfun(group = c("cyl", "gear"))  ## works!
myfun(group = cyl)  ## works!
myfun(group = gear)  ## works!
myfun(group = c(cyl, gear))  ## works!


dplyr::arrange()
dplyr::group_by()

dplyr::across()
.data
