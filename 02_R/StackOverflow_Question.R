### Stack overflow question


# Q1 ----------------------------------------------------------------------

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



# Q2 ----------------------------------------------------------------------


## I am more and more shifting to use dplyr also for writing functions
## Most of it works bettern then in base R but I did not yet manage to
## be as efficient in debunging as when using base R.
## How can I execute a chain of pipes line by line and in between have a look
## at the intermediate results without changing my code?

## Note: there are may solutions **with** changing the code like here,
## here, or here. But changing the code take time and have to be reverted
## once, the bug is found.

fun_dplyr <- function(dat, n_min){
  dat %>% 
    group_by(Species) %>% 
    summarise(n = n(),
              mu = mean(Sepal.Length)) %>% 
    filter(n > n_min) %>% 
    mutate(a = ifelse(mu > 5.2, "large", "small"))
}
debugonce(fun_dplyr) 
fun_dplyr(dat = iris, n_min = 40)
## I would like to go through each line step by step and execute code in between
## However, the chain of pipes is executed as one block...


## With base r, debuging works perfectly line by line
fun_base <- function(dat, n_min){
  dat_ag <- aggregate(Sepal.Length ~ Species,
                      FUN = function(x) c(n = length(x), mu = mean(x)),
                      data = dat)
  dat_ag <- do.call(data.frame, dat_ag)
  dat_ag <- dat_ag[dat_ag$Sepal.Length.n > n_min,]
  dat_ag$a <- ifelse(dat_ag$Sepal.Length.mu > 5.2, "large", "small")
  dat_ag
}
debugonce(fun_base)
fun_base(dat = iris, n_min = 40)



# Q3 ----------------------------------------------------------------------

# Pass variables as character vector to dplyr::arrange() and allow for NULL

# In the [Data mask programming patterns help manual](https://search.r-project.org/CRAN/refmans/rlang/html/topic-data-mask-programming.html)
# it says:
# Use this bridge technique to connect vectors of names to a data-masked context. 
my_group_by <- function(data, vars) {
  data %>% dplyr::group_by(across(all_of(vars)))
}

mtcars %>% my_group_by(c("cyl", "am"))

# this also works fine if we pass NULL as argument (apply no grouping)

mtcars %>% my_group_by(NULL)


# I want a similar behaviour for arrange()

my_arrange <- function(data, vars){
  data %>% dplyr::arrange(across(all_of(vars)))
}

mtcars %>% my_arrange(c("cyl", "am"))

# this does not work if we pass NULL as an argument (apply no arranging)
mtcars %>% my_arrange(NULL)


## What would be the best way to allow for NULL as an argument?


################ New try
my_arrange <- function(data, vars){
  data %>% dplyr::arrange_at(vars)
}

mtcars %>% my_arrange(c("cyl", "am"))

# this does not work if we pass NULL as an argument (apply no arranging)
mtcars %>% my_arrange(NULL)

