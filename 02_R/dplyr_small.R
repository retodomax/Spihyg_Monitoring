

# case_when ---------------------------------------------------------------

# case_when() as possible alternative to long chains of ifelse()

dat <- tibble(x = c(1, 2, 3, 1, 2, 3, NA, 3, NA))

dat %>% 
  mutate(x = ifelse(x == 1, "a", ifelse(x == 2, "b", x)))
dat %>% 
  mutate(x = case_when(x == 1 ~ "a",
                       x == 2 ~ "b",
                       is.na(x) ~ NA_character_,
                       TRUE ~ as.character(x)))

        