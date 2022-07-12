#' ---
#' project: Zivi2022R    ##################################################
#' title:   functions
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-07 14:10:58
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
library(lemon)


# Functions ---------------------------------------------------------------

## function to get all Messperiode levels
year_full_levels <- function(x){
  x <- as.character(x)
  ## 1) extract range
  year_levels <- sort(unique(x))
  year_range <- year_levels[c(1, length(year_levels))]
  
  ## 2) make numeric
  toNum <- function(x){
    x <- as.numeric(x)
    x[1] + ifelse(x[2] == 2, 0.5, 0)
  }
  year_range_num <- sapply(strsplit(year_range, "-"), toNum)
  
  ## 3) make sequence
  year_seq <- seq(year_range_num[1], year_range_num[2], 0.5)
  
  ## 4) transform back
  toChar <- function(x){
    x <- paste0(x[1], "-", ifelse(x[2] == 5, 2, 1))
  }
  sapply(strsplit(format(year_seq, nsmall = 1), "\\."), toChar)
}

## function to get binomial confidence interval
binom_CI <- function(x, n, l) {
  if(is.na(n) | is.na(x) | n < x | n == 0){
    NA
  } else {
    binom.test(x, n, alternative =  "two.sided")$conf.int[l]
  }
}

## function to get prop_period table
prop_period_fun <- function(dat, response, by, filter_crit, full_grid){
  if(!is.na(filter_crit)){
    dat <- dat %>% 
      filter(.data[[filter_crit]] == "Ja")
  }
  dat_by <- dat %>% 
    group_by(Messperiode, .data[[by]]) %>% 
    summarize(n = sum(!is.na(.data[[response]])),
              n_correct = sum(.data[[response]] == 1, na.rm = TRUE),
              n_wrong = sum(.data[[response]] == 0, na.rm = TRUE)) %>% 
    mutate(percent_correct = n_correct/n) %>% 
    rowwise() %>% 
    mutate(CI_lo = binom_CI(n_correct, n, 1),
           CI_up = binom_CI(n_correct, n, 2)) %>% 
    ungroup() %>% 
    full_join(full_grid) %>% 
    filter(!is.na(.data[[by]])) %>% 
    mutate(n = ifelse(is.na(n), 0, n))
  dat_agg <- dat_by %>%
    group_by(Messperiode) %>% 
    summarise(n = sum(n, na.rm = TRUE),
              n_correct = sum(n_correct, na.rm = TRUE),
              n_wrong = sum(n_wrong, na.rm = TRUE)) %>% 
    mutate(percent_correct = n_correct/n) %>% 
    rowwise() %>% 
    mutate(CI_lo = binom_CI(n_correct, n, 1),
           CI_up = binom_CI(n_correct, n, 2)) %>% 
    ungroup() %>% 
    full_join(tibble(Messperiode = year_full_levels(dat$Messperiode))) %>% 
    mutate(n = ifelse(is.na(n), 0, n))
  list(dat_by = dat_by, dat_agg = dat_agg)
}

## function to make plots
plot_prop_period <- function(prop_period, fileprefix, facet_var = "Station",
                             n_min = 10, ncols = 2, width1 = 8, height1 = 6){
  p1 <- prop_period[[2]] %>% 
    mutate(percent_correct = ifelse(n < n_min, NA, percent_correct),
           CI_lo = ifelse(n < n_min, NA, CI_lo),
           CI_up = ifelse(n < n_min, NA, CI_up)) %>% 
    ggplot(aes(x = Messperiode, y = percent_correct, group = 1,
               label = round(percent_correct, 2))) +
    geom_errorbar(aes(ymin = CI_lo, ymax = CI_up), width = 0.1, col = "darkgray") +
    geom_point() +
    geom_text(hjust=-.3, vjust=-.3, size = 2.5) +
    geom_line() +
    theme_bw() +
    scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
    geom_text(aes(label = n, y = -.25)) +
    annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
             hjust = 0) +
    ylab("Anteil korrekte Beobachtungen") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  cols1 <- c("Spezifische Abteilung" = "black")
  cols2 <- c("USZ Durchschnitt" = "lightgray")
  p2 <- prop_period[[1]] %>% 
    mutate(percent_correct = ifelse(n < n_min, NA, percent_correct),
           CI_lo = ifelse(n < n_min, NA, CI_lo),
           CI_up = ifelse(n < n_min, NA, CI_up)) %>% 
    ggplot(aes(x = Messperiode, y = percent_correct, group = 1)) +
    geom_area(data = prop_period[[2]], aes(x = Messperiode, y = percent_correct,
                                           fill = "USZ Durchschnitt")) +
    geom_errorbar(aes(ymin = CI_lo, ymax = CI_up), width = 0.1, col = "darkgray") +
    geom_point(aes(col = "Spezifische Abteilung")) +
    geom_line(aes(col = "Spezifische Abteilung")) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
    geom_text(aes(label = n, y = -.25)) +
    annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
             hjust = 0) +
    facet_rep_wrap(~ .data[[facet_var]], ncol = ncols, repeat.tick.labels = 'x') +
    ylab("Anteil korrekte Beobachtungen") +
    scale_colour_manual(name = "",values = cols1) +
    scale_fill_manual(name = "", values = cols2) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0(fileprefix, "_agg.png"), p1, width = width1, height = height1,
         units = "cm", scale = 1.5)
  ggsave(paste0(fileprefix, "_by.png"), p2, width = 16, height = 20,
         units = "cm", scale = 1.5)
  list(p1, p2)
}

## HDM Plots
plot_hdm <- function(dat = dat_station, filter_by = "Intermediate-Care-Stationen (IMC)",
                     filter_level = "Hauptbereich", wrap_by = "Station",
                     filename){
  p <- dat %>% 
    filter(.data[[filter_level]] == filter_by) %>% 
    ggplot(aes(x = Messperiode, y = desinf_per_careday, group = 1,
               label = round(desinf_per_careday))) +
    geom_point() +
    geom_line() +
    geom_text(hjust=0, vjust=-1.3, size = 2.5) +
    ylab("Desinfektionsmittel/Pflegetag [ml/t]") +
    facet_wrap(~ .data[[wrap_by]]) +
    theme_bw() + 
    scale_y_continuous(expand = expansion(mult = c(0.1,0.4))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(filename = filename, plot = p,
         width = 16, height = 14, units = "cm", scale = 1.5)
  p
}