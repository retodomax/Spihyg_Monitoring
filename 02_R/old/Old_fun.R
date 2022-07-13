#' ---
#' project: Zivi2022R    ##################################################
#' title:   Old Functions
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2022-07-13 16:02:50
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(tidyverse)
library(lemon)


# fun ---------------------------------------------------------------------

## function to get prop_period table (13.07.2022)
prop_period_fun_old <- function(dat, response, by, filter_crit, full_grid){
  if(!is.na(filter_crit)){
    dat <- dat %>% 
      filter(.data[[filter_crit]] == "Ja")
  }
  if(!is.na(by)){
    dat <- dat %>% group_by(.data[[by]])
  }
  dat_by <- dat %>% 
    group_by(Messperiode, .add = TRUE) %>% 
    summarize(n = sum(!is.na(.data[[response]])),
              n_correct = sum(.data[[response]] == 1, na.rm = TRUE),
              n_wrong = sum(.data[[response]] == 0, na.rm = TRUE)) %>% 
    mutate(percent_correct = n_correct/n) %>% 
    rowwise() %>% 
    mutate(CI_lo = binom_CI(n_correct, n, 1),
           CI_up = binom_CI(n_correct, n, 2)) %>% 
    ungroup() %>% 
    full_join(full_grid) %>% 
    arrange(Messperiode) %>% 
    mutate(n = ifelse(is.na(n), 0, n))
  if(!is.na(by)){
    dat_by <- dat_by %>% filter(!is.na(.data[[by]]))
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
  } else {
    dat_by
  }
}


## function to make plots (13.07.2022)
plot_prop_period <- function(prop_period, fileprefix, facet_var = "Station",
                             n_min = 10, ncols = 2, width1 = 8, height1 = 6,
                             title = NULL, gray_area = TRUE){
  p2 <- NULL
  suffix <- ""
  ag_table <- prop_period
  if(class(prop_period)[1] == "list"){
    cols1 <- c("Spezifische Abteilung" = "black")
    cols2 <- c("USZ Durchschnitt" = "lightgray")
    p2 <- prop_period[[1]] %>% 
      mutate(percent_correct = ifelse(n < n_min, NA, percent_correct),
             CI_lo = ifelse(n < n_min, NA, CI_lo),
             CI_up = ifelse(n < n_min, NA, CI_up)) %>% 
      ggplot(aes(x = Messperiode, y = percent_correct, group = 1)) +
      {if(gray_area) geom_area(data = prop_period[[2]],
                               aes(x = Messperiode, y = percent_correct,
                                   fill = "USZ Durchschnitt"))} +
      geom_errorbar(aes(ymin = CI_lo, ymax = CI_up), width = 0.1, col = "darkgray") +
      geom_point(aes(col = "Spezifische Abteilung")) +
      geom_line(aes(col = "Spezifische Abteilung")) +
      theme_bw() +
      {if(gray_area) theme(legend.position = "bottom")} +
      {if(!gray_area) theme(legend.position="none")} +
      scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
      geom_text(aes(label = n, y = -.25)) +
      annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
               hjust = 0) +
      facet_rep_wrap(~ .data[[facet_var]], ncol = ncols, repeat.tick.labels = 'x') +
      ylab("Anteil korrekte Beobachtungen") +
      scale_colour_manual(name = "",values = cols1) +
      scale_fill_manual(name = "", values = cols2) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    ggsave(paste0(fileprefix, "_by.png"), p2, width = 16, height = 20,
           units = "cm", scale = 1.5)
    suffix <- "_agg"
    ag_table <- prop_period[[2]]
  }
  p1 <- ag_table %>% 
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
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(title) + theme(plot.title = element_text(size = 12))
  ggsave(paste0(fileprefix, suffix, ".png"), p1, width = width1, height = height1,
         units = "cm", scale = 1.5)
  list(p1, p2)
}