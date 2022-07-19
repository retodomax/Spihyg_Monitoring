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

## function to get proportion pivot table
prop_table <- function(dat, response, by = NULL, full_grid){
  dat %>% 
    group_by(Messperiode, across(any_of(by))) %>% 
    summarize(n = sum(!is.na(.data[[response]])),
              n_correct = sum(.data[[response]] == 1, na.rm = TRUE),
              n_wrong = sum(.data[[response]] == 0, na.rm = TRUE)) %>% 
    mutate(percent_correct = n_correct/n) %>% 
    rowwise() %>% 
    mutate(CI_lo = binom_CI(n_correct, n, 1),
           CI_up = binom_CI(n_correct, n, 2)) %>% 
    ungroup() %>% 
    right_join(full_grid) %>% 
    arrange_at(c("Messperiode", by)) %>% 
    mutate(n = ifelse(is.na(n), 0, n))
}

## Applies prop_table twice if we specify `by` argument
prop_period_fun <- function(dat, response, by = NULL, filter_crit = NULL){
  if(!is.null(filter_crit)){
    dat <- dat %>% filter(.data[[filter_crit]] == "Ja")
  }
  small_grid <- tibble(Messperiode = year_full_levels(dat$Messperiode))
  out <- dat_agg <- prop_table(dat = dat, response = response,
                               by = NULL, full_grid = small_grid)
  if(!is.null(by)){
    full_grid <- expand_grid(year_full_levels(dat$Messperiode),
                             unique(na.omit(dat[[by]])))
    colnames(full_grid) <- c("Messperiode", by)
    dat_by <- prop_table(dat = dat, response = response,
                         by = by, full_grid = full_grid)
    out <- list(dat_by = dat_by, dat_agg = dat_agg)
  }
  out
}

## HDM Plots
plot_hdm <- function(dat = dat_station, filter_by = "Intermediate-Care-Stationen (IMC)",
                     filter_level = "Hauptbereich", facet_var = "Station",
                     filename){
  p <- dat %>% 
    filter(.data[[filter_level]] == filter_by) %>% 
    ggplot(aes(x = Messperiode, y = desinf_per_careday, group = 1,
               label = round(desinf_per_careday))) +
    geom_point() +
    geom_line() +
    geom_text(hjust=0, vjust=-1.3, size = 2.5) +
    ylab("Desinfektionsmittel/Pflegetag [ml/t]") +
    facet_wrap(~ .data[[facet_var]]) +
    theme_bw() + 
    scale_y_continuous(expand = expansion(mult = c(0.1,0.4))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(filename = filename, plot = p,
         width = 16, height = 14, units = "cm", scale = 1.5)
  p
}


## plot_by
plot_by <- function(dat, fileprefix, facet_var = "Station",
                    n_min = 10, ncols = 2, gray_area = TRUE,
                    width2 = 16, height2 = 20){
  cols1 <- c("Spezifische Abteilung" = "black")
  cols2 <- c("USZ Durchschnitt" = "lightgray")
  p2 <- dat[[1]] %>% 
    mutate(across(all_of(c("percent_correct", "CI_lo", "CI_up")),
                  ~ ifelse(n < n_min, NA, .x))) %>% 
    ggplot(aes(x = Messperiode, y = percent_correct, group = 1)) +
    {if(gray_area) geom_area(data = dat[[2]],
                             aes(x = Messperiode, y = percent_correct,
                                 fill = "USZ Durchschnitt"))} +
    geom_errorbar(aes(ymin = CI_lo, ymax = CI_up), width = 0.1, col = "darkgray") +
    geom_point(aes(col = "Spezifische Abteilung")) +
    geom_line(aes(col = "Spezifische Abteilung")) +
    theme_bw() +
    {if(gray_area) theme(legend.position = "bottom")} +
    {if(!gray_area) theme(legend.position="none")} +
    scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
    geom_text(aes(label = n, y = -.2), angle = 90, size = 2, hjust = 1) +
    annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
             hjust = 0, size = 2.5) +
    facet_rep_wrap(~ .data[[facet_var]], ncol = ncols, repeat.tick.labels = 'x') +
    ylab("Anteil korrekte Beobachtungen") +
    scale_colour_manual(name = "",values = cols1) +
    scale_fill_manual(name = "", values = cols2) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0(fileprefix, "_by.png"), p2, width = width2, height = height2,
         units = "cm", scale = 1.5)
  p2
}


## plot_agg
plot_agg <- function(dat, fileprefix,
                     n_min = 10, width1 = 8, height1 = 6,
                     title = NULL, suffix = ""){
  p1 <- dat %>% 
    # mutate(percent_correct = ifelse(n < n_min, NA, percent_correct),
    #        CI_lo = ifelse(n < n_min, NA, CI_lo),
    #        CI_up = ifelse(n < n_min, NA, CI_up)) %>% 
    mutate(across(all_of(c("percent_correct", "CI_lo", "CI_up")),
                  ~ ifelse(n < n_min, NA, .x))) %>% 
    ggplot(aes(x = Messperiode, y = percent_correct, group = 1,
               label = round(percent_correct, 2))) +
    geom_errorbar(aes(ymin = CI_lo, ymax = CI_up), width = 0.1, col = "darkgray") +
    geom_point() +
    geom_text(hjust=-.3, vjust=-.3, size = 2.5) +
    geom_line() +
    theme_bw() +
    scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
    geom_text(aes(label = n, y = -.2), angle = 90, size = 2, hjust = 1) +
    annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
             hjust = 0, size = 2.5) +
    ylab("Anteil korrekte Beobachtungen") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(title) + theme(plot.title = element_text(size = 12))
  ggsave(paste0(fileprefix, suffix, ".png"), p1, width = width1, height = height1,
         units = "cm", scale = 1.5)
  p1
}
  


## function to make plots from prop_period output
plot_prop_period <- function(prop_period, fileprefix, facet_var = "Station",
                             n_min = 10, ncols = 2, width1 = 8, height1 = 6,
                             width2 = 16, height2 = 20,
                             title = NULL, gray_area = TRUE){
  p2 <- NULL
  suffix <- ""
  ag_table <- prop_period
  if(class(prop_period)[1] == "list"){
    p2 <- plot_by(dat = prop_period, fileprefix = fileprefix,
                  facet_var = facet_var, n_min = n_min,
                  ncols = ncols, gray_area = gray_area,
                  width2 = width2, height2 = height2)
    suffix <- "_agg"
    ag_table <- prop_period[[2]]
  }
  p1 <- plot_agg(dat = ag_table, fileprefix = fileprefix,
                 n_min = n_min, width1 = width1, height1 = height1,
                 title = title, suffix = suffix)
  list(p1, p2)
}



# Barplots ----------------------------------------------------------------


barplot_agg <- function(dat, response, fileprefix){
  dat <- dat %>%
    filter(!is.na(.data[[response]])) %>%
    mutate(Messperiode = factor(Messperiode,
                                levels = year_full_levels(dat$Messperiode)))
  dat_agg <- dat %>%
    group_by(Messperiode, .drop = FALSE) %>%
    summarise(n = n())
  response <- sym(response)
  ag_bar <- dat %>%
    ggplot(aes(x = Messperiode)) +
    geom_bar(aes(fill = !!response), position = "fill", width = 0.5) +
    scale_x_discrete(drop = FALSE) +
    theme_bw() +
    ylab("Antiel Beobachtungen") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
    geom_text(data = dat_agg, aes(label = n, y = -.2, x = Messperiode),
              # inherit.aes = FALSE,
              angle = 90, size = 2, hjust = 1) +
    annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
             hjust = 0, size = 2.5)
  ggsave(paste0(fileprefix, "_bar_agg.png"), plot = ag_bar,
         width = 16, height = 8, units = "cm", scale = 1.5)
  ag_bar
}



barplot_by <- function(dat, response, by, fileprefix, n_min = 10,
                       gray_area_dat){
  max_per <- max(as.character(dat$Messperiode))
  dat <- dat %>%
    filter(!is.na(.data[[response]])) %>%
    mutate(Messperiode = factor(Messperiode,
                                levels = year_full_levels(dat$Messperiode)))
  dat_agg <- dat %>%
    group_by(Messperiode, across(any_of(by)), .drop = FALSE) %>%
    summarise(n = n())
  
  myclin <- dat_agg %>%
    filter(Messperiode == max_per, n >= n_min) %>% pull(Klinik)
  dat2 <- dat %>% filter(Klinik %in% myclin)
  dat_agg <- dat_agg %>% filter(Klinik %in% myclin)
  
  response <- sym(response)
  cols2 <- c("USZ_Durchschnitt" = "lightgray")
  by_bar <- dat2 %>%
    ggplot(aes(x = Messperiode)) +
    geom_area(data = gray_area_dat,
              mapping = aes(x = Messperiode, y = percent_correct, group = 1,
                            fill = "USZ_Durchschnitt")) + 
    scale_fill_manual(name = "", values = cols2) +
    new_scale_fill() +
    geom_bar(aes(fill = !!response), position = "fill", width = 0.5) +
    scale_x_discrete(drop = FALSE) +
    theme_bw() +
    ylab("Antiel Beobachtungen") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_y_continuous(breaks = seq(0,1,0.25), limits = c(-.3,1)) +
    geom_text(data = dat_agg, aes(label = n, y = -.2, x = Messperiode),
              inherit.aes = FALSE, angle = 90, size = 2, hjust = 1) +
    annotate(geom="text", x= 0.8, y=-.1, label="Gesamt N",
             hjust = 0, size = 2.5) +
    facet_wrap(eval(expr(~!!ensym(by))))
  ggsave(paste0(fileprefix, "_bar_by.png"), plot = by_bar,
         width = 16, height = 8, units = "cm", scale = 1.5)
  by_bar
}