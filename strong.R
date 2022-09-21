library(tidyverse)
library(lubridate)
library(hrbrthemes)

process_file_upload <- function(fp)
  fp %>%
  read_delim(delim = ";", trim_ws = TRUE,
             escape_double = FALSE, na = "NA") %>%
  mutate(Weight = replace_na(Weight, 0)) %>%
  # subtract assisted pounds
  mutate(Weight = ifelse(grepl('(Assisted)', .$`Exercise Name`), 
                         -Weight,
                         Weight)) %>%
  mutate(lbs = Reps * Weight) %>%
  mutate(Date = as_date(Date)) 

lift_options <- c()

lift_pr <- function(df, lift){
  df %>%
  subset(`Exercise Name` == lift) %>%
  summarize(pr = max(Weight)) %>%
  as.numeric() %>%
  return()
}

lift_tot <- function(df, lift){
  df %>%
  subset(`Exercise Name` == lift) %>%
  summarize(tot = sum(lbs)) %>%
  as.numeric() %>%
  return()
}

make_lift_plot <- function(df, lift){
  df %>%
    subset(`Exercise Name` == lift) %>%
    group_by(Date) %>%
    slice_max(Weight) %>%
    group_by(Date, `Workout Name`) %>%
    summarise(Reps = n(), Weight = Weight) %>%
    ggplot(aes(x = Date, y= Weight, color = round(`Reps`))) + 
    geom_point() + 
    geom_line() +
    theme_ft_rc() + 
    viridis::scale_color_viridis(name="# Reps",
                                option = 'C',
                                direction = 1,
                                na.value = "grey93") %>%
    return()
}


make_fitness_cal <- function(df){
  cal <- df %>% 
    # remove assissted weight
    filter(Weight > 0) %>%
    # split by date
    group_by(Date) 
  
  # fill in missing days
  cal <- rbind(cal, tibble(Date = seq(min(cal$Date), max(cal$Date), by = "days"), lbs = 0)) %>%
    # split by date
    group_by(Date) %>%
    summarise(`Total Moved` = sum(lbs))
  
  cal %>%
    # make axes
    mutate(Week = week(Date), Year = year(Date),
           Weekday = wday(Date, label = T)) %>%
    ggplot(aes(x = Week, y = Weekday, fill = `Total Moved`)) + 
    geom_tile(color = 'white', size = 0.1) +
    #facet_wrap('year', ncol = 1) + 
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 52, length = 12),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
    theme_ft_rc() + 
    viridis::scale_fill_viridis(name="Total Moved (lbs)",
                                option = 'C',
                                direction = 1,
                                na.value = "grey93") %>%
    return()
}
    
  
  
  
  
  
  
  
