rm(list=ls())  # clear the environment
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#-------Import necessary packages here-------------------#

#------Uploading PERMID ---------------------------------#
PERMID <- "3436250"
PERMID <- as.numeric(gsub("\\D", "", PERMID))
set.seed(PERMID)

library(tidyverse)
library(janitor)
library(dplyr)


ibt_data <- read_csv("ibt_testdata.csv")

ibt_data <- ibt_data[-c(1, 2), ]

ibt_data <- ibt_data %>%
  janitor :: clean_names() %>%
  select(duration_in_seconds, finished, q65_page_submit, q66_page_submit, 
         race_ethnicity, english)

ibt_data_finished <- ibt_data %>%
  filter(finished == "TRUE")

race_count <- ibt_data_finished %>%
  count(race_ethnicity)


ibt_data_clean <- ibt_data_finished
ibt_data_clean$race <- ifelse(grepl("asi", ibt_data_clean$race_ethnicity, ignore.case = T),
                              "asian", ibt_data_clean$race_ethnicity)
ibt_data_clean$race <- ifelse(grepl("chi", ibt_data_clean$race, ignore.case = T),
                              "asian", ibt_data_clean$race)
ibt_data_clean$race <- ifelse(grepl("korean", ibt_data_clean$race, ignore.case = T),
                              "asian", ibt_data_clean$race)
ibt_data_clean$race <- ifelse(grepl("white", ibt_data_clean$race, ignore.case = T),
                              "white", ibt_data_clean$race)
ibt_data_clean$race <- ifelse(ibt_data_clean$race %in% c("white", "asian"),
                              ibt_data_clean$race, "other")

ibt_data_clean$duration_in_seconds <- as.numeric(ibt_data_clean$duration_in_seconds)
ibt_data_clean$q65_page_submit <- as.numeric(ibt_data_clean$q65_page_submit)
ibt_data_clean$q66_page_submit <- as.numeric(ibt_data_clean$q66_page_submit)

# Question 2a

avg_time <- mean(ibt_data_clean$q65_page_submit, na.rm = T)

native_q65_stat <- ibt_data_clean %>%
  group_by(english) %>%
  summarise(
    above_average = paste0(mean(q65_page_submit >= avg_time, na.rm = T) * 100, "%"),
    below_average = paste0(mean(q65_page_submit < avg_time, na.rm = T) * 100, "%")) %>%
  bind_rows(
    ibt_data_clean %>%
      summarise(
        english = "Total",
        above_average = paste0(mean(q65_page_submit >= avg_time, na.rm = T) * 100, "%"),
        below_average = paste0(mean(q65_page_submit < avg_time, na.rm = T) * 100, "%")
      )
  )


native_q65_stat$above_average <- as.numeric(sub("%", "", native_q65_stat$above_average))
native_q65_stat$below_average <- as.numeric(sub("%", "", native_q65_stat$below_average))

# The Total Column

native_q65_stat$Total <- rowSums(native_q65_stat[, c("above_average", "below_average")], na.rm = T)

native_q65_stat$above_average <- paste0(sprintf("%.1f%%", native_q65_stat$above_average))
native_q65_stat$below_average <- paste0(sprintf("%.1f%%", native_q65_stat$below_average))
native_q65_stat$Total <- paste0(sprintf("%.1f%%", native_q65_stat$Total))
colnames(native_q65_stat)[colnames(native_q65_stat) == "english"] <- "native english/q65_indicator"

# 2b 

avg_time2 <- mean(ibt_data_clean$q66_page_submit, na.rm = T)

native_q66_stat <- ibt_data_clean %>%
  group_by(english) %>%
  summarise(
    above_average = paste0(mean(q66_page_submit >= avg_time2, na.rm = T) * 100, "%"),
    below_average = paste0(mean(q66_page_submit < avg_time2, na.rm = T) * 100, "%")) %>%
  bind_rows(
    ibt_data_clean %>%
      summarise(
        english = "Total",
        above_average = paste0(mean(q66_page_submit >= avg_time2, na.rm = T) * 100, "%"),
        below_average = paste0(mean(q66_page_submit < avg_time2, na.rm = T) * 100, "%")
      )
  )


native_q66_stat$above_average <- as.numeric(sub("%", "", native_q66_stat$above_average))
native_q66_stat$below_average <- as.numeric(sub("%", "", native_q66_stat$below_average))

# The Total Column

native_q66_stat$Total <- rowSums(native_q66_stat[, c("above_average", "below_average")], na.rm = T)

native_q66_stat$above_average <- paste0(sprintf("%.1f%%", native_q66_stat$above_average))
native_q66_stat$below_average <- paste0(sprintf("%.1f%%", native_q66_stat$below_average))
native_q66_stat$Total <- paste0(sprintf("%.1f%%", native_q66_stat$Total))
colnames(native_q66_stat)[colnames(native_q66_stat) == "english"] <- "native english/q66_indicator"


# Box Plot

install.packages("reshape2")
library(reshape2)



box_plot_table <- ibt_data_clean %>%
  select(q65_page_submit, q66_page_submit, english) %>% rename(`Question 65` = q65_page_submit,
                                                               `Question 66` = q66_page_submit) %>%
  melt(id = "english") 


box_plot_plot <- box_plot_table %>% ggplot(aes(x = variable, y = value, color = english)) + 
  geom_boxplot() +
  theme(plot.title = element_text(hjust = .5, size = 14), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA)) + labs(
          title = "Response Time: Native vs Non-Native English Speakers",
          x = "Question",
          y = "Time (seconds)",
          color = "English"
        ) + scale_color_discrete(labels = c("Non-Native", "Native"))


# Second Plot
new_65 <- native_q65_stat[-3, ] %>%
  select(-4)

new_66 <- native_q66_stat[-3, ] %>%
  select(-4)

averages_table <- full_join(new_65, new_66) %>%
  mutate(
    english = coalesce(`native english/q65_indicator`, `native english/q66_indicator`)
  ) %>% select(above_average, below_average, english)

question <- c("65", "65", "66", "66")

real_table <- averages_table %>%
  mutate(
    Question = question
  ) %>%
  melt(id.vars = c("Question", "english")) %>% mutate(
    variable2 = paste0(english, variable)
  )
  
second_plot <- real_table %>% ggplot(aes(x = Question, y = value, fill = variable2)) + 
  geom_bar(position = "dodge", stat = "identity") + labs(
    title = "Performance Statistics by English Status",
    y = "Percentage",
    fill = "Legend"
  ) +
  theme(plot.title = element_text(hjust = .5, size = 14), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA)) + 
  scale_fill_discrete(
    labels = 
                         c("Above Average Non-Native", 
                           "Below Average Non-Native", 
                           "Above Average Native", 
                           "Below Average Native"))


                       
                       
                       
                       
 