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
  

race_count_clean <- ibt_data_clean %>%
  count(race)

# 6
avg_time <- mean(ibt_data_clean$q65_page_submit, na.rm = T)

race_q65_stat <- ibt_data_clean %>%
  group_by(race) %>%
  summarise(
    above_average = paste0(mean(q65_page_submit >= avg_time, na.rm = T) * 100, "%"),
    below_average = paste0(mean(q65_page_submit < avg_time, na.rm = T) * 100, "%")) %>%
      bind_rows(
        ibt_data_clean %>%
          summarise(
            race = "Total",
            above_average = paste0(mean(q65_page_submit >= avg_time, na.rm = T) * 100, "%"),
            below_average = paste0(mean(q65_page_submit < avg_time, na.rm = T) * 100, "%")
          )
      )
  

race_q65_stat$above_average <- as.numeric(sub("%", "", race_q65_stat$above_average))
race_q65_stat$below_average <- as.numeric(sub("%", "", race_q65_stat$below_average))

# The Total Column

race_q65_stat$Total <- rowSums(race_q65_stat[, c("above_average", "below_average")], na.rm = T)

race_q65_stat$above_average <- paste0(sprintf("%.1f%%", race_q65_stat$above_average))
race_q65_stat$below_average <- paste0(sprintf("%.1f%%", race_q65_stat$below_average))
race_q65_stat$Total <- paste0(sprintf("%.1f%%", race_q65_stat$Total))
colnames(race_q65_stat)[colnames(race_q65_stat) == "race"] <- "race/q65_indicator"

# 7

avg_time2 <- mean(ibt_data_clean$q66_page_submit, na.rm = T)

race_q66_stat <- ibt_data_clean %>%
  group_by(race) %>%
  summarise(
    above_average = paste0(mean(q66_page_submit >= avg_time2, na.rm = T) * 100, "%"),
    below_average = paste0(mean(q66_page_submit < avg_time2, na.rm = T) * 100, "%")) %>%
  bind_rows(
    ibt_data_clean %>%
      summarise(
        race = "Total",
        above_average = paste0(mean(q66_page_submit >= avg_time2, na.rm = T) * 100, "%"),
        below_average = paste0(mean(q66_page_submit < avg_time2, na.rm = T) * 100, "%")
      )
  )


race_q66_stat$above_average <- as.numeric(sub("%", "", race_q66_stat$above_average))
race_q66_stat$below_average <- as.numeric(sub("%", "", race_q66_stat$below_average))



race_q66_stat$Total <- rowSums(race_q66_stat[, c("above_average", "below_average")], na.rm = T)

race_q66_stat$above_average <- paste0(sprintf("%.1f%%", race_q66_stat$above_average))
race_q66_stat$below_average <- paste0(sprintf("%.1f%%", race_q66_stat$below_average))
race_q66_stat$Total <- paste0(sprintf("%.1f%%", race_q66_stat$Total))
colnames(race_q66_stat)[colnames(race_q66_stat) == "race"] <- "race/q66_indicator"


# Bar plot

new_table <- tibble(
  Race = c("Asian", "Other", "White", "Total"),
  `Part 1` = race_q65_stat$above_average,
  `Part 2` = race_q66_stat$above_average
) %>% arrange(`Part 1`) %>% mutate(
  Race = fct_inorder(Race)
)

new_table %>% gather(key, value, -1) %>% ggplot(aes(x = Race, y = value, fill = key)) + 
  geom_bar(position = "dodge", stat = "identity") + theme(plot.title = element_text(hjust = .5, size = 14), 
                                                          panel.background = element_rect(fill = "white"),
                                                          panel.grid = element_blank(),
                                                          panel.border = element_rect(fill = NA)) +
  labs(
    title = "Percent Above Average",
    y = "Percentage",
    fill = "Test Section"
  ) 





# Other graph


ggplot(new_table, aes(x = `Part 1`, y = `Part 2`)) + geom_text(aes(label = Race)) + 
  theme(plot.title = element_text(hjust = .5, size = 14), 
        panel.border = element_rect(fill = NA)) +
  labs(
    title = "Percent Above Average By Section"
  )

