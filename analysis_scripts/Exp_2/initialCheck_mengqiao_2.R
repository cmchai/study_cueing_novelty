############ the initial check of S-PRO data ################# 

### set wd and load packages ###

setwd("D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data/2nd_batch")
library(tidyverse)
library(feather)
library(rstatix)

### import data and merge them into one file ###

dir_rawdata <- "D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data/2nd_batch"
list_rawdata <- list.files(path = dir_rawdata,
                                   pattern = "*.csv",
                                   full.names = TRUE) %>%
  lapply(read_csv)


fun_RtToDbl <- function(dataframe) {
  if (typeof(dataframe$rt) != "double"){
    dataframe$rt <- parse_double(dataframe$rt, na = "null")
  }
  if ("os_version" %in% colnames(dataframe)) {
    dataframe$os_version <- NULL
  }
  if ("key_press" %in% colnames(dataframe)) {
    if (typeof(dataframe$key_press) != "double"){
      dataframe$key_press <- parse_double(dataframe$key_press, na = "null")
    }
  }
  if ("age" %in% colnames(dataframe)) {
    dataframe$age <- as.character(dataframe$age)
    dataframe$age <- as.integer(substring(dataframe$age, 1, 2))
  }
  return(dataframe)
}

nlist_rawdata <- lapply(list_rawdata, fun_RtToDbl)
bigdf <- bind_rows(nlist_rawdata)

### check the browser information ###
bigdf <- read_feather("bigdf_full_2.feather")
subset_target <- filter(bigdf, trial_part == "target")

names(bigdf)
unique(bigdf$browser_name)
sona <- unique(bigdf$sona)
age <-  unique(bigdf$age)

### add a column specifying either prac or test session ###
bigdf$block_dich[bigdf$block_type %in% c("prac", "pracMix")] <- "prac"
bigdf$block_dich[bigdf$block_type %in% c("testCue", "testNocue")] <- "test"

### check the RT distribution of each subject ###

ggplot(subset_target, aes(rt)) +
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=.2, fill="#B0E0E6")+
  facet_wrap(~ subject, nrow = 10, scales = "free_y")


summary_target <- bigdf %>%
  filter(trial_part == "target") %>%
  group_by(subject, block_dich) %>%
  summarise(sonaID = unique(sona),
            count = n(),
            meanRT = mean(rt, na.rm = TRUE),
            accRate = sum(accuracy)/length(accuracy),
            naRate = sum(is.na(rt))/length(accuracy)) %>%
  ungroup() %>%
  filter(accRate < 0.6 | naRate > 0.1)

unique(summary_target$sonaID)

ggplot(summary_target, aes(x = meanRT)) +
  geom_dotplot() +
  scale_y_continuous(NULL, breaks = NULL)

ggplot(summary_target, aes(x = accRate)) +
  geom_dotplot() +
  scale_y_continuous(NULL, breaks = NULL)

ggplot(summary_target, aes(x = naRate)) +
  geom_dotplot() +
  scale_y_continuous(NULL, breaks = NULL)

### check the data for each weird subject ###

weird_sub_1 <- subset_target %>%
  filter(subject == "2e0nrgzd", block_dich == "test")

weird_sub_2 <- subset_target %>%
  filter(subject == "49y3n352", !is.na(rt)) %>%
  convert_as_factor(key_press, logical_rule) %>%
  group_by(key_press) %>%
  summarise(count = n())
  

################## save data for the next processing stage #####################
library(feather)

write_feather(bigdf, "bigdf_full_2.feather")
write_feather(bigdf, "bigdf_full_2_for_age.feather")

