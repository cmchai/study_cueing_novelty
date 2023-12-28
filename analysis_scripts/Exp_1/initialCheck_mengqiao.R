############ the initial check of S-PRO data ################# 

### set wd and load packages ###

setwd("D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data")
library(tidyverse)
library(feather)


### import data and merge them into one file ###

dir_rawdata <- "D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data"
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
names(bigdf)
unique(bigdf$browser_name)
sona <- unique(bigdf$sona)
age <- unique(bigdf$age)

sona_times <- bigdf %>%
  group_by(sona) %>%
  summarise(count = n())

duplicate_subject <- bigdf %>%
  filter(sona == 8326)

unique(duplicate_subject$subject) 

# get rid of the data from the second try:exh286ez
bigdf <- filter(bigdf, subject != "exh286ez")

aggregate(bigdf$browser_name,
           by = list(subject = bigdf$subject),
           unique)


### check if all the subjects finish all the blocks and trials ###
subject_trial_finish <- aggregate(bigdf$trial_num,
                                  by = list(block = bigdf$block,
                                            subject = bigdf$sona),
                                  max,
                                  na.rm=TRUE)


### check the target Rt mean and distribution ###
bigdf <- read_feather("bigdf_full.feather")

unique(bigdf$trial_part)
subset_target <- filter(bigdf, trial_part == "target")
subset_taskcue <- filter(bigdf, trial_part == "taskcue")

subset_target$rt_log <- log(subset_target$rt)  # log the rt data


aggregate(bigdf$rt,
          by = list(subject = bigdf$subject,
                    interval = bigdf$trial_part),
          mean,
          na.rm=TRUE)

slow_taskcue_subject <- bigdf %>%
  filter(subject == "ge924m6y", block_dich == "test", trial_part == "taskcue")

ggplot(slow_taskcue_subject, aes(rt)) +
  geom_histogram(colour="black", fill="white")+
  geom_density(alpha=.2, fill="#B0E0E6")


### check the RT distribution of each subject ###

ggplot(subset_target, aes(rt)) +
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=.2, fill="#B0E0E6")+
  facet_wrap(~ subject, nrow = 10)

ggplot(subset_taskcue, aes(rt)) +
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=.2, fill="#B0E0E6")+
  facet_wrap(~ subject, nrow = 10)

ggplot(subset_taskcue, aes(rt)) +
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=.2, fill="#B0E0E6")+
  facet_grid(~ subject, space = "free_x", scales = "free")

ggplot(subset_taskcue, aes(rt)) +
  geom_histogram(colour="black", fill="white")+
  facet_wrap(~ subject, nrow = 3)+
  xlim(3000, NA)+
  ylim(NA, 50)+
  stat_bin(bins = 25, aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
  geom_vline(xintercept = c(3000,20000), linetype="dotted", 
             color = "blue", size=1.5)


fun_longtaskcue <- function(rt_vec) {
  nlongrt <- sum(rt_vec > 3000, na.rm = TRUE)
  return(nlongrt)
}

aggregate(subset_taskcue$rt,
          by = list(subject = subset_taskcue$subject),
          fun_longtaskcue)


aggregate(subset_target$rt,
          by = list(subject = subset_target$subject,
                    blocktype = subset_target$block_type),
          mean,
          na.rm=TRUE)

aggregate(subset_target$rt,
          by = list(blockID = subset_target$block),
          mean,
          na.rm=TRUE)

aggregate(subset_target$rt,
          by = list(logical = subset_target$logical_rule),
          mean,
          na.rm=TRUE)

### check the accuracy rate ###
acc_overall <- sum(subset_target$accuracy)/length(subset_target$accuracy)

fun_accrate <- function(acc_vec){
  acc_rate <- sum(acc_vec)/length(acc_vec)
  return(acc_rate)
}

aggregate(subset_target$accuracy,
          by = list(subject = subset_target$subject),
          fun_accrate)

aggregate(subset_target$accuracy,
          by = list(subject = subset_target$subject,
                    blocktype = subset_target$block_type),
          fun_accrate)

aggregate(subset_target$accuracy,
          by = list(blocktype = subset_target$block_type,
                    tasktype = subset_target$prac_or_novel),
          fun_accrate)

### check missing value ###
sum(is.na(subset_target$rt))


################## savedata for the next processing stage #####################
library(feather)
write_feather(bigdf, "bigdf_full_for_age.feather")

