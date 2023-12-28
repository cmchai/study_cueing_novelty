#####################################################################################
#################### the initial check of PRO3 replication study ####################
#####################################################################################

######################### import packages and data #########################
library(here)
library(tidyverse)
dodge <- position_dodge(width = 0.7)

data_path <- here("data")

bigdf <- read_csv(paste0(data_path, "/Mengqiao_PRO3_1rep_data_log_final.csv"),
                  col_types = list(
                    rt = col_double(),
                    key_press = col_integer(),
                    trial_num = col_integer(),
                    first_answer = col_logical(),
                    second_answer = col_logical(),
                    correct_answer = col_logical(),
                    accuracy = col_logical()
                  ),
                  na = "NULL") %>%
  mutate(section = ifelse(str_detect(block, "prac"), "prac", "test"))

glimpse(bigdf)

################# check browser information and duplication ###############

names(bigdf)

unique(bigdf$browser_name)

id_times <- bigdf %>%
  group_by(prolific) %>%
  summarise(count = n())

id_taskcomplete <- bigdf %>%
  filter(!is.na(block)) %>%
  group_by(prolific, block) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(block) %>%
  summarise(count = n()) %>%
  ungroup()

######################## get rid of incomplete data ######################
n_entry <- 722   # although there is one participant who did not finish the survey at the end

id_incomplete <- bigdf %>%
  group_by(prolific) %>%
  summarise(count = n()) %>%
  filter(count < n_entry) %>%
  pull(prolific)

class(id_incomplete)
is.vector(id_incomplete) 
is.list(id_incomplete)

bigdf <- bigdf %>%
  filter(!(prolific %in% id_incomplete))

length(unique(bigdf$prolific)) 

##########################################################################
#################### some basic descriptive statistics ###################
##########################################################################

############## choose the data set that you want to summarize ############

data_of_interest <- bigdf

length(unique(data_of_interest$prolific))

################# Average and Distribution of RT and ACC #################

##### RT summary #####
grand_meanRT <- data_of_interest %>%
  filter(trial_part == "target", section == "test") %>%  # get rid of the survey trials
  summarise(grand_meanRT = mean(rt, na.rm = TRUE)) %>%
  pull(grand_meanRT)

bysub_meanRT <- data_of_interest %>%
  filter(trial_part == "target", section == "test") %>%  # get rid of the survey trials, only look at testing section
  group_by(prolific) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE))

byblock_meanRT <- data_of_interest %>%
  filter(trial_part == "target", section == "test") %>%  # get rid of the survey trials
  group_by(prolific, block_type) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE))

byfam_meanRT <- data_of_interest %>%
  filter(trial_part == "target", section == "test") %>%  # get rid of the survey trials
  group_by(prolific, prac_or_novel) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE))

data_of_interest %>%
  filter(trial_part == "target") %>%
  drop_na(rt) %>%
  ggplot(aes(rt)) +
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=.2, fill="#B0E0E6")+
  facet_wrap(~ prolific, nrow = 7)

##### ACC summary #####
grand_acc_rate <- data_of_interest %>%
  filter(trial_part == "target") %>%  # get rid of the survey trials
  summarise(grand_accRate = sum(accuracy)/length(accuracy)) %>%
  pull(grand_accRate)

bysub_acc_rate <- data_of_interest %>%
  filter(trial_part == "target", test_begin_with == "testCue") %>%  # get rid of the survey trials
  group_by(date_open, prolific, section) %>%
  summarise(accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

ggplot(data = bysub_acc_rate, aes(x = prolific, y = accRate, fill = section)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
low_acc_sub <- bysub_acc_rate %>%  # low accuracy subjects
  filter(accRate < .6) %>%
  pull(prolific) %>%
  unique()

bysub_low_acc_rate <- data_of_interest %>%
  filter(trial_part == "target", prolific %in% low_acc_sub) %>%  # get rid of the survey trials
  group_by(prolific, block) %>%
  summarise(accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

byblock_acc_rate <- data_of_interest %>%
  filter(trial_part == "target", section == "test") %>%  # get rid of the survey trials
  group_by(prolific, block_type) %>%
  summarise(accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

byfam_acc_rate <- data_of_interest %>%
  filter(trial_part == "target", section == "test") %>%  # get rid of the survey trials
  group_by(prolific, prac_or_novel) %>%
  summarise(accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

#! comparison of 2 response ddl !#
byfam_acc_rate <- data_of_interest %>%
  filter(trial_part == "target", section == "test") %>%  # get rid of the survey trials
  group_by(date_open, prolific, prac_or_novel) %>%
  summarise(accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  mutate(responseDDL = case_when(date_open == "2022-07-25" ~ "2 sec",
                                 date_open == "2022-07-27" ~ "1.5 sec",
                                 TRUE ~ NA_character_))

(p <- ggplot(byfam_acc_rate, aes(x = prac_or_novel, y = accRate)) +
  geom_point(aes(color = responseDDL), size = 5) +
  geom_line(aes(group = prolific), color = "grey")) 
  

byfam_acc_rate %>%
  pivot_wider(
    names_from = prac_or_novel,
    values_from = c(accRate)
  ) %>%
  mutate(novel_cost = `prac task` - `novel task`) %>%
  ggplot(aes(x = responseDDL, y = novel_cost)) +
  geom_dotplot(binaxis='y', stackdir='center') +
  stat_summary(fun=mean, geom="point", shape=18,
               size=6, color="red") +
  geom_hline(yintercept=0, linetype='dotted', size = 1)

### check missing value ###
na_count <- data_of_interest %>%
  filter(is.na(rt)) %>%
  group_by(prolific) %>%
  summarise(count = n()) %>%
  ungroup()

### check if the starting block is counter-balanced ###
start_block <- data_of_interest %>%
  group_by(subject, test_begin_with) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(test_begin_with) %>%
  summarise(count = n())

################## save data for the next analysis stage #################

save(bigdf, file = paste0(here(), "/results/bigdf.Rdata"))
