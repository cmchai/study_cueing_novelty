############### preprocessing script -- PRO_3 ####################

###### Step 1. load big dataframe(bigdf) and all the necessary packages ######
library(here)
library(tidyverse)
library(rstatix)

load(file = paste0(here(), "/results/bigdf.Rdata"))
glimpse(bigdf)

### demographic info before any data exclusion ###
length(unique(bigdf$subject))

demo_age <- bigdf %>%
  group_by(subject, gender, age) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(
    meanAge = mean(age, na.rm = TRUE),
    minAge = min(age, na.rm = TRUE),
    maxAge = max(age, na.rm = TRUE),
    sdAge = sd(age, na.rm = TRUE)
  )

demo_gender <- demo_age %>%
  group_by(gender) %>%
  summarise(count = n())

###### ONLY analyze the test section ######
bigdf_test <- bigdf %>%
  filter(section == "test" & trial_part == "target")

length(unique(bigdf$subject))

#################################################################
################### Subject level exclusion #####################
#################################################################

### Step 2. check the subject level acc rate for both prac and test blocks ###

# compute the outliers
lowacc_subject <- bigdf %>%
  filter(trial_part == "target") %>%
  group_by(subject, section, test_begin_with) %>%
  summarise(
    count = n(),
    accRate = sum(accuracy)/length(accuracy)
  ) %>%
  mutate(exclude = (section == "prac" & accRate < 0.7) | (section == "test" & accRate < 0.55)) %>%
  filter(exclude == TRUE) %>%
  ungroup()

unique(lowacc_subject$subject)

# exclude the outliers
bigdf_ex_lowacc_sub <- filter(bigdf, !(subject %in% unique(lowacc_subject$subject))) # the data frame without low acc subjects
sum(bigdf_ex_lowacc_sub$subject %in% unique(lowacc_subject$subject))                 # should be zero


### Step 3. compute the cross-subject mean of RT and ER and the boundary of subject-level exclusion ###

# for the target RT and acc rate(ONLY consider test blocks) #
target_outlier_subject <- bigdf_ex_lowacc_sub %>%
  filter(section == "test" & trial_part == "target") %>%
  group_by(subject) %>%
  summarise(
    count = n(),
    meanRT = mean(rt, na.rm = TRUE),
    medianRT = median(rt, na.rm = TRUE),
    accRate = sum(accuracy)/length(accuracy)
  ) %>%
  mutate(
    grandmean_RT = mean(medianRT),
    sd_RT = sd(medianRT),
    lowBound_RT = grandmean_RT - 2.5*sd_RT,
    highBound_RT = grandmean_RT + 2.5*sd_RT,
    
    grandmean_acc = mean(accRate),
    sd_acc = sd(accRate),
    lowBound_acc = grandmean_acc - 2.5*sd_acc,
    highBound_acc = grandmean_acc + 2.5*sd_acc,
    
    exclude_RT = (medianRT < lowBound_RT | medianRT > highBound_RT),
    exclude_acc = (accRate < lowBound_acc | accRate > highBound_acc),
    exclude = exclude_RT | exclude_acc
  ) %>%
  filter(exclude == TRUE) %>%
  ungroup()


# exclude outliers
outlier_subject <- unique(target_outlier_subject$subject)
bigdf_clean_subject <- filter(bigdf_ex_lowacc_sub, ! (subject %in% outlier_subject))

# check if the starting block is counter-balanced #
start_block <- bigdf_clean_subject %>%
  group_by(subject, test_begin_with) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(test_begin_with) %>%
  summarise(count = n())

# add a column specifying if the trial is either repeat or switch, only for the testing section
bigdf_clean_subject <- bigdf_clean_subject %>%
  group_by(subject, block) %>%
  mutate(prev_prac_or_novel = lag(prac_or_novel)) %>%
  ungroup() %>%
  mutate(switch_type = case_when((prac_or_novel == "prac task" & prev_prac_or_novel == "prac task") ~ "repeat_prac",
                                 (prac_or_novel == "prac task" & prev_prac_or_novel == "novel task") ~ "switch_prac",
                                 (prac_or_novel == "novel task" & prev_prac_or_novel == "novel task") ~ "repeat_novel",
                                 (prac_or_novel == "novel task" & prev_prac_or_novel == "prac task") ~ "switch_novel",
                                 TRUE ~ NA_character_
                                 )
         ) %>%
  ungroup()

# add another column specifying how much overlapping in terms of task rule between neighboring trials
bigdf_clean_subject <- bigdf_clean_subject %>%
  group_by(subject, block) %>%
  mutate(prev_logical = lag(logical_rule),
         prev_semantic = lag(semantic_rule),
         prev_motor = lag(motor_rule)) %>%
  mutate(logical_overlap = (logical_rule == prev_logical),
         semantic_overlap = (semantic_rule == prev_semantic),
         motor_overlap = (motor_rule == prev_motor),
         rule_overlap = logical_overlap + semantic_overlap + motor_overlap) %>%
  ungroup()


save(bigdf_clean_subject, file = paste0(here(), "/results/bigdf_clean_subject.Rdata"))

#################################################################
##################### Trial level exclusion #####################
#################################################################

### Step 6. exclude trials with too short RT(200 ms) ###

short_trialID <- bigdf_clean_subject %>%
  filter(section == "test", trial_part == "target", rt <= 200) %>%
  pull(trial_id)

### Step 7. exclude error trials and the post-error trials ###

error_trials <- bigdf_clean_subject %>%
  filter(section == "test",  trial_part == "target", accuracy == FALSE) %>%
  mutate(next_trial_num = trial_num + 1,
         next_trial_id = str_c(subject, block, next_trial_num, sep = "_"))

error_trialID <- unique(error_trials$trial_id)
post_error_trialID <- unique(error_trials$next_trial_id) # may include trial number above 60

length(error_trialID) - length(intersect(short_trialID, error_trialID)) # the number of error trials except the short trials

# exclude these short, error, and post-error trials

exclude_trialID <- unique(c(short_trialID,
                            error_trialID,
                            post_error_trialID))

bigdf_ex_short_error_post <- filter(bigdf_clean_subject, !(trial_id %in% exclude_trialID))

(nrow(bigdf_clean_subject) - nrow(bigdf_ex_short_error_post)) - 2112 - 16 # the number of post-error trials


### Step 8. compute the within-subject mean and sd, set excluding threshold for each subject ###

# During the target part
outlier_target_trialID <- bigdf_ex_short_error_post %>%
  filter(section == "test", trial_part == "target") %>%
  group_by(subject) %>%
  mutate(mean_RT_subject = mean(rt, na.rm = TRUE),
         median_RT_subject = median(rt, na.rm = TRUE),
         sd_RT_subject = sd(rt, na.rm = TRUE),
         lowbound = mean_RT_subject - 2.5*sd_RT_subject,
         highbound = mean_RT_subject + 2.5*sd_RT_subject,
         lowquantile = unname(quantile(rt, na.rm = TRUE))[2],
         highquantile = unname(quantile(rt, na.rm = TRUE))[4],
         exclude = (rt < lowbound | rt > highbound)) %>%
  filter(exclude == TRUE) %>%
  ungroup() %>%
  pull(trial_id)

bigdf_clean <- filter(bigdf_ex_short_error_post, !(trial_id %in% outlier_target_trialID))

clean_target <- bigdf_clean %>%
  filter(section == "test", trial_part == "target")

all_target <- bigdf %>%
  filter(section == "test", trial_part == "target")

all_target_subject <- bigdf_clean_subject %>%
  filter(section == "test", trial_part == "target")

nrow(clean_target)/nrow(all_target) # the percentage of remaining testing dataset
nrow(clean_target)/nrow(all_target_subject)

### final step. save the clean data before analysis ###

save(bigdf_clean, file = paste0(here(), "/results/bigdf_clean.Rdata"))

###################################################################################################################
### final step 2. implementing data exclusion except for the error trials in order to compare the accuracy rate ###
###################################################################################################################

### not exclude the error trials ###
exclude_trialID2 <- unique(c(short_trialID,
                             post_error_trialID))

bigdf_ex_short_post <- filter(bigdf_clean_subject, !(trial_id %in% exclude_trialID2))

# recalculating the outliers during the target part
outlier_target_error_trialID <- bigdf_ex_short_post %>%
  group_by(subject) %>%
  filter(section == "test" & trial_part == "target") %>%
  mutate(mean_RT_subject = mean(rt, na.rm = TRUE),
         sd_RT_subject = sd(rt, na.rm = TRUE),
         lowbound = mean_RT_subject - 2.5*sd_RT_subject,
         highbound = mean_RT_subject + 2.5*sd_RT_subject,
         lowquantile = unname(quantile(rt, na.rm = TRUE))[2],
         highquantile = unname(quantile(rt, na.rm = TRUE))[4],
         exclude = (rt < lowbound | rt > highbound)) %>%
  filter(exclude == TRUE) %>%
  ungroup() %>%
  pull(trial_id)

bigdf_clean_error <- filter(bigdf_ex_short_post, !(trial_id %in% outlier_target_error_trialID))

save(bigdf_clean_error, file = paste0(here(), "/results/bigdf_clean_error.Rdata"))


