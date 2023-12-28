############### preprocessing script ####################

#### all the functions ####


### Step 1. load big data frame and all the necessary packages ###

setwd("D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data/2nd_batch")
library(feather)
library(tidyverse)

bigdf <- read_feather("bigdf_full_2.feather")
glimpse(bigdf)

save(bigdf, file = "D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data/2nd_batch/raw_data_exp2.RData")

### Step 2. Add column specifying either the block is prac or test ###
blockType <- unique(bigdf$block_type)
filter(bigdf, is.na(bigdf$block_type)) # check what is NA in block_type means: survey items

### Add a column to generate a unique trial number combining subject, block, and trial information ###

bigdf <- mutate(bigdf, trialID = str_c(subject, block, trial_num, sep = "_"))

### Add another column specifying the test sequence: either testCue first or testNoCue first ###

first_test <- bigdf %>%
  filter(block == "test_1", block_type == "testCue")
  
first_testcue_subjects <- unique(first_test$subject)

bigdf$testSeq[bigdf$subject %in% first_testcue_subjects] <- "cuefirst"
bigdf$testSeq[!(bigdf$subject %in% first_testcue_subjects)] <- "nocuefirst"

bigdf %>%
  group_by(testSeq) %>%
  summarise(count = n()) %>% # check the data
  ungroup()

### demographic info before any data exclusion ###
bigdf_for_age <- read_feather("bigdf_full_2_for_age.feather")
length(unique(bigdf_for_age$subject))

demo_age <- bigdf_for_age %>%
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


#############################################
####### Subject level exclusion #############
#############################################


### Step 3. check the subject level acc rate for both prac and test blocks ###

# compute the outlier
lowacc_subject <- bigdf %>%
  filter(trial_part == "target") %>%
  group_by(subject, block_dich) %>%
  summarise(
    count = n(),
    accRate = sum(accuracy)/length(accuracy)
  ) %>%
  mutate(exclude = (block_dich == "prac" & accRate < 0.7) | (block_dich == "test" & accRate < 0.55)) %>%
  filter(exclude == TRUE) %>%
  ungroup()

# exclude the outliers
bigdf_ex_lowacc_sub <- filter(bigdf, !(subject %in% unique(lowacc_subject$subject))) # the dataframe without low acc subjects
sum(bigdf_ex_lowacc_sub$subject %in% unique(lowacc_subject$subject))                 # should be zero


### Step 4. compute the cross-subject mean of RT and ER and the boundry of subject-level exclusion ###

# for the target RT and acc rate(include both practice and test blocks)
target_outlier_subject <- bigdf_ex_lowacc_sub %>%
  filter(block_dich == "test" & trial_part == "target") %>%
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
write_feather(bigdf_clean_subject, "bigdf_cleansubject_2.feather")


# add a columne specifying if the trial is either repeat or switch, only for the testing section

trial_label_switch <- bigdf %>%
  filter(block_dich == "test", trial_part == "target") %>%
  group_by(subject, block) %>%
  mutate(prev_prac_or_novel = lag(prac_or_novel)) %>%
  ungroup() %>%
  mutate(switch_type = case_when((prac_or_novel == "gekende taak" & prev_prac_or_novel == "gekende taak") ~ "repeat_prac",
                                 (prac_or_novel == "gekende taak" & prev_prac_or_novel == "nieuwe taak") ~ "switch_prac",
                                 (prac_or_novel == "nieuwe taak" & prev_prac_or_novel == "nieuwe taak") ~ "repeat_novel",
                                 (prac_or_novel == "nieuwe taak" & prev_prac_or_novel == "gekende taak") ~ "switch_novel",
                                 TRUE ~ NA_character_
                                 )
         ) %>%
  select(trialID, switch_type)

write_feather(trial_label_switch, "switch_or_repeat_list.feather")
glimpse(trial_label_switch)



##########################################
############ trial level exclusion #######
##########################################


### Step 6. exclude trials with too short RT(200 ms) ###

short_RT <- bigdf_clean_subject %>%
  filter(block_dich == "test", trial_part == "target", rt <= 200)

short_trialID <- unique(short_RT$trialID)


### Step 7. exclude error trials and the post-error trials ###

error_trials <- bigdf_clean_subject %>%
  filter(block_dich == "test",  trial_part == "target", accuracy == FALSE) %>%
  mutate(next_trial_num = trial_num + 1,
         next_trialID = str_c(subject, block, next_trial_num, sep = "_"))

error_trialID <- unique(error_trials$trialID)
post_error_trialID <- unique(error_trials$next_trialID) # may include trial number above 60

length(error_trialID) - length(intersect(short_trialID, error_trialID)) # the number of error trials except the short trials


# exclude these short, error, and post-error trials

exclude_trials <- unique(c(short_trialID,
                          error_trialID,
                          post_error_trialID))

bigdf_ex_short_error_post <- filter(bigdf_clean_subject, !(trialID %in% exclude_trials))

(nrow(bigdf_clean_subject) - nrow(bigdf_ex_short_error_post)) - 1688 - 49 # the number of post-error trials



### Step 8. compute the within-subject mean and sd, set excluding threhold for each subject ###

# During the target part
outlier_target_trials <- bigdf_ex_short_error_post %>%
  group_by(subject) %>%
  filter(block_dich == "test", trial_part == "target") %>%
  mutate(mean_RT_subject = mean(rt, na.rm = TRUE),
         median_RT_subject = median(rt, na.rm = TRUE),
         sd_RT_subject = sd(rt, na.rm = TRUE),
         lowbound = mean_RT_subject - 2.5*sd_RT_subject,
         highbound = mean_RT_subject + 2.5*sd_RT_subject,
         lowquantile = unname(quantile(rt, na.rm = TRUE))[2],
         highquantile = unname(quantile(rt, na.rm = TRUE))[4],
         exclude = (rt < lowbound | rt > highbound)) %>%
  filter(exclude == TRUE)

outlier_target_trialID <- unique(outlier_target_trials$trialID)

bigdf_clean <- filter(bigdf_ex_short_error_post, !(trialID %in% outlier_target_trialID))

clean_target <- bigdf_clean %>%
  filter(block_dich == "test", trial_part == "target")

all_target <- bigdf %>%
  filter(block_dich == "test", trial_part == "target")

all_target_subject <- bigdf_clean_subject %>%
  filter(block_dich == "test", trial_part == "target")

nrow(clean_target)/nrow(all_target) # the percentage of remaining testing dataset
nrow(clean_target)/nrow(all_target_subject)

### final step. save the clean data before analysis ###

write_feather(bigdf_clean, "bigdf_clean_full_2.feather")

### final step 2. implementing data exclusion except for the error trials in order to compare the accuracy rate ###


# not exclude the erro trials
exclude_trial_error <- unique(c(short_trialID,
                                post_error_trialID))

bigdf_ex_short_post <- filter(bigdf_clean_subject, !(trialID %in% exclude_trial_error))



# recalculating the outliers During the target part
outlier_target_error_trials <- bigdf_ex_short_post %>%
  group_by(subject) %>%
  filter(block_dich == "test" & trial_part == "target") %>%
  mutate(mean_RT_subject = mean(rt, na.rm = TRUE),
         median_RT_subject = median(rt, na.rm = TRUE),
         sd_RT_subject = sd(rt, na.rm = TRUE),
         lowbound = mean_RT_subject - 2.5*sd_RT_subject,
         highbound = mean_RT_subject + 2.5*sd_RT_subject,
         lowquantile = unname(quantile(rt, na.rm = TRUE))[2],
         highquantile = unname(quantile(rt, na.rm = TRUE))[4],
         exclude = (rt < lowbound | rt > highbound)) %>%
  filter(exclude == TRUE)

outlier_target_error_trialID <- unique(outlier_target_error_trials$trialID)

bigdf_clean_error <- filter(bigdf_ex_short_post, !(trialID %in% outlier_target_error_trialID))

write_feather(bigdf_clean_error, "bigdf_clean_error_full_2.feather")






