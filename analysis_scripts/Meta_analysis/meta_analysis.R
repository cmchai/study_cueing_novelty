############# Between-Experiment Meta-Analysis ##############
###### For 4 PRO Experiment in terms of the Cue effect ######

library(here)
library(tidyverse)
library(ez)
library(feather)
library(rstatix)
library(wesanderson)
library(ggsignif)
library(esc)
library(meta)

###########################################################
################### Combining DataSets ####################-----

################### For Reaction Time ####################
# load the dataframe of data from experiment 1
exp1_file <- "D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data/bigdf_clean_full_new.feather"

bigdf_exp1 <- read_feather(exp1_file) %>%
  mutate(study = 1) %>%
  filter(trial_part == "target")

# load the dataframe of data from experiment 2
exp2_file <- "D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data/2nd_batch/bigdf_clean_full_2.feather"

bigdf_exp2 <- read_feather(exp2_file) %>%
  mutate(study = 2)

# load the dataframe of data from experiment 3
exp3_file <- "D:/Ghent_Braem/Experiment/1st_research_PRO/3rd_experiement/results/bigdf_clean.Rdata"

load(file = exp3_file)

bigdf_exp3 <- bigdf_clean %>%
  mutate(study = 3)

rm(bigdf_clean)

# load the dataframe of data from experiment 4
exp4_file <- "D:/Ghent_Braem/Experiment/1st_research_PRO/3rd_experiement/Replication/results/bigdf_clean.Rdata"

load(file = exp4_file)

bigdf_exp4 <- bigdf_clean %>%
  mutate(study = 4)

rm(bigdf_clean)

# Bind all 4 datasets

hugedf <- bind_rows(bigdf_exp1, bigdf_exp2, bigdf_exp3, bigdf_exp4) %>%
  select(study, subject, block_type, prac_or_novel, rt, accuracy) %>%
  filter(block_type %in% c("testNocue", "testCue"))

################ For Accuracy Rate  #################
# load the dataframe of data from experiment 1
exp1_acc_file <- "D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data/bigdf_clean_error_full_new.feather"

bigdf_exp1_acc <- read_feather(exp1_acc_file) %>%
  mutate(study = 1) %>%
  filter(trial_part == "target", !is.na(rt))


# load the dataframe of data from experiment 2
exp2_acc_file <- "D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data/2nd_batch/bigdf_clean_error_full_2.feather"

bigdf_exp2_acc <- read_feather(exp2_acc_file) %>%
  mutate(study = 2) %>%
  filter(trial_part == "target", !is.na(rt))

# load the dataframe of data from experiment 3
exp3_acc_file <- "D:/Ghent_Braem/Experiment/1st_research_PRO/3rd_experiement/results/bigdf_clean_error.Rdata"

load(file = exp3_acc_file)

bigdf_exp3_acc <- bigdf_clean_error %>%
  mutate(study = 3) %>%
  filter(trial_part == "target", !is.na(rt))

rm(bigdf_clean_error)

# load the dataframe of data from experiment 4
exp4_acc_file <- "D:/Ghent_Braem/Experiment/1st_research_PRO/3rd_experiement/Replication/results/bigdf_clean_error.Rdata"

load(file = exp4_acc_file)

bigdf_exp4_acc <- bigdf_clean_error %>%
  mutate(study = 4)%>%
  filter(trial_part == "target", !is.na(rt))

rm(bigdf_clean_error)

# Bind all 4 datasets

hugedf_acc <- bind_rows(bigdf_exp1_acc, bigdf_exp2_acc, bigdf_exp3_acc, bigdf_exp4_acc) %>%
  select(study, subject, block_type, prac_or_novel, rt, accuracy) %>%
  filter(block_type %in% c("testNocue", "testCue"))

save(hugedf, hugedf_acc, file = "D:/Ghent_Braem/Experiment/1st_research_PRO/Between_Exp/data/hugedfs.Rdata")


###########################################################
########### Meta Analysis on the Reaction time ############
###########################################################

hugedfs_path = "/Users/mengqiao/Documents/PRO_exps/Between_Exp/data/hugedfs.Rdata"
load(file = hugedfs_path)

#### Main effect of Block type ----

blocktype_corr <- hugedf %>%
  group_by(study, subject, block_type) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = block_type, values_from = meanRT) %>%
  group_by(study) %>%
  summarise(r = cor(testCue, testNocue))
  
mean_and_sd <- hugedf %>%
  group_by(study, subject, block_type) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(study, block_type) %>%
  summarise(M = round(mean(meanRT),2),
            SD = round(sd(meanRT),2),
            N = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = block_type, values_from = c(M, SD, N)) %>%
  rename()

for_effect_size <- mean_and_sd %>%
  left_join(blocktype_corr, by = "study")

meta_ef <- esc_mean_sd(grp1m = for_effect_size$M_testCue,
                       grp1sd = for_effect_size$SD_testCue,
                       grp1n = for_effect_size$N_testCue,
                       grp2m = for_effect_size$M_testNocue,
                       grp2sd = for_effect_size$SD_testNocue,
                       grp2n = for_effect_size$N_testNocue,
                       r = for_effect_size$r,
                       study = for_effect_size$study,
                       es.type = "d") %>% 
  as.data.frame()

meta_rt <- metagen(meta_ef$es, 
                   meta_ef$se, 
                   data = meta_ef,
                   studlab = paste(c("exp1", "exp2", "exp3","exp4")), 
                   comb.fixed = TRUE, 
                   comb.random = FALSE,
                   prediction = TRUE, sm= "MD")

summary(meta_rt)

forest(meta_rt, leftlabs = c("Study", "d", "SE"))

#### Interaction between Block type and task novelty ----

Inter_corr <- hugedf %>%
  mutate(task_novelty = if_else((prac_or_novel == "gekende taak" | prac_or_novel == "prac task"), "prac", "novel")) %>%
  select(-prac_or_novel) %>%
  group_by(study, subject, block_type, task_novelty) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(block_type, task_novelty), values_from = meanRT) %>%
  mutate(cue_benefit_novel = testNocue_novel - testCue_novel,
         cue_benefit_prac = testNocue_prac - testCue_prac) %>%
  ungroup() %>%
  group_by(study) %>%
  summarise(r = cor(cue_benefit_novel, cue_benefit_prac))

Inter_mean_and_sd <- hugedf %>%
  mutate(task_novelty = if_else((prac_or_novel == "gekende taak" | prac_or_novel == "prac task"), "prac", "novel")) %>%
  select(-prac_or_novel) %>%
  group_by(study, subject, block_type, task_novelty) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(block_type, task_novelty), values_from = meanRT) %>%
  mutate(cue_benefit_novel = testNocue_novel - testCue_novel,
         cue_benefit_prac = testNocue_prac - testCue_prac) %>%
  ungroup() %>%
  select(-c(testNocue_novel, testCue_novel, testNocue_prac, testCue_prac)) %>%
  pivot_longer(c(cue_benefit_novel, cue_benefit_prac), names_to = "task_novelty", values_to = "cue_benefit") %>%
  group_by(study, task_novelty) %>%
  summarise(M = round(mean(cue_benefit),2),
            SD = round(sd(cue_benefit),2),
            N = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = task_novelty, values_from = c(M, SD, N)) %>%
  rename()

for_inter_effect_size <- Inter_mean_and_sd %>%
  left_join(Inter_corr, by = "study")

meta_ef_inter <- esc_mean_sd(grp1m = for_inter_effect_size$M_cue_benefit_novel,
                             grp1sd = for_inter_effect_size$SD_cue_benefit_novel,
                             grp1n = for_inter_effect_size$N_cue_benefit_novel,
                             grp2m = for_inter_effect_size$M_cue_benefit_prac,
                             grp2sd = for_inter_effect_size$SD_cue_benefit_prac,
                             grp2n = for_inter_effect_size$N_cue_benefit_prac,
                             r = for_inter_effect_size$r,
                             study = for_inter_effect_size$study,
                             es.type = "d") %>% 
  as.data.frame()

meta_rt_inter <- metagen(meta_ef_inter$es, 
                         meta_ef_inter$se, 
                         data = meta_ef_inter,
                         studlab = paste(c("exp1", "exp2", "exp3","exp4")), 
                         comb.fixed = TRUE, 
                         comb.random = FALSE,
                         prediction = TRUE, sm= "MD")

summary(meta_rt_inter)

forest(meta_rt_inter, leftlabs = c("Study", "d", "SE"))

###########################################################
############# Meta Analysis on the Accuracy ###############
###########################################################

#### Main effect of Block type ----
blocktype_acc_corr <- hugedf_acc %>%
  group_by(study, subject, block_type) %>%
  summarise(acc = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  pivot_wider(names_from = block_type, values_from = acc) %>%
  group_by(study) %>%
  summarise(r = cor(testCue, testNocue))

mean_and_sd_acc <- hugedf_acc %>%
  group_by(study, subject, block_type) %>%
  summarise(acc = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  group_by(study, block_type) %>%
  summarise(M = round(mean(acc),2),
            SD = round(sd(acc),2),
            N = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = block_type, values_from = c(M, SD, N)) %>%
  rename()

for_acc_effect_size <- mean_and_sd_acc %>%
  left_join(blocktype_acc_corr, by = "study")

meta_ef_acc <- esc_mean_sd(grp1m = for_acc_effect_size$M_testCue,
                           grp1sd = for_acc_effect_size$SD_testCue,
                           grp1n = for_acc_effect_size$N_testCue,
                           grp2m = for_acc_effect_size$M_testNocue,
                           grp2sd = for_acc_effect_size$SD_testNocue,
                           grp2n = for_acc_effect_size$N_testNocue,
                           r = for_acc_effect_size$r,
                           study = for_acc_effect_size$study) %>% 
  as.data.frame()

meta_acc <- metagen(meta_ef_acc$es, 
                    meta_ef_acc$se, 
                    data = meta_ef_acc,
                    studlab = paste(c("exp1", "exp2", "exp3","exp4")), 
                    comb.fixed = TRUE, 
                    comb.random = FALSE,
                    prediction = TRUE, sm= "MD")

summary(meta_acc)

forest(meta_acc, leftlabs = c("Study", "d", "SE"))

#### Interaction between Block type and task novelty ----

Inter_acc_corr <- hugedf_acc %>%
  mutate(task_novelty = if_else((prac_or_novel == "gekende taak" | prac_or_novel == "prac task"), "prac", "novel")) %>%
  select(-prac_or_novel) %>%
  group_by(study, subject, block_type, task_novelty) %>%
  summarise(acc = sum(accuracy)/length(accuracy)) %>%
  pivot_wider(names_from = c(block_type, task_novelty), values_from = acc) %>%
  mutate(cue_benefit_novel = testCue_novel - testNocue_novel,
         cue_benefit_prac = testCue_prac - testNocue_prac) %>%
  ungroup() %>%
  group_by(study) %>%
  summarise(r = cor(cue_benefit_novel, cue_benefit_prac))

Inter_mean_and_sd_acc <- hugedf_acc %>%
  mutate(task_novelty = if_else((prac_or_novel == "gekende taak" | prac_or_novel == "prac task"), "prac", "novel")) %>%
  select(-prac_or_novel) %>%
  group_by(study, subject, block_type, task_novelty) %>%
  summarise(acc = sum(accuracy)/length(accuracy)) %>%
  pivot_wider(names_from = c(block_type, task_novelty), values_from = acc) %>%
  mutate(cue_benefit_novel = testCue_novel - testNocue_novel,
         cue_benefit_prac = testCue_prac - testNocue_prac) %>%
  ungroup() %>%
  select(-c(testNocue_novel, testCue_novel, testNocue_prac, testCue_prac)) %>%
  pivot_longer(c(cue_benefit_novel, cue_benefit_prac), names_to = "task_novelty", values_to = "cue_benefit") %>%
  group_by(study, task_novelty) %>%
  summarise(M = round(mean(cue_benefit),2),
            SD = round(sd(cue_benefit),2),
            N = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = task_novelty, values_from = c(M, SD, N)) %>%
  rename()

for_inter_acc_effect_size <- Inter_mean_and_sd_acc %>%
  left_join(Inter_acc_corr, by = "study")

meta_ef_acc_inter <- esc_mean_sd(grp1m = for_inter_acc_effect_size$M_cue_benefit_novel,
                                grp1sd = for_inter_acc_effect_size$SD_cue_benefit_novel,
                                grp1n = for_inter_acc_effect_size$N_cue_benefit_novel,
                                grp2m = for_inter_acc_effect_size$M_cue_benefit_prac,
                                grp2sd = for_inter_acc_effect_size$SD_cue_benefit_prac,
                                grp2n = for_inter_acc_effect_size$N_cue_benefit_prac,
                                r = for_inter_acc_effect_size$r,
                                study = for_inter_acc_effect_size$study,
                                es.type = "d") %>% 
  as.data.frame()

meta_acc_inter <- metagen(meta_ef_acc_inter$es, 
                          meta_ef_acc_inter$se, 
                         data = meta_ef_acc_inter,
                         studlab = paste(c("exp1", "exp2", "exp3","exp4")), 
                         comb.fixed = TRUE, 
                         comb.random = FALSE,
                         prediction = TRUE, sm= "MD")

summary(meta_acc_inter)

forest(meta_acc_inter, leftlabs = c("Study", "d", "SE"))

