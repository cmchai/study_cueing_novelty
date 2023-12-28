################# statistical analysis ######################
setwd("D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data/2nd_batch")

library(tidyverse)
library(ez)
library(feather)
library(rstatix)
library(wesanderson)
library(papaja)
library(rempsyc)
library(emmeans)
library(patchwork)
library(BayesFactor)

options(contrasts = c("contr.sum", "contr.poly"))

#############################################################
############## Analysis on the reaction time ################
#############################################################

bigdf_clean <- read_feather("bigdf_clean_full_2.feather")
# bigdf_clean <- read_feather("bigdf_clean_highaccthre.feather") # the dataset after the high acc threshold exclusion criterion

glimpse(bigdf_clean)
dodge <- position_dodge(width = 0.7)
n_sub = length(unique(bigdf_clean$subject))

### demographic info after data exclusion ###
bigdf_for_age <- read_feather("bigdf_full_2_for_age.feather")

bigdf_clean_for_age <- bigdf_for_age %>%
  filter(subject %in% unique(bigdf_clean$subject))

length(unique(bigdf_clean_for_age$subject))

demo_age <- bigdf_clean_for_age %>%
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

##### split half violin plot ------
GeomSplitViolin <- ggproto(
  "GeomSplitViolin", 
  GeomViolin, 
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <- transform(data, 
                      xminv = x - violinwidth * (x - xmin), 
                      xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1,'group']
    newdata <- plyr::arrange(
      transform(data, x = if(grp%%2==1) xminv else xmaxv), 
      if(grp%%2==1) y else -y
    )
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin", 
                       grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    } else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <- function (mapping = NULL, 
                               data = NULL, 
                               stat = "ydensity", 
                               position = "identity", ..., 
                               draw_quantiles = NULL, 
                               trim = TRUE, 
                               scale = "area", 
                               na.rm = FALSE, 
                               show.legend = NA, 
                               inherit.aes = TRUE) {
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomSplitViolin, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(trim = trim, 
                      scale = scale, 
                      draw_quantiles = draw_quantiles, 
                      na.rm = na.rm, ...)
  )
}

### step1. sub-setting the data into target, survey, and practice vs. test session ###
test_target_df <- filter(bigdf_clean, block_dich == "test" & trial_part == "target")
unique(test_target_df$accuracy) # should be all TRUE

survey_val_df <- bigdf_clean %>%
  filter(trial_part == "survey_validity") %>%
  convert_as_factor(reponse)

subj_tasktype_help <- survey_val_df %>%
  filter(reponse %in% c(4,5)) %>% # 4 being "mildly helpful" and 5 being "very helpful"
  pull(subject)

survey_fam_df <- bigdf_clean %>%
  filter(trial_part == "survey_familiarity") %>%
  convert_as_factor(reponse)

#### step2. run RM ANOVA on the target RT ####----
unique(test_target_df$block_type) # 2 levels for cue_validity
unique(test_target_df$prac_or_novel) # 2 levels for familiarity
unique(test_target_df$testSeq) # 2 levels for test sequence, either cue first or nocue first

test_target_df$cue_validity <- as.factor(test_target_df$block_type)
test_target_df$task_familiarity <- as.factor(test_target_df$prac_or_novel)
test_target_df$test_sequence <- as.factor(test_target_df$testSeq)
test_target_df$subject_ID <- as.factor(test_target_df$subject)

glimpse(test_target_df)

#################### Descriptive Data ##################-----
# compute the subject level median RT for each condition
test_target_median <- test_target_df %>%
  group_by(subject, cue_validity, task_familiarity, test_sequence) %>%
  summarise(count = n(),
            medianRT = median(rt, na.rm = TRUE),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

test_target_median2 <- test_target_df %>%  # without including the starting block type as a factor, which should not change the following plots at all
  group_by(subject, cue_validity, task_familiarity) %>%
  summarise(count = n(),
            medianRT = median(rt, na.rm = TRUE),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

test_target_median$subject <- as.factor(test_target_median$subject)

# for computing the Bayes Factor for the null hypothesis regarding the cue effect
test_target_mean_forbayes <- test_target_df %>%
  group_by(subject, cue_validity) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  pivot_wider(names_from = cue_validity,
              values_from = meanRT) %>%
  mutate(cue_benefit = testNocue - testCue)

bf_target_cuebenefit = ttestBF(x = test_target_mean_forbayes$cue_benefit, nullInterval=c(0, Inf))
bf_target_cuebenefit
1/bf_target_cuebenefit

## for the people who reported that the task type cues were helpful ##
test_target_median_help <- test_target_median %>%
  filter(subject %in% subj_tasktype_help)

##### reporting for publication including mean and sd (main text)
targetRT_descrip_familiarity <- test_target_df %>%
  group_by(subject, task_familiarity) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(task_familiarity) %>%
  summarise(M = round(mean(meanRT),2),
            subsd = round(sd(meanRT),2),
            SE = round(subsd/((n_sub)^.5), 2)) %>%
  ungroup()

targetRT_descrip_cue <- test_target_df %>%
  group_by(subject, cue_validity) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(cue_validity) %>%
  summarise(M = round(mean(meanRT),2),
            subsd = round(sd(meanRT),2),
            SE = round(subsd/((n_sub)^.5), 2)) %>%
  ungroup()

##### reporting for publication including mean and sd (for the descriptive table)
targetRT_descrip2 <- test_target_df %>%
  group_by(subject, cue_validity, task_familiarity) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(cue_validity, task_familiarity) %>%
  summarise(M = round(mean(meanRT),3),
            subsd = sd(meanRT),
            SE = round(subsd/((n_sub)^.5), 3)) %>%
  ungroup() %>%
  mutate(block_type = if_else(cue_validity == "testCue", "informative", "uninformative"),
         task_type = if_else(task_familiarity == "gekende taak", "practiced", "novel")) %>%
  select(!c(cue_validity, task_familiarity, SE)) %>%  
  pivot_wider(names_from = c(block_type, task_type),
              values_from = c(M, subsd)) %>%
  select(1,5,2,6,3,7,4,8)

##### data visualization ####----
# for the subject-level distribution
(p_target_rt <- ggplot(test_target_median, aes(x = cue_validity, y = meanRT, fill = task_familiarity)) +
  geom_violin(position = dodge, size = 1) +
  scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
  geom_boxplot(width = 0.3, position = dodge, size = 1)  +
  stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
  ggtitle("factorized subject-level RT median") +
  theme_bw())


# for the trial level distribution
(p_target_rt_trial <- ggplot(test_target_df, aes(x = cue_validity, y = rt, fill = task_familiarity)) +
    geom_violin(position = dodge, size = 1) +
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
    geom_boxplot(width = 0.3, position = dodge, size = 1)  +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("factorized trial-level RT median") +
    theme_bw())

# plot for the publication
(p_target_rt2 <- ggplot(test_target_median, aes(x = cue_validity, y = meanRT, fill = task_familiarity)) +
    geom_split_violin(trim = FALSE, alpha = .3, width = .7)+
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .6),
                      name = "Task type:",
                      breaks = c("gekende taak", "nieuwe taak"),
                      labels = c("Practiced", "Novel")) +
    geom_boxplot(width = 0.34, fatten = NULL, show.legend = FALSE, size = 1, position = position_dodge(width = .4))  +
    stat_summary(fun = mean, geom="point", shape=20, size=5, color="black", position = position_dodge(.37)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = .2, position = position_dodge(width = .37))+
    theme_apa(base_size = 14) +
    labs(x = "block type", y = "Target reaction time") +
    scale_x_discrete(labels = c("testCue" = "informative", "testNocue" = "uninformative")) +
    theme(axis.title.x = element_blank(),
          legend.position = "none"))

############################################
############## run ANOVA ###################
### using ezANOVA ----
rt_target_anova <- ezANOVA(data = test_target_median, 
                           dv = .(meanRT), 
                           wid = .(subject), 
                           within = .(cue_validity, task_familiarity),
                           between = .(test_sequence),
                           type = 3)

rt_target_anova


# add some ez plot ----
rt_target_familiarity <- ezPlot(data = test_target_df,
                                dv = .(rt),
                                wid = .(subject),
                                within = .(task_familiarity),
                                x = .(task_familiarity),
                                do_lines = FALSE,
                                x_lab = 'task_familiarity',
                                y_lab = 'RT(ms)',
                                type = 2)

print(rt_target_familiarity)


rt_target_validity <- ezPlot(data = test_target_df,
                             dv = .(rt),
                             wid = .(subject),
                             within = .(cue_validity),
                             x = .(cue_validity),
                             do_lines = FALSE,
                             x_lab = 'cue_validity',
                             y_lab = 'RT(ms)',
                             type = 2)

print(rt_target_validity)

# try another package to run RM ANOVA ----
rt_target_anova2 <- anova_test(data = test_target_median, 
                               dv = medianRT, 
                               wid = subject, 
                               within = c(cue_validity, task_familiarity))

get_anova_table(rt_target_anova2)

### only look at the subjects who found the task type cue helpful ----

rt_target_help_anova <- ezANOVA(data = test_target_median_help,
                                dv = .(meanRT), 
                                wid = .(subject), 
                                within = .(cue_validity, task_familiarity),
                                type = 2)

rt_target_help_anova

####################################################
#### step 4. run RM ANOVA on accuracy rate data #### ----

bigdf_clean_error <- read_feather("bigdf_clean_error_full_2.feather")
# bigdf_clean_error <- read_feather("bigdf_clean_error_highaccthre.feather") # the dataset after the high acc threshold exclusion criterion

glimpse(bigdf_clean_error)

test_target_error_df <- bigdf_clean_error %>%
  filter(block_dich == "test" & trial_part == "target") %>%
  filter(!is.na(rt)) # delete the missing trial

test_target_error_df$cue_validity <- as.factor(test_target_error_df$block_type)
test_target_error_df$task_familiarity <- as.factor(test_target_error_df$prac_or_novel)
test_target_error_df$test_sequence <- as.factor(test_target_error_df$testSeq)

##### descriptive data on accuracy ####----
acc_rate_subject <- test_target_error_df %>%
  group_by(subject, cue_validity, task_familiarity, test_sequence) %>%
  summarise(count = n(),
            accuracy_rate = sum(accuracy)/length(accuracy))

# for computing the Bayes Factor for the null hypothesis regarding the cue effect
acc_rate_forbayes <- test_target_error_df %>%
  group_by(subject, cue_validity) %>%
  summarise(accuracy_rate = sum(accuracy)/length(accuracy)) %>%
  pivot_wider(names_from = cue_validity,
              values_from = accuracy_rate) %>%
  mutate(cue_benefit = testCue - testNocue)

bf_acc_cuebenefit = ttestBF(x = acc_rate_forbayes$cue_benefit, nullInterval=c(0, Inf))
bf_acc_cuebenefit
1/bf_acc_cuebenefit

## for the people who reported that the task type cues were helpful ##
acc_rate_subject_help <- acc_rate_subject %>%
  filter(subject %in% subj_tasktype_help)

##### reporting for publication including mean and sd (main text)
targetACC_descrip_familiarity <- test_target_error_df %>%
  group_by(subject, task_familiarity) %>%
  summarise(count = n(),
            accuracy_rate = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  group_by(task_familiarity) %>%
  summarise(M = round(mean(accuracy_rate),2),
            subsd = round(sd(accuracy_rate),2),
            SE = round(subsd/((n_sub)^.5), 2)) %>%
  ungroup()

##### reporting for publication including mean and sd 
targetACC_descrip2 <- test_target_error_df %>%
  group_by(subject, cue_validity, task_familiarity) %>%
  summarise(count = n(),
            accuracy_rate = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  group_by(cue_validity, task_familiarity) %>%
  summarise(M = round(mean(accuracy_rate),3),
            subsd = sd(accuracy_rate),
            SE = round(subsd/((n_sub)^.5), 3)) %>%
  ungroup() %>%
  mutate(block_type = if_else(cue_validity == "testCue", "informative", "uninformative"),
         task_type = if_else(task_familiarity == "gekende taak", "practiced", "novel")) %>%
  select(!c(cue_validity, task_familiarity, SE)) %>%  
  pivot_wider(names_from = c(block_type, task_type),
              values_from = c(M, subsd)) %>%
  select(1,5,2,6,3,7,4,8)

#### visualize the result ####----
(p_acc_rate <- ggplot(acc_rate_subject, aes(x = cue_validity, y = accuracy_rate, fill = task_familiarity)) +
  geom_violin(position = dodge, size = 1) +
  scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
  geom_boxplot(width = 0.3, position = dodge, size = 1)  +
  stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
  theme_bw())

## for publication
(p_acc_rate2 <- ggplot(acc_rate_subject, aes(x = cue_validity, y = accuracy_rate, fill = task_familiarity)) +
    geom_split_violin(trim = FALSE, alpha = .3, width = .7)+
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .6),
                      name = "Task type:",
                      breaks = c("gekende taak", "nieuwe taak"),
                      labels = c("Practiced", "Novel")) +
    geom_boxplot(width = 0.34, fatten = NULL, show.legend = FALSE, size = 1, position = position_dodge(width = .4))  +
    stat_summary(fun = mean, geom="point", shape=20, size=5, color="black", position = position_dodge(.37)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = .2, position = position_dodge(width = .37))+
    theme_apa(base_size = 14) +
    labs(x = "block type", y = "Average accuracy") +
    coord_cartesian(ylim=c(0.6, 1)) +
    scale_x_discrete(labels = c("testCue" = "informative", "testNocue" = "uninformative")) +
    theme(axis.title.x = element_blank()))

# combining all plots for publication
(p_target_rt2 | p_acc_rate2) + 
  plot_annotation(title = 'The behavioral results of Experiment 2',
                  tag_levels = 'A')

#####################################################
################# run RM ANOVA ##################----
# ezANOVA ----
acc_rate_anova <- ezANOVA(data = acc_rate_subject, 
                          dv = .(accuracy_rate), 
                          wid = .(subject), 
                          within = .(cue_validity, task_familiarity),
                          between = .(test_sequence),
                          type = 3)

acc_rate_anova
# anova for subjects who found the cue helpful ----
acc_rate_help_anova <- ezANOVA(data = acc_rate_subject_help, 
                               dv = .(accuracy_rate), 
                               wid = .(subject), 
                               within = .(cue_validity, task_familiarity),
                               return_aov = TRUE)

acc_rate_help_anova$ANOVA

# visualize and explore the intriguing 2-way interaction
acc_rate_help_anova_stat <- ezStats(data = acc_rate_subject_help, 
                                    dv = .(accuracy_rate), 
                                    wid = .(subject), 
                                    within = .(task_familiarity, cue_validity))

print(acc_rate_help_anova_stat)

emm_acc_help <- emmeans(acc_rate_help_anova$aov, pairwise ~ cue_validity|task_familiarity)
emm_acc_help

################## creating table for publication ###############----
pub_table <- bind_rows(targetRT_descrip2, targetACC_descrip2)
pub_table$measure <- c("Target RT", "Accuracy")
pub_table <- pub_table %>%
  select(9,c(1:8))

colnames(pub_table) <- c("Measures","Informative.Practiced.M","Informative.Practiced.SD","Informative.Novel.M", "Informative.Novel.SD","Uninformative.Practiced.M", "Uninformative.Practiced.SD", "Uninformative.Novel.M", "Uninformative.Novel.SD")

pub_table2 <- as.data.frame(pub_table)

fun_round <- function(x) {formatC(x, format = "f", digits = 3)}

table_ready <- nice_table(pub_table2,
                          format.custom = "fun_round",
                          separate.header = TRUE)

save_as_docx(table_ready, path = "D:/Ghent_Braem/Experiment/1st_research_PRO/data/raw_data/2nd_batch/table_M_SD.docx")

# run paired t test
acc_rate_prac_df <- filter(acc_rate_subject, task_familiarity == "gekende taak")

acc_rate_ttest <- t.test(accuracy_rate ~ cue_validity, data = acc_rate_prac_df, paired = TRUE)
acc_rate_ttest

### the Survey ###

ggplot(survey_val_df, aes(reponse)) +
  stat_count(width = 0.5)+
  ggtitle("survey on cue validity") +
  theme_bw()


ggplot(survey_fam_df, aes(reponse)) +
  stat_count(width = 0.5)+
  ggtitle("survey on task familiarity")+
  theme_bw()

### find the effect of 4 participants which find the cue helpful ###

subject_helpful <- survey_val_df %>%
  filter(reponse == 4) %>%
  pull(subject) %>%
  unique()

# compute the subject level median RT for each condition

test_target_median_helpful <- test_target_df %>%
  filter(subject %in% subject_helpful) %>%
  convert_as_factor(subject) %>%
  group_by(subject, cue_validity, task_familiarity, test_sequence) %>%
  summarise(count = n(),
            medianRT = median(rt, na.rm = TRUE),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

# data visualization

# for the subject-level distribution
(p_target_rt_helpful <- ggplot(test_target_median_helpful, aes(x = cue_validity, y = meanRT, colour = task_familiarity)) +
    geom_point(position = dodge, size = 1)  +
    scale_colour_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
    ggtitle("factorized subject-level RT median for all the subejcts who find the cue helpful") +
    theme_bw())


# for the trial level distribution
(p_target_rt_trial_helpful <- ggplot(subset(test_target_df, subject %in% subject_helpful), aes(x = cue_validity, y = rt, fill = task_familiarity)) +
    geom_violin(position = dodge, size = 1) +
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
    geom_boxplot(width = 0.3, position = dodge, size = 1)  +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("factorized trial-level RT for all the subject who find the cue helpful") +
    theme_bw())

# run the t test between cued and no-cued condition
target_helpful_cue <- test_target_df %>%
  filter(subject %in% subject_helpful, cue_validity == "testCue") %>%
  select(rt) %>%
  deframe()

target_helpful_nocue <- test_target_df %>%
  filter(subject %in% subject_helpful, cue_validity == "testNocue") %>%
  select(rt) %>%
  deframe()

t.test(target_helpful_cue, target_helpful_nocue)


# run mixed effect model instead

target_helpful <- test_target_df %>%
  filter(subject %in% subject_helpful)

options(contrasts = c("contr.sum", "contr.poly"))
model_RT <- lmer(formula = rt ~ 1 + cue_validity + task_familiarity + cue_validity:task_familiarity + (1 + cue_validity + task_familiarity + cue_validity:task_familiarity |subject_ID),
                 data =  target_helpful)

summary(model_RT)

# adding those 4 subjects from the original paradigm
bigdf_clean_old <- read_feather("D:/Ghent_Braem/Experiment/Pilot/data/raw_data/bigdf_clean_full_new.feather")

subject_helpful_old <- bigdf_clean_old %>%
  filter(trial_part == "survey_validity", reponse == 4) %>%
  pull(subject) %>%
  unique()

test_target_helpful_old <- bigdf_clean_old %>%
  filter(block_dich == "test" & trial_part == "target", subject %in% subject_helpful_old) %>%
  mutate(cue_validity = as.factor(block_type),
         task_familiarity = as.factor(prac_or_novel),
         test_sequence = as.factor(testSeq),
         subject_ID = as.factor(subject)
         )

test_target_helpful <- test_target_df %>%
  filter(subject %in% subject_helpful)

test_target_helpful_combined <- bind_rows(test_target_helpful, test_target_helpful_old)

test_target_median_helpful_combined <- test_target_helpful_combined %>%
  convert_as_factor(subject) %>%
  group_by(subject, cue_validity, task_familiarity, test_sequence) %>%
  summarise(count = n(),
            medianRT = median(rt, na.rm = TRUE),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

(p_target_rt_helpful_combined <- ggplot(test_target_median_helpful_combined, aes(x = cue_validity, y = meanRT, colour = task_familiarity)) +
    geom_point(position = dodge, size = 2)  +
    scale_colour_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
    geom_boxplot(aes(fill = task_familiarity),width = 0.3, position = dodge, size = 1)  +
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .2)) +
    ggtitle("factorized subject-level RT median for all the subejcts who find the cue helpful") +
    theme_bw())

(p_target_rt_trial_helpful_combined <- ggplot(test_target_helpful_combined, aes(x = cue_validity, y = rt, fill = task_familiarity)) +
    geom_violin(position = dodge, size = 1) +
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
    geom_boxplot(width = 0.3, position = dodge, size = 1)  +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("factorized trial-level RT for all the subject who find the cue helpful") +
    theme_bw())

#######################################################
### the conditional accuracy function(CAF) analysis ###
#######################################################

bigdf_clean_subject <- read_feather("bigdf_cleansubject_2.feather")

target_caf <- bigdf_clean_subject %>%
  filter(block_dich == "test", trial_part == "target", !is.na(rt)) %>%
  select(rt, logical_rule, block_type, prac_or_novel, accuracy) %>%
  convert_as_factor(logical_rule, block_type, prac_or_novel) %>%
  group_by(logical_rule, block_type, prac_or_novel) %>%
  mutate(quantile_RT = ntile(rt,4)) %>%
  convert_as_factor(quantile_RT) %>%
  group_by(quantile_RT, .add = TRUE) %>%
  summarise(count = n(),
            acc_rate = sum(accuracy)/length(accuracy)) %>%
  ungroup()


group_vars(target_caf)


(
p <- ggplot(target_caf, aes(x = quantile_RT, y = acc_rate,
                            group = interaction(block_type, prac_or_novel))) +
  geom_point(aes(color = prac_or_novel), size = 4) +
  geom_line(aes(color = prac_or_novel, linetype = block_type), size = 1) +
  scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
  facet_wrap(~logical_rule, nrow = 2) +
  theme_bw()
)

########################################################
### testing the effect of logical rules on target RT ###
########################################################

test_target_df$logical_rule <- as.factor(test_target_df$logical_rule)
glimpse(test_target_df)


test_target_logical <- test_target_df %>%
  group_by(logical_rule, subject) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()


pairwise.t.test(test_target_logical$meanRT, 
                test_target_logical$logical_rule, 
                p.adjust.method = "bonferroni", 
                paired = TRUE)



(p_logical <- ggplot(test_target_logical, aes(x = logical_rule, y = meanRT, fill = logical_rule)) +
    geom_violin(position = dodge, size = 1) +
    scale_fill_manual(values = alpha(wes_palette(n=6, name="Zissou1", type = "continuous"), .5)) +
    geom_boxplot(width = 0.3, position = dodge, size = 1)  +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("the target RT for diff logical rules") + 
    scale_x_discrete(labels = c('both','first','neither','at least one', 'second', 'different')) +
    guides(fill=FALSE) +
    theme_bw())

### the assymmetrical switching effect ###

prac_mixed_target <- bigdf_clean %>%
  filter(block_type == 'pracMix', trial_part == 'target') %>%
  mutate(difficulty_level = case_when(
    logical_rule == "eerste" ~ 1,
    logical_rule == "tweede" ~ 1,
    logical_rule == "beide" ~ 2,
    logical_rule == "ten minste een" ~ 2,
    logical_rule == "geen" ~ 3,
    logical_rule == "verschillend" ~ 3
  )) %>%
  group_by(subject) %>%
  mutate(previous_logical_rule = lag(logical_rule),
         previous_difficulty_level = lag(difficulty_level),
         difficulty_gap = difficulty_level - previous_difficulty_level) %>%
  convert_as_factor(difficulty_gap) %>%
  filter(accuracy == TRUE, !is.na(difficulty_gap)) %>%
  group_by(difficulty_gap, .add = TRUE) %>%
  summarise(
    meanRT = mean(rt, na.rm = TRUE)
  ) %>%
  ungroup
  
# visualize the RT
(
  p_asy_switch <- ggplot(prac_mixed_target, aes(x = difficulty_gap, y = meanRT, fill = difficulty_gap)) +
    geom_violin(position = dodge, size = .5) +
    scale_fill_manual(values = alpha(wes_palette(n=6, name="Zissou1", type = "continuous"), .5)) +
    geom_boxplot(width = 0.3, position = dodge, size = 1)  +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("the target RT for switching to diff difficulties") + 
    guides(fill=FALSE) +
    theme_bw()
  )


#######################################################################
#### the correlation between task cue RT and target RT and acc rate ###
#######################################################################

taskcue_target_subject <- bigdf_clean %>%
  filter(block_dich == "test", trial_part == "taskcue" | trial_part == "target") %>%
  select(subject, block_type, prac_or_novel, testSeq, trial_part, trialID, rt) %>%
  convert_as_factor(subject, block_type, prac_or_novel, testSeq) %>%
  pivot_wider(names_from = trial_part, values_from = rt) %>%
  group_by(subject, block_type, prac_or_novel) %>%
  summarise(cor_coef = cor(taskcue, target)) %>%
  ungroup()

glimpse(taskcue_target_subject)

(p_rt_cor <- ggplot(taskcue_target_subject, aes(x = block_type, y = cor_coef, fill = prac_or_novel)) +
    geom_violin(position = dodge, size = 1) +
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
    geom_boxplot(width = 0.3, position = dodge, size = 1) +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("factorized correlation between taskcue and target RT") +
    theme_bw())

rt_cor_anova <- anova_test(data = taskcue_target_subject, 
                           dv = cor_coef, 
                           wid = subject, 
                           within = c(block_type, prac_or_novel))

get_anova_table(rt_cor_anova)


acc_rate_anova2 <- ezANOVA(data = taskcue_target_subject, 
                          dv = .(cor_coef), 
                          wid = .(subject), 
                          within = .(block_type, prac_or_novel))

acc_rate_anova2


### the correlation between taskcue RT and acc rate ###

taskcue_RT_bysubject <- test_taskcue_median %>%
  mutate(key = str_c(subject, cue_validity, task_familiarity, sep = "_")) %>%
  select(key, meanRT)

a <- acc_rate_subject %>%
  mutate(key = str_c(subject, cue_validity, task_familiarity, sep = "_")) %>%
  left_join(taskcue_RT_bysubject, by = "key") %>%
  group_by(cue_validity, task_familiarity) %>%
  arrange(subject) %>%
  summarise(cor_coef = cor(meanRT, accuracy_rate))

##########################################################
####### Analysis on switch cost across conditions ########
##########################################################

trial_label_switch <- read_feather("switch_or_repeat_list.feather")

############## the switch cost on target RT ##############

# join the test_df with the switch label
switch_target_df <- test_target_df %>%
  left_join(trial_label_switch, by = "trialID") %>%
  convert_as_factor(switch_type) %>%
  filter(!is.na(switch_type)) %>%
  mutate(switch_or_repeat = if_else(str_detect(switch_type, "switch"), "switch", "repeat"))

# calculate the average RT for each switch condition and factor ----
switch_cost_target_bysubject <- switch_target_df %>%
  group_by(subject, cue_validity, switch_type, test_sequence) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = switch_type, values_from = meanRT) %>%
  mutate(switch_cost_prac = switch_prac - repeat_prac,
         switch_cost_novel = switch_novel - repeat_novel) %>%
  select(subject, cue_validity, test_sequence, switch_cost_prac, switch_cost_novel) %>%
  pivot_longer(c("switch_cost_prac", "switch_cost_novel"), names_to = "task_familiarity", values_to = "switch_cost")


(p_switch_cost <- ggplot(switch_cost_target_bysubject, aes(x = cue_validity, y = switch_cost, fill = task_familiarity)) +
    geom_violin(position = dodge, size = 1) +
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
    geom_boxplot(width = 0.3, position = dodge, size = 1) +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("factorized subject-level target switch cost") +
    theme_bw())

switch_cost_target_anova <- ezANOVA(data = switch_cost_target_bysubject, 
                           dv = .(switch_cost), 
                           wid = .(subject), 
                           within = .(cue_validity, task_familiarity),
                           between = .(test_sequence),
                           type = 3)

switch_cost_target_anova
# another way of modeling switch cost, which is the way for REPORTING statistics ----

switch_type_target_bysubject <- switch_target_df %>%
  convert_as_factor(subject, switch_or_repeat) %>%
  group_by(subject, cue_validity, task_familiarity, switch_or_repeat, test_sequence) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()


options(contrasts = c("contr.sum", "contr.poly"))
switch_type_target_anova <- ezANOVA(data = switch_type_target_bysubject, 
                                    dv = .(meanRT), 
                                    wid = .(subject), 
                                    within = .(cue_validity, task_familiarity, switch_or_repeat),
                                    between = .(test_sequence),
                                    type = 3,
                                    return_aov = TRUE)

switch_type_target_anova$ANOVA

# plotting for publication
switch_type_target_plot <- switch_target_df %>%
  convert_as_factor(subject, switch_or_repeat) %>%
  group_by(subject, cue_validity, task_familiarity, switch_or_repeat) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  reorder_levels("task_familiarity", order = c("gekende taak", "nieuwe taak"))


(p_switch_type_target2 <- ggplot(switch_type_target_plot, aes(x = cue_validity, y = meanRT, fill = task_familiarity, color = switch_or_repeat)) +
    geom_split_violin(trim = FALSE, alpha = .3, width = .7)+
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .4),
                      name = "task novelty",
                      breaks = c("gekende taak", "nieuwe taak"),
                      labels = c("practiced", "novel")) +
    scale_color_manual(values = alpha(c("#c0c0c0", "#000000"), .8),
                       name = "trial type",
                       breaks = c("repeat", "switch"),
                       labels = c("repeat", "switch")) +
    geom_boxplot(width = 0.34, fatten = NULL, show.legend = FALSE, size = 1, position = position_dodge(width = .4))  +
    stat_summary(fun = mean, geom="point", shape=20, size=6, position = position_dodge(.39)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = .2, position = position_dodge(width = .39))+
    theme_apa(base_size = 14) +
    labs(x = "block type", y = "Average accuracy rate") +
    scale_x_discrete(labels = c("testCue" = "informative", "testNocue" = "uninformative")) +
    theme(axis.title.x = element_blank()))

# visualize and explore the intriguing 2-way interaction
switch_type_target_anova_stat <- ezStats(data = switch_type_target_bysubject, 
                                         dv = .(meanRT), 
                                         wid = .(subject), 
                                         within = .(switch_or_repeat, cue_validity))

print(switch_type_target_anova_stat)

emm <- emmeans(switch_type_target_anova$aov, pairwise ~ cue_validity|switch_or_repeat)
emm

# only for people who find the cue helpful ----
switch_type_target_bysubject_help <- switch_type_target_bysubject %>%
  filter(subject %in% subj_tasktype_help)

switch_type_target_help_anova <- ezANOVA(data = switch_type_target_bysubject_help, 
                                         dv = .(meanRT), 
                                         wid = .(subject), 
                                         within = .(cue_validity, task_familiarity, switch_or_repeat),
                                         type = 3,
                                         return_aov = TRUE) # without task sequence as between-subject factor here because all 4 participants started with non-informative blocks 

switch_type_target_help_anova$ANOVA

# visualize and explore the intriguing 2-way interaction
switch_type_target_help_anova_stat <- ezStats(data = switch_type_target_bysubject, 
                                              dv = .(meanRT), 
                                              wid = .(subject), 
                                              within = .(switch_or_repeat, cue_validity))

print(switch_type_target_help_anova_stat)

emm <- emmeans(switch_type_target_anova$aov, pairwise ~ cue_validity|switch_or_repeat)
emm

# -use lsmeans package ----
ls_means <- lsmeans(switch_type_target_anova$aov, ~ "cue_validity")

# summarize the rt based on the accuracy type ----
switch_type_rt_bysubject <- switch_rt_target_df %>%
  group_by(subject, test_sequence, cue_validity, switch_type) %>%
  summarise(acc_rate = sum(accuracy)/length(accuracy)) %>%
  ungroup()


(p_switch_type <- ggplot(switch_type_acc_bysubject, aes(x = cue_validity, y = acc_rate, fill = switch_type)) +
    geom_violin(position = dodge, size = 1) +
    scale_fill_manual(values=wes_palette(n=4, name="Royal1")) +
    geom_boxplot(width = 0.3, position = dodge, size = 1) +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("factorized subject-level target accuray rate by switch type") +
    theme_bw()
)


############## the switch cost on acc rate ############## ----

# join the test_df with the switch label
switch_acc_target_df <- test_target_error_df %>%
  left_join(trial_label_switch, by = "trialID") %>%
  convert_as_factor(switch_type) %>%
  filter(!is.na(switch_type)) %>%
  mutate(switch_or_repeat = if_else(str_detect(switch_type, "switch"), "switch", "repeat"))

# Analysis on the Switch Cost  ----
switch_cost_acc_bysubject <- switch_acc_target_df %>%
  group_by(subject, cue_validity, switch_type, test_sequence) %>%
  summarise(accuracy_rate = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  pivot_wider(names_from = switch_type, values_from = accuracy_rate) %>%
  mutate(switch_cost_prac = repeat_prac - switch_prac,
         switch_cost_novel = repeat_novel - switch_novel) %>%
  select(subject, cue_validity, test_sequence, switch_cost_prac, switch_cost_novel) %>%
  pivot_longer(c("switch_cost_prac", "switch_cost_novel"), names_to = "task_familiarity", values_to = "switch_cost_acc")


(p_switch_cost <- ggplot(switch_cost_acc_bysubject, aes(x = cue_validity, y = switch_cost_acc, fill = task_familiarity)) +
    geom_violin(position = dodge, size = 1) +
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
    geom_boxplot(width = 0.3, position = dodge, size = 1) +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("factorized subject-level target switch cost of accuracy rate") +
    theme_bw())

switch_cost_acc_anova <- ezANOVA(data = switch_cost_acc_bysubject, 
                                    dv = .(switch_cost_acc), 
                                    wid = .(subject), 
                                    within = .(cue_validity, task_familiarity),
                                    between = .(test_sequence),
                                    type = 3)

switch_cost_acc_anova

# Another way of modeling switch cost, which is the way for REPORTING statistics ----
switch_type_acc_bysubject <- switch_acc_target_df %>%
  convert_as_factor(subject, switch_or_repeat) %>%
  group_by(subject, cue_validity, task_familiarity, switch_or_repeat, test_sequence) %>%
  summarise(acc_rate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

switch_type_acc_anova <- ezANOVA(data = switch_type_acc_bysubject, 
                                 dv = .(acc_rate), 
                                 wid = .(subject), 
                                 within = .(cue_validity, task_familiarity, switch_or_repeat),
                                 between = .(test_sequence),
                                 type = 3,
                                 return_aov = TRUE)

switch_type_acc_anova$ANOVA

# visualize the significant 2-way interaction ----
switch_cost_acc_anova_plot <- ezPlot(data = switch_cost_acc_bysubject, 
                                     dv = .(switch_cost_acc), 
                                     wid = .(subject), 
                                     within = .(cue_validity, task_familiarity),
                                     between = .(test_sequence),
                                     x = .(cue_validity),
                                     split = .(task_familiarity))

print(rt_target_anova_plot)

# only for people who find the cue helpful ----
switch_type_acc_bysubject_help <- switch_type_acc_bysubject %>%
  filter(subject %in% subj_tasktype_help)

switch_type_acc_help_anova <- ezANOVA(data = switch_type_acc_bysubject_help, 
                                      dv = .(acc_rate), 
                                      wid = .(subject), 
                                      within = .(cue_validity, task_familiarity, switch_or_repeat),
                                      return_aov = TRUE)

switch_type_acc_help_anova$ANOVA


#####################################################
########### mixed effect model ######################
#####################################################

library(lme4) 
library(lmerTest)
library(lattice)
library(car)

### the mixed effect model of target RT ###
sum(is.na(test_target_df$rt)) # should be zero

model_RT <- lmer(formula = rt ~ 1 + cue_validity + task_familiarity + cue_validity:task_familiarity + (1 + cue_validity + task_familiarity + cue_validity:task_familiarity |subject_ID),
                 data =  test_target_df)

summary(model_RT)


# diagnostics

densityplot(resid(model_RT, scale = TRUE))   # the residuals are not normally distributed

qqmath(model_RT)
qqPlot(resid(model_RT, scale = TRUE))

















