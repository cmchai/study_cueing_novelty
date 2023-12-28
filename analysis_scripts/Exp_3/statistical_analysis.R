################# statistical analysis ######################
############### for PRO Paradigm Experiment 3 ###############
library(here)
library(tidyverse)
library(ez)
library(rstatix)
library(wesanderson)
library(papaja)
library(afex)
library(rempsyc)
library(emmeans)
library(patchwork)
library(BayesFactor)

dodge <- position_dodge(width = 0.7)
options(contrasts = c("contr.sum", "contr.poly")) # very important for using emmeans on the ezANOVA results

#############################################################
############## Analysis on the reaction time ################
#############################################################

load(file = paste0(here(), "/results/bigdf_clean.Rdata"))

n_sub = length(unique(bigdf_clean$subject))

### Demographic info ###
length(unique(bigdf_clean$subject))

demo_age <- bigdf_clean %>%
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
test_target_df <- bigdf_clean %>%
  filter(section == "test" & trial_part == "target")

unique(test_target_df$accuracy) # should be all TRUE

survey_val_df <- bigdf_clean %>%
  filter(trial_part == "survey_validity") %>%
  convert_as_factor(key_press)

subj_tasktype_help <- survey_val_df %>%
  filter(key_press %in% c(4,5)) %>% # 4 being "mildly helpful" and 5 being "very helpful"
  pull(subject)

survey_fam_df <- bigdf_clean %>%
  filter(trial_part == "survey_familiarity") %>%
  convert_as_factor(key_press)

### step2. run RM ANOVA on the target RT ###
unique(test_target_df$block_type) # 2 levels for cue_validity
unique(test_target_df$prac_or_novel) # 2 levels for familiarity
unique(test_target_df$test_begin_with) # 2 levels for test sequence, either cue first or nocue first

test_target_df$cue_validity <- as.factor(test_target_df$block_type)
test_target_df$task_familiarity <- as.factor(test_target_df$prac_or_novel)
test_target_df$test_sequence <- as.factor(test_target_df$test_begin_with)
test_target_df$subject_ID <- as.factor(test_target_df$subject)

glimpse(test_target_df)

### compute the subject level median RT for each condition ###
test_target_median <- test_target_df %>%
  group_by(subject, cue_validity, task_familiarity, test_sequence) %>%
  summarise(count = n(),
            medianRT = median(rt, na.rm = TRUE),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

test_target_median$subject <- as.factor(test_target_median$subject)
test_target_median$task_familiarity <- factor(test_target_median$task_familiarity, levels = c("prac task", "novel task"))

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

##### reporting for publication including mean and sd 
targetRT_descrip2 <- test_target_df %>%
  convert_as_factor(task_familiarity) %>%
  reorder_levels("task_familiarity", order = c("prac task", "novel task")) %>%
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
         task_type = if_else(task_familiarity == "prac task", "practiced", "novel")) %>%
  select(!c(cue_validity, task_familiarity, SE)) %>%  
  pivot_wider(names_from = c(block_type, task_type),
              values_from = c(M, subsd)) %>%
  select(1,5,2,6,3,7,4,8)

# data visualization
dodge <- position_dodge(width = 0.7)

# for the subject-level distribution
(p_target_rt <- ggplot(test_target_median, aes(x = cue_validity, y = medianRT, fill = task_familiarity)) +
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
test_target_median_p <- test_target_df %>%
  convert_as_factor(task_familiarity, subject) %>%
  reorder_levels("task_familiarity", order = c("prac task", "novel task")) %>%
  group_by(subject, cue_validity, task_familiarity) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()


(p_target_rt2 <- ggplot(test_target_median_p, aes(x = cue_validity, y = meanRT, fill = task_familiarity)) +
    geom_split_violin(trim = FALSE, alpha = .3, width = .7)+
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .6),
                      name = "Task type:",
                      breaks = c("prac task", "novel task"),
                      labels = c("Practiced", "Novel")) +
    geom_boxplot(width = 0.34, fatten = NULL, show.legend = FALSE, size = 1, position = position_dodge(width = .4))  +
    stat_summary(fun = mean, geom="point", shape=20, size=5, color="black", position = position_dodge(.37)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = .2, position = position_dodge(width = .37))+
    theme_apa(base_size = 14) +
    labs(x = "block type", y = "Target reaction time") +
    scale_x_discrete(labels = c("testCue" = "informative", "testNocue" = "uninformative"))+
    theme(axis.title.x = element_blank(),
          legend.position = "none"))

############################################
############## run ANOVA ###################

## using ezANOVA to compute ----
rt_target_anova <- ezANOVA(data = test_target_median, 
                           dv = .(meanRT), 
                           wid = .(subject), 
                           within = .(cue_validity, task_familiarity),
                           between = .(test_sequence),
                           type = 3)

rt_target_anova

# plot the interaction test sequence * cue validity
test_target_median2 <- test_target_df %>%
  group_by(subject, test_sequence, cue_validity) %>%
  summarise(count = n(),
            medianRT = median(rt, na.rm = TRUE))

(p_target_rt_inter <- ggplot(test_target_median2, aes(x = test_sequence, y = medianRT, fill = cue_validity)) +
                      geom_violin(position = dodge, size = 1) +
                      scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
                      geom_boxplot(width = 0.3, position = dodge, size = 1)  +
                      stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
                      ggtitle("factorized subject-level RT median") +
                      theme_bw())

# add some ez plot ...
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

# compute the marginal mean of task familiarity
rt_target_help_anova_stat <- ezStats(data = test_target_median_help, 
                                     dv = .(meanRT), 
                                     wid = .(subject), 
                                     within = .(task_familiarity))

print(rt_target_help_anova_stat)

#############################################################
################# Analysis on the ACC rate ##################
#############################################################

load(file = paste0(here(), "/results/bigdf_clean_error.Rdata"))
glimpse(bigdf_clean_error)

test_target_error_df <- bigdf_clean_error %>%
  filter(section == "test" & trial_part == "target") %>%
  filter(!is.na(rt))  # delete the missing trial

test_target_error_df$cue_validity <- as.factor(test_target_error_df$block_type)
test_target_error_df$task_familiarity <- as.factor(test_target_error_df$prac_or_novel)
test_target_error_df$test_sequence <- as.factor(test_target_error_df$test_begin_with)
test_target_error_df$subject_ID <- as.factor(test_target_error_df$subject)

# descriptive data on the accuracy rate
acc_rate_subject <- test_target_error_df %>%
  convert_as_factor(task_familiarity) %>%
  reorder_levels("task_familiarity", order = c("prac task", "novel task")) %>%
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

# for the people who reported that the task type cues were helpful #
acc_rate_subject_help <- acc_rate_subject %>%
  filter(subject %in% subj_tasktype_help)

##### reporting for publication including mean and sd 
targetACC_descrip2 <- test_target_error_df %>%
  convert_as_factor(task_familiarity) %>%
  reorder_levels("task_familiarity", order = c("prac task", "novel task")) %>%
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
         task_type = if_else(task_familiarity == "prac task", "practiced", "novel")) %>%
  select(!c(cue_validity, task_familiarity, SE)) %>%  
  pivot_wider(names_from = c(block_type, task_type),
              values_from = c(M, subsd)) %>%
  select(1,5,2,6,3,7,4,8)

# visualize the result
(p_acc_rate <- ggplot(acc_rate_subject, aes(x = cue_validity, y = accuracy_rate, fill = task_familiarity)) +
  geom_violin(position = dodge, size = 1) +
  scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
  geom_boxplot(width = 0.3, position = dodge, size = 1)  +
  stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
  theme_bw())

## for publication
acc_rate_subject_p <- test_target_error_df %>%
  convert_as_factor(task_familiarity, subject) %>%
  reorder_levels("task_familiarity", order = c("prac task", "novel task")) %>%
  group_by(subject, cue_validity, task_familiarity) %>%
  summarise(count = n(),
            accuracy_rate = sum(accuracy)/length(accuracy))

(p_acc_rate2 <- ggplot(acc_rate_subject_p, aes(x = cue_validity, y = accuracy_rate, fill = task_familiarity)) +
    geom_split_violin(trim = FALSE, alpha = .3, width = .7)+
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .6),
                      name = "Task type:",
                      breaks = c("prac task", "novel task"),
                      labels = c("Practiced", "Novel")) +
    geom_boxplot(width = 0.34, fatten = NULL, show.legend = FALSE, size = 1, position = position_dodge(width = .4))  +
    stat_summary(fun = mean, geom="point", shape=20, size=5, color="black", position = position_dodge(.37)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = .2, position = position_dodge(width = .37))+
    theme_apa(base_size = 14) +
    labs(x = "block type", y = "Average accuracy") +
    coord_cartesian(ylim=c(0.6, 1)) +
    scale_x_discrete(labels = c("testCue" = "informative", "testNocue" = "uninformative")) +
    theme(axis.title.x = element_blank(),
          legend.position = "none"))

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
                               within = .(cue_validity, task_familiarity))

acc_rate_help_anova

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

save_as_docx(table_ready, path = paste0(here(), "/results/table_M_SD.docx"))

#############################################################
############## Analysis on the Survey data ##################
#############################################################

ggplot(survey_val_df, aes(key_press)) +
  stat_count(width = 0.5)+
  ggtitle("survey on cue validity") +
  theme_bw()


ggplot(survey_fam_df, aes(key_press)) +
  stat_count(width = 0.5)+
  ggtitle("survey on task familiarity")+
  theme_bw()


### the conditional accuracy function(CAF) analysis ###

bigdf_clean_subject <- read_feather("bigdf_cleansubject.feather")

target_caf <- bigdf_clean_subject %>%
  filter(block_dich == "test", trial_part == "target", !is.na(rt)) %>%
  select(rt, subject, block_type, prac_or_novel, accuracy) %>%
  convert_as_factor(subject, block_type, prac_or_novel) %>%
  group_by(subject, block_type, prac_or_novel) %>%
  mutate(quantile_RT = ntile(rt,4)) %>%
  convert_as_factor(quantile_RT) %>%
  group_by(quantile_RT, .add = TRUE) %>%
  summarise(count = n(),
            acc_rate = sum(accuracy)/length(accuracy)) %>%
  group_by(quantile_RT, .add = TRUE) %>%
  ungroup(subject) %>%
  select(-count) %>%
  summarise(count = n(),
            mean_accrate = mean(acc_rate, na.rm = TRUE),
            sd = sd(acc_rate, na.rm = TRUE)) %>%
  ungroup()


group_vars(target_caf)


(
p <- ggplot(target_caf, aes(x = quantile_RT, y = mean_accrate,
                            group = interaction(block_type, prac_or_novel))) +
  geom_point(aes(color = prac_or_novel), size = 4) +
  geom_line(aes(color = prac_or_novel, linetype = block_type), size = 1) +
  geom_errorbar(aes(ymin = mean_accrate - sd, 
                    ymax = mean_accrate + sd,
                    width = .2,
                    color = prac_or_novel,
                    linetype = block_type)) +
  scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
  theme_bw()
)


##########################################################################
### testing the effect of logical rules on target RT and accuracy rate ###
##########################################################################

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


###### the accuracy rate between logical rules and semantic rules #######

unique(bigdf_clean_error$block_type)

(
accRate_semantic <- bigdf_clean_error %>%
  filter(str_detect(block_type, 'test'), trial_part == 'target') %>%
  group_by(semantic_rule, subject) %>%
  summarise(count = n(),
            accRate = sum(accuracy, na.rm = TRUE)/length(accuracy)) %>%
  ggplot(aes(x = semantic_rule, y = accRate, fill = semantic_rule)) +
  geom_violin(size = 1) +
  stat_summary(fun = mean, geom="point", shape=20, size=4, color="black")
)

group_vars(accRate_semantic)

(
  accRate_logical <- bigdf_clean_error %>%
    filter(str_detect(block_type, 'test'), trial_part == 'target') %>%
    group_by(logical_rule, subject) %>%
    summarise(count = n(),
              accRate = sum(accuracy, na.rm = TRUE)/length(accuracy)) %>%
    ggplot(aes(x = logical_rule, y = accRate, fill = logical_rule)) +
    geom_violin(size = 1) +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black")
)


########################################################################
#### the correlation between task cue RT and target RT and acc rate ####
########################################################################

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

################### on Reaction Time #####################
# load the df including the information of the switch type of previous trial
load(file = paste0(here(), "/results/2back_switch.Rdata"))

# get rid of the NA switch type
switch_target_df <- test_target_df %>%
  convert_as_factor(switch_type) %>%
  filter(!is.na(switch_type)) %>%
  mutate(switch_or_repeat = if_else(str_detect(switch_type, "switch"), "switch", "repeat")) %>%
  left_join(back2_switch_df, by = "trial_id")

# calculate the average RT for each switch condition and factor(analysis of switch cost) ----
switch_cost_target_bysubject <- switch_target_df %>%
  group_by(subject, cue_validity, switch_type, test_sequence) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = switch_type, values_from = meanRT) %>%
  mutate(switch_cost_prac = switch_prac - repeat_prac,
         switch_cost_novel = switch_novel - repeat_novel) %>%
  select(subject, cue_validity, test_sequence, switch_cost_prac, switch_cost_novel) %>%
  pivot_longer(c("switch_cost_prac", "switch_cost_novel"), names_to = "task_familiarity", values_to = "rt_switch_cost") %>%
  convert_as_factor(task_familiarity) %>%
  reorder_levels("task_familiarity", order = c("switch_cost_prac", "switch_cost_novel"))


(p_switch_cost <- ggplot(switch_cost_target_bysubject, aes(x = cue_validity, y = rt_switch_cost, fill = task_familiarity)) +
    geom_violin(position = dodge, size = 1) +
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
    geom_boxplot(width = 0.3, position = dodge, size = 1) +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("factorized subject-level reaction time switch cost") +
    theme_bw())

# plotting for publication

(p_switch_cost2 <- ggplot(switch_cost_target_bysubject, aes(x = cue_validity, y = rt_switch_cost, fill = task_familiarity)) +
    geom_split_violin(trim = FALSE, alpha = .3, width = .7)+
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .6),
                      name = "Switch type:",
                      breaks = c("switch_cost_prac", "switch_cost_novel"),
                      labels = c("To practiced task", "To novel task")) +
    geom_boxplot(width = 0.34, fatten = NULL, show.legend = FALSE, size = 1, position = position_dodge(width = .4))  +
    stat_summary(fun = mean, geom="point", shape=20, size=5, color="black", position = position_dodge(.37)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = .2, position = position_dodge(width = .37))+
    theme_apa(base_size = 14) +
    labs(x = "block type", y = "Switch cost in ms") +
    scale_x_discrete(labels = c("testCue" = "informative", "testNocue" = "uninformative")))


switch_cost_target_anova <- ezANOVA(data = switch_cost_target_bysubject, 
                           dv = .(rt_switch_cost), 
                           wid = .(subject), 
                           within = .(cue_validity, task_familiarity),
                           between = .(test_sequence),
                           type = 3)

switch_cost_target_anova

## another way of modeling switch cost, which is the way for REPORTING statistics ----
switch_type_target_bysubject <- switch_target_df %>%
  convert_as_factor(subject, switch_or_repeat) %>%
  group_by(subject, cue_validity, task_familiarity, switch_or_repeat, test_sequence) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

switch_type_target_anova <- ezANOVA(data = switch_type_target_bysubject, 
                                    dv = .(meanRT), 
                                    wid = .(subject), 
                                    within = .(cue_validity, task_familiarity, switch_or_repeat),
                                    between = .(test_sequence),
                                    type = 3,
                                    return_aov = TRUE)

switch_type_target_anova$ANOVA

# explore the 2-back switch type effect
back2_switch_type_target_bysubject <- switch_target_df %>%
  filter(!is.na(pre_switch_or_repeat)) %>%
  convert_as_factor(subject, switch_or_repeat, pre_switch_or_repeat) %>%
  group_by(subject, cue_validity, task_familiarity, switch_or_repeat, pre_switch_or_repeat, test_sequence) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

back2_switch_type_target_anova <- ezANOVA(data = back2_switch_type_target_bysubject,
                                    dv = .(meanRT), 
                                    wid = .(subject), 
                                    within = .(cue_validity, task_familiarity, switch_or_repeat, pre_switch_or_repeat),
                                    between = .(test_sequence),
                                    type = 3,
                                    return_aov = TRUE)

back2_switch_type_target_anova$ANOVA

# only for people who find the cue helpful ----
switch_type_target_bysubject_help <- switch_type_target_bysubject %>%
  filter(subject %in% subj_tasktype_help)

switch_type_target_help_anova <- ezANOVA(data = switch_type_target_bysubject_help, 
                                         dv = .(meanRT), 
                                         wid = .(subject), 
                                         within = .(cue_validity, task_familiarity, switch_or_repeat),
                                         return_aov = TRUE) # without task sequence as between-subject factor here because all 4 participants started with non-informative blocks 

switch_type_target_help_anova$ANOVA

################### on Accuracy Rate ####################

# get rid of the NA switch type
switch_acc_target_df <- test_target_error_df %>%
  filter(!is.na(switch_type)) %>%
  mutate(switch_or_repeat = if_else(str_detect(switch_type, "switch"), "switch", "repeat")) %>%
  left_join(back2_switch_df, by = "trial_id")

# compute and analyze the switch cost of different conditions ----
switch_cost_acc_bysubject <- switch_acc_target_df %>%
  group_by(subject, cue_validity, switch_type, test_sequence) %>%
  summarise(acc_rate = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  pivot_wider(names_from = switch_type, values_from = acc_rate) %>%
  mutate(switch_cost_prac = - (repeat_prac - switch_prac),
         switch_cost_novel = - (repeat_novel - switch_novel)) %>%
  select(subject, cue_validity, test_sequence, switch_cost_prac, switch_cost_novel) %>%
  pivot_longer(c("switch_cost_prac", "switch_cost_novel"), names_to = "task_familiarity", values_to = "acc_switch_cost") %>%
  convert_as_factor(task_familiarity) %>%
  reorder_levels("task_familiarity", order = c("switch_cost_prac", "switch_cost_novel"))


(p_switch_cost_acc <- ggplot(switch_cost_acc_bysubject, aes(x = cue_validity, y = acc_switch_cost, fill = task_familiarity)) +
    geom_violin(position = dodge, size = 1) +
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .5)) +
    geom_boxplot(width = 0.3, position = dodge, size = 1) +
    stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
    ggtitle("factorized subject-level target switch cost") +
    theme_bw())

##### reporting for publication including mean and sd 
switch_cost_acc_bysubject3 <- switch_acc_target_df %>%
  group_by(subject, cue_validity, switch_type) %>%
  summarise(acc_rate = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  pivot_wider(names_from = switch_type, values_from = acc_rate) %>%
  mutate(switch_cost_prac = - (repeat_prac - switch_prac),
         switch_cost_novel = - (repeat_novel - switch_novel)) %>%
  select(subject, cue_validity, switch_cost_prac, switch_cost_novel) %>%
  pivot_longer(c("switch_cost_prac", "switch_cost_novel"), names_to = "task_familiarity", values_to = "acc_switch_cost") %>%
  convert_as_factor(task_familiarity) %>%
  reorder_levels("task_familiarity", order = c("switch_cost_prac", "switch_cost_novel")) %>%
  ungroup()

switch_cost_acc_descip <- switch_cost_acc_bysubject3 %>%
  group_by(cue_validity, task_familiarity) %>%
  summarise(M = round(mean(acc_switch_cost),3),
            subsd = sd(acc_switch_cost),
            SE = round(subsd/((n_sub)^.5), 3)) %>%
  ungroup()

## Plotting for publication
(p_switch_cost_acc2 <- ggplot(switch_cost_acc_bysubject, aes(x = cue_validity, y = acc_switch_cost, fill = task_familiarity)) +
    geom_split_violin(trim = FALSE, alpha = .3, width = .7)+
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .6),
                      name = "Switch type:",
                      breaks = c("switch_cost_prac", "switch_cost_novel"),
                      labels = c("To practiced task", "To novel task")) +
    geom_boxplot(width = 0.34, fatten = NULL, show.legend = FALSE, size = 1, position = position_dodge(width = .4))  +
    stat_summary(fun = mean, geom="point", shape=20, size=5, color="black", position = position_dodge(.37)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = .2, position = position_dodge(width = .37))+
    theme_apa(base_size = 14) +
    labs(x = "block type", y = "Switch cost in accuracy rate") +
    ylim(-0.3, 0.3) +
    scale_x_discrete(labels = c("testCue" = "informative", "testNocue" = "uninformative")))


# running ANOVA on the result
switch_cost_acc_anova <- ezANOVA(data = switch_cost_acc_bysubject, 
                                 dv = .(acc_switch_cost), 
                                 wid = .(subject), 
                                 within = .(cue_validity, task_familiarity),
                                 between = .(test_sequence),
                                 type = 3)

switch_cost_acc_anova

switch_cost_acc_anova_plot <- ezPlot(data = switch_cost_acc_bysubject, 
                                     dv = .(acc_switch_cost), 
                                     wid = .(subject), 
                                     within = .(cue_validity, task_familiarity),
                                     x = .(cue_validity),
                                     split = .(task_familiarity))

print(switch_cost_acc_anova_plot)


##### reporting for interaction effect
switch_cost_acc_prac <- switch_cost_acc_bysubject3 %>%
  filter(task_familiarity == "switch_cost_prac")

switch_cost_acc_novel <- switch_cost_acc_bysubject3 %>%
  filter(task_familiarity == "switch_cost_novel")


pt <- pairwise.t.test(switch_cost_acc_novel$acc_switch_cost, 
                      switch_cost_acc_novel$cue_validity, 
                      p.adjust.method = "bonferroni", 
                      paired = TRUE)

print(pt)
# another way of modeling switch cost, which is the way for REPORTING statistics ----

# switch_type_acc_bysubject <- switch_acc_target_df %>%
#   group_by(subject, test_sequence, cue_validity, switch_type) %>%
#   summarise(acc_rate = sum(accuracy)/length(accuracy)) %>%
#   ungroup()

switch_type_acc_bysubject_2 <- switch_acc_target_df %>%
  group_by(subject, test_sequence, cue_validity, switch_or_repeat, task_familiarity) %>%
  summarise(acc_rate = sum(accuracy)/length(accuracy)) %>%
  reorder_levels("task_familiarity", order = c("prac task", "novel task")) %>%
  mutate(exp = "exp3") %>%
  ungroup()

save(switch_type_acc_bysubject_2, file = paste0(here(), "/results/data_for_ANOVA_exp3.Rdata")) # save data for subsequent across exp analysis with exp 4

# (p_switch_type <- ggplot(switch_type_acc_bysubject, aes(x = cue_validity, y = acc_rate, fill = switch_type)) +
#     geom_violin(position = dodge, size = 1) +
#     scale_fill_manual(values=wes_palette(n=4, name="Royal1")) +
#     geom_boxplot(width = 0.3, position = dodge, size = 1) +
#     stat_summary(fun = mean, geom="point", shape=20, size=4, color="black", position = dodge) +
#     ggtitle("factorized subject-level target accuray rate by switch type") +
#     theme_bw()
# )
# 
# switch_type_acc_anova <- ezANOVA(data = switch_type_acc_bysubject, 
#                                  dv = .(acc_rate), 
#                                  wid = .(subject), 
#                                  within = .(cue_validity, switch_type),
#                                  between = .(test_sequence),
#                                  type = 3)

# switch_type_acc_anova

switch_type_acc_anova_2 <- ezANOVA(data = switch_type_acc_bysubject_2, 
                                   dv = .(acc_rate), 
                                   wid = .(subject), 
                                   within = .(cue_validity, task_familiarity, switch_or_repeat),
                                   between = .(test_sequence),
                                   type = 3,
                                   return_aov = TRUE)

switch_type_acc_anova_2$ANOVA

# more about the significant 3-way interaction
switch_type_acc_anova_2_stat <- ezStats(data = switch_type_acc_bysubject_2, 
                                         dv = .(acc_rate), 
                                         wid = .(subject), 
                                         within = .(cue_validity, task_familiarity, switch_or_repeat))

print(switch_type_acc_anova_2_stat)


emm_acc <- emmeans(switch_type_acc_anova_2$aov, pairwise ~ (switch_or_repeat * task_familiarity|cue_validity))
emm_acc

# explore the effect of 2-back switch type(switch or repeat):
back2_switch_type_acc_bysubject_2 <- switch_acc_target_df %>%
  filter(!is.na(pre_switch_or_repeat)) %>%
  group_by(subject, test_sequence, cue_validity, switch_or_repeat, pre_switch_or_repeat, task_familiarity) %>%
  summarise(acc_rate = sum(accuracy)/length(accuracy)) %>%
  reorder_levels("task_familiarity", order = c("prac task", "novel task")) %>%
  mutate(exp = "exp3") %>%
  ungroup()

switch_type_acc_anova_3 <- ezANOVA(data = back2_switch_type_acc_bysubject_2, 
                                   dv = .(acc_rate), 
                                   wid = .(subject), 
                                   within = .(cue_validity, task_familiarity, switch_or_repeat, pre_switch_or_repeat),
                                   between = .(test_sequence),
                                   type = 3,
                                   return_aov = TRUE)

switch_type_acc_anova_3$ANOVA


# reporting for publication
switch_type_acc_descript <- switch_acc_target_df %>%
  group_by(subject, cue_validity, task_familiarity, switch_or_repeat) %>%
  summarise(acc_rate = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  group_by(cue_validity, task_familiarity, switch_or_repeat) %>%
  summarise(M = round(mean(acc_rate),3),
            subsd = sd(acc_rate),
            SE = round(subsd/((n_sub)^.5), 3)) %>%
  ungroup()


# plotting for publication
switch_type_acc_plot <- switch_acc_target_df %>%
  group_by(subject, cue_validity, task_familiarity, switch_or_repeat) %>%
  summarise(acc_rate = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  reorder_levels("task_familiarity", order = c("prac task", "novel task"))


(p_switch_type_acc2 <- ggplot(switch_type_acc_plot, aes(x = cue_validity, y = acc_rate, fill = task_familiarity, color = switch_or_repeat)) +
    geom_split_violin(trim = FALSE, alpha = .3, width = .7)+
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .4),
                      name = "task novelty",
                      breaks = c("prac task", "novel task"),
                      labels = c("practiced", "novel")) +
    scale_color_manual(values = alpha(c("#c0c0c0", "#000000"), .8),
                      name = "trial type",
                      breaks = c("repeat", "switch"),
                      labels = c("repeat", "switch")) +
    geom_boxplot(width = 0.34, fatten = NULL, show.legend = FALSE, size = 1, position = position_dodge(width = .4))  +
    stat_summary(fun = mean, geom="point", shape=20, size=6, position = position_dodge(.39)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = .2, position = position_dodge(width = .39))+
    theme_apa(base_size = 14) +
    labs(x = "block type", y = "Average accuracy") +
    coord_cartesian(ylim=c(0.6, 0.99)) +
    scale_x_discrete(labels = c("testCue" = "informative", "testNocue" = "uninformative")) +
    theme(axis.title.x = element_blank()))
  

# combining all plots for publication
(p_target_rt2 | p_acc_rate2) / p_switch_type_acc2 + 
  plot_annotation(title = 'The behavioral results of Experiment 3',
                  tag_levels = 'A')


# only for people who find the cue helpful ----
switch_type_acc_bysubject_help <- switch_type_acc_bysubject_2 %>%
  filter(subject %in% subj_tasktype_help)

switch_type_acc_help_anova <- ezANOVA(data = switch_type_acc_bysubject_help, 
                                      dv = .(acc_rate), 
                                      wid = .(subject), 
                                      within = .(cue_validity, task_familiarity, switch_or_repeat),
                                      return_aov = TRUE)

switch_type_acc_help_anova$ANOVA

######################################################
########### rule overlapping analysis ################
######################################################

################## analysis on RT ####################

rule_overlap_rt_bysub <- test_target_df %>%
  filter(block_type == "testNocue", prac_or_novel == "novel task", !is.na(rule_overlap)) %>%
  group_by(subject, rule_overlap) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(rule_overlap < 2)

a <- rule_overlap_rt_bysub %>%
  group_by(subject) %>%
  summarise(count = n())

rule_overlap_rt_bysub %>%
  filter(subject == "ckxxfv9h")


(
  p_rule_rt_overlap <- ggplot(rule_overlap_rt_bysub, aes(x = rule_overlap, y = meanRT)) +
    geom_point(size = 4, colour = "#4E84C4") +
    geom_line(aes(group = subject), 
              size = 0.7, alpha = .5) +
    stat_summary(fun = mean, geom="point", size=8, color="red")+
    stat_summary(fun = mean, geom="line", size=2, color="red")+
    theme_bw()
)

rule_overlap_rt_anova <- ezANOVA(data = rule_overlap_rt_bysub, 
                                 dv = .(meanRT), 
                                 wid = .(subject), 
                                 within = .(rule_overlap),
                                 type = 3)

rule_overlap_rt_anova


################## analysis on ACC ###################

rule_overlap_acc_bysub <- test_target_error_df %>%
  filter(block_type == "testNocue", prac_or_novel == "novel task", !is.na(rule_overlap)) %>%
  group_by(subject, rule_overlap) %>%
  summarise(count = n(),
            accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup() %>%
  filter(rule_overlap < 2)

(
  p_rule_overlap_acc <- ggplot(rule_overlap_acc_bysub, aes(x = rule_overlap, y = accRate)) +
    geom_point(size = 4, colour = "#4E84C4") +
    geom_line(aes(group = subject), 
              size = 0.7, alpha = .5) +
    stat_summary(fun = mean, geom="point", size=8, color="red") +
    stat_summary(fun = mean, geom="line", size=2, color="red") +
    theme_bw()
)

rule_overlap_acc_anova <- ezANOVA(data = rule_overlap_acc_bysub, 
                                 dv = .(accRate), 
                                 wid = .(subject), 
                                 within = .(rule_overlap),
                                 type = 3)

rule_overlap_acc_anova

## comparing the switching cost effect of exp3(significant) with exp4(non-significant)

load(file = paste0(here(), "/results/data_for_ANOVA_exp3.Rdata"))

switch_type_acc_bysubject_exp3 <- switch_type_acc_bysubject_2
rm(switch_type_acc_bysubject_2)

load(file = paste0(here(), "/Replication", "/results/data_for_ANOVA_exp4.Rdata"))
switch_type_acc_bysubject_exp4 <- switch_type_acc_bysubject_2
rm(switch_type_acc_bysubject_2)

switch_type_acc_bysubject_both <- rbind(switch_type_acc_bysubject_exp3, switch_type_acc_bysubject_exp4)

# switch_type_acc_anova
switch_type_acc_anova_exp <- ezANOVA(data = switch_type_acc_bysubject_both, 
                                   dv = .(acc_rate), 
                                   wid = .(subject), 
                                   within = .(cue_validity, task_familiarity, switch_or_repeat),
                                   between = .(test_sequence),
                                   type = 3,
                                   return_aov = TRUE)

switch_type_acc_anova_exp$ANOVA
