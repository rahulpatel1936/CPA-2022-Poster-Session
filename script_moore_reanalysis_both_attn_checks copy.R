# Author: Rahul Patel
# Email: rpatel40@uoguelph.ca
# Notes: This script is sample code run for the secondary analysis. The results here do not represent the actual
## results ascertained from the analyses. For a description of the actual results, please view the CPA
## PowerPoint file. 

# Packages ----------------------------------------------------------------
options("scipen" = 100, "digits" = 7)
options(max.print = 2000)
pacman::p_load(tidyverse, janitor, skimr, Hmisc, effsize, MBESS, car, TOSTER, apaTables, remotes)
remotes::install_github("dstanley4/ggpsyc")

# Generate Data ---------------------------------------------------------------
set.seed(123)
moore_data <- data.frame(
  consent = factor(sample(c("0", "1"), size = 215, replace = TRUE, prob = c(0.01, 0.99)), levels = c("0", "1")),
  finished = factor(sample(c("0", "1"), size = 215, replace = TRUE, prob = c(0.01, 0.99)), levels = c("0", "1")),
  condition = factor(sample(c("Absent", "Present", "Rapport"), size = 215, replace = TRUE, prob = c(0.33, 0.33, 0.33)), levels = c("Absent", "Present", "Rapport")),
  
  DIM_1 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_2 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_3 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_4 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_5 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_6 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_7 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_8 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_9 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_10 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_11 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_12 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_13 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_14 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_15 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  DIM_16 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  
  HIM_1 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_2 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_3 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_4 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_5 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_6 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_7 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_8 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_9 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_10 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_11 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  HIM_12 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = NULL),
  
  ATTN_1 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = c(0.02, 0.94, 0.02, 0.02, 0.02)),
  ATTN_2 = sample(c(1, 2, 3, 4, 5), size = 215, replace = TRUE, prob = c(0.02, 0.02, 0.02, 0.94, 0.02))
)


# Data Cleaning -----------------------------------------------------------
moore_data_clean <- moore_data %>%
  filter(consent == 1,
         finished == 1,
         ATTN_1 == 2 & ATTN_2 == 4,
         condition != "Rapport")

nrow(moore_data_clean) # number of participants

# How many participants in each condition 
moore_data_clean %>%
  group_by(condition) %>%
  summarise(n())

# Scale Scores ------------------------------------------------------------
## Map Item Names ----------------------------------------------------------
keys <- list(
  # Honest IM
  honest_im = c("HIM_1", "HIM_2", "HIM_3", "HIM_4", "HIM_5", "HIM_6", "HIM_7", "HIM_8", "HIM_9", "HIM_10", "HIM_11", "HIM_12"),
  
  # Deceptive IM
  deceptive_im = c("DIM_1", "DIM_2", "DIM_3", "DIM_4", "DIM_5", "DIM_6", "DIM_7", "DIM_8", "DIM_9", "DIM_10", "DIM_11", "DIM_12", "DIM_13", "DIM_14", "DIM_15", "DIM_16")
)

scoring_details <- psych::scoreItems(keys,
                              moore_data_clean,
                              min = 1,
                              max = 5,
                              missing = TRUE,
                              impute = "none")


scoring_details$scores

moore_data_clean_items_scores <- cbind(moore_data_clean, scoring_details$scores)

# Reliability Analyses ----------------------------------------------------

## Honest IM ---------------------------------------------------------------
honest_im_alpha <- moore_data_clean %>%
  select(starts_with("HIM")) %>%
  psych::alpha(.)

print(honest_im_alpha$total)

## Deceptive IM ---------------------------------------------------------------
deceptive_im_alpha <- moore_data_clean %>%
  select(starts_with("DIM")) %>%
  psych::alpha(.)

print(deceptive_im_alpha$total)


# Pull Honest and Deceptive IM scores
# Honest IM
honest_IM_average_question <- moore_data_clean_items_scores %>%
  filter(condition == "Present") %>%
  select(honest_im) %>%
  pull()

honest_IM_average_control <- moore_data_clean_items_scores %>%
  filter(condition == "Absent") %>%
  select(honest_im) %>%
  pull()

# Deceptive IM
deceptive_IM_average_question <- moore_data_clean_items_scores %>%
  filter(condition == "Present") %>%
  select(deceptive_im) %>%
  pull()

deceptive_IM_average_control<- moore_data_clean_items_scores %>%
  filter(condition == "Absent") %>%
  select(deceptive_im) %>%
  pull()

# Calculate mean difference Mdiff (CI provided in output for t-test)
honest_IM_average_mdiff <- mean(honest_IM_average_question) - mean(honest_IM_average_control)
print(honest_IM_average_mdiff)


# calculate difference in standardized units (effect size)
honest_IM_average_d <- cohen.d(honest_IM_average_question, honest_IM_average_control,
                               hedges.correction = TRUE,
                               na.rm = TRUE,
                               pooled = TRUE)
print(honest_IM_average_d)

# Calculate mean difference Mdiff (CI provided in output for t-test)
deceptive_IM_average_mdiff <- mean(deceptive_IM_average_question) - mean(deceptive_IM_average_control)
print(deceptive_IM_average_mdiff)


# calculate difference in standardized units (effect size)
deceptive_IM_average_d <- cohen.d(deceptive_IM_average_question, deceptive_IM_average_control,
                                  hedges.correction = TRUE,
                                  na.rm = TRUE,
                                  pooled = TRUE)
print(deceptive_IM_average_d)

# Visualization -----------------------------------------------------------
moore_data_clean_items_scores %>%
  select(honest_im, deceptive_im, condition) %>%
  group_by(condition) %>%
  skim()

# Honest IM
honest_IM_average_graph_enhanced <- moore_data_clean_items_scores %>%
  ggplot(aes(x = condition, y = honest_im)) +
  ggpsyc::stat_jitter_dodge(size = .50) +
  stat_summary(fun = mean,
               geom = "point",
               size = 2) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = 0.2) +
  ggpsyc::stat_two_group_ci(paired = FALSE, text_size = 2) +
  coord_cartesian(ylim = c(1, 5)) +
  scale_y_continuous(breaks = seq(1, 5, by = 0.5),
                     expand = c(0,0)) +
  xlab("Question Provision Condition") +
  ylab("Average Honest Impression Management") +
  theme_classic(18)

print(honest_IM_average_graph_enhanced)

ggsave(plot = honest_IM_average_graph_enhanced, 
       height = 7, 
       width = 7, 
       filename = "Figure1.TIFF", 
       dpi = "print")

# Deceptive IM 
deceptive_IM_average_graph_enhanced <- moore_data_clean_items_scores %>%
  ggplot(aes(x = condition, y = deceptive_im)) +
  ggpsyc::stat_jitter_dodge(size = .50) +
  stat_summary(fun = mean,
               geom = "point",
               size = 2) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = 0.2) +
  ggpsyc::stat_two_group_ci(paired = FALSE, text_size = 2) +
  coord_cartesian(ylim = c(1, 5)) +
  scale_y_continuous(breaks = seq(1, 5, by = 0.5),
                     expand = c(0,0)) +
  xlab("Question Provision Condition") +
  ylab("Average Deceptive Impression Management") +
  theme_classic(18)

print(deceptive_IM_average_graph_enhanced)


#OR (MAC OR PC)
ggsave(plot = deceptive_IM_average_graph_enhanced, 
       height = 7, 
       width = 7, 
       filename = "Figure2.TIFF", 
       dpi = "print")



# Statistical Analyses ----------------------------------------------------
## Honest IM ---------------------------------------------------------------
### Levene's Homogeneity of Variance Assumption -----------------------------
HIM_average_levene <- leveneTest(honest_im ~ condition, 
                                 data = moore_data_clean_items_scores,
                                 center = "median")
print(HIM_average_levene)


### T-test ------------------------------------------------------------------
HIM_average_t <- t.test(honest_im ~ condition,
                        data = moore_data_clean_items_scores,
                        var.equal = TRUE,
                        alternative = "two.sided")
print(HIM_average_t)

## Deceptive IM ---------------------------------------------------------------
### Levene's Homogeneity of Variance Assumption -----------------------------
DIM_average_levene <- leveneTest(deceptive_im ~ condition, 
                                 data = moore_data_clean_items_scores,
                                 center = "median")
print(DIM_average_levene)

### T-test ------------------------------------------------------------------
DIM_average_t <- t.test(deceptive_im ~ condition,
                        data = moore_data_clean_items_scores,
                        var.equal = TRUE,
                        alternative = "two.sided")
print(DIM_average_t)



### Equivalence Test --------------------------------------------------------
equivalence_test <- tsum_TOST(m1 = mean(deceptive_IM_average_question),
        sd1 = sd(deceptive_IM_average_question),
        m2 = mean(deceptive_IM_average_control),
        sd2 = sd(deceptive_IM_average_control),
        n1 = length(deceptive_IM_average_question), n2 = length(deceptive_IM_average_control),
        low_eqbound = -0.17,
        high_eqbound = 0.17,
        eqbound_type = "SMD",
        alpha = 0.05,
        var.equal = TRUE)

summary(equivalence_test)
plot(equivalence_test)

ggsave(plot = equivalence_test, 
       height = 7, 
       width = 7, 
       filename = "Figure3.TIFF", 
       dpi = "print")
