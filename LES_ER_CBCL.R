### Set working directory ####
setwd("C:/Users/Kate Scheuer/OneDrive - UW/Desktop/Lab/aces_emotionreg_cbcl")

### Load libraries ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(nortest)
library(psych)
library(lavaan)
library(ggpattern)
library(misty)

### Read in raw data ####
#### Gender data ####
gish_y_gi <- read.csv("./data/gish_y_gi.csv", header = T)
#### DERS-P for emotion regulation ####
mh_p_ders <- read.csv("./data/mh_p_ders.csv", header = T)
#### CBCL for psychopathology symptoms ####
mh_p_cbcl <- read.csv("./data/mh_p_cbcl.csv", header = T)
#### Longitudinal tracking data ####
abcd_y_lt <- read.csv("./data/abcd_y_lt.csv", header = T)
#### LES for exposure to negative life events
mh_y_le <- read.csv("./data/mh_y_le.csv", header = T)

### Prepare gender data for analysis ####
#### Identify gender groups ####
genderdata <- 
    # raw gender data
    gish_y_gi %>%
    # keep only data from year 3 and 4 follow-up visits
    # Before this step, n should be 49083. After this step, n should be 15064.
    filter(eventname=="3_year_follow_up_y_arm_1"|
           eventname=="4_year_follow_up_y_arm_1") %>%
    # convert numeric sex values to human-readable character strings
    mutate(sex = case_when(
                    kbi_sex_assigned_at_birth==1 ~ "male",
                    kbi_sex_assigned_at_birth==2 ~ "female",
                    kbi_sex_assigned_at_birth==777 ~ "dont_know",
                    kbi_sex_assigned_at_birth==999 ~ "refuse"
                    )) %>%
    # make binary column for whether or not participant is female
    mutate(sex_female = if_else(sex=="female",1,0)) %>%
    # convert numeric gender values to human-readable character strings
    mutate(gender = case_when(
                      kbi_gender==1 ~ "boy",
                      kbi_gender==2 ~ "girl",
                      kbi_gender==3 ~ "nb",
                      kbi_gender==4 ~ "dont_understand",
                      kbi_gender==777 ~ "refuse"
                      )) %>%
    # convert numeric values for trans identity to human-readable character strings
    mutate(trans = case_when(
                      kbi_y_trans_id==1 ~ "yes",
                      kbi_y_trans_id==2 ~ "maybe",
                      kbi_y_trans_id==3 ~ "no",
                      kbi_y_trans_id==4 ~ "dont_understand",
                      kbi_y_trans_id==777 ~ "refuse"
                      )) %>%
    # combine gender and trans identity information to make three gender groups
    mutate(genderid = case_when(
                        kbi_gender==777 ~ "refuse",
                        kbi_y_trans_id==777 ~ "refuse",
                        kbi_gender==4 ~ "dont_understand",
                        kbi_y_trans_id==4 ~ "dont_understand",
                        kbi_gender==1 & kbi_y_trans_id==1 ~ "gd", #"trans_boy",
                        kbi_gender==1 & kbi_y_trans_id==2 ~ "gd", #"trans_boy",
                        kbi_gender==1 & kbi_y_trans_id==3 ~ "cis_boy",
                        kbi_gender==2 & kbi_y_trans_id==1 ~ "gd", #"trans_girl",
                        kbi_gender==2 & kbi_y_trans_id==2 ~ "gd", #"trans_girl",
                        kbi_gender==2 & kbi_y_trans_id==3 ~ "cis_girl",
                        kbi_gender==3 & kbi_y_trans_id==1 ~ "gd", #"nb",
                        kbi_gender==3 & kbi_y_trans_id==2 ~ "gd", #"nb",
                        kbi_gender==3 & kbi_y_trans_id==3 ~ "gd" #"nb"
                        )) %>%
    # make binary column for whether or not participant is cis girl
    mutate(gender_cisgirl = if_else(genderid=="cis_girl",1,0)) %>%
    # make binary column for whether or not participant is gender diverse
    mutate(gender_gd = if_else(genderid=="gd",1,0)) %>%
    # make binary column for whether or not participant is cis boy
    mutate(gender_cisboy = if_else(genderid=="cis_boy",1,0)) %>%
    # keep only columns relevant to analysis
    select(src_subject_id,eventname,sex,gender,trans,sex_female,
           genderid,gender_cisgirl,gender_gd,gender_cisboy) %>%
    # remove subjects who refused to answer and/or did not understand gender
    # or trans questions. Before this step, n should be 15064. After this step,
    # n should be 14495.
    filter(genderid!="refuse",
           genderid!="dont_understand")

#### Count number of subjects per gender group per data collection year ####
genderdata %>% group_by(eventname) %>% count(genderid)

### Prepare CBCL data for analysis ####
cbcldata <- 
    # raw CBCL data
    mh_p_cbcl %>%
    # select only columns relevant to analysis
    select(src_subject_id,eventname,
           cbcl_scr_syn_totprob_t,
           cbcl_scr_syn_internal_t,
           cbcl_scr_syn_external_t
           ) %>%
    # rename subscale columns to be more human-readable and shorter
    rename(cbcl_total = cbcl_scr_syn_totprob_t,
           cbcl_ext = cbcl_scr_syn_external_t,
           cbcl_int = cbcl_scr_syn_internal_t
           ) %>%
    # add column with log-transformed CBCL total problems values
    mutate(log_cbcl_total = log(cbcl_total)) %>%
    # add column with log-transformed CBCL internalizing values
    mutate(log_cbcl_int = log(cbcl_int)) %>%
    # add column with log-transformed CBCL externalizing values
    mutate(log_cbcl_ext = log(cbcl_ext)) 

### Prepare DERS-P data for analysis ####
#### Create cumulative score ####
dersdata <- 
    # raw DERS-P data
    mh_p_ders %>%
    # remove subjects who refused to answer one or more items. Before this step,
    # n should be 14708. After this step, n should be 14225.
    filter(!if_any(everything(), ~ . == 777)) %>%
    # add column to reverse score "my child pays attention to how he/she feels"
    mutate(rev_ders_attn_awareness_p = 5 + 1 - ders_attn_awareness_p) %>%
    # add column to reverse score "my child is attentive to his/her feelings"
    mutate(rev_ders_feelings_attentive_p = 5 + 1 - ders_feelings_attentive_p) %>%
    # add column to reverse score "my child cares about what he/she is feeling"
    mutate(rev_ders_feelings_care_p = 5 + 1 - ders_feelings_care_p) %>%
    # add column to reverse score "when my child is upset, he/she acknowledges
    # his/her emotions"
    mutate(rev_ders_upset_ack_p = 5 + 1 - ders_upset_ack_p) %>%
    # add column to reverse score "my child is clear about his/her feelings"
    mutate(rev_ders_clear_feelings_p = 5 + 1 - ders_clear_feelings_p) %>%
    # add column to reverse score "my child knows exactly how he/she is feeling"
    mutate(rev_ders_feelings_know_p = 5 + 1 - ders_feelings_know_p) %>%
    # add column to reverse score "when my child is upset, he/she feels like
    # he/she can remin in control of his/her behaviors"
    mutate(rev_ders_upset_behavior_control_p = 5 + 1 - ders_upset_behavior_control_p) %>%
    # add column to reverse score "when my child is upset, he/she knows that
    # he/she can find a way to eventually feel better"
    mutate(rev_ders_upset_better_p = 5 + 1 - ders_upset_better_p) %>%
    # add column to sum across all items (using using reverse-scored versions of
    # eight items above) and make one cumulative score
    mutate(ders_total = rowSums(
                          across(!all_of(
                                    c("src_subject_id",
                                      "eventname",
                                      "ders_p_select_language___1",
                                      "ders_attn_awareness_p",
                                      "ders_feelings_attentive_p",
                                      "ders_feelings_care_p",
                                      "ders_upset_ack_p",
                                      "ders_clear_feelings_p",
                                      "ders_feelings_know_p",
                                      "ders_upset_behavior_control_p",
                                      "ders_upset_better_p"
                                      ))))) %>%
    # add column for log-transformed cumulative score
    mutate(log_ders_total = log(ders_total)) %>%
    # select only columns relevant to analysis
    select(src_subject_id,eventname,ders_total,log_ders_total)

#### Provide summary statistics for DERS-P data by data collection year ####
dersdata %>% group_by(eventname) %>% summary()

### Prepare LES data for analysis ####
#### Identify and prepare relevant columns ####
ledata <- 
    # raw LES data
    mh_y_le %>% 
    # remove rows with NA in any of the main items asking about whether event
    # was or was not experienced because sum of all bad events counts NA values
    # as 0 (ie subject with all NA to individual items will still be given 0
    # for the sum score). note: exclude items about homelessness and knowing
    # someone who attempted suicide because those were only asked in year 4.
    # Before this step, n should be 49151. After this step, n should be 14850.
    filter(!if_any(all_of(
                        c("ple_died_y","ple_injured_y","ple_crime_y",
                          "ple_friend_y","ple_friend_injur_y",
                          "ple_financial_y","ple_sud_y","ple_ill_y",
                          "ple_injur_y","ple_argue_y","ple_job_y",
                          "ple_away_y","ple_arrest_y","ple_friend_died_y",
                          "ple_mh_y","ple_sib_y","ple_victim_y","ple_separ_y",
                          "ple_law_y","ple_school_y","ple_move_y","ple_jail_y",
                          "ple_step_y","ple_new_job_y","ple_new_sib_y",
                          "ple_foster_care_y","ple_hit_y","ple_hospitalized_y",
                          "ple_lockdown_y","ple_shot_y","ple_deported_y"
                          )), is.na)) %>%
    # select only columns relevant to analysis
    select(src_subject_id,eventname,ple_y_ss_total_bad) %>%
    # rename column with total number of events experienced and described as bad
    rename(total_bad_le = ple_y_ss_total_bad) %>%
    # add column for log-transformed version of total number of bad events
    mutate(log_total_bad_le = log(total_bad_le+1))

#### Provide summary statistics for LES data by data collection year ####
ledata %>% group_by(eventname) %>% summary()

### Combine all data for analysis into one data frame ####
alldata <- 
  # prepared gender data
  genderdata %>%
  # add longitudinal tracking data based on subject ID and data collection year
  left_join(select(abcd_y_lt,
                      c(src_subject_id,eventname,
                        site_id_l,interview_age)),
            by=c("src_subject_id","eventname")) %>%
  # rename age column
  rename(age = interview_age, site = site_id_l) %>%
  # add DERS-P data based on subject ID and data collection year
  left_join(dersdata,by=c("src_subject_id","eventname")) %>%
  # add LES data based on subject ID and data collection year
  left_join(ledata,by=c("src_subject_id","eventname")) %>%
  # add CBCL data based on subject ID and data collection year
  left_join(cbcldata,by=c("src_subject_id","eventname")) %>%
  # Z score continuous variables
  mutate(across(
    c(age, ders_total, total_bad_le, 
      cbcl_total, cbcl_int, cbcl_ext,
      log_ders_total, log_total_bad_le, log_cbcl_total, 
      log_cbcl_int, log_cbcl_ext),
    ~ as.numeric(scale(.)),
    .names = "Z_{.col}"
  )) %>%
  # make genderid, sex, and site factors rather than characters
  mutate(across(c(genderid, sex, site), as.factor)) %>%
  # remove subjects without LES or DERS or CBCL data in either year 3 or year 4
  # follow-up. Before this step, n should be 14495. After this step, n should 
  # be 13513
  filter(!is.na(ders_total), !is.na(total_bad_le),
         !is.na(cbcl_total), !is.na(cbcl_int), !is.na(cbcl_ext))

### Get general overview of all data ####
#### See type of each column ####
str(alldata)

#### See all unique values for each column ####
map(alldata,unique)

#### Get raw number and percentage for each gender group and data collection year ####
alldata %>% 
  group_by(eventname) %>% 
  count(genderid) %>% 
  mutate(percentage = n / sum(n) * 100)

#### Create separate data frame for just data from year 4 follow-up visit ####
# n should be 4372
yr4data <- alldata %>% filter(eventname=="4_year_follow_up_y_arm_1")

#### Create separate data frame for just data from year 3 follow-up visit ####
# n should be 9326
yr3data <- alldata %>% filter(eventname=="3_year_follow_up_y_arm_1")

#### Get general summary of values for each column for data from year 4 visit ####
yr4data %>% summary()

#### Get general summary of values for each column for data from year 3 visit ####
yr3data %>% summary()

#### Determine how many subjects switched gender groups between years 3 and 4 ####
gender_change <-
    # raw year 4 data
    yr4data %>%
    # select only genderid and subject id columns
    select(src_subject_id,genderid) %>%
    # rename genderid column to show corresponds to genderid in year 4
    rename(genderid_yr4 = genderid) %>%
    # add genderid from year 3 for subjects present in year 4 data
    left_join(select(yr3data,
                     c(src_subject_id,genderid)),
              by=c("src_subject_id")) %>%
    # rename genderid column to show correspond to genderid in year 3
    rename(genderid_yr3 = genderid) %>%
    filter(genderid_yr4 != genderid_yr3) %>%
    group_by(genderid_yr3,genderid_yr4) %>% 
    count()
gender_change


### Assess normality of distributions of each variable from all data ####
# p-value < 0.05 suggests data is not normally distributed
# walk() applies a function, in this case ad.test, to a list
# cat() makes it print the variable name before each test
# note: Shapiro-Wilk test only allows up to 5000 samples so using Anderson-
# Darling test.
# All variables are non-normally distributed, even after log transformation.
walk(c("total_bad_le", "log_total_bad_le", 
       "ders_total", "log_ders_total", 
       "cbcl_total", "log_cbcl_total", 
       "cbcl_int", "log_cbcl_int", 
       "cbcl_ext", "log_cbcl_ext"), 
     ~ {
       cat("Variable:", .x, "\n")
       print(ad.test(alldata[[.x]]))
     })

### Create basic histogram for each variable ####
# Store histograms in list in case want to view later
variable_histograms <- 
  map(c("total_bad_le", "log_total_bad_le", 
       "ders_total", "log_ders_total", 
       "cbcl_total", "log_cbcl_total", 
       "cbcl_int", "log_cbcl_int", 
       "cbcl_ext", "log_cbcl_ext"), 
      # note that !!sym(.x) turns the variables in the list above into arguments
      # that can be passed to ggplot
     ~ ggplot(alldata, aes(x = !!sym(.x))) + 
         geom_histogram() +
         ggtitle(.x)
             )
# Print all histograms from stored list
print(variable_histograms)

### Create correlation matrix for all variables from year 4 data ####
# Make correlation matrix
# All variables are significantly correlated.
corrmat <- 
  # year 4 data only
  yr4data %>% 
  # select relevant columns
  select(c(total_bad_le,ders_total,
           cbcl_total,cbcl_int,cbcl_ext)) %>% 
  # run correlation tests for all pairs of variables, adjust using
  corr.test(adjust="fdr")
# print correlation matrix, correlation coefficients, and p-values
# note that values are already rounded to the decimal place which is showing
print(corrmat,digits=3)
# See fdr-adjusted p-value for each correlation test
# note that list gives p values above diagonal, going across rows (ie not down
# columns) 
corrmat$p.adj

### Plot variables against each other ####
#### Without gender
##### Scatterplots of DERS vs CBCL total problems, internalizing, externalizing ####
outcome_list <- c("cbcl_total","cbcl_int","cbcl_ext")
ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  ders_plot <- ggplot(aes(x=ders_total,y=.data[[outcome]]),data=yr4data) +
                  geom_point(size=1) +
                  geom_smooth(method="lm",
                              se=FALSE,
                              linewidth=1.25,color="black") +
                  scale_x_continuous(expand = c(0,0),
                                     breaks=seq(25,150,25),
                                     limits = c(25,155)) +
                  scale_y_continuous(expand = c(0,0),
                                     breaks=seq(0,100,20),
                                     limits = c(0,100)) +
                  theme_classic()
  ders_plot_list[[outcome]] <- ders_plot
  # Save plot
  # ggsave(paste0("ders_vs_",outcome,"_nogender.tiff"),
  #        width=6,height=6,units = "in")
}

##### Barplots of LES vs CBCL total problems, internalizing, and externalizing ####
###### Create table with summary stats for barplot ####
les_vs_cbcl_summary_stats <- yr4data %>% 
  group_by(total_bad_le) %>% 
  summarise(cbcl_total=mean(cbcl_total,na.rm=TRUE),
            cbcl_int=mean(cbcl_int,na.rm=TRUE),
            cbcl_ext=mean(cbcl_ext,na.rm=TRUE),
            n=n(),
            .groups="drop") %>%
  complete(total_bad_le)
###### Create barplots ####
outcome_list <- c("cbcl_total","cbcl_int","cbcl_ext")
les_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  les_plot <- ggplot(aes(x=total_bad_le,y=.data[[outcome]]),
                     data=les_vs_cbcl_summary_stats) +
                geom_bar(stat="identity",
                         position="dodge",
                         color="black",width=0.75) +
                scale_y_continuous(expand=c(0,0),
                                   breaks=seq(0,80,10),limits = c(0,80)) +
                scale_x_continuous(breaks=seq(0,15,1),expand=c(0,0)) +
                theme_classic()
  les_plot_list[[outcome]] <- les_plot
  # Save plot
  # ggsave(paste0("les_vs_",outcome,"_nogender.tiff"),
  #        width=6,height=6,units = "in")
}

#### With gender
##### Scatterplots of DERS vs CBCL total problems, internalizing, externalizing ####
outcome_list <- c("cbcl_total","cbcl_int","cbcl_ext")
ders_gender_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  ders_gender_plot <- ggplot(aes(x=ders_total,y=.data[[outcome]],
                                 color=genderid,
                                 shape=genderid),
                             data=yr4data) +
    geom_point(size=1) +
    geom_smooth(method="lm",
                se=FALSE,
                size=1.25) +
    scale_x_continuous(expand = c(0,0),
                       breaks=seq(25,150,25),
                       limits = c(25,155)) +
    scale_y_continuous(expand = c(0,0),
                       breaks=seq(0,100,20),
                       limits = c(0,100)) +
    theme_classic()
  ders_gender_plot_list[[outcome]] <- ders_gender_plot
  # Save plot
  # ggsave(paste0("ders_vs_",outcome,"_gender.tiff"),
  #        width=6,height=6,units = "in")
}

##### Barplots of LES vs CBCL total problems, internalizing, and externalizing ####
###### Create table with summary stats for barplot ####
les_vs_cbcl_w_gender_summary_stats <- 
  yr4data %>% 
  group_by(total_bad_le,genderid) %>% 
  summarise(cbcl_total=mean(cbcl_total,na.rm=TRUE),
            cbcl_int=mean(cbcl_int,na.rm=TRUE),
            cbcl_ext=mean(cbcl_ext,na.rm=TRUE),
            n=n(),
            .groups="drop") %>%
  # ensure all combinations of gender and number of bad events are
  # present so can be included on plot
  complete(total_bad_le, genderid, fill = list(n = 0, prop = 0)) %>% 
  mutate(total_bad_le = replace_na(total_bad_le,0))
###### Create barplots ####
outcome_list <- c("cbcl_total","cbcl_int","cbcl_ext")
les_gender_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  les_plot <- ggplot(aes(x=total_bad_le,y=.data[[outcome]],
                         color=genderid,
                         fill=genderid),
                     data=les_vs_cbcl_w_gender_summary_stats) +
    geom_bar(stat="identity",
             position="dodge",
             width=0.75) +
    scale_y_continuous(expand=c(0,0),
                       breaks=seq(0,80,10),limits = c(0,80)) +
    scale_x_continuous(breaks=seq(0,15,1),expand=c(0,0)) +
    theme_classic()
  les_gender_plot_list[[outcome]] <- les_plot
  # Save plot
  # ggsave(paste0("les_vs_",outcome,"_nogender.tiff"),
  #        width=6,height=6,units = "in")
}

### Determine whether age differs based only on gender ####
#### Full data set ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether age differs 
# significantly based on gender group
# Test is significant (p = 9.018e-06)
kruskal.test(age ~ genderid, data = alldata)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on age
pairwise.wilcox.test(alldata$total_bad_le, alldata$genderid,
                     p.adjust.method = "fdr")

#### Year 3 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether age differs
# significantly based on gender group
# Test is significant (p = 0.007951)
kruskal.test(age ~ genderid, data = yr3data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on age
pairwise.wilcox.test(yr3data$total_bad_le, yr3data$genderid,
                     p.adjust.method = "fdr")

#### Year 4 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether age differs
# significantly based on gender group
# Test is not significant (p = 0.07834)
kruskal.test(age ~ genderid, data = yr4data)

#### Get summary statistics for age ####
##### Summary statistics for full data set ####
alldata %>%
  group_by(genderid) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    n = n()
  )

##### Summary statistics for by year ####
alldata %>%
  group_by(eventname,genderid) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    n = n()
  )


### Determine whether LES differs based only on gender ####
#### Full data set ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether total_bad_le
# differs significantly based on gender group
# Test is significant (p = 2.03e-14)
kruskal.test(total_bad_le ~ genderid, data = alldata)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on total_bad_le
pairwise.wilcox.test(alldata$total_bad_le, alldata$genderid,
                     p.adjust.method = "fdr")

#### Year 3 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether total_bad_le
# differs significantly based on gender group
# Test is significant (p = 0.0001489)
kruskal.test(total_bad_le ~ genderid, data = yr3data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on total_bad_le
pairwise.wilcox.test(yr3data$total_bad_le, yr3data$genderid,
                     p.adjust.method = "fdr")

#### Year 4 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether total_bad_le
# differs significantly based on gender group
# Test is significant (p = 3.339e-13)
kruskal.test(total_bad_le ~ genderid, data = yr4data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on total_bad_le
pairwise.wilcox.test(yr4data$total_bad_le, yr4data$genderid,
                     p.adjust.method = "fdr")

##### Create bar graph of LES by gender group ####
# Use LES on x-axis and proportion of subjects with a given LES score for 
# each specific gender group (rather than raw number of subjects per gender
# group) so that differences in GD group are more clear despite having a
# much smaller sample size
# Create data frame with proportions rather than raw numbers
les_df_proportions <- 
  yr4data %>%
  group_by(genderid, total_bad_le) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(yr4_proportion = n / sum(n)) %>%
  ungroup() %>%
  # ensure all combinations of gender and number of bad events are
  # present so can be included on plot
  complete(total_bad_le, genderid, fill = list(n = 0, prop = 0)) %>% 
  mutate(yr4_proportion = replace_na(yr4_proportion,0))
# Make actual bar graph
ggplot(les_df_proportions, 
       aes(x=total_bad_le,y=yr4_proportion, fill=genderid)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(min(les_df_proportions$total_bad_le),
                     max(les_df_proportions$total_bad_le), by = 1)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(0,0.3,by=0.05),
                     limits=c(0,0.30)) +
  theme_classic()
# Save bar graph
# ggsave("LES_by_gender.tiff",width=7,height=3,unit="in")

#### Get summary statistics for LES ####
##### Summary statistics for full data set ####
alldata %>%
  group_by(genderid) %>%
  summarise(
    mean_total_bad_le = mean(total_bad_le, na.rm = TRUE),
    sd_total_bad_le = sd(total_bad_le, na.rm = TRUE),
    min_total_bad_le = min(total_bad_le, na.rm = TRUE),
    max_total_bad_le = max(total_bad_le, na.rm = TRUE),
    median_total_bad_le = median(total_bad_le, na.rm = TRUE),
    n = n()
  )

##### Summary statistics for by year ####
alldata %>%
  group_by(eventname,genderid) %>%
  summarise(
    mean_total_bad_le = mean(total_bad_le, na.rm = TRUE),
    sd_total_bad_le = sd(total_bad_le, na.rm = TRUE),
    min_total_bad_le = min(total_bad_le, na.rm = TRUE),
    max_total_bad_le = max(total_bad_le, na.rm = TRUE),
    median_total_bad_le = median(total_bad_le, na.rm = TRUE),
    n = n()
  )

### Determine whether DERS differs based only on gender ####
#### Full data set ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether ders_total
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(ders_total ~ genderid, data = alldata)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on ders_total
pairwise.wilcox.test(alldata$ders_total, alldata$genderid,
                     p.adjust.method = "fdr")

#### Year 3 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether ders_total
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(ders_total ~ genderid, data = yr3data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on ders_total
pairwise.wilcox.test(yr3data$ders_total, yr3data$genderid,
                     p.adjust.method = "fdr")

#### Year 4 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether ders_total
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(ders_total ~ genderid, data = yr4data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on ders_total
pairwise.wilcox.test(yr4data$ders_total, yr4data$genderid,
                     p.adjust.method = "fdr")

##### Create bar graph of LES by gender group ####
# Use DERS on x-axis and proportion of subjects with a given DERS score for
# each specific gender group (rather than raw number of subjects per gender
# group) so that differences in GD group are more clear despite having a
# much smaller sample size
# Create data frame with proportions rather than raw numbers
ders_df_proportions <- 
  yr4data %>%
  # create bins
  mutate(ders_total_bin = 
           cut(ders_total, 
               breaks = seq(0, 
                        max(ders_total,na.rm=TRUE)+10, by = 10), 
               right = FALSE)) %>% 
  group_by(genderid, ders_total_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(yr4_proportion = n / sum(n)) %>%
  ungroup() %>%
  # ensure all combinations of gender and number of bad events are
  # present so can be included on plot
  complete(ders_total_bin, genderid, fill = list(n = 0, prop = 0)) %>% 
  mutate(yr4_proportion = replace_na(yr4_proportion,0))

# Make actual bar graph
ggplot(ders_df_proportions, 
       aes(x = ders_total_bin,y=yr4_proportion,fill=genderid)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59",
                            "60-69","70-79","80-89","90-99","100-109",
                            "110-119","120-129","130-139")) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(min(ders_df_proportions$yr4_proportion),
                                      0.3,
                                      by=0.05),limits = c(0,0.31)) +
  theme_classic()

# Save bar graph
# ggsave("DERS_by_gender.tiff",width=7,height=3,unit="in")

#### Get summary statistics for DERS ####
##### Summary statistics for full data set ####
alldata %>%
  group_by(genderid) %>%
  summarise(
    mean_ders_total = mean(ders_total, na.rm = TRUE),
    sd_ders_total = sd(ders_total, na.rm = TRUE),
    min_ders_total = min(ders_total, na.rm = TRUE),
    max_ders_total = max(ders_total, na.rm = TRUE),
    median_ders_total = median(ders_total, na.rm = TRUE),
    n = n()
  )

##### Summary statistics for by year ####
alldata %>%
  group_by(eventname,genderid) %>%
  summarise(
    mean_ders_total = mean(ders_total, na.rm = TRUE),
    sd_ders_total = sd(ders_total, na.rm = TRUE),
    min_ders_total = min(ders_total, na.rm = TRUE),
    max_ders_total = max(ders_total, na.rm = TRUE),
    median_ders_total = median(ders_total, na.rm = TRUE),
    n = n()
  )

### Determine whether CBCL total problems differs based only on gender ####
#### Full data set ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether cbcl_total
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(cbcl_total ~ genderid, data = alldata)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on cbcl_total
pairwise.wilcox.test(alldata$cbcl_total, alldata$genderid,
                     p.adjust.method = "fdr")

#### Year 3 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether cbcl_total
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(cbcl_total ~ genderid, data = yr3data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on cbcl_total
pairwise.wilcox.test(yr3data$cbcl_total, yr3data$genderid,
                     p.adjust.method = "fdr")

#### Year 4 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether cbcl_total
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(cbcl_total ~ genderid, data = yr4data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# GD youth are significantly different from cis girls and cis boys, but the
# difference between cis girls and cis boys is not significant.
pairwise.wilcox.test(yr4data$cbcl_total, yr4data$genderid,
                     p.adjust.method = "fdr")

##### Create bar graph of CBCL total problems by gender group ####
# Use CBCL total problems on x-axis and proportion of subjects with a given
# CBCL total problems score for each specific gender group (rather than raw 
# number of subjects per gender group) so that differences in GD group are 
# more clear despite having a much smaller sample size
# Create data frame with proportions rather than raw numbers
cbcl_total_df_proportions <- yr4data %>%
  # Create bins
  mutate(cbcl_total_bin = cut(cbcl_total, 
                              breaks = seq(0, 100, by = 10), 
                              right = FALSE)) %>% # Create bins
  group_by(genderid, cbcl_total_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(yr4_proportion = n / sum(n)) %>%
  ungroup() %>%
  # ensure all combinations of gender and number of bad events are
  # present so can be included on plot
  complete(cbcl_total_bin, genderid, fill = list(n = 0, prop = 0)) %>% 
  mutate(yr4_proportion = replace_na(yr4_proportion,0))
# Make actual bar graph
ggplot(cbcl_total_df_proportions, 
       aes(x = cbcl_total_bin,y=yr4_proportion,fill=genderid)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49",
                            "50-59","60-69","70-79","80-89","90-99")) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(
                       min(cbcl_total_df_proportions$yr4_proportion),
                       0.4,
                       by=0.05),limits = c(0,0.4)) + 
  theme_classic()

# Save bar graph
# ggsave("CBCL_total_problems_by_gender.tiff",width=5.45,height=3.5,unit="in")

#### Get summary statistics for CBCL total problems ####
##### Summary statistics for full data set ####
alldata %>%
  group_by(genderid) %>%
  summarise(
    mean_cbcl_total = mean(cbcl_total, na.rm = TRUE),
    sd_cbcl_total = sd(cbcl_total, na.rm = TRUE),
    min_cbcl_total = min(cbcl_total, na.rm = TRUE),
    max_cbcl_total = max(cbcl_total, na.rm = TRUE),
    median_cbcl_total = median(cbcl_total, na.rm = TRUE),
    n = n()
  )

##### Summary statistics for by year ####
alldata %>%
  group_by(eventname,genderid) %>%
  summarise(
    mean_cbcl_total = mean(cbcl_total, na.rm = TRUE),
    sd_cbcl_total = sd(cbcl_total, na.rm = TRUE),
    min_cbcl_total = min(cbcl_total, na.rm = TRUE),
    max_cbcl_total = max(cbcl_total, na.rm = TRUE),
    median_cbcl_total = median(cbcl_total, na.rm = TRUE),
    n = n()
  )

### Determine whether CBCL internalizing differs based only on gender ####
#### Full data set ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether cbcl_int
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(cbcl_int ~ genderid, data = alldata)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on cbcl_int
pairwise.wilcox.test(alldata$cbcl_int, alldata$genderid,
                     p.adjust.method = "fdr")

#### Year 3 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether cbcl_int
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(cbcl_int ~ genderid, data = yr3data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on cbcl_int
pairwise.wilcox.test(yr3data$cbcl_int, yr3data$genderid,
                     p.adjust.method = "fdr")

#### Year 4 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether cbcl_int
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(cbcl_int ~ genderid, data = yr4data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# GD youth are significantly different from cis girls and cis boys, but the
# difference between cis girls and cis boys is not significant.
pairwise.wilcox.test(yr4data$cbcl_int, yr4data$genderid,
                     p.adjust.method = "fdr")

##### Create bar graph of CBCL internalizing by gender group ####
# Use CBCL internalizing on x-axis and proportion of subjects with a given
# CBCL internalizing score for each specific gender group (rather than raw 
# number of subjects per gender group) so that differences in GD group are 
# more clear despite having a much smaller sample size
# Create data frame with proportions rather than raw numbers
cbcl_int_df_proportions <- 
  yr4data %>%
  # Create bins
  mutate(cbcl_int_bin = cut(cbcl_int, 
                              breaks = seq(0, 100, by = 10), 
                              right = FALSE)) %>% # Create bins
  group_by(genderid, cbcl_int_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(yr4_proportion = n / sum(n)) %>%
  ungroup() %>%
  # ensure all combinations of gender and number of bad events are
  # present so can be included on plot
  complete(cbcl_int_bin, genderid, fill = list(n = 0, prop = 0)) %>% 
  mutate(yr4_proportion = replace_na(yr4_proportion,0))
# Make actual bar graph
ggplot(cbcl_int_df_proportions, 
       aes(x = cbcl_int_bin,y=yr4_proportion,fill=genderid)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49",
                            "50-59","60-69","70-79","80-89","90-99")) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(
                       min(cbcl_int_df_proportions$yr4_proportion),
                       0.4,
                       by=0.05),limits = c(0,0.4)) + 
  theme_classic()

# Save bar graph
# ggsave("cbcl_int_problems_by_gender.tiff",width=5.45,height=3.5,unit="in")

#### Get summary statistics for CBCL total problems ####
##### Summary statistics for full data set ####
alldata %>%
  group_by(genderid) %>%
  summarise(
    mean_cbcl_int = mean(cbcl_int, na.rm = TRUE),
    sd_cbcl_int = sd(cbcl_int, na.rm = TRUE),
    min_cbcl_int = min(cbcl_int, na.rm = TRUE),
    max_cbcl_int = max(cbcl_int, na.rm = TRUE),
    median_cbcl_int = median(cbcl_int, na.rm = TRUE),
    n = n()
  )

##### Summary statistics for by year ####
alldata %>%
  group_by(eventname,genderid) %>%
  summarise(
    mean_cbcl_int = mean(cbcl_int, na.rm = TRUE),
    sd_cbcl_int = sd(cbcl_int, na.rm = TRUE),
    min_cbcl_int = min(cbcl_int, na.rm = TRUE),
    max_cbcl_int = max(cbcl_int, na.rm = TRUE),
    median_cbcl_int = median(cbcl_int, na.rm = TRUE),
    n = n()
  )

### Determine whether CBCL externalizing differs based only on gender ####
#### Full data set ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether cbcl_ext
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(cbcl_ext ~ genderid, data = alldata)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on cbcl_ext
pairwise.wilcox.test(alldata$cbcl_ext, alldata$genderid,
                     p.adjust.method = "fdr")

#### Year 3 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether cbcl_ext
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(cbcl_ext ~ genderid, data = yr3data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on cbcl_ext
pairwise.wilcox.test(yr3data$cbcl_ext, yr3data$genderid,
                     p.adjust.method = "fdr")

#### Year 4 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether cbcl_ext
# differs significantly based on gender group
# Test is significant (p = 2.758e-10)
kruskal.test(cbcl_ext ~ genderid, data = yr4data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on cbcl_ext
pairwise.wilcox.test(yr4data$cbcl_ext, yr4data$genderid,
                     p.adjust.method = "fdr")

##### Create bar graph of CBCL internalizing by gender group ####
# Use CBCL internalizing on x-axis and proportion of subjects with a given
# CBCL internalizing score for each specific gender group (rather than raw 
# number of subjects per gender group) so that differences in GD group are 
# more clear despite having a much smaller sample size
# Create data frame with proportions rather than raw numbers
cbcl_ext_df_proportions <- 
  yr4data %>%
  # Create bins
  mutate(cbcl_ext_bin = cut(cbcl_ext, 
                            breaks = seq(0, 100, by = 10), 
                            right = FALSE)) %>% # Create bins
  group_by(genderid, cbcl_ext_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(yr4_proportion = n / sum(n)) %>%
  ungroup() %>%
  # ensure all combinations of gender and number of bad events are
  # present so can be included on plot
  complete(cbcl_ext_bin, genderid, fill = list(n = 0, prop = 0)) %>% 
  mutate(yr4_proportion = replace_na(yr4_proportion,0))
# Make actual bar graph
ggplot(cbcl_ext_df_proportions, 
       aes(x = cbcl_ext_bin,y=yr4_proportion,fill=genderid)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49",
                            "50-59","60-69","70-79","80-89","90-99")) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(
                       min(cbcl_ext_df_proportions$yr4_proportion),
                       0.4,
                       by=0.05),limits = c(0,0.41)) + 
  theme_classic()

# Save bar graph
# ggsave("cbcl_ext_problems_by_gender.tiff",width=5.45,height=3.5,unit="in")

#### Get summary statistics for CBCL total problems ####
##### Summary statistics for full data set ####
alldata %>%
  group_by(genderid) %>%
  summarise(
    mean_cbcl_ext = mean(cbcl_ext, na.rm = TRUE),
    sd_cbcl_ext = sd(cbcl_ext, na.rm = TRUE),
    min_cbcl_ext = min(cbcl_ext, na.rm = TRUE),
    max_cbcl_ext = max(cbcl_ext, na.rm = TRUE),
    median_cbcl_ext = median(cbcl_ext, na.rm = TRUE),
    n = n()
  )

##### Summary statistics for by year ####
alldata %>%
  group_by(eventname,genderid) %>%
  summarise(
    mean_cbcl_ext = mean(cbcl_ext, na.rm = TRUE),
    sd_cbcl_ext = sd(cbcl_ext, na.rm = TRUE),
    min_cbcl_ext = min(cbcl_ext, na.rm = TRUE),
    max_cbcl_ext = max(cbcl_ext, na.rm = TRUE),
    median_cbcl_ext = median(cbcl_ext, na.rm = TRUE),
    n = n()
  )

### Establish relationships between all pairs of variables individually ####
#### DERS ~ LES + age + (1|site) ####
# DERS scores differ significantly based on LES (p = 1.68e-12) but not based on
# age (p = 0.0526).
ders_les_reg <- lmer(Z_ders_total ~ Z_total_bad_le + Z_age + 
                     (1|site),
                     data=yr4data)
summary(ders_les_reg)

#### CBCL total problems ~ LES + age + (1|site) ####
# CBCL total problems scores differ significantly based on LES (p < 2e-16) and
# based on age (p = 0.00366)
cbcl_total_les_reg <- lmer(Z_cbcl_total ~ Z_total_bad_le + Z_age + 
                           (1|site),
                           data=yr4data)
summary(cbcl_total_les_reg)

#### CBCL internalizing ~ LES + age + (1|site) ####
# CBCL internalizing scores differ significantly based on LES (p < 2e-16) but
# not based on age (p = 0.264)
cbcl_int_les_reg <- lmer(Z_cbcl_int ~ Z_total_bad_le + Z_age + 
                             (1|site),
                           data=yr4data)
summary(cbcl_int_les_reg)

#### CBCL externalizing ~ LES + age + (1|site) ####
# CBCL externalizing scores differ significantly based on LES (p < 2e-16) and
# based on age (p = 0.0014)
cbcl_ext_les_reg <- lmer(Z_cbcl_ext ~ Z_total_bad_le + Z_age + 
                           (1|site),
                         data=yr4data)
summary(cbcl_ext_les_reg)

#### CBCL total problems ~ DERS + age + (1|site) ####
# CBCL total problems scores differ significantly based on DERS (p < 2e-16) but
# not based on age (p = 0.0967)
cbcl_total_les_reg <- lmer(Z_cbcl_total ~ Z_ders_total + Z_age + 
                             (1|site),
                           data=yr4data)
summary(cbcl_total_les_reg)

#### CBCL internalizing ~ DERS + age + (1|site) ####
# CBCL internalizing scores differ significantly based on DERS (p < 2e-16) but
# not based on age (p = 0.855)
cbcl_int_les_reg <- lmer(Z_cbcl_int ~ Z_ders_total + Z_age + 
                           (1|site),
                         data=yr4data)
summary(cbcl_int_les_reg)

#### CBCL externalizing ~ DERS + age + (1|site) ####
# CBCL externalizing scores differ significantly based on DERS (p < 2e-16) and
# based on age (p = 0.0365)
cbcl_ext_les_reg <- lmer(Z_cbcl_ext ~ Z_ders_total + Z_age + 
                           (1|site),
                         data=yr4data)
summary(cbcl_ext_les_reg)

### Mediation models with lavaan  ####
#### Select only data for mediation analysis ####

###############################################################################
#################### FOR ANALYSIS USING ALL YEAR 4 DATA #######################
###############################################################################

med_data <- 
  yr4data %>%
  select(src_subject_id,
         gender_cisboy,
         gender_cisgirl,
         gender_gd,
         sex,
         sex_female,
         site,
         Z_age,
         Z_total_bad_le,
         Z_ders_total,
         Z_cbcl_total,
         Z_cbcl_int,
         Z_cbcl_ext) %>%
  rename(
         age = Z_age,
         cisboy = gender_cisboy,
         cisgirl = gender_cisgirl,
         gd = gender_gd,
         female = sex_female,
         LES = Z_total_bad_le,
         DERS = Z_ders_total,
         totalCBCL = Z_cbcl_total,
         intCBCL = Z_cbcl_int,
         extCBCL = Z_cbcl_ext
  )

sex_med_data <- med_data %>% filter(sex!="refuse",sex!="dont_know")

###############################################################################
# FOR ANALYSIS USING YEAR 3 LES & DERS WITH YEAR 4 OUTCOMES, GENDER, & COVARIATES ###############################################################################

med_data <- 
  yr4data %>%
  # CBCL outcomes, gender, and covariates from year 4
  select(src_subject_id,
         gender_cisboy,
         gender_cisgirl,
         gender_gd,
         sex,
         sex_female,
         site,
         Z_age,
         # Z_total_bad_le,
         # Z_ders_total,
         Z_cbcl_total,
         Z_cbcl_int,
         Z_cbcl_ext) %>%
  # LES and DERS from year 3
  left_join(select(yr3data,c(src_subject_id,
                             Z_total_bad_le,Z_ders_total)),
            by=c("src_subject_id")) %>%
  rename(
    age = Z_age,
    cisboy = gender_cisboy,
    cisgirl = gender_cisgirl,
    gd = gender_gd,
    female = sex_female,
    LES = Z_total_bad_le,
    DERS = Z_ders_total,
    totalCBCL = Z_cbcl_total,
    intCBCL = Z_cbcl_int,
    extCBCL = Z_cbcl_ext
  )

sex_med_data <- med_data %>% filter(sex!="refuse",sex!="dont_know")

###############################################################################
############# FOR ANALYSIS USING LOG TRANSFORMED YEAR 4 DATA ##################
###############################################################################

med_data <- 
  yr4data %>%
  select(src_subject_id,
         gender_cisboy,
         gender_cisgirl,
         gender_gd,
         sex,
         sex_female,
         site,
         Z_age,
         Z_log_total_bad_le,
         Z_log_ders_total,
         Z_log_cbcl_total,
         Z_log_cbcl_int,
         Z_log_cbcl_ext) %>%
  rename(
    age = Z_age,
    cisboy = gender_cisboy,
    cisgirl = gender_cisgirl,
    gd = gender_gd,
    female = sex_female,
    LES = Z_log_total_bad_le,
    DERS = Z_log_ders_total,
    totalCBCL = Z_log_cbcl_total,
    intCBCL = Z_log_cbcl_int,
    extCBCL = Z_log_cbcl_ext
  )

sex_med_data <- med_data %>% filter(sex!="refuse",sex!="dont_know")
###############################################################################

#### Run mediation analysis ####
##### CBCL total problems ####
###### Without gender or sex ####
nogendertotalmodel <- 
    ' # direct effect
        totalCBCL~ c*LES + age
      # mediator
        DERS ~ a*LES  + age
      # indirect effect
        totalCBCL~ b*DERS
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b) '
nogendertotalfit <- sem(nogendertotalmodel, 
                        data = med_data, 
                        meanstructure = TRUE,
                        se = "robust.cluster",
                        cluster = "site")
summary(nogendertotalfit, fit.measures=T, 
        standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(nogendertotalfit, boot.ci.type = "bca.simple")

###### With gender, using cis boys as the comparison group ####
gendertotalmodel_compcisboy <- 
   ' # direct effect 
       totalCBCL~ c*LES + cisgirl + gd + age + LES:cisgirl + LES:gd
     # mediator 
       DERS ~ a*LES  + cisgirl + gd + age + LES:cisgirl + LES:gd
     # indirect effect 
       totalCBCL~ b*DERS
     # indirect effect w mediator (a*b)
       ab := a*b
     # total effect
       total := c + (a*b) '
gendertotalfit_compcisboy <- sem(gendertotalmodel_compcisboy, 
                                 data = med_data, 
                                 meanstructure = TRUE,
                                 se = "robust.cluster",
                                 cluster = "site")
summary(gendertotalfit_compcisboy, 
        fit.measures=T, 
        standardized=T, 
        ci=TRUE, 
        rsquare=TRUE)
parameterEstimates(gendertotalfit_compcisboy, boot.ci.type = "bca.simple")

###### With gender, using cis girls as the comparison group ####
gendertotalmodel_compcisgirl <- 
    ' # direct effect 
        totalCBCL~ c*LES + cisboy + gd + age + LES:cisboy + LES:gd
      # mediator 
        DERS ~ a*LES  + cisboy + gd + age + LES:cisboy + LES:gd
      # indirect effect 
        totalCBCL~ b*DERS
      # indirect effect w mediator (a*b)
        ab := a*b
      # total effect
        total := c + (a*b) '
gendertotalfit_compcisgirl <- sem(gendertotalmodel_compcisgirl, 
                                  data = med_data, 
                                  meanstructure = TRUE,
                                  se = "robust.cluster",
                                  cluster = "site")
summary(gendertotalfit_compcisgirl, 
        fit.measures=T, standardized=T, 
        ci=TRUE, 
        rsquare=TRUE)
parameterEstimates(gendertotalfit_compcisgirl, boot.ci.type = "bca.simple")

###### With sex ####
sextotalmodel <- 
    ' # direct effect
        totalCBCL~ c*LES + female + age + LES:female
      # mediator
        DERS ~ a*LES  + female + age + LES:female
      # indirect effect
        totalCBCL~ b*DERS
      # indirect effect w mediator (a*b)
        ab := a*b
      # total effect
        total := c + (a*b) '
sextotalfit <- sem(sextotalmodel, 
                   data = sex_med_data, 
                   meanstructure = TRUE,
                   se = "robust.cluster",
                   cluster = "site")
summary(sextotalfit, 
        fit.measures=T, 
        standardized=T, 
        ci=TRUE, 
        rsquare=TRUE)
parameterEstimates(sextotalfit, boot.ci.type = "bca.simple")

##### CBCL internalizing ####
###### Without gender or sex ####
nogenderintmodel <- 
  ' # direct effect
        intCBCL~ c*LES + age
      # mediator
        DERS ~ a*LES  + age
      # indirect effect
        intCBCL~ b*DERS
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b) '
nogenderintfit <- sem(nogenderintmodel, 
                        data = med_data, 
                        meanstructure = TRUE,
                        se = "robust.cluster",
                        cluster = "site")
summary(nogenderintfit, fit.measures=T, 
        standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(nogenderintfit, boot.ci.type = "bca.simple")

###### With gender, using cis boys as the comparison group ####
genderintmodel_compcisboy <- 
  ' # direct effect 
       intCBCL~ c*LES + cisgirl + gd + age + LES:cisgirl + LES:gd
     # mediator 
       DERS ~ a*LES  + cisgirl + gd + age + LES:cisgirl + LES:gd
     # indirect effect 
       intCBCL~ b*DERS
     # indirect effect w mediator (a*b)
       ab := a*b
     # total effect
       total := c + (a*b) '
genderintfit_compcisboy <- sem(genderintmodel_compcisboy, 
                                 data = med_data, 
                                 meanstructure = TRUE,
                                 se = "robust.cluster",
                                 cluster = "site")
summary(genderintfit_compcisboy, 
        fit.measures=T, 
        standardized=T, 
        ci=TRUE, 
        rsquare=TRUE)
parameterEstimates(genderintfit_compcisboy, boot.ci.type = "bca.simple")

###### With gender, using cis girls as the comparison group ####
genderintmodel_compcisgirl <- 
  ' # direct effect 
        intCBCL~ c*LES + cisboy + gd + age + LES:cisboy + LES:gd
      # mediator 
        DERS ~ a*LES  + cisboy + gd + age + LES:cisboy + LES:gd
      # indirect effect 
        intCBCL~ b*DERS
      # indirect effect w mediator (a*b)
        ab := a*b
      # total effect
        total := c + (a*b) '
genderintfit_compcisgirl <- sem(genderintmodel_compcisgirl, 
                                  data = med_data, 
                                  meanstructure = TRUE,
                                  se = "robust.cluster",
                                  cluster = "site")
summary(genderintfit_compcisgirl, 
        fit.measures=T, standardized=T, 
        ci=TRUE, 
        rsquare=TRUE)
parameterEstimates(genderintfit_compcisgirl, boot.ci.type = "bca.simple")

###### With sex ####
sexintmodel <- 
  ' # direct effect
        intCBCL~ c*LES + female + age + LES:female
      # mediator
        DERS ~ a*LES  + female + age + LES:female
      # indirect effect
        intCBCL~ b*DERS
      # indirect effect w mediator (a*b)
        ab := a*b
      # total effect
        total := c + (a*b) '
sexintfit <- sem(sexintmodel, 
                   data = sex_med_data, 
                   meanstructure = TRUE,
                   se = "robust.cluster",
                   cluster = "site")
summary(sexintfit, 
        fit.measures=T, 
        standardized=T, 
        ci=TRUE, 
        rsquare=TRUE)
parameterEstimates(sexintfit, boot.ci.type = "bca.simple")

##### CBCL externalizing ####
###### Without gender or sex ####
nogenderextmodel <- 
  ' # direct effect
        extCBCL~ c*LES + age
      # mediator
        DERS ~ a*LES  + age
      # indirect effect
        extCBCL~ b*DERS
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b) '
nogenderextfit <- sem(nogenderextmodel, 
                      data = med_data, 
                      meanstructure = TRUE,
                      se = "robust.cluster",
                      cluster = "site")
summary(nogenderextfit, fit.measures=T, 
        standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(nogenderextfit, boot.ci.type = "bca.simple")

###### With gender, using cis boys as the comparison group ####
genderextmodel_compcisboy <- 
  ' # direct effect 
       extCBCL~ c*LES + cisgirl + gd + age + LES:cisgirl + LES:gd
     # mediator 
       DERS ~ a*LES  + cisgirl + gd + age + LES:cisgirl + LES:gd
     # indirect effect 
       extCBCL~ b*DERS
     # indirect effect w mediator (a*b)
       ab := a*b
     # total effect
       total := c + (a*b) '
genderextfit_compcisboy <- sem(genderextmodel_compcisboy, 
                               data = med_data, 
                               meanstructure = TRUE,
                               se = "robust.cluster",
                               cluster = "site")
summary(genderextfit_compcisboy, 
        fit.measures=T, 
        standardized=T, 
        ci=TRUE, 
        rsquare=TRUE)
parameterEstimates(genderextfit_compcisboy, boot.ci.type = "bca.simple")

###### With gender, using cis girls as the comparison group ####
genderextmodel_compcisgirl <- 
  ' # direct effect 
        extCBCL~ c*LES + cisboy + gd + age + LES:cisboy + LES:gd
      # mediator 
        DERS ~ a*LES  + cisboy + gd + age + LES:cisboy + LES:gd
      # indirect effect 
        extCBCL~ b*DERS
      # indirect effect w mediator (a*b)
        ab := a*b
      # total effect
        total := c + (a*b) '
genderextfit_compcisgirl <- sem(genderextmodel_compcisgirl, 
                                data = med_data, 
                                meanstructure = TRUE,
                                se = "robust.cluster",
                                cluster = "site")
summary(genderextfit_compcisgirl, 
        fit.measures=T, standardized=T, 
        ci=TRUE, 
        rsquare=TRUE)
parameterEstimates(genderextfit_compcisgirl, boot.ci.type = "bca.simple")

###### With sex ####
sexextmodel <- 
  ' # direct effect
        extCBCL~ c*LES + female + age + LES:female
      # mediator
        DERS ~ a*LES  + female + age + LES:female
      # indirect effect
        extCBCL~ b*DERS
      # indirect effect w mediator (a*b)
        ab := a*b
      # total effect
        total := c + (a*b) '
sexextfit <- sem(sexextmodel, 
                 data = sex_med_data, 
                 meanstructure = TRUE,
                 se = "robust.cluster",
                 cluster = "site")
summary(sexextfit, 
        fit.measures=T, 
        standardized=T, 
        ci=TRUE, 
        rsquare=TRUE)
parameterEstimates(sexextfit, boot.ci.type = "bca.simple")

