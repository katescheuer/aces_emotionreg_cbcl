### Set working directory ####
setwd("C:/Users/Kate Scheuer/OneDrive - UW/Desktop/Lab/aces_emotionreg_cbcl/data")

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

gish_y_gi <- read_csv("gish_y_gi.csv")

#### DERS-P for emotion regulation ####
mh_p_ders <- read_csv("mh_p_ders.csv")

#### CBCL for parent-report psychopathology symptoms ####
mh_p_cbcl <- read_csv("mh_p_cbcl.csv")

#### BPM for youth-report psychopathology symptoms ####
mh_y_bpm <- read_csv("mh_y_bpm.csv")

#### Longitudinal tracking data ####
abcd_y_lt <- read_csv("abcd_y_lt.csv")

#### LES for exposure to negative life events
mh_y_le <- read_csv("mh_y_le.csv")

### Determine initial number of year 4 participants from each data frame ####

gish_y_gi %>% 
  filter(eventname=="4_year_follow_up_y_arm_1") %>%
  count() 
mh_p_ders %>% 
  filter(eventname=="4_year_follow_up_y_arm_1") %>% 
  count()
mh_p_cbcl %>% 
  filter(eventname=="4_year_follow_up_y_arm_1") %>% 
  count()
mh_y_bpm %>% 
  filter(eventname=="4_year_follow_up_y_arm_1") %>% 
  count()
abcd_y_lt %>% 
  filter(eventname=="4_year_follow_up_y_arm_1") %>% 
  count()
mh_y_le %>% 
  filter(eventname=="4_year_follow_up_y_arm_1") %>% 
  count()

### Prepare gender data for analysis ####
#### Identify gender groups ####
genderdata <- gish_y_gi %>%
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
  # effect code sex
  mutate(sex_female = if_else(sex=="female",1,-1)) %>%
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
  
  # dummy coding table below to help conceptually convert from one column with
  # three categories into two new, dummy-coded columns with cis BOY as reference
  ### gender group ### gender_cisgirl ### gender_gd ###
  ###    cis boy   ###      0         ###     0     ###
  ###    cis girl  ###      1         ###     0     ###
  ###     gd       ###      0         ###     1     ###
  
  # dummy coding table below to help conceptually convert from one column with
  # three categories into two new, dummy-coded columns with cis GIRL as reference
  ### gender group ### gender_cisboy ### gender_gd ###
  ###    cis boy   ###      1         ###     0     ###
  ###    cis girl  ###      0         ###     0     ###
  ###     gd       ###      0         ###     1     ###

  # code to execute dummy coding 
  # mutate(gender_cisgirl = if_else(genderid=="cis_girl",1,0)) %>%
  # mutate(gender_gd = if_else(genderid=="gd",1,0)) %>%
  # mutate(gender_cisboy = if_else(genderid=="cis_boy",1,0)) %>%
  
  # effect coding table below to help conceptually convert from one column with
  # three categories into two new, effect-coded columns with cis BOY as reference
  ### gender group ### eff_cisgirl_ref_cisboy ### eff_gd_ref_cisboy ###
  ###    cis boy   ###             -1         ###            -1     ###
  ###    cis girl  ###              1         ###             0     ###
  ###     gd       ###              0         ###             1     ###

  # effect coding table below to help conceptually convert from one column with
  # three categories into two new, effect-coded columns with cis GIRL as reference
  ### gender group ### eff_cisboy_ref_cisgirl ### eff_gd_ref_cisgirl ###
  ###    cis boy   ###              1         ###              0     ###
  ###    cis girl  ###             -1         ###             -1     ###
  ###     gd       ###              0         ###              1     ###
  
  # code to execute effect coding
  mutate(eff_cisgirl_ref_cisboy = 
           case_when(genderid=="cis_boy" ~ -1,
                     genderid=="cis_girl" ~ 1,
                     genderid=="gd" ~ 0)) %>%
  mutate(eff_gd_ref_cisboy = 
           case_when(genderid=="cis_boy" ~ -1,
                     genderid=="cis_girl" ~ 0,
                     genderid=="gd" ~ 1)) %>%
  mutate(eff_cisboy_ref_cisgirl = 
           case_when(genderid=="cis_boy" ~ 1,
                     genderid=="cis_girl" ~ -1,
                     genderid=="gd" ~ 0)) %>%
  mutate(eff_gd_ref_cisgirl = 
           case_when(genderid=="cis_boy" ~ 0,
                     genderid=="cis_girl" ~ -1,
                     genderid=="gd" ~ 1)) %>%

  # keep only columns relevant to analysis
  select(src_subject_id,eventname,sex,gender,trans,sex_female,
         genderid,
         # gender_cisgirl,gender_gd,gender_cisboy
         eff_cisgirl_ref_cisboy,eff_gd_ref_cisboy,
         eff_cisboy_ref_cisgirl,eff_gd_ref_cisgirl
         ) %>%
  # remove subjects who refused to answer and/or did not understand gender
  # or trans questions. Before this step, n should be 15064. After this step,
  # n should be 14495.
  filter(genderid!="refuse",
         genderid!="dont_understand") %>%

  # make genderid a factor (automatically uses cisboy as reference
  mutate(genderid_refcisboy = as.factor(genderid)) %>%
  # make another column that uses cis_girl as reference instead
  mutate(genderid_refcisgirl = relevel(genderid_refcisboy,"cis_girl"))


#### Count number of subjects per gender group per data collection year ####
genderdata %>% 
  group_by(eventname) %>% 
  count(genderid)

### Prepare CBCL data for analysis ####
cbcldata <- mh_p_cbcl %>%
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

### Prepare BPM data for analysis ####
bpmdata <- mh_y_bpm %>%
  # select only columns relevant to analysis
  select(src_subject_id,eventname,
         bpm_y_scr_totalprob_t,
         bpm_y_scr_internal_t,
         bpm_y_scr_external_t
  ) %>%
  # rename subscale columns to be more human-readable and shorter
  rename(bpm_total = bpm_y_scr_totalprob_t,
         bpm_ext = bpm_y_scr_external_t,
         bpm_int = bpm_y_scr_internal_t
  ) %>%
  # add column with log-transformed BPM total problems values
  mutate(log_bpm_total = log(bpm_total)) %>%
  # add column with log-transformed BPM internalizing values
  mutate(log_bpm_int = log(bpm_int)) %>%
  # add column with log-transformed BPM externalizing values
  mutate(log_bpm_ext = log(bpm_ext)) 

### Prepare DERS-P data for analysis ####
#### Create cumulative score ####
dersdata <- mh_p_ders %>%
  # remove subjects who refused to answer one or more items. Before this step,
  # n should be 14708. After this step, n should be 14225.
  filter(!if_any(everything(), ~ . == 777)) %>%
  # add column to reverse score "my child pays attention to how he/she feels"
  mutate(rev_ders_attn_awareness_p = 
           case_when(ders_attn_awareness_p == 1 ~ 5,
                     ders_attn_awareness_p == 2 ~ 4,
                     ders_attn_awareness_p == 3 ~ 3,
                     ders_attn_awareness_p == 4 ~ 2,
                     ders_attn_awareness_p == 5 ~ 1)) %>%
  # add column to reverse score "my child is attentive to his/her feelings"
  mutate(rev_ders_feelings_attentive_p = 
           case_when(ders_feelings_attentive_p == 1 ~ 5,
                     ders_feelings_attentive_p == 2 ~ 4,
                     ders_feelings_attentive_p == 3 ~ 3,
                     ders_feelings_attentive_p == 4 ~ 2,
                     ders_feelings_attentive_p == 5 ~ 1)) %>%
  # add column to reverse score "my child cares about what he/she is feeling"
  mutate(rev_ders_feelings_care_p = 
           case_when(ders_feelings_care_p == 1 ~ 5,
                      ders_feelings_care_p == 2 ~ 4,
                      ders_feelings_care_p == 3 ~ 3,
                      ders_feelings_care_p == 4 ~ 2,
                      ders_feelings_care_p == 5 ~ 1)) %>%
  # add column to reverse score "when my child is upset, he/she acknowledges
  # his/her emotions"
  mutate(rev_ders_upset_ack_p = 
           case_when(ders_upset_ack_p == 1 ~ 5,
                      ders_upset_ack_p == 2 ~ 4,
                      ders_upset_ack_p == 3 ~ 3,
                      ders_upset_ack_p == 4 ~ 2,
                      ders_upset_ack_p == 5 ~ 1)) %>%
  # add column to reverse score "my child is clear about his/her feelings"
  mutate(rev_ders_clear_feelings_p = 
           case_when(ders_clear_feelings_p == 1 ~ 5,
                     ders_clear_feelings_p == 2 ~ 4,
                     ders_clear_feelings_p == 3 ~ 3,
                     ders_clear_feelings_p == 4 ~ 2,
                     ders_clear_feelings_p == 5 ~ 1)) %>%
  # add column to reverse score "my child knows exactly how he/she is feeling"
  mutate(rev_ders_feelings_know_p = 
           case_when(ders_feelings_know_p == 1 ~ 5,
                      ders_feelings_know_p == 2 ~ 4,
                      ders_feelings_know_p == 3 ~ 3,
                      ders_feelings_know_p == 4 ~ 2,
                      ders_feelings_know_p == 5 ~ 1)) %>%
  # add column to reverse score "when my child is upset, he/she feels like
  # he/she can remin in control of his/her behaviors"
    mutate(rev_ders_upset_behavior_control_p = 
             case_when(ders_upset_behavior_control_p == 1 ~ 5,
                       ders_upset_behavior_control_p == 2 ~ 4,
                       ders_upset_behavior_control_p == 3 ~ 3,
                       ders_upset_behavior_control_p == 4 ~ 2,
                       ders_upset_behavior_control_p == 5 ~ 1)) %>%
  # add column to reverse score "when my child is upset, he/she knows that
  # he/she can find a way to eventually feel better"
    mutate(rev_ders_upset_better_p = 
             case_when(ders_upset_better_p == 1 ~ 5,
                       ders_upset_better_p == 2 ~ 4,
                       ders_upset_better_p == 3 ~ 3,
                       ders_upset_better_p == 4 ~ 2,
                       ders_upset_better_p == 5 ~ 1)) %>%
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

#### See all unique values for each column in DERS-P data ####
# see all unique values (can visually check for NA or errors)
map(dersdata,unique)
# explicitly check for any NA values
which(is.na(dersdata))

#### Provide summary statistics for DERS-P data by data collection year ####
dersdata %>% 
  group_by(eventname) %>% 
  summary()

### Prepare LES data for analysis ####
#### Identify and prepare relevant columns ####
ledata <- mh_y_le %>% 
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
ledata %>% 
  group_by(eventname) %>% 
  summary()

### Combine all data for analysis into one data frame ####
alldata <- genderdata %>%
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
  # add BPM data based on subject ID and data collection year
  left_join(bpmdata,by=c("src_subject_id","eventname")) %>%
  # Z score continuous variables
  mutate(across(
    c(age, ders_total, total_bad_le, 
      cbcl_total, cbcl_int, cbcl_ext,
      bpm_total, bpm_int, bpm_ext,
      log_ders_total, log_total_bad_le, 
      log_cbcl_total, log_cbcl_int, log_cbcl_ext,
      log_bpm_total, log_bpm_int, log_bpm_ext),
    ~ as.numeric(scale(.)),
    .names = "Z_{.col}"
  )) %>%
  # make genderid, sex, and site factors rather than characters
  mutate(across(c(genderid, sex, site), as.factor)) %>%
  # remove subjects without LES or DERS or CBCL data in either year 3 or year 4
  # follow-up. Before this step, n should be 14495. After this step, n should 
  # be 12104
  filter(!is.na(ders_total), !is.na(total_bad_le), !is.na(bpm_total),
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
# n should be 3661
yr4data <- alldata %>% filter(eventname=="4_year_follow_up_y_arm_1")

#### Create separate data frame for just data from year 3 follow-up visit ####
# n should be 8443
yr3data <- alldata %>% filter(eventname=="3_year_follow_up_y_arm_1")

#### Get general summary of values for each column for data from year 4 visit ####
yr4data %>% describe()

#### Get general summary of values for each column for data from year 3 visit ####
yr3data %>% describe()

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
       "cbcl_ext", "log_cbcl_ext",
       "bpm_total", "log_bpm_total", 
       "bpm_int", "log_bpm_int", 
       "bpm_ext", "log_bpm_ext"), 
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
       "cbcl_ext", "log_cbcl_ext",
       "bpm_total", "log_bpm_total", 
       "bpm_int", "log_bpm_int", 
       "bpm_ext", "log_bpm_ext"), 
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
           cbcl_total,cbcl_int,cbcl_ext,
           bpm_total,bpm_int,bpm_ext)) %>% 
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
outcome_list <- c("cbcl_total","cbcl_int","cbcl_ext",
                  "bpm_total","bpm_int","bpm_ext")
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
les_vs_cbcl_bpm_summary_stats <- yr4data %>% 
  group_by(total_bad_le) %>% 
  summarise(cbcl_total=mean(cbcl_total,na.rm=TRUE),
            cbcl_int=mean(cbcl_int,na.rm=TRUE),
            cbcl_ext=mean(cbcl_ext,na.rm=TRUE),
            bpm_total=mean(bpm_total,na.rm=TRUE),
            bpm_int=mean(bpm_int,na.rm=TRUE),
            bpm_ext=mean(bpm_ext,na.rm=TRUE),
            n=n(),
            .groups="drop") %>%
  complete(total_bad_le)
###### Create barplots ####
outcome_list <- c("cbcl_total","cbcl_int","cbcl_ext",
                  "bpm_total","bpm_int","bpm_ext")
les_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  les_plot <- ggplot(aes(x=total_bad_le,y=.data[[outcome]]),
                     data=les_vs_cbcl_bpm_summary_stats) +
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
outcome_list <- c("cbcl_total","cbcl_int","cbcl_ext",
                  "bpm_total","bpm_int","bpm_ext")
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
les_vs_cbcl_bpm_w_gender_summary_stats <- 
  yr4data %>% 
  group_by(total_bad_le,genderid) %>% 
  summarise(cbcl_total=mean(cbcl_total,na.rm=TRUE),
            cbcl_int=mean(cbcl_int,na.rm=TRUE),
            cbcl_ext=mean(cbcl_ext,na.rm=TRUE),
            bpm_total=mean(bpm_total,na.rm=TRUE),
            bpm_int=mean(bpm_int,na.rm=TRUE),
            bpm_ext=mean(bpm_ext,na.rm=TRUE),
            n=n(),
            .groups="drop") %>%
  # ensure all combinations of gender and number of bad events are
  # present so can be included on plot
  complete(total_bad_le, genderid, fill = list(n = 0, prop = 0)) %>% 
  mutate(total_bad_le = replace_na(total_bad_le,0))
###### Create barplots ####
outcome_list <- c("cbcl_total","cbcl_int","cbcl_ext",
                  "bpm_total","bpm_int","bpm_ext")
les_gender_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  les_plot <- ggplot(aes(x=total_bad_le,y=.data[[outcome]],
                         color=genderid,
                         fill=genderid),
                     data=les_vs_cbcl_bpm_w_gender_summary_stats) +
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
# Test is significant (p = 1.606e-05)
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
# Test is significant (p = 0.01412)
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
# Test is not significant (p = 0.1779)
kruskal.test(age ~ genderid, data = yr4data)

#### Get summary statistics for age ####
##### Summary statistics for full data set by gender ####

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

##### Summary statistics by gender and by year ####

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

##### Summary statistics for year 4 overall ####
yr4data %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    n = n()
  )

##### Summary statistics for year 4 by gender ####
yr4data %>%
  group_by(genderid) %>%
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
# Test is significant (p = 4.672e-11)
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
# Test is significant (p = 0.002221)
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
# Test is significant (p = 4.803e-11)
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
##### Summary statistics for year 4 ####
yr4data %>%
  summarise(
    mean_total_bad_le = mean(total_bad_le, na.rm = TRUE),
    sd_total_bad_le = sd(total_bad_le, na.rm = TRUE),
    min_total_bad_le = min(total_bad_le, na.rm = TRUE),
    max_total_bad_le = max(total_bad_le, na.rm = TRUE),
    median_total_bad_le = median(total_bad_le, na.rm = TRUE),
    n = n()
  )

##### Summary statistics by year ####
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
# Test is significant (p = 2.022e-13)
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
##### Summary statistics for year 4 ####
yr4data %>%
  # group_by(genderid) %>%
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
##### Summary statistics for year 4 ####
yr4data %>%
  # group_by(genderid) %>%
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

#### Get summary statistics for CBCL internalizing problems ####
##### Summary statistics for year 4 ####
yr4data %>%
  # group_by(genderid) %>%
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
# Test is significant (p = 9.4e-09)
kruskal.test(cbcl_ext ~ genderid, data = yr4data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on cbcl_ext
pairwise.wilcox.test(yr4data$cbcl_ext, yr4data$genderid,
                     p.adjust.method = "fdr")

##### Create bar graph of CBCL externalizing by gender group ####
# Use CBCL externalizing on x-axis and proportion of subjects with a given
# CBCL externalizing score for each specific gender group (rather than raw 
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

#### Get summary statistics for CBCL externalizing problems ####
##### Summary statistics for year 4 ####
yr4data %>%
  # group_by(genderid) %>%
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

### Determine whether BPM total problems differs based only on gender ####
#### Full data set ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether bpm_total
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(bpm_total ~ genderid, data = alldata)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on bpm_total
pairwise.wilcox.test(alldata$bpm_total, alldata$genderid,
                     p.adjust.method = "fdr")

#### Year 3 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether bpm_total
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(bpm_total ~ genderid, data = yr3data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on bpm_total
pairwise.wilcox.test(yr3data$bpm_total, yr3data$genderid,
                     p.adjust.method = "fdr")

#### Year 4 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether bpm_total
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(bpm_total ~ genderid, data = yr4data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on bpm_total
pairwise.wilcox.test(yr4data$bpm_total, yr4data$genderid,
                     p.adjust.method = "fdr")

##### Create bar graph of bpm total problems by gender group ####
# Use bpm total problems on x-axis and proportion of subjects with a given
# bpm total problems score for each specific gender group (rather than raw 
# number of subjects per gender group) so that differences in GD group are 
# more clear despite having a much smaller sample size
# Create data frame with proportions rather than raw numbers
bpm_total_df_proportions <- yr4data %>%
  # Create bins
  mutate(bpm_total_bin = cut(bpm_total, 
                              breaks = seq(0, 100, by = 10), 
                              right = FALSE)) %>% # Create bins
  group_by(genderid, bpm_total_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(yr4_proportion = n / sum(n)) %>%
  ungroup() %>%
  # ensure all combinations of gender and number of bad events are
  # present so can be included on plot
  complete(bpm_total_bin, genderid, fill = list(n = 0, prop = 0)) %>% 
  mutate(yr4_proportion = replace_na(yr4_proportion,0))
# Make actual bar graph
ggplot(bpm_total_df_proportions, 
       aes(x = bpm_total_bin,y=yr4_proportion,fill=genderid)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49",
                            "50-59","60-69","70-79","80-89","90-99")) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(
                       min(bpm_total_df_proportions$yr4_proportion),
                       0.9,
                       by=0.05),limits = c(0,0.9)) +
  theme_classic()

# Save bar graph
# ggsave("bpm_total_problems_by_gender.tiff",width=5.45,height=3.5,unit="in")

#### Get summary statistics for bpm total problems ####
##### Summary statistics for year 4 ####
yr4data %>%
  # group_by(genderid) %>%
  summarise(
    mean_bpm_total = mean(bpm_total, na.rm = TRUE),
    sd_bpm_total = sd(bpm_total, na.rm = TRUE),
    min_bpm_total = min(bpm_total, na.rm = TRUE),
    max_bpm_total = max(bpm_total, na.rm = TRUE),
    median_bpm_total = median(bpm_total, na.rm = TRUE),
    n = n()
  )

##### Summary statistics for by year ####
alldata %>%
  group_by(eventname,genderid) %>%
  summarise(
    mean_bpm_total = mean(bpm_total, na.rm = TRUE),
    sd_bpm_total = sd(bpm_total, na.rm = TRUE),
    min_bpm_total = min(bpm_total, na.rm = TRUE),
    max_bpm_total = max(bpm_total, na.rm = TRUE),
    median_bpm_total = median(bpm_total, na.rm = TRUE),
    n = n()
  )

### Determine whether BPM internalizing differs based only on gender ####
#### Full data set ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether bpm_int
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(bpm_int ~ genderid, data = alldata)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on bpm_int
pairwise.wilcox.test(alldata$bpm_int, alldata$genderid,
                     p.adjust.method = "fdr")

#### Year 3 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether bpm_int
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(bpm_int ~ genderid, data = yr3data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# GD youth are significantly different from cis girls and cis boys, but the
# difference between cis girls and cis boys is not significant.
pairwise.wilcox.test(yr3data$bpm_int, yr3data$genderid,
                     p.adjust.method = "fdr")

#### Year 4 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether bpm_int
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(bpm_int ~ genderid, data = yr4data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on bpm_total
pairwise.wilcox.test(yr4data$bpm_int, yr4data$genderid,
                     p.adjust.method = "fdr")

##### Create bar graph of BPM internalizing by gender group ####
# Use bpm internalizing on x-axis and proportion of subjects with a given
# bpm internalizing score for each specific gender group (rather than raw 
# number of subjects per gender group) so that differences in GD group are 
# more clear despite having a much smaller sample size
# Create data frame with proportions rather than raw numbers
bpm_int_df_proportions <- 
  yr4data %>%
  # Create bins
  mutate(bpm_int_bin = cut(bpm_int, 
                            breaks = seq(0, 100, by = 10), 
                            right = FALSE)) %>% # Create bins
  group_by(genderid, bpm_int_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(yr4_proportion = n / sum(n)) %>%
  ungroup() %>%
  # ensure all combinations of gender and number of bad events are
  # present so can be included on plot
  complete(bpm_int_bin, genderid, fill = list(n = 0, prop = 0)) %>% 
  mutate(yr4_proportion = replace_na(yr4_proportion,0))
# Make actual bar graph
ggplot(bpm_int_df_proportions, 
       aes(x = bpm_int_bin,y=yr4_proportion,fill=genderid)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49",
                            "50-59","60-69","70-79","80-89","90-99")) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(
                       min(bpm_int_df_proportions$yr4_proportion),
                       0.9,
                       by=0.05),limits = c(0,0.9)) + 
  theme_classic()

# Save bar graph
# ggsave("bpm_int_problems_by_gender.tiff",width=5.45,height=3.5,unit="in")

#### Get summary statistics for bpm internalizing problems ####
##### Summary statistics for year 4 ####
yr4data %>%
  # group_by(genderid) %>%
  summarise(
    mean_bpm_int = mean(bpm_int, na.rm = TRUE),
    sd_bpm_int = sd(bpm_int, na.rm = TRUE),
    min_bpm_int = min(bpm_int, na.rm = TRUE),
    max_bpm_int = max(bpm_int, na.rm = TRUE),
    median_bpm_int = median(bpm_int, na.rm = TRUE),
    n = n()
  )

##### Summary statistics for by year ####
alldata %>%
  group_by(eventname,genderid) %>%
  summarise(
    mean_bpm_int = mean(bpm_int, na.rm = TRUE),
    sd_bpm_int = sd(bpm_int, na.rm = TRUE),
    min_bpm_int = min(bpm_int, na.rm = TRUE),
    max_bpm_int = max(bpm_int, na.rm = TRUE),
    median_bpm_int = median(bpm_int, na.rm = TRUE),
    n = n()
  )

### Determine whether BPM externalizing differs based only on gender ####
#### Full data set ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether bpm_ext
# differs significantly based on gender group
# Test is significant (p < 2.2e-16)
kruskal.test(bpm_ext ~ genderid, data = alldata)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on bpm_ext
pairwise.wilcox.test(alldata$bpm_ext, alldata$genderid,
                     p.adjust.method = "fdr")

#### Year 3 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether bpm_ext
# differs significantly based on gender group
# Test is significant (p = 5.787e-06)
kruskal.test(bpm_ext ~ genderid, data = yr3data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on bpm_ext
pairwise.wilcox.test(yr3data$bpm_ext, yr3data$genderid,
                     p.adjust.method = "fdr")

#### Year 4 data only ####
##### Kruskal-Wallis test ####
# (non-parametric version of one-way ANOVA) to test whether bpm_ext
# differs significantly based on gender group
# Test is significant (p = 2.073e-13)
kruskal.test(bpm_ext ~ genderid, data = yr4data)

##### Post-hoc pairwise Wilcoxon rank sum test using false discovery rate ####
# to identify which pairs of gender groups are significantly different
# all pairs of gender groups differ significantly based on bpm_ext
pairwise.wilcox.test(yr4data$bpm_ext, yr4data$genderid,
                     p.adjust.method = "fdr")

##### Create bar graph of BPM externalizing by gender group ####
# Use bpm externalizing on x-axis and proportion of subjects with a given
# bpm externalizing score for each specific gender group (rather than raw 
# number of subjects per gender group) so that differences in GD group are 
# more clear despite having a much smaller sample size
# Create data frame with proportions rather than raw numbers
bpm_ext_df_proportions <- 
  yr4data %>%
  # Create bins
  mutate(bpm_ext_bin = cut(bpm_ext, 
                            breaks = seq(0, 100, by = 10), 
                            right = FALSE)) %>% # Create bins
  group_by(genderid, bpm_ext_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(yr4_proportion = n / sum(n)) %>%
  ungroup() %>%
  # ensure all combinations of gender and number of bad events are
  # present so can be included on plot
  complete(bpm_ext_bin, genderid, fill = list(n = 0, prop = 0)) %>% 
  mutate(yr4_proportion = replace_na(yr4_proportion,0))
# Make actual bar graph
ggplot(bpm_ext_df_proportions, 
       aes(x = bpm_ext_bin,y=yr4_proportion,fill=genderid)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49",
                            "50-59","60-69","70-79","80-89","90-99")) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(
                       min(bpm_ext_df_proportions$yr4_proportion),
                       1,
                       by=0.05),limits = c(0,1)) +
  theme_classic()

# Save bar graph
# ggsave("bpm_ext_problems_by_gender.tiff",width=5.45,height=3.5,unit="in")

#### Get summary statistics for bpm externalizing problems ####
##### Summary statistics for year 4 ####
yr4data %>%
  # group_by(genderid) %>%
  summarise(
    mean_bpm_ext = mean(bpm_ext, na.rm = TRUE),
    sd_bpm_ext = sd(bpm_ext, na.rm = TRUE),
    min_bpm_ext = min(bpm_ext, na.rm = TRUE),
    max_bpm_ext = max(bpm_ext, na.rm = TRUE),
    median_bpm_ext = median(bpm_ext, na.rm = TRUE),
    n = n()
  )

##### Summary statistics for by year ####
alldata %>%
  group_by(eventname,genderid) %>%
  summarise(
    mean_bpm_ext = mean(bpm_ext, na.rm = TRUE),
    sd_bpm_ext = sd(bpm_ext, na.rm = TRUE),
    min_bpm_ext = min(bpm_ext, na.rm = TRUE),
    max_bpm_ext = max(bpm_ext, na.rm = TRUE),
    median_bpm_ext = median(bpm_ext, na.rm = TRUE),
    n = n()
  )

### Determine whether any variable differs based on sex ####

sex_yr4data <- 
  yr4data %>%
  filter(!(sex=="dont_know")) %>%
  filter(!(sex=="refuse")) 

sex_yr4data %>%
  group_by(sex) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_les = mean(total_bad_le, na.rm = TRUE),
    sd_les = sd(total_bad_le, na.rm = TRUE),
    mean_ders = mean(ders_total, na.rm = TRUE),
    sd_ders = sd(ders_total, na.rm = TRUE),
    mean_cbcl_total = mean(cbcl_total, na.rm = TRUE),
    sd_cbcl_total = sd(cbcl_total, na.rm = TRUE),
    mean_cbcl_int = mean(cbcl_int, na.rm = TRUE),
    sd_cbcl_int = sd(cbcl_int, na.rm = TRUE),
    mean_cbcl_ext = mean(cbcl_ext, na.rm = TRUE),
    sd_cbcl_ext = sd(cbcl_ext, na.rm = TRUE),
    n = n()
  )

wilcox.test(age ~ sex, data = sex_yr4data)
wilcox.test(total_bad_le ~ sex, data = sex_yr4data)
wilcox.test(ders_total ~ sex, data = sex_yr4data)
wilcox.test(cbcl_total ~ sex, data = sex_yr4data)
wilcox.test(cbcl_int ~ sex, data = sex_yr4data)
wilcox.test(cbcl_ext ~ sex, data = sex_yr4data)
wilcox.test(bpm_total ~ sex, data = sex_yr4data)
wilcox.test(bpm_int ~ sex, data = sex_yr4data)
wilcox.test(bpm_ext ~ sex, data = sex_yr4data)


### Establish relationships between all pairs of variables individually ####

# lm_var_list <- colnames(yr4data)[31:35]
# lm_var_list <- colnames(yr4data)[31:46]
# summary_lm <- list()
# list_name<-list()
# var_counter <- 1
# for (var in lm_var_list) {
#   #print(var)
#   #print(nrow(yr4data[,var]))
#   #print(length(yr4data$Z_age))
#   for (var2 in lm_var_list) {
#   #print(var2)
#   #print(nrow(yr4data[,var]))
#     if (var != var2) {
#       
#       lm_out <- lmer(paste(var, "~", var2, "+ Z_age + (1|site)"), data=yr4data)
#       summary_lm[var_counter] <- lm_out
#       list_name[var_counter]<-paste0(var," ~ ",var2)
#       var_counter <- var_counter + 1
#     }
#   }
# }
# 
# print(summary(summary_lm[[1]]))

#### DERS ~ Gender + age + (1|site) ####
# DERS scores differ significantly based on Gender (p = 1.71e-11) but not based on
# age (p = 0.164).
ders_gender_reg <- lmer(Z_ders_total ~ genderid + Z_age + 
                       (1|site),
                     data=yr4data)
summary(ders_gender_reg)

#### LES ~ Gender + age + (1|site) ####
# LES scores differ significantly based on Gender (p = 1.71e-11) but not based on
# age (p = 0.164).
les_gender_reg <- lmer(Z_total_bad_le ~ genderid + Z_age + 
                          (1|site),
                        data=yr4data)
summary(les_gender_reg)

#### CBCL int ~ Gender + age + (1|site) ####
# CBCL int scores differ significantly based on Gender (p = 1.71e-11) but not based on
# age (p = 0.164).
cbcl_int_gender_reg <- lmer(Z_cbcl_int ~ genderid + Z_age + 
                         (1|site),
                       data=yr4data)
summary(cbcl_int_gender_reg)

#### CBCL ext ~ Gender + age + (1|site) ####
# CBCL ext scores differ significantly based on Gender (p = 1.71e-11) but not based on
# age (p = 0.164).
cbcl_ext_gender_reg <- lmer(Z_cbcl_ext ~ genderid + Z_age + 
                              (1|site),
                            data=yr4data)
summary(cbcl_ext_gender_reg)

#### BPM int ~ Gender + age + (1|site) ####
# BPM int scores differ significantly based on Gender (p = 1.71e-11) but not based on
# age (p = 0.164).
bpm_int_gender_reg <- lmer(Z_bpm_int ~ genderid + Z_age + 
                              (1|site),
                            data=yr4data)
summary(bpm_int_gender_reg)

#### BPM ext ~ Gender + age + (1|site) ####
# BPM ext scores differ significantly based on Gender (p = 1.71e-11) but not based on
# age (p = 0.164).
bpm_ext_gender_reg <- lmer(Z_bpm_ext ~ genderid + Z_age + 
                              (1|site),
                            data=yr4data)
summary(bpm_ext_gender_reg)

#### DERS ~ LES + age + (1|site) ####
# DERS scores differ significantly based on LES (p = 1.71e-11) but not based on
# age (p = 0.164).
ders_les_reg <- lmer(Z_ders_total ~ Z_total_bad_le + Z_age + 
                     (1|site),
                     data=yr4data)
summary(ders_les_reg)

#### DERS ~ LES*gender + age + (1|site) ####
ders_les_gender_reg <- lmer(Z_ders_total ~ Z_total_bad_le*genderid + Z_age + 
                                 (1|site),
                               data=yr4data)
summary(ders_les_gender_reg)
# test <- emmeans(ders_les_gender_reg, ~ genderid)
# contrast(test,method="pairwise",adjust="fdr")

# DERS scores differ significantly based on LES and gender but interactions are
# not significant
# ders_les_refcisboy_reg <- lmer(Z_ders_total ~ Z_total_bad_le*genderid_refcisboy + Z_age + 
#                        (1|site),
#                      data=yr4data)
# summary(ders_les_refcisboy_reg)
# # DERS scores differ significantly based on LES and gender but interactions are
# # not significant
# ders_les_refcisgirl_reg <- lmer(Z_ders_total ~ Z_total_bad_le*genderid_refcisgirl + Z_age + 
#                                  (1|site),
#                                data=yr4data)
# summary(ders_les_refcisgirl_reg)

#### CBCL total problems ~ LES + age + (1|site) ####
# CBCL total problems scores differ significantly based on LES (p < 2e-16) and
# based on age (p = 0.00664)
# cbcl_total_les_reg <- lmer(Z_cbcl_total ~ Z_total_bad_le + Z_age + 
#                            (1|site),
#                            data=yr4data)
# summary(cbcl_total_les_reg)
 
#### CBCL internalizing ~ LES + age + (1|site) ####
# CBCL internalizing scores differ significantly based on LES (p < 2e-16) but
# not based on age (p = 0.278)
cbcl_int_les_reg <- lmer(Z_cbcl_int ~ Z_total_bad_le + Z_age + 
                             (1|site),
                           data=yr4data)
summary(cbcl_int_les_reg)

#### CBCL internalizing ~ LES*gender + age + (1|site) ####
cbcl_int_les_gender_reg <- lmer(Z_cbcl_int ~ Z_total_bad_le*genderid + Z_age + 
                              (1|site),
                            data=yr4data)
summary(cbcl_int_les_gender_reg)

#### CBCL internalizing ~ LES*gender + age + (1|site) ####
cbcl_int_les_genderrefcisboy_reg <- lmer(Z_cbcl_int ~ Z_total_bad_le*genderid_refcisboy + Z_age + 
                                  (1|site),
                                data=yr4data)
summary(cbcl_int_les_genderrefcisboy_reg)

#### CBCL internalizing ~ LES*gender + age + (1|site) ####
cbcl_int_les_genderrefcisgirl_reg <- lmer(Z_cbcl_int ~ Z_total_bad_le*genderid_refcisgirl + Z_age + 
                                  (1|site),
                                data=yr4data)
summary(cbcl_int_les_genderrefcisgirl_reg)

#### CBCL externalizing ~ LES + age + (1|site) ####
# CBCL externalizing scores differ significantly based on LES (p = 3.48e-16) and
# based on age (p = 0.00287)
cbcl_ext_les_reg <- lmer(Z_cbcl_ext ~ Z_total_bad_le + Z_age + 
                           (1|site),
                         data=yr4data)
summary(cbcl_ext_les_reg)

#### CBCL externalizing ~ LES*gender + age + (1|site) ####
cbcl_ext_les_gender_reg <- lmer(Z_cbcl_ext ~ Z_total_bad_le*genderid + Z_age + 
                                  (1|site),
                                data=yr4data)
summary(cbcl_ext_les_gender_reg)

#### BPM total problems ~ LES + age + (1|site) ####
# BPM total problems scores differ significantly based on LES (p < 2e-16) but
# not age (p = 0.716)
# bpm_total_les_reg <- lmer(Z_bpm_total ~ Z_total_bad_le + Z_age + 
#                              (1|site),
#                            data=yr4data)
# summary(bpm_total_les_reg)

#### BPM internalizing ~ LES + age + (1|site) ####
# BPM internalizing scores differ significantly based on LES (p < 2e-16) but
# not age (p = 0.143)
bpm_int_les_reg <- lmer(Z_bpm_int ~ Z_total_bad_le + Z_age + 
                           (1|site),
                         data=yr4data)
summary(bpm_int_les_reg)

#### BPM internalizing ~ LES*gender + age + (1|site) ####
bpm_int_les_gender_reg <- lmer(Z_bpm_int ~ Z_total_bad_le*genderid + Z_age + 
                                  (1|site),
                                data=yr4data)
summary(bpm_int_les_gender_reg)

#### BPM externalizing ~ LES + age + (1|site) ####
# BPM externalizing scores differ significantly based on LES (p < 2e-16) but not
# age (p = 0.657)
bpm_ext_les_reg <- lmer(Z_bpm_ext ~ Z_total_bad_le + Z_age + 
                           (1|site),
                         data=yr4data)
summary(bpm_ext_les_reg)

#### BPM externalizing ~ LES*gender + age + (1|site) ####
bpm_ext_les_gender_reg <- lmer(Z_bpm_ext ~ Z_total_bad_le*genderid + Z_age + 
                                 (1|site),
                               data=yr4data)
summary(bpm_ext_les_gender_reg)

#### CBCL total problems ~ DERS + age + (1|site) ####
# CBCL total problems scores differ significantly based on DERS (p < 2e-16) but
# not based on age (p = 0.0644)
# cbcl_total_les_reg <- lmer(Z_cbcl_total ~ Z_ders_total + Z_age + 
#                              (1|site),
#                            data=yr4data)
# summary(cbcl_total_les_reg)

#### CBCL internalizing ~ DERS + age + (1|site) ####
# CBCL internalizing scores differ significantly based on DERS (p < 2e-16) but
# not based on age (p = 0.892)
cbcl_int_ders_reg <- lmer(Z_cbcl_int ~ Z_ders_total + Z_age + 
                           (1|site),
                         data=yr4data)
summary(cbcl_int_ders_reg)

#### CBCL internalizing ~ DERS*gender + age + (1|site) ####
cbcl_int_ders_gender_reg <- lmer(Z_cbcl_int ~ Z_ders_total*genderid + Z_age + 
                                 (1|site),
                               data=yr4data)
summary(cbcl_int_ders_gender_reg)

#### CBCL externalizing ~ DERS + age + (1|site) ####
# CBCL externalizing scores differ significantly based on DERS (p < 2e-16) and
# based on age (p = 0.0288)
cbcl_ext_ders_reg <- lmer(Z_cbcl_ext ~ Z_ders_total + Z_age + 
                           (1|site),
                         data=yr4data)
summary(cbcl_ext_ders_reg)

#### CBCL externalizing ~ DERS*gender + age + (1|site) ####
cbcl_ext_ders_gender_reg <- lmer(Z_cbcl_ext ~ Z_ders_total*genderid + Z_age + 
                                  (1|site),
                                data=yr4data)
summary(cbcl_ext_ders_gender_reg)

#### BPM total problems ~ DERS + age + (1|site) ####
# BPM total problems scores differ significantly based on DERS (p < 2e-16) but
# not age (p = 0.183)
# bpm_total_les_reg <- lmer(Z_bpm_total ~ Z_ders_total + Z_age + 
#                              (1|site),
#                            data=yr4data)
# summary(bpm_total_les_reg)

#### BPM internalizing ~ DERS + age + (1|site) ####
# BPM internalizing scores differ significantly based on DERS (p < 2e-16) and
# age (p = 0.021)
bpm_int_les_reg <- lmer(Z_bpm_int ~ Z_ders_total + Z_age + 
                           (1|site),
                         data=yr4data)
summary(bpm_int_les_reg)

#### BPM internalizing ~ DERS*gender + age + (1|site) ####
bpm_int_les_gender_reg <- lmer(Z_bpm_int ~ Z_ders_total*genderid + Z_age + 
                                  (1|site),
                                data=yr4data)
summary(bpm_int_les_gender_reg)

#### BPM internalizing ~ DERS*gender + age + (1|site) ####
bpm_int_les_genderrefcisboy_reg <- lmer(Z_bpm_int ~ Z_ders_total*genderid_refcisboy + Z_age + 
                                 (1|site),
                               data=yr4data)
summary(bpm_int_les_genderrefcisboy_reg)

#### BPM internalizing ~ DERS*gender + age + (1|site) ####
bpm_int_les_genderrefcisgirl_reg <- lmer(Z_bpm_int ~ Z_ders_total*genderid_refcisgirl + Z_age + 
                                 (1|site),
                               data=yr4data)
summary(bpm_int_les_genderrefcisgirl_reg)

#### BPM externalizing ~ DERS + age + (1|site) ####
# BPM externalizing scores differ significantly based on DERS (p < 2e-16) but
# not age (p = 0.219)
bpm_ext_les_reg <- lmer(Z_bpm_ext ~ Z_ders_total + Z_age + 
                           (1|site),
                         data=yr4data)
summary(bpm_ext_les_reg)

#### BPM externalizing ~ DERS*gender + age + (1|site) ####
bpm_ext_les_gender_reg <- lmer(Z_bpm_ext ~ Z_ders_total*genderid + Z_age + 
                                 (1|site),
                               data=yr4data)
summary(bpm_ext_les_gender_reg)







##### to try controlling for T3 levels of LES and ER
meddata <- yr4data %>%
            rename(
                   Z_yr4_total_bad_le = Z_total_bad_le,
                   Z_yr4_ders_total = Z_ders_total,
                   Z_yr4_cbcl_int = Z_cbcl_int,
                   Z_yr4_cbcl_ext = Z_cbcl_ext,
                   Z_yr4_bpm_int = Z_bpm_int,
                   Z_yr4_bpm_ext = Z_bpm_ext,
                   Z_log_yr4_total_bad_le = Z_log_total_bad_le,
                   Z_log_yr4_ders_total = Z_log_ders_total,
                   Z_log_yr4_cbcl_int = Z_log_cbcl_int,
                   Z_log_yr4_cbcl_ext = Z_log_cbcl_ext,
                   Z_log_yr4_bpm_int = Z_log_bpm_int,
                   Z_log_yr4_bpm_ext = Z_log_bpm_ext,
                   Z_yr4_age = Z_age) %>%
            left_join(select(yr3data, 
                             c(src_subject_id,Z_age,
                               Z_total_bad_le,Z_ders_total,
                               Z_cbcl_int,Z_cbcl_ext,
                               Z_bpm_int,Z_bpm_ext)),
                      by="src_subject_id") %>%
            rename(Z_yr3_total_bad_le = Z_total_bad_le,
                   Z_yr3_ders_total = Z_ders_total,
                   Z_yr3_cbcl_int = Z_cbcl_int,
                   Z_yr3_cbcl_ext = Z_cbcl_ext,
                   Z_yr3_bpm_int = Z_bpm_int,
                   Z_yr3_bpm_ext = Z_bpm_ext,
                   Z_yr3_age = Z_age)
            














library(bruceR)

##### MODELS WITHOUT GENDER ####
cbclint_nogender_model4 <- PROCESS(
  meddata,
  # y = "Z_yr3_cbcl_int",
  y = "Z_yr4_cbcl_int",
  # y = "Z_log_yr4_cbcl_int",
  x = "Z_yr4_total_bad_le",
  # x = "Z_yr3_total_bad_le",
  # x = "Z_log_yr4_total_bad_le",
  # meds = c("Z_yr3_ders_total"),
  meds = c("Z_yr4_ders_total"),
  # meds = c("Z_log_yr4_ders_total"),
  # mods = c("genderid"),
  # mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c(
           # "Z_yr3_age"
           "Z_yr4_age"
           # "Z_yr3_ders_total"
           # "Z_yr3_cbcl_int"
           # "Z_yr3_total_bad_le"
  ),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  # mod.path = c(
  #   "x-y",
  #   # "x-m", 
  #   "m-y" 
  #   # "all"
  # ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)

cbclext_nogender_model4 <- PROCESS(
  meddata,
  # y = "Z_yr3_cbcl_ext",
  y = "Z_yr4_cbcl_ext",
  # y = "Z_log_yr4_cbcl_ext",
  x = "Z_yr4_total_bad_le",
  # x = "Z_yr3_total_bad_le",
  # x = "Z_log_yr4_total_bad_le",
  # meds = c("Z_yr3_ders_total"),
  meds = c("Z_yr4_ders_total"),
  # meds = c("Z_log_yr4_ders_total"),
  # mods = c("genderid"),
  # mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c(
           # "Z_yr3_age"
           "Z_yr4_age"
           # "Z_yr3_ders_total"
           # "Z_yr3_cbcl_ext"
           # "Z_yr3_total_bad_le"
  ),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  # mod.path = c(
  #   "x-y",
  #   # "x-m", 
  #   "m-y" 
  #   # "all"
  # ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)

bpmint_nogender_model4 <- PROCESS(
  meddata,
  # y = "Z_yr3_bpm_int",
  y = "Z_yr4_bpm_int",
  # y = "Z_log_yr4_bpm_int",
  x = "Z_yr4_total_bad_le",
  # x = "Z_yr3_total_bad_le",
  # x = "Z_log_yr4_total_bad_le",
  # meds = c("Z_yr3_ders_total"),
  meds = c("Z_yr4_ders_total"),
  # meds = c("Z_log_yr4_ders_total"),
  # mods = c("genderid"),
  # mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_bpm_int"
    # "Z_yr3_total_bad_le"
  ),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  # mod.path = c(
  #   "x-y",
  #   # "x-m", 
  #   "m-y" 
  #   # "all"
  # ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)

bpmext_nogender_model4 <- PROCESS(
  meddata,
  # y = "Z_yr3_bpm_ext",
  y = "Z_yr4_bpm_ext",
  # y = "Z_log_yr4_bpm_ext",
  x = "Z_yr4_total_bad_le",
  # x = "Z_yr3_total_bad_le",
  # x = "Z_log_yr4_total_bad_le",
  # meds = c("Z_yr3_ders_total"),
  meds = c("Z_yr4_ders_total"),
  # meds = c("Z_log_yr4_ders_total"),
  # mods = c("genderid"),
  # mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_bpm_ext"
    # "Z_yr3_total_bad_le"
  ),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  # mod.path = c(
  #   "x-y",
  #   # "x-m", 
  #   "m-y" 
  #   # "all"
  # ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)

##### MODELS WITH GENDER ####
cbclint_gender_model15 <- PROCESS(
  meddata,
  y = "Z_yr4_cbcl_int",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # mods = c("genderid"),
  mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_cbcl_int"
    # "Z_yr3_total_bad_le"
  ),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)

cbclext_gender_model15 <- PROCESS(
  meddata,
  y = "Z_yr4_cbcl_ext",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # mods = c("genderid"),
  mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_cbcl_ext"
    # "Z_yr3_total_bad_le"
  ),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)

bpmint_gender_model15 <- PROCESS(
  meddata,
  y = "Z_yr4_bpm_int",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # mods = c("genderid"),
  mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_bpm_int"
    # "Z_yr3_total_bad_le"
  ),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)

bpmext_gender_model15 <- PROCESS(
  meddata,
  y = "Z_yr4_bpm_ext",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # mods = c("genderid"),
  # mods = c("genderid_refcisboy"),
  mods = c("genderid_refcisgirl"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_bpm_ext"
    # "Z_yr3_total_bad_le"
  ),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)
























cbclintmodel15 <- PROCESS(
  meddata,
  y = "Z_yr4_cbcl_int",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # mods = c("genderid"),
  mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c("Z_yr4_age"
           # "Z_yr3_ders_total","Z_yr3_cbcl_int"
           # "Z_yr3_total_bad_le"
           ),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  mod.path = c(
    "x-y",
    # "x-m", 
    "m-y" 
    # "all"
  ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  # nsim = 10000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)

bpmintmodel15 <- PROCESS(
  meddata,
  y = "Z_yr4_bpm_int",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # mods = c("genderid"),
  mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c("Z_yr4_age","Z_yr3_ders_total","Z_yr3_bpm_int"),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  mod.path = c(
    "x-y",
    # "x-m", 
    "m-y" 
    # "all"
  ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  # nsim = 10000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)




cbclintmodel15 <- PROCESS(
          yr4data,
          y = "Z_cbcl_int",
          x = "Z_total_bad_le",
          meds = c("Z_ders_total"),
          # mods = c("genderid"),
          mods = c("genderid_refcisboy"),
          # mods = c("genderid_refcisgirl"),
          covs = c("Z_age"),
          # clusters = c(),
          # hlm.re.m = "",
          hlm.re.y = "site",
          # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
          # med.type = c("parallel", "serial"),
          # mod.type = c("2-way", "3-way"),
          mod.path = c(
                       "x-y",
                       # "x-m", 
                       "m-y" 
                       # "all"
                       ),
          cov.path = c(
                       # "y", 
                       # "m", 
                       "both"),
          # mod1.val = NULL,
          # mod2.val = NULL,
          # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
          # nsim = 10000,
          seed = 1234,
          center = TRUE,
          std = FALSE,
          digits = 3,
          # file = NULL
        )


bpmintmodel15 <- PROCESS(
          yr4data,
          y = "Z_bpm_int",
          x = "Z_total_bad_le",
          meds = c("Z_ders_total"),
          # mods = c("genderid"),
          mods = c("genderid_refcisboy"),
          # mods = c("genderid_refcisgirl"),
          covs = c("Z_age"),
          # clusters = c(),
          # hlm.re.m = "",
          hlm.re.y = "site",
          # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
          # med.type = c("parallel", "serial"),
          # mod.type = c("2-way", "3-way"),
          mod.path = c(
            "x-y",
            # "x-m", 
            "m-y" 
            # "all"
          ),
          cov.path = c(
            # "y", 
            # "m", 
            "both"),
          # mod1.val = NULL,
          # mod2.val = NULL,
          # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
          # nsim = 10000,
          seed = 1234,
          center = TRUE,
          std = FALSE,
          digits = 3,
          # file = NULL
        )
        
        





cbclintmodel59 <- PROCESS(
  yr4data,
  y = "Z_cbcl_int",
  x = "Z_total_bad_le",
  meds = c("Z_ders_total"),
  # mods = c("genderid"),
  mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c("Z_age"),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  mod.path = c(
    # "x-y",
    # "x-m", 
    # "m-y" 
    "all"
  ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  # nsim = 10000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)

bpmintmodel59 <- PROCESS(
  yr4data,
  y = "Z_bpm_int",
  x = "Z_total_bad_le",
  meds = c("Z_ders_total"),
  # mods = c("genderid"),
  mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c("Z_age"),
  # clusters = c(),
  # hlm.re.m = "",
  hlm.re.y = "site",
  # hlm.type = c("1-1-1", "2-1-1", "2-2-1"),
  # med.type = c("parallel", "serial"),
  # mod.type = c("2-way", "3-way"),
  mod.path = c(
    # "x-y",
    # "x-m", 
    # "m-y" 
    "all"
  ),
  cov.path = c(
    # "y", 
    # "m", 
    "both"),
  # mod1.val = NULL,
  # mod2.val = NULL,
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  # nsim = 10000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3,
  # file = NULL
)










### Mediation models with lavaan  ####
#### Select only data for mediation analysis ####

###############################################################################
#################### FOR ANALYSIS USING ALL YEAR 4 DATA #######################
###############################################################################

### Create clean data for mediation analysis to make syntax more clear

med_data <- 
  yr4data %>%
  select(src_subject_id,
         eff_cisgirl_ref_cisboy,
         eff_gd_ref_cisboy,
         eff_cisboy_ref_cisgirl,
         eff_gd_ref_cisgirl,
         # gender_cisboy,
         # gender_cisgirl,
         # gender_gd,
         sex,
         sex_female,
         site,
         Z_age,
         Z_total_bad_le,
         Z_ders_total,
         Z_cbcl_total,
         Z_cbcl_int,
         Z_cbcl_ext,
         Z_bpm_total,
         Z_bpm_int,
         Z_bpm_ext) %>%
  rename(
         age = Z_age,
         # cisboy = gender_cisboy,
         # cisgirl = gender_cisgirl,
         # gd = gender_gd,
         female = sex_female,
         LES = Z_total_bad_le,
         DERS = Z_ders_total,
         totalCBCL = Z_cbcl_total,
         intCBCL = Z_cbcl_int,
         extCBCL = Z_cbcl_ext,
         totalBPM = Z_bpm_total,
         intBPM = Z_bpm_int,
         extBPM = Z_bpm_ext
  )

### For mediation analyses using sex instead of gender, need to remove subjects
### who refused to answer or answered "don't know" for sex. Don't want to remove
### these earlier in the code so that we retain all subjects with information on
### gender regardless of whether they have sex data

sex_med_data <- med_data %>% 
  filter(sex!="refuse",sex!="dont_know")

###############################################################################
# FOR ANALYSIS USING YEAR 3 LES & DERS WITH YEAR 4 OUTCOMES, GENDER, & COVARIATES ###############################################################################

### Create clean data for mediation analysis to make syntax more clear

med_data <- 
  yr4data %>%
  # CBCL outcomes, gender, and covariates from year 4
  select(src_subject_id,
         # gender_cisboy,
         # gender_cisgirl,
         # gender_gd,
         eff_cisgirl_ref_cisboy,
         eff_gd_ref_cisboy,
         eff_cisboy_ref_cisgirl,
         eff_gd_ref_cisgirl,
         sex,
         sex_female,
         site,
         Z_age,
         # Z_total_bad_le,
         # Z_ders_total,
         Z_cbcl_total,
         Z_cbcl_int,
         Z_cbcl_ext,
         Z_bpm_total,
         Z_bpm_int,
         Z_bpm_ext) %>%
  # LES and DERS from year 3
  left_join(select(yr3data,c(src_subject_id,
                             Z_total_bad_le,Z_ders_total)),
            by=c("src_subject_id")) %>%
  rename(
    age = Z_age,
    # cisboy = gender_cisboy,
    # cisgirl = gender_cisgirl,
    # gd = gender_gd,
    female = sex_female,
    LES = Z_total_bad_le,
    DERS = Z_ders_total,
    totalCBCL = Z_cbcl_total,
    intCBCL = Z_cbcl_int,
    extCBCL = Z_cbcl_ext,
    totalBPM = Z_bpm_total,
    intBPM = Z_bpm_int,
    extBPM = Z_bpm_ext
  )

### For mediation analyses using sex instead of gender, need to remove subjects
### who refused to answer or answered "don't know" for sex. Don't want to remove
### these earlier in the code so that we retain all subjects with information on
### gender regardless of whether they have sex data

sex_med_data <- med_data %>% 
  filter(sex!="refuse",sex!="dont_know")

###############################################################################
############# FOR ANALYSIS USING LOG TRANSFORMED YEAR 4 DATA ##################
###############################################################################

### Create clean data for mediation analysis to make syntax more clear

med_data <- 
  yr4data %>%
  select(src_subject_id,
         # gender_cisboy,
         # gender_cisgirl,
         # gender_gd,
         eff_cisgirl_ref_cisboy,
         eff_gd_ref_cisboy,
         eff_cisboy_ref_cisgirl,
         eff_gd_ref_cisgirl,
         sex,
         sex_female,
         site,
         Z_age,
         Z_log_total_bad_le,
         Z_log_ders_total,
         Z_log_cbcl_total,
         Z_log_cbcl_int,
         Z_log_cbcl_ext,
         Z_log_bpm_total,
         Z_log_bpm_int,
         Z_log_bpm_ext) %>%
  rename(
    age = Z_age,
    # cisboy = gender_cisboy,
    # cisgirl = gender_cisgirl,
    # gd = gender_gd,
    female = sex_female,
    LES = Z_log_total_bad_le,
    DERS = Z_log_ders_total,
    totalCBCL = Z_log_cbcl_total,
    intCBCL = Z_log_cbcl_int,
    extCBCL = Z_log_cbcl_ext,
    totalBPM = Z_log_bpm_total,
    intBPM = Z_log_bpm_int,
    extBPM = Z_log_bpm_ext
  )

### For mediation analyses using sex instead of gender, need to remove subjects
### who refused to answer or answered "don't know" for sex. Don't want to remove
### these earlier in the code so that we retain all subjects with information on
### gender regardless of whether they have sex data

sex_med_data <- med_data %>% 
  filter(sex!="refuse",sex!="dont_know")

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
       totalCBCL~ c*LES + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
     # mediator 
       DERS ~ a*LES  + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
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
        totalCBCL~ c*LES + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
      # mediator 
        DERS ~ a*LES  + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
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
       intCBCL~ c*LES + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
     # mediator 
       DERS ~ a*LES  + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
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
        intCBCL~ c*LES + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
      # mediator 
        DERS ~ a*LES  + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
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
       extCBCL~ c*LES + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
     # mediator 
       DERS ~ a*LES  + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
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
        extCBCL~ c*LES + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
      # mediator 
        DERS ~ a*LES  + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
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





















##### BPM total problems ####
###### Without gender or sex ####
nogendertotalmodel <- 
  ' # direct effect
        totalBPM~ c*LES + age
      # mediator
        DERS ~ a*LES  + age
      # indirect effect
        totalBPM~ b*DERS
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
       totalBPM~ c*LES + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
     # mediator 
       DERS ~ a*LES  + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
     # indirect effect 
       totalBPM~ b*DERS
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
        totalBPM~ c*LES + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
      # mediator 
        DERS ~ a*LES  + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
      # indirect effect 
        totalBPM~ b*DERS
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
        totalBPM~ c*LES + female + age + LES:female
      # mediator
        DERS ~ a*LES  + female + age + LES:female
      # indirect effect
        totalBPM~ b*DERS
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


##### BPM internalizing ####
###### Without gender or sex ####
nogenderintmodel <- 
  ' # direct effect
        intBPM~ c*LES + age
      # mediator
        DERS ~ a*LES  + age
      # indirect effect
        intBPM~ b*DERS
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
       intBPM~ c*LES + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
     # mediator 
       DERS ~ a*LES  + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
     # indirect effect 
       intBPM~ b*DERS
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
        intBPM~ c*LES + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
      # mediator 
        DERS ~ a*LES  + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
      # indirect effect 
        intBPM~ b*DERS
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
        intBPM~ c*LES + female + age + LES:female
      # mediator
        DERS ~ a*LES  + female + age + LES:female
      # indirect effect
        intBPM~ b*DERS
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

##### BPM externalizing ####
###### Without gender or sex ####
nogenderextmodel <- 
  ' # direct effect
        extBPM~ c*LES + age
      # mediator
        DERS ~ a*LES  + age
      # indirect effect
        extBPM~ b*DERS
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
       extBPM~ c*LES + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
     # mediator 
       DERS ~ a*LES  + eff_cisgirl_ref_cisboy + eff_gd_ref_cisboy + age + LES:eff_cisgirl_ref_cisboy + LES:eff_gd_ref_cisboy
     # indirect effect 
       extBPM~ b*DERS
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
        extBPM~ c*LES + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
      # mediator 
        DERS ~ a*LES  + eff_cisboy_ref_cisgirl + eff_gd_ref_cisgirl + age + LES:eff_cisboy_ref_cisgirl + LES:eff_gd_ref_cisgirl
      # indirect effect 
        extBPM~ b*DERS
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
        extBPM~ c*LES + female + age + LES:female
      # mediator
        DERS ~ a*LES  + female + age + LES:female
      # indirect effect
        extBPM~ b*DERS
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