### Set working directory ####
setwd("C:/Users/Kate Scheuer/OneDrive - UW/Desktop/Lab/aces_emotionreg_cbcl")

### Load libraries ####
library(tidyverse)
library(FSA)
library(lme4)
library(lmerTest)
library(nortest)
library(psych)
library(lavaan)
library(ggpattern)
library(misty)
library(bruceR)



## PREP DATA ####

### Read in raw data ####
#### Gender data ####

gish_y_gi <- read_csv("data/gish_y_gi.csv")

#### DERS-P for emotion regulation ####
mh_p_ders <- read_csv("data/mh_p_ders.csv")

#### CBCL for parent-report psychopathology symptoms ####
mh_p_cbcl <- read_csv("data/mh_p_cbcl.csv")

#### BPM for youth-report psychopathology symptoms ####
mh_y_bpm <- read_csv("data/mh_y_bpm.csv")

#### Longitudinal tracking data ####
abcd_y_lt <- read_csv("data/abcd_y_lt.csv")

#### LES (youth-reported) ####
#### LES for exposure to negative life events
mh_y_le <- read_csv("data/mh_y_le.csv")

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
  # combine gender and trans identity information to make five gender groups
  mutate(gender_details = case_when(
                            kbi_gender==777 ~ "refuse",
                            kbi_y_trans_id==777 ~ "refuse",
                            kbi_gender==4 ~ "dont_understand",
                            kbi_y_trans_id==4 ~ "dont_understand",
                            kbi_gender==1 & kbi_y_trans_id==1 ~ "trans_boy", #"gd",
                            kbi_gender==1 & kbi_y_trans_id==2 ~ "trans_boy", #"gd",
                            kbi_gender==1 & kbi_y_trans_id==3 ~ "cis_boy",
                            kbi_gender==2 & kbi_y_trans_id==1 ~ "trans_girl", #"gd",
                            kbi_gender==2 & kbi_y_trans_id==2 ~ "trans_girl", #"gd",
                            kbi_gender==2 & kbi_y_trans_id==3 ~ "cis_girl",
                            kbi_gender==3 & kbi_y_trans_id==1 ~ "nb", #"gd",
                            kbi_gender==3 & kbi_y_trans_id==2 ~ "nb", #"gd",
                            kbi_gender==3 & kbi_y_trans_id==3 ~ "nb" #"gd"
  )) %>%
  # combine all not cis groups due to sample size to make one gender diverse group (gd)
  mutate(genderid = case_when(
                      gender_details=="trans_boy" ~ "gd",
                      gender_details=="trans_girl" ~ "gd",
                      gender_details=="nb" ~ "gd",
                      gender_details=="cis_boy" ~ "cis_boy",
                      gender_details=="cis_girl" ~ "cis_girl"
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
  select(src_subject_id,eventname,sex,gender,trans,gender_details,
         sex_female,
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

# excluding participants who said "don't know" or "refuse" for sex
# n should be 3638
yr4sexdata <- yr4data %>% 
    filter(sex!="refuse",
           sex!="dont_know") %>%
    mutate(sex = fct_relevel(sex, "male"))

#### Create separate data frame for just data from year 3 follow-up visit ####
# n should be 8443
yr3data <- alldata %>% filter(eventname=="3_year_follow_up_y_arm_1")

# excluding participants who said "don't know" or "refuse" for sex
# n should be 8316
yr3sexdata <- yr3data %>% 
  filter(sex!="refuse",
         sex!="dont_know")


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

#### Determine how many subjects in each more detailed gender group ####
yr4data %>%
  group_by(gender_details) %>%
  count()

#### Determine how many subjects in each combination of gender and sex group ####
yr4data %>%
  group_by(genderid, sex) %>%
  count()



## BASIC STATS ####

### Get summary stats for each variable ####
sumstats <- 
  yr4data %>%
  # yr4sexdata %>%
  # group_by(genderid) %>%
  group_by(gender_details) %>%
  # group_by(sex) %>%
  summarise(
    n = n(),
    across(
      c("age","total_bad_le","ders_total",
        "cbcl_int","cbcl_ext","bpm_int","bpm_ext"),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE)
        # min = ~min(.x, na.rm = TRUE),
        # max = ~max(.x, na.rm = TRUE),
        # median = ~median(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  # transpose so gender groups are columns and summary stats are rows
  t() %>% 
  # make data frame so values are not strings
  as.data.frame() %>%
  # make first row ie gender group names into column names
  set_names(.[1, ]) %>%
  # remove first row which is now column names
  slice(-1) %>%
  # make all values numeric
  mutate(across(everything(), as.numeric)) %>%
  # round all values to two decimal places
  mutate(across(where(is.numeric), ~ round(.x, 2)))
sumstats

### Assess normality of distributions of each variable from all data ####
# p-value < 0.05 suggests data is not normally distributed
# walk() applies a function, in this case ad.test, to a list
# cat() makes it print the variable name before each test
# note: Shapiro-Wilk test only allows up to 5000 samples so using Anderson-
# Darling test.
# All variables are non-normally distributed, even after log transformation.
walk(c("total_bad_le", "log_total_bad_le", 
       "ders_total", "log_ders_total", 
       "cbcl_int", "log_cbcl_int", 
       "cbcl_ext", "log_cbcl_ext",
       "bpm_int", "log_bpm_int", 
       "bpm_ext", "log_bpm_ext"), 
     ~ {
       cat("Variable:", .x, "\n")
       print(ad.test(yr4data[[.x]]))
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
           cbcl_int,cbcl_ext,
           bpm_int,bpm_ext,
           age)) %>% 
  # run correlation tests for all pairs of variables, adjust using
  corr.test(adjust="fdr")
# print correlation matrix, correlation coefficients, and p-values
# note that values are already rounded to the decimal place which is showing
print(corrmat,digits=3)
# See fdr-adjusted p-value for each correlation test
# note that list gives p values above diagonal, going across rows (ie not down
# columns) 
corrmat$p.adj




## PLOTS ####

### Plot variables against each other ####
#### Without gender
##### Scatterplots of DERS vs CBCL internalizing and externalizing ####
outcome_list <- c("cbcl_int","cbcl_ext")
cbcl_ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  cbcl_ders_plot <- ggplot(aes(x=ders_total,y=.data[[outcome]]),data=yr4data) +
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
  cbcl_ders_plot_list[[outcome]] <- cbcl_ders_plot
  # Save plot
  ggsave(paste0("ders_vs_",outcome,"_nogender.tiff"),
         width=6,height=6,units = "in",
         path="figures/")
}

##### Scatterplots of DERS vs BPM internalizing and externalizing ####
outcome_list <- c("bpm_int","bpm_ext")
bpm_ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  bpm_ders_plot <- ggplot(aes(x=ders_total,y=.data[[outcome]]),data=yr4data) +
    geom_point(size=1) +
    geom_smooth(method="lm",
                se=FALSE,
                linewidth=1.25,color="black") +
    scale_x_continuous(expand = c(0,0),
                       breaks=seq(25,150,25),
                       limits = c(25,155)) +
    scale_y_continuous(expand = c(0,0),
                       breaks=seq(40,80,10),
                       limits = c(40,80)) +
    theme_classic()
  bpm_ders_plot_list[[outcome]] <- bpm_ders_plot
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

####### Create bar graph of LES by gender group ####
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


####### Create bar graph of DERS by gender group ####
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


##### Bargraph of LES vs DERS by gender ####
ggplot(yr4data, 
       aes(x=total_bad_le,y=ders_total, fill=genderid)) +
  geom_bar(stat="summary",fun="mean",
           position=position_dodge2(preserve = "single"),
           color="gray",width=.7) +
  scale_fill_grey(start=0.7,end=0.2) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 1)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(0,125,by=25),
                     limits=c(0,125)) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_ders_bygender.tiff",width=14,height=6,unit="in",path="figures")

##### Bargraph of LES vs CBCL internalizing by gender ####
ggplot(yr4data, 
       aes(x=total_bad_le,y=cbcl_int, fill=genderid)) +
  geom_bar(stat="summary",fun="mean",
           position=position_dodge2(preserve = "single"),
           color="gray",width=.7) +
  # scale_fill_grey(start=0.7,end=0.2) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 1)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(0,90,by=10),
                     limits=c(0,90)) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_cbclint_bygender.tiff",width=14,height=6,unit="in",path="figures")

##### Bargraph of LES vs CBCL externalizing by gender ####
ggplot(yr4data, 
       aes(x=total_bad_le,y=cbcl_ext, fill=genderid)) +
  geom_bar(stat="summary",fun="mean",
           position=position_dodge2(preserve = "single"),
           color="gray",width=.7) +
  # scale_fill_grey(start=0.7,end=0.2) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 1)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(0,90,by=10),
                     limits=c(0,90)) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_cbclext_bygender.tiff",width=14,height=6,unit="in",path="figures")

##### Bargraph of LES vs BPM internalizing by gender ####
ggplot(yr4data, 
       aes(x=total_bad_le,y=bpm_int, fill=genderid)) +
  geom_bar(stat="summary",fun="mean",
           position=position_dodge2(preserve = "single"),
           color="gray",width=.7) +
  # scale_fill_grey(start=0.7,end=0.2) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 1)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(0,90,by=10),
                     limits=c(0,90)) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_bpmint_bygender.tiff",width=14,height=6,unit="in",path="figures")

##### Bargraph of LES vs BPM externalizing by gender ####
ggplot(yr4data, 
       aes(x=total_bad_le,y=bpm_ext, fill=genderid)) +
  geom_bar(stat="summary",fun="mean",
           position=position_dodge2(preserve = "single"),
           color="gray",width=.7) +
  # scale_fill_grey(start=0.7,end=0.2) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 1)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(0,90,by=10),
                     limits=c(0,90)) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_bpmext_bygender.tiff",width=14,height=6,unit="in",path="figures")

##### Scatterplots of DERS vs CBCL by gender ####
outcome_list <- c("cbcl_int","cbcl_ext")
cbcl_ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  cbcl_ders_plot <- ggplot(aes(x=ders_total,y=.data[[outcome]],
                               color=genderid),data=yr4data) +
    geom_point(size=1,) +
    geom_smooth(method="lm",
                se=FALSE,
                linewidth=1.25) +
    # geom_hline(yintercept=70,linetype='dashed') +
    scale_x_continuous(expand = c(0,0),
                       breaks=seq(25,150,25),
                       limits = c(25,155)) +
    scale_y_continuous(expand = c(0,0),
                       breaks=seq(20,90,10),
                       limits = c(20,90)) +
    theme_classic()
  cbcl_ders_plot_list[[outcome]] <- cbcl_ders_plot
  # Save plot
  # ggsave(paste0("ders_vs_",outcome,"_bygender.tiff"),
  #        width=7,height=6,units = "in",path="figures")
}

##### Scatterplots of DERS vs BPM by gender ####
outcome_list <- c("bpm_int","bpm_ext")
bpm_ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  bpm_ders_plot <- ggplot(aes(x=ders_total,y=.data[[outcome]],
                              color=genderid),data=yr4data) +
    geom_point(size=1) +
    geom_smooth(method="lm",
                se=FALSE,
                linewidth=1.25) +
    # geom_hline(yintercept=65,linetype='dashed') +
    scale_x_continuous(expand = c(0,0),
                       breaks=seq(25,150,25),
                       limits = c(25,155)) +
    scale_y_continuous(expand = c(0,0),
                       breaks=seq(40,80,10),
                       limits = c(40,80)) +
    theme_classic()
  bpm_ders_plot_list[[outcome]] <- bpm_ders_plot
  # Save plot
  # ggsave(paste0("ders_vs_",outcome,"_bygender.tiff"),
  #        width=7,height=6,units = "in",path="figures")
}

####### Create bar graph of BPM internalizing by gender group ####
# Use bpm internalizing on x-axis and proportion of subjects with a given
# bpm internalizing score for each specific gender group (rather than raw 
# number of subjects per gender group) so that differences in GD group are 
# more clear despite having a much smaller sample size
# Create data frame with proportions rather than raw numbers
bpm_int_df_proportions <- 
  yr4data %>%
  # Create bins
  mutate(bpm_int_bin = cut(bpm_int, 
                           breaks = seq(45, 80, by = 5), 
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
  scale_x_discrete(labels=c("45-49","50-54","55-59","60-64","65-69",
                            "70-74","75-79")) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(
                       min(bpm_int_df_proportions$yr4_proportion),
                       1,
                       by=.2),limits = c(0,1)) +
  theme_classic()
# Save bar graph
# ggsave("bpm_int_bygender.tiff",width=7,height=6,unit="in",path="figures")

####### Create bar graph of BPM externalizing by gender group ####
# Use bpm externalizing on x-axis and proportion of subjects with a given
# bpm externalizing score for each specific gender group (rather than raw 
# number of subjects per gender group) so that differences in GD group are 
# more clear despite having a much smaller sample size
# Create data frame with proportions rather than raw numbers
bpm_ext_df_proportions <- 
  yr4data %>%
  # Create bins
  mutate(bpm_ext_bin = cut(bpm_ext, 
                           breaks = seq(45, 80, by = 5), 
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
  scale_x_discrete(labels=c("45-49","50-54","55-59","60-64","65-69",
                            "70-74","75-79")) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(
                       min(bpm_ext_df_proportions$yr4_proportion),
                       1,
                       by=.2),limits = c(0,1)) +
  theme_classic()
# Save bar graph
# ggsave("bpm_ext_bygender.tiff",width=7,height=6,unit="in",path="figures")













####### Create bar graph of CBCL internalizing by gender group ####
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

####### Create bar graph of CBCL externalizing by gender group ####
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


## STEP ONE: BASIC GROUP DIFFERENCES AND REGRESSION ####

### Kruskal-Wallis (non-parametric version of one-way ANOVA) and Dunn test ie ####
### pairwise Wilcoxon tests to determine whether variables differ based on 
### gender
#### Age (year 4) does not differ significantly based on gender (p = 0.1779)
kruskal.test(age ~ genderid, data = yr4data)
# kruskal.test(age ~ gender_details, data = yr4data)
#### LES (year 4) does differ significantly based on gender (p = 4.803e-11), and
#### all gender groups are significantly different from each other
kruskal.test(total_bad_le ~ genderid, data = yr4data)
# kruskal.test(total_bad_le ~ gender_details, data = yr4data)
dunnTest(yr4data$total_bad_le, yr4data$genderid, method = "bh")
# dunnTest(yr4data$total_bad_le, yr4data$gender_details, method = "bh")
#### LES (year 3) does differ significantly based on gender (p = 0.002221), and
#### all gender groups are significantly different from each other
kruskal.test(total_bad_le ~ genderid, data = yr3data)
dunnTest(yr3data$total_bad_le, yr3data$genderid, method = "bh")
#### DERS (year 4) does differ significantly based on gender (p = 2.022e-13), 
#### and all gender groups are significantly different from each other
kruskal.test(ders_total ~ genderid, data = yr4data)
# kruskal.test(ders_total ~ gender_details, data = yr4data)
dunnTest(yr4data$ders_total, yr4data$genderid, method = "bh")
# dunnTest(yr4data$ders_total, yr4data$gender_details, method = "bh")
#### DERS (year 3) does differ significantly based on gender (p < 2.2e-16), and
#### all gender groups are significantly different from each other
kruskal.test(ders_total ~ genderid, data = yr3data)
dunnTest(yr3data$ders_total, yr3data$genderid, method = "bh")
#### CBCL internalizing (year 4) does differ significantly based on gender 
#### (p < 2.2e-16). GD are significantly different from cis boy and cis girls, 
#### but cis boy and cis girls are not significantly different from each other.
kruskal.test(cbcl_int ~ genderid, data = yr4data)
# kruskal.test(cbcl_int ~ gender_details, data = yr4data)
dunnTest(yr4data$cbcl_int, yr4data$genderid, method = "bh")
# dunnTest(yr4data$cbcl_int, yr4data$gender_details, method = "bh")
#### CBCL internalizing (year 3) does differ significantly based on gender 
#### (p < 2.2e-16), and all gender groups are significantly different from each
#### other.
kruskal.test(cbcl_int ~ genderid, data = yr3data)
dunnTest(yr3data$cbcl_int, yr3data$genderid, method = "bh")
#### CBCL externalizing (year 4) does differ significantly based on gender 
#### (p = 9.4e-09), and all gender groups are significantly different from each
#### other.
kruskal.test(cbcl_ext ~ genderid, data = yr4data)
# kruskal.test(cbcl_ext ~ gender_details, data = yr4data)
dunnTest(yr4data$cbcl_ext, yr4data$genderid, method = "bh")
# dunnTest(yr4data$cbcl_ext, yr4data$gender_details, method = "bh")
#### CBCL externalizing (year 3) does differ significantly based on gender 
#### (p < 2.2e-16), and all gender groups are significantly different from each
#### other.
kruskal.test(cbcl_ext ~ genderid, data = yr3data)
dunnTest(yr3data$cbcl_ext, yr3data$genderid, method = "bh")
#### BPM internalizing (year 4) does differ significantly based on gender 
#### (p < 2.2e-16), and all gender groups are significantly different from each
#### other. 
kruskal.test(bpm_int ~ genderid, data = yr4data)
# kruskal.test(bpm_int ~ gender_details, data = yr4data)
dunnTest(yr4data$bpm_int, yr4data$genderid, method = "bh")
# dunnTest(yr4data$bpm_int, yr4data$gender_details, method = "bh")
#### BPM internalizing (year 3) does differ significantly based on gender 
#### (p < 2.2e-16). GD are significantly different from cis boy and cis girls, 
#### but cis boy and cis girls are not significantly different from each other.
kruskal.test(bpm_int ~ genderid, data = yr3data)
dunnTest(yr3data$bpm_int, yr3data$genderid, method = "bh")
#### BPM externalizing (year 4) does differ significantly based on gender 
#### (p = 2.073e-13), and all gender groups are significantly different from each
#### other.
kruskal.test(bpm_ext ~ genderid, data = yr4data)
# kruskal.test(bpm_ext ~ gender_details, data = yr4data)
dunnTest(yr4data$bpm_ext, yr4data$genderid, method = "bh")
# dunnTest(yr4data$bpm_ext, yr4data$gender_details, method = "bh")
#### BPM externalizing (year 3) does differ significantly based on gender 
#### (p = 5.787e-06), and all gender groups are significantly different from each
#### other.
kruskal.test(bpm_ext ~ genderid, data = yr3data)
dunnTest(yr3data$bpm_ext, yr3data$genderid, method = "bh")

### Mann-Whitney U (non-parametric version of two-sample t-test) to determine ####
### whether variables differ based on sex
#### Age (year 4) does not differ significantly based on sex (p = 0.2516)
wilcox.test(age ~ sex, data = yr4sexdata)
#### LES (year 4) does differ significantly based on sex (p = 2.9e-08)
wilcox.test(total_bad_le ~ sex, data = yr4sexdata)
#### LES (year 3) does differ significantly based on sex (p = 0.01768)
wilcox.test(total_bad_le ~ sex, data = yr3sexdata)
#### DERS (year 4) does not differ significantly based on sex (p = 0.191)
wilcox.test(ders_total ~ sex, data = yr4sexdata)
#### DERS (year 3) does differ significantly based on sex (p = 5.757e-16)
wilcox.test(ders_total ~ sex, data = yr3sexdata)
#### CBCL internalizing (year 4) does differ significantly based on sex 
#### (p = 0.008146)
wilcox.test(cbcl_int ~ sex, data = yr4sexdata)
#### CBCL internalizing (year 3) does differ significantly based on sex 
#### (p = 0.01794)
wilcox.test(cbcl_int ~ sex, data = yr3sexdata)
#### CBCL externalizing (year 4) does not differ significantly based on sex 
#### (p = 0.1392)
wilcox.test(cbcl_ext ~ sex, data = yr4sexdata)
#### CBCL externalizing (year 3) does differ significantly based on sex 
#### (p = 1.665e-08)
wilcox.test(cbcl_ext ~ sex, data = yr3sexdata)
#### BPM internalizing (year 4) does differ significantly based on sex 
#### (p = 1.247e-12)
wilcox.test(bpm_int ~ sex, data = yr4sexdata)
#### BPM internalizing (year 3) does differ significantly based on sex 
#### (p = 0.005661)
wilcox.test(bpm_int ~ sex, data = yr3sexdata)
#### BPM externalizing (year 4) does differ significantly based on sex 
#### (p = 4.917e-08)
wilcox.test(bpm_ext ~ sex, data = yr4sexdata)
#### BPM externalizing (year 3) does differ significantly based on sex 
#### (p = 0.003231)
wilcox.test(bpm_ext ~ sex, data = yr3sexdata)

### Mixed effect linear regression to determine whether any variable differs ####
### based on age, using site as random intercept  
#### LES ~ age + (1|site) ####
# LES scores differ significantly based on age (p = 0.0105)
les_age_reg <- lmer(Z_total_bad_le ~ Z_age + (1|site), data=yr4data)
summary(les_age_reg)
#### DERS ~ age + (1|site) ####
# DERS scores do not differ significantly based on age (p = 0.2704)
ders_age_reg <- lmer(Z_ders_total ~ Z_age + (1|site), data=yr4data)
summary(ders_age_reg)
#### CBCL internalizing ~ age + (1|site) ####
# CBCL internalizing scores do not differ significantly based on age (p = 0.489)
cbcl_int_age_reg <- lmer(Z_cbcl_int ~ Z_age + (1|site), data=yr4data)
summary(cbcl_int_age_reg)
#### CBCL externalizing ~ age + (1|site) ####
# CBCL externalizing scores do differ significantly based on age (p = 0.0098)
cbcl_ext_age_reg <- lmer(Z_cbcl_ext ~ Z_age + (1|site), data=yr4data)
summary(cbcl_ext_age_reg)
#### BPM internalizing ~ age + (1|site) ####
# BPM internalizing scores do differ significantly based on age (p = 0.0413)
bpm_int_age_reg <- lmer(Z_bpm_int ~ Z_age + (1|site), data=yr4data)
summary(bpm_int_age_reg)
#### BPM externalizing ~ age + (1|site) ####
# BPM externalizing scores do not differ significantly based on age (p = 0.400)
bpm_ext_age_reg <- lmer(Z_bpm_ext ~ Z_age + (1|site), data=yr4data)
summary(bpm_ext_age_reg)

### Mixed effect linear regression to determine whether DERS differs based ####
### on LES, using age as fixed effect covariate and site as random intercept
#### DERS ~ LES + age + (1|site) ####
# DERS scores differ significantly based on LES (p = 1.71e-11)
ders_les_age_reg <- lmer(Z_ders_total ~ Z_total_bad_le + Z_age + (1|site),
# ders_les_age_reg <- lmer(Z_log_ders_total ~ Z_log_total_bad_le + Z_age + (1|site),
                         data=yr4data)
                         # data=yr3data)
summary(ders_les_age_reg)

### Mixed effect linear regression to determine whether CBCL or BPM differ ####
### based on LES or DERS, using age as fixed effect covariate and site as random 
### intercept
#### CBCL internalizing ~ LES + age + (1|site) ####
# CBCL internalizing scores differ significantly based on LES (p < 2e-16)
cbcl_int_les_age_reg <- lmer(Z_cbcl_int ~ Z_total_bad_le + Z_age + (1|site),
# cbcl_int_les_age_reg <- lmer(Z_log_cbcl_int ~ Z_log_total_bad_le + Z_age + (1|site), 
                           data=yr4data)
                         # data=yr3data)
summary(cbcl_int_les_age_reg)
#### CBCL externalizing ~ LES + age + (1|site) ####
# CBCL externalizing scores differ significantly based on LES (p = 3.48e-16)
cbcl_ext_les_age_reg <- lmer(Z_cbcl_ext ~ Z_total_bad_le + Z_age + (1|site),
# cbcl_ext_les_age_reg <- lmer(Z_log_cbcl_ext ~ Z_log_total_bad_le + Z_age + (1|site), 
                             data=yr4data)
                             # data=yr3data)
summary(cbcl_ext_les_age_reg)
# BPM internalizing scores differ significantly based on LES (p < 2e-16)
bpm_int_les_age_reg <- lmer(Z_bpm_int ~ Z_total_bad_le + Z_age + (1|site),
# bpm_int_les_age_reg <- lmer(Z_log_bpm_int ~ Z_log_total_bad_le + Z_age + (1|site), 
                            data=yr4data)
                             # data=yr3data)
summary(bpm_int_les_age_reg)
#### BPM externalizing ~ LES + age + (1|site) ####
# BPM externalizing scores differ significantly based on LES (p < 2e-16)
bpm_ext_les_age_reg <- lmer(Z_bpm_ext ~ Z_total_bad_le + Z_age + (1|site),
# bpm_ext_les_age_reg <- lmer(Z_log_bpm_ext ~ Z_log_total_bad_le + Z_age + (1|site), 
                            data=yr4data)
                             # data=yr3data)
summary(bpm_ext_les_age_reg)
#### CBCL internalizing ~ DERS + age + (1|site) ####
# CBCL internalizing scores differ significantly based on DERS (p < 2e-16)
cbcl_int_ders_age_reg <- lmer(Z_cbcl_int ~ Z_ders_total + Z_age + (1|site),
# cbcl_int_ders_age_reg <- lmer(Z_log_cbcl_int ~ Z_log_ders_total + Z_age + (1|site), 
                              data=yr4data)
summary(cbcl_int_ders_age_reg)
#### CBCL externalizing ~ DERS + age + (1|site) ####
# CBCL externalizing scores differ significantly based on DERS (p < 2e-16)
cbcl_ext_ders_age_reg <- lmer(Z_cbcl_ext ~ Z_ders_total + Z_age + (1|site),
# cbcl_ext_ders_age_reg <- lmer(Z_log_cbcl_ext ~ Z_log_ders_total + Z_age + (1|site), 
                              data=yr4data)
summary(cbcl_ext_ders_age_reg)
# BPM internalizing scores differ significantly based on DERS (p < 2e-16)
bpm_int_ders_age_reg <- lmer(Z_bpm_int ~ Z_ders_total + Z_age + (1|site),
# bpm_int_ders_age_reg <- lmer(Z_log_bpm_int ~ Z_log_ders_total + Z_age + (1|site), 
                             data=yr4data)
summary(bpm_int_ders_age_reg)
#### BPM externalizing ~ DERS + age + (1|site) ####
# BPM externalizing scores differ significantly based on DERS (p < 2e-16)
bpm_ext_ders_age_reg <- lmer(Z_bpm_ext ~ Z_ders_total + Z_age + (1|site),
# bpm_ext_ders_age_reg <- lmer(Z_log_bpm_ext ~ Z_log_ders_total + Z_age + (1|site), 
                             data=yr4data)
summary(bpm_ext_ders_age_reg)



## STEP TWO: MODERATING EFFECTS OF GENDER OR SEX ####

### Mixed effect linear regression to determine whether gender moderates ####
### relationship between age and any variable, using site as random intercept
#### LES ~ age*gender + (1|site) ####
# Relationship between LES and age does not differ significantly based on gender
les_age_gendercisboy_reg <- lmer(Z_total_bad_le ~ Z_age*genderid_refcisboy + (1|site), data=yr4data)
summary(les_age_gendercisboy_reg)
les_age_gendercisgirl_reg <- lmer(Z_total_bad_le ~ Z_age*genderid_refcisgirl + (1|site), data=yr4data)
summary(les_age_gendercisgirl_reg)
#### DERS ~ age*gender + (1|site) ####
# Relationship between DERS and age does not differ significantly based on gender
ders_age_gendercisboy_reg <- lmer(Z_ders_total ~ Z_age*genderid_refcisboy + (1|site), data=yr4data)
summary(ders_age_gendercisboy_reg)
ders_age_gendercisgirl_reg <- lmer(Z_ders_total ~ Z_age*genderid_refcisgirl + (1|site), data=yr4data)
summary(ders_age_gendercisgirl_reg)
#### CBCL internalizing ~ age*gender + (1|site) ####
# Relationship between CBCL internalizing and age significantly different for 
# gd vs cis girls and for gd vs cis boys
cbcl_int_age_gendercisboy_reg <- lmer(Z_cbcl_int ~ Z_age*genderid_refcisboy + (1|site), data=yr4data)
summary(cbcl_int_age_gendercisboy_reg)
cbcl_int_age_gendercisgirl_reg <- lmer(Z_cbcl_int ~ Z_age*genderid_refcisgirl + (1|site), data=yr4data)
summary(cbcl_int_age_gendercisgirl_reg)
#### CBCL externalizing ~ age*gender + (1|site) ####
# Relationship between CBCL externalizing and age does not differ significantly 
# based on gender
cbcl_ext_age_gendercisboy_reg <- lmer(Z_cbcl_ext ~ Z_age*genderid_refcisboy + (1|site), data=yr4data)
summary(cbcl_ext_age_gendercisboy_reg)
cbcl_ext_age_gendercisgirl_reg <- lmer(Z_cbcl_ext ~ Z_age*genderid_refcisgirl + (1|site), data=yr4data)
summary(cbcl_ext_age_gendercisgirl_reg)
#### BPM internalizing ~ age*gender + (1|site) ####
# Relationship between BPM internalizing and age does not differ significantly 
# based on gender
bpm_int_age_gendercisboy_reg <- lmer(Z_bpm_int ~ Z_age*genderid_refcisboy + (1|site), data=yr4data)
summary(bpm_int_age_gendercisboy_reg)
bpm_int_age_gendercisgirl_reg <- lmer(Z_bpm_int ~ Z_age*genderid_refcisgirl + (1|site), data=yr4data)
summary(bpm_int_age_gendercisgirl_reg)
#### BPM externalizing ~ age*gender + (1|site) ####
# Relationship between BPM externalizing and age significantly different for gd
# vs cis boys and for gd vs cis girls
bpm_ext_age_gendercisboy_reg <- lmer(Z_bpm_ext ~ Z_age*genderid_refcisboy + (1|site), data=yr4data)
summary(bpm_ext_age_gendercisboy_reg)
bpm_ext_age_gendercisgirl_reg <- lmer(Z_bpm_ext ~ Z_age*genderid_refcisgirl + (1|site), data=yr4data)
summary(bpm_ext_age_gendercisgirl_reg)

### Mixed effect linear regression to determine whether gender moderates ####
### relationship between DERS and LES, use age as fixed effect covariate and
### site as random intercept
#### DERS ~ LES*gender + age + (1|site) ####
# Relationship between DERS and LES does not differ significantly based on gender
ders_les_gendercisboy_reg <- lmer(Z_ders_total ~ Z_total_bad_le*genderid_refcisboy + Z_age + (1|site), 
                         data=yr4data)
summary(ders_les_gendercisboy_reg)
ders_les_gendercisgirl_reg <- lmer(Z_ders_total ~ Z_total_bad_le*genderid_refcisgirl + Z_age + (1|site), 
                                      data=yr4data)
summary(ders_les_gendercisgirl_reg)

### Mixed effect linear regression to determine whether gender moderates ####
### relationship between LES or DERS and CBCL or BPM, using age as fixed effect 
### covariate and site as random intercept
#### CBCL internalizing ~ LES*gender + age + (1|site) ####
# Relationship between LES and CBCL internalizing does not differ based on gender
cbcl_int_les_gendercisboy_reg <- lmer(Z_cbcl_int ~ Z_total_bad_le*genderid_refcisboy + Z_age + (1|site), 
                             data=yr4data)
summary(cbcl_int_les_gendercisboy_reg)
cbcl_int_les_gendercisgirl_reg <- lmer(Z_cbcl_int ~ Z_total_bad_le*genderid_refcisgirl + Z_age + (1|site), 
                                      data=yr4data)
summary(cbcl_int_les_gendercisgirl_reg)
#### CBCL externalizing ~ LES*gender + age + (1|site) ####
# Relationship between LES and CBCL externalizing does not differ based on gender
cbcl_ext_les_gendercisboy_reg <- lmer(Z_cbcl_ext ~ Z_total_bad_le*genderid_refcisboy + Z_age + (1|site), 
                                      data=yr4data)
summary(cbcl_ext_les_gendercisboy_reg)
cbcl_ext_les_gendercisgirl_reg <- lmer(Z_cbcl_ext ~ Z_total_bad_le*genderid_refcisgirl + Z_age + (1|site), 
                                       data=yr4data)
summary(cbcl_ext_les_gendercisgirl_reg)
#### BPM internalizing ~ LES*gender + age + (1|site) ####
# Relationship between LES and BPM internalizing differs significantly for cis
# girls vs cis boys and for gd vs cis boys but not for gd vs cis girls
bpm_int_les_gendercisboy_reg <- lmer(Z_bpm_int ~ Z_total_bad_le*genderid_refcisboy + Z_age + (1|site), 
                                      data=yr4data)
summary(bpm_int_les_gendercisboy_reg)
bpm_int_les_gendercisgirl_reg <- lmer(Z_bpm_int ~ Z_total_bad_le*genderid_refcisgirl + Z_age + (1|site), 
                                       data=yr4data)
summary(bpm_int_les_gendercisgirl_reg)
#### BPM externalizing ~ LES*gender + age + (1|site) ####
# Relationship between LES and BPM externalizing does not differ based on gender
bpm_ext_les_gendercisboy_reg <- lmer(Z_bpm_ext ~ Z_total_bad_le*genderid_refcisboy + Z_age + (1|site), 
                                      data=yr4data)
summary(bpm_ext_les_gendercisboy_reg)
bpm_ext_les_gendercisgirl_reg <- lmer(Z_bpm_ext ~ Z_total_bad_le*genderid_refcisgirl + Z_age + (1|site), 
                                       data=yr4data)
summary(bpm_ext_les_gendercisgirl_reg)
#### CBCL internalizing ~ DERS*gender + age + (1|site) ####
# Relationship between DERS and CBCL internalizing differs significantly for cis
# girls vs cis boy but not for gd vs cis boys or for gd vs cis girls
cbcl_int_ders_gendercisboy_reg <- lmer(Z_cbcl_int ~ Z_ders_total*genderid_refcisboy + Z_age + (1|site), 
                                      data=yr4data)
summary(cbcl_int_ders_gendercisboy_reg)
cbcl_int_ders_gendercisgirl_reg <- lmer(Z_cbcl_int ~ Z_ders_total*genderid_refcisgirl + Z_age + (1|site), 
                                       data=yr4data)
summary(cbcl_int_ders_gendercisgirl_reg)
#### CBCL externalizing ~ DERS*gender + age + (1|site) ####
# Relationship between DERS and CBCL externalizing does not differ based on gender
cbcl_ext_ders_gendercisboy_reg <- lmer(Z_cbcl_ext ~ Z_ders_total*genderid_refcisboy + Z_age + (1|site), 
                                      data=yr4data)
summary(cbcl_ext_ders_gendercisboy_reg)
cbcl_ext_ders_gendercisgirl_reg <- lmer(Z_cbcl_ext ~ Z_ders_total*genderid_refcisgirl + Z_age + (1|site), 
                                       data=yr4data)
summary(cbcl_ext_ders_gendercisgirl_reg)
#### BPM internalizing ~ DERS*gender + age + (1|site) ####
# Relationship between DERS and BPM internalizing does not differ based on gender
bpm_int_ders_gendercisboy_reg <- lmer(Z_bpm_int ~ Z_ders_total*genderid_refcisboy + Z_age + (1|site), 
                                     data=yr4data)
summary(bpm_int_ders_gendercisboy_reg)
bpm_int_ders_gendercisgirl_reg <- lmer(Z_bpm_int ~ Z_ders_total*genderid_refcisgirl + Z_age + (1|site), 
                                      data=yr4data)
summary(bpm_int_ders_gendercisgirl_reg)
#### BPM externalizing ~ DERS*gender + age + (1|site) ####
# Relationship between DERS and BPM externalizing does not differ based on gender
bpm_ext_ders_gendercisboy_reg <- lmer(Z_bpm_ext ~ Z_ders_total*genderid_refcisboy + Z_age + (1|site), 
                                     data=yr4data)
summary(bpm_ext_ders_gendercisboy_reg)
bpm_ext_ders_gendercisgirl_reg <- lmer(Z_bpm_ext ~ Z_ders_total*genderid_refcisgirl + Z_age + (1|site), 
                                      data=yr4data)
summary(bpm_ext_ders_gendercisgirl_reg)

### Mixed effect linear regression to determine whether sex moderates ####
### relationship between age and any variable, using site as random intercept
#### LES ~ age*sex + (1|site) ####
# Relationship between LES and age does not differ significantly based on sex
les_age_sex_reg <- lmer(Z_total_bad_le ~ Z_age*sex + (1|site), data=yr4sexdata)
summary(les_age_sex_reg)
#### DERS ~ age*sex + (1|site) ####
# Relationship between DERS and age does not differ significantly based on sex
ders_age_sex_reg <- lmer(Z_ders_total ~ Z_age*sex + (1|site), data=yr4sexdata)
summary(ders_age_sex_reg)
#### CBCL internalizing ~ age*sex + (1|site) ####
# Relationship between CBCL internalizing and age does not differ significantly 
# based on sex
cbcl_int_age_sex_reg <- lmer(Z_cbcl_int ~ Z_age*sex + (1|site), data=yr4sexdata)
summary(cbcl_int_age_sex_reg)
#### CBCL externalizing ~ age*sex + (1|site) ####
# Relationship between CBCL externalizing and age does not differ significantly 
# based on sex
cbcl_ext_age_sex_reg <- lmer(Z_cbcl_ext ~ Z_age*sex + (1|site), data=yr4sexdata)
summary(cbcl_ext_age_sex_reg)
#### BPM internalizing ~ age*sex + (1|site) ####
# Relationship between BPM internalizing and age does not differ significantly 
# based on sex
bpm_int_age_sex_reg <- lmer(Z_bpm_int ~ Z_age*sex + (1|site), data=yr4sexdata)
summary(bpm_int_age_sex_reg)
#### BPM externalizing ~ age*sex + (1|site) ####
# Relationship between BPM externalizing and age does not differ significantly 
# based on sex
bpm_ext_age_sex_reg <- lmer(Z_bpm_ext ~ Z_age*sex + (1|site), data=yr4sexdata)
summary(bpm_ext_age_sex_reg)

### Mixed effect linear regression to determine whether sex moderates ####
### relationship between DERS and LES, use age as fixed effect covariate and
### site as random intercept
#### DERS ~ LES*sex + age + (1|site) ####
# Relationship between DERS and LES does not differ significantly based on sex
ders_les_sex_reg <- lmer(Z_ders_total ~ Z_total_bad_le*sex + Z_age + (1|site), 
                                  data=yr4sexdata)
summary(ders_les_sex_reg)

### Mixed effect linear regression to determine whether sex moderates ####
### relationship between LES or DERS and CBCL or BPM, using age as fixed effect 
### covariate and site as random intercept
#### CBCL internalizing ~ LES*sex + age + (1|site) ####
# Relationship between LES and CBCL internalizing does differ significantly
# based on sex
cbcl_int_les_sex_reg <- lmer(Z_cbcl_int ~ Z_total_bad_le*sex + Z_age + (1|site), 
                                      data=yr4sexdata)
summary(cbcl_int_les_sex_reg)
#### CBCL externalizing ~ LES*sex + age + (1|site) ####
# Relationship between LES and CBCL externalizing does not differ based on sex
cbcl_ext_les_sex_reg <- lmer(Z_cbcl_ext ~ Z_total_bad_le*sex + Z_age + (1|site), 
                                      data=yr4sexdata)
summary(cbcl_ext_les_sex_reg)
#### BPM internalizing ~ LES*sex + age + (1|site) ####
# Relationship between LES and BPM internalizing does differ significantly 
# based on sex
bpm_int_les_sex_reg <- lmer(Z_bpm_int ~ Z_total_bad_le*sex + Z_age + (1|site), 
                                     data=yr4sexdata)
summary(bpm_int_les_sex_reg)
#### BPM externalizing ~ LES*sex + age + (1|site) ####
# Relationship between LES and BPM externalizing does not differ based on sex
bpm_ext_les_sex_reg <- lmer(Z_bpm_ext ~ Z_total_bad_le*sex + Z_age + (1|site), 
                                     data=yr4sexdata)
summary(bpm_ext_les_sex_reg)
#### CBCL internalizing ~ DERS*sex + age + (1|site) ####
# Relationship between DERS and CBCL internalizing does differ significantly
# based on sex
cbcl_int_ders_sex_reg <- lmer(Z_cbcl_int ~ Z_ders_total*sex + Z_age + (1|site), 
                                       data=yr4sexdata)
summary(cbcl_int_ders_sex_reg)
#### CBCL externalizing ~ DERS*sex + age + (1|site) ####
# Relationship between DERS and CBCL externalizing does not differ based on sex
cbcl_ext_ders_sex_reg <- lmer(Z_cbcl_ext ~ Z_ders_total*sex + Z_age + (1|site), 
                                       data=yr4sexdata)
summary(cbcl_ext_ders_sex_reg)
#### BPM internalizing ~ DERS*sex + age + (1|site) ####
# Relationship between DERS and BPM internalizing does differ significantly
# based on sex
bpm_int_ders_sex_reg <- lmer(Z_bpm_int ~ Z_ders_total*sex + Z_age + (1|site), 
                                      data=yr4sexdata)
summary(bpm_int_ders_sex_reg)
#### BPM externalizing ~ DERS*sex + age + (1|site) ####
# Relationship between DERS and BPM externalizing does not differ based on sex
bpm_ext_ders_sex_reg <- lmer(Z_bpm_ext ~ Z_ders_total*sex + Z_age + (1|site), 
                                      data=yr4sexdata)
summary(bpm_ext_ders_sex_reg)






## STEP TWO: MEDIATING EFFECT OF ER ON CBCL OR BPM ~ LES (HAYES MODEL 4) #### 

### Combine year 4 and year 3 data to have option to use year 3 as x variable ####
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

#### make version of data for mediation which excludes participants who answered
#### "refuse" or "dont_know" to sex question, set male as reference level

sex_meddata <- meddata %>% 
                  filter(sex!="refuse",
                         sex!="dont_know") %>%
                  mutate(sex = fct_relevel(sex, "male"))

### Simple mediation model (Hayes model 4) to test whether DERS mediates #### 
### relationship between LES and CBCL internalizing
# DERS partially mediates relationship between LES and CBCL internalizing when
# all variables at yr 4; and when LES at yr 3 but DERS and CBCL internalizing at
# yr 4; and when all variables at yr 4 but log transformed
cbcl_int_model4 <- PROCESS(
  meddata,
  y = "Z_yr4_cbcl_int",
  # y = "Z_yr3_cbcl_int",
  # y = "Z_log_yr4_cbcl_int",
  x = "Z_yr4_total_bad_le",
  # x = "Z_yr3_total_bad_le",
  # x = "Z_log_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # meds = c("Z_yr3_ders_total"),
  # meds = c("Z_log_yr4_ders_total"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_cbcl_int"
    # "Z_yr3_total_bad_le"
  ),
  hlm.re.m = "site",
  hlm.re.y = "site",
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)

### Simple mediation model (Hayes model 4) to test whether DERS mediates #### 
### relationship between LES and CBCL externalizing
# DERS partially mediates relationship between LES and CBCL externalizing when
# all variables at yr 4; and when LES at yr 3 but DERS and CBCL externalizing at
# yr 4; and when all variables at yr 4 but log transformed
cbcl_ext_model4 <- PROCESS(
  meddata,
  y = "Z_yr4_cbcl_ext",
  # y = "Z_yr3_cbcl_ext",
  # y = "Z_log_yr4_cbcl_ext",
  x = "Z_yr4_total_bad_le",
  # x = "Z_yr3_total_bad_le",
  # x = "Z_log_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # meds = c("Z_yr3_ders_total"),
  # meds = c("Z_log_yr4_ders_total"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_cbcl_ext"
    # "Z_yr3_total_bad_le"
  ),
  hlm.re.m = "site",
  hlm.re.y = "site",
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)

### Simple mediation model (Hayes model 4) to test whether DERS mediates #### 
### relationship between LES and BPM internalizing
# DERS partially mediates relationship between LES and BPM internalizing when
# all variables at yr 4; and when LES at yr 3 but DERS and BPM internalizing at
# yr 4; and when all variables at yr 4 but log transformed
bpm_int_model4 <- PROCESS(
  meddata,
  y = "Z_yr4_bpm_int",
  # y = "Z_yr3_bpm_int",
  # y = "Z_log_yr4_bpm_int",
  x = "Z_yr4_total_bad_le",
  # x = "Z_yr3_total_bad_le",
  # x = "Z_log_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # meds = c("Z_yr3_ders_total"),
  # meds = c("Z_log_yr4_ders_total"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_bpm_int"
    # "Z_yr3_total_bad_le"
  ),
  hlm.re.m = "site",
  hlm.re.y = "site",
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)

### Simple mediation model (Hayes model 4) to test whether DERS mediates #### 
### relationship between LES and BPM externalizing
# DERS partially mediates relationship between LES and BPM externalizing when
# all variables at yr 4; and when LES at yr 3 but DERS and BPM externalizing at
# yr 4; and when all variables at yr 4 but log transformed
bpm_ext_model4 <- PROCESS(
  meddata,
  y = "Z_yr4_bpm_ext",
  # y = "Z_yr3_bpm_ext",
  # y = "Z_log_yr4_bpm_ext",
  x = "Z_yr4_total_bad_le",
  # x = "Z_yr3_total_bad_le",
  # x = "Z_log_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # meds = c("Z_yr3_ders_total"),
  # meds = c("Z_log_yr4_ders_total"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_bpm_ext"
    # "Z_yr3_total_bad_le"
  ),
  hlm.re.m = "site",
  hlm.re.y = "site",
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)





## STEP THREE: MODERATING EFFECT OF GENDER OR SEX ON MEDIATION (HAYES MODEL 15) ####

### Moderated mediation model (Hayes model 15) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### internalizing 
cbcl_int_gender_model15 <- PROCESS(
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
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)

### Moderated mediation model (Hayes model 15) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### externalizing 
cbcl_ext_gender_model15 <- PROCESS(
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
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)

### Moderated mediation model (Hayes model 15) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and BPM
### internalizing 
bpm_int_gender_model15 <- PROCESS(
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
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)

### Moderated mediation model (Hayes model 15) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and BPM
### externalizing 
bpm_ext_gender_model15 <- PROCESS(
  meddata,
  y = "Z_yr4_bpm_ext",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # mods = c("genderid"),
  mods = c("genderid_refcisboy"),
  # mods = c("genderid_refcisgirl"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_bpm_ext"
    # "Z_yr3_total_bad_le"
  ),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)

### Moderated mediation model (Hayes model 15) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### internalizing 
cbcl_int_sex_model15 <- PROCESS(
  sex_meddata,
  y = "Z_yr4_cbcl_int",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  mods = c("sex"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_cbcl_int"
    # "Z_yr3_total_bad_le"
  ),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)

### Moderated mediation model (Hayes model 15) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### externalizing 
cbcl_ext_sex_model15 <- PROCESS(
  sex_meddata,
  y = "Z_yr4_cbcl_ext",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # mods = c("sexid"),
  mods = c("sex"),
  # mods = c("sex"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_cbcl_ext"
    # "Z_yr3_total_bad_le"
  ),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)

### Moderated mediation model (Hayes model 15) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and BPM
### internalizing 
bpm_int_sex_model15 <- PROCESS(
  sex_meddata,
  y = "Z_yr4_bpm_int",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # mods = c("sexid"),
  mods = c("sex"),
  # mods = c("sex"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_bpm_int"
    # "Z_yr3_total_bad_le"
  ),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)

### Moderated mediation model (Hayes model 15) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and BPM
### externalizing 
bpm_ext_sex_model15 <- PROCESS(
  sex_meddata,
  y = "Z_yr4_bpm_ext",
  x = "Z_yr4_total_bad_le",
  meds = c("Z_yr4_ders_total"),
  # mods = c("sexid"),
  mods = c("sex"),
  # mods = c("sex"),
  covs = c(
    # "Z_yr3_age"
    "Z_yr4_age"
    # "Z_yr3_ders_total"
    # "Z_yr3_bpm_ext"
    # "Z_yr3_total_bad_le"
  ),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    # "x-m",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  # ci = c("boot", "bc.boot", "bca.boot", "mcmc"),
  nsim = 1000,
  seed = 1234,
  center = TRUE,
  std = FALSE,
  digits = 3)
