### Set working directory ####
setwd("C:/Users/Kate Scheuer/OneDrive - UW/Desktop/Lab/aces_emotionreg_cbcl")

### Load libraries ####
library(tidyverse)
library(FSA)
library(lme4)
library(lmerTest)
library(rsq)
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
  # mutate(sex_female = if_else(sex=="female",1,-1)) %>%
  # dummy code sex
  mutate(sex_female = case_when(
                        sex=="female" ~ 1,
                        sex=="male" ~ 0
  )) %>%
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
  mutate(gender_cisgirl = if_else(genderid=="cis_girl",1,0)) %>%
  mutate(gender_gd = if_else(genderid=="gd",1,0)) %>%
  mutate(gender_cisboy = if_else(genderid=="cis_boy",1,0)) %>%
  
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
         gender_cisgirl,gender_gd,gender_cisboy,
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
  # Grand-mean center continuous variables
  mutate(across(
    c(age, ders_total, total_bad_le, 
      cbcl_total, cbcl_int, cbcl_ext,
      bpm_total, bpm_int, bpm_ext,
      log_ders_total, log_total_bad_le, 
      log_cbcl_total, log_cbcl_int, log_cbcl_ext,
      log_bpm_total, log_bpm_int, log_bpm_ext),
    # ~ as.numeric(scale(.)),
    ~ as.numeric(misty::center(.,type = c("CGM"))),
    .names = "C_{.col}"
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


### Combine year 4 and year 3 data to have option to use year 3 as x variable ####
meddata <- yr4data %>%
  rename(
    C_yr4_total_bad_le = C_total_bad_le,
    C_yr4_ders_total = C_ders_total,
    C_yr4_cbcl_int = C_cbcl_int,
    C_yr4_cbcl_ext = C_cbcl_ext,
    C_yr4_bpm_int = C_bpm_int,
    C_yr4_bpm_ext = C_bpm_ext,
    C_log_yr4_total_bad_le = C_log_total_bad_le,
    C_log_yr4_ders_total = C_log_ders_total,
    C_log_yr4_cbcl_int = C_log_cbcl_int,
    C_log_yr4_cbcl_ext = C_log_cbcl_ext,
    C_log_yr4_bpm_int = C_log_bpm_int,
    C_log_yr4_bpm_ext = C_log_bpm_ext,
    C_yr4_age = C_age) %>%
  left_join(select(yr3data, 
                   c(src_subject_id,C_age,
                     C_total_bad_le,C_ders_total,
                     C_cbcl_int,C_cbcl_ext,
                     C_bpm_int,C_bpm_ext)),
            by="src_subject_id") %>%
  rename(C_yr3_total_bad_le = C_total_bad_le,
         C_yr3_ders_total = C_ders_total,
         C_yr3_cbcl_int = C_cbcl_int,
         C_yr3_cbcl_ext = C_cbcl_ext,
         C_yr3_bpm_int = C_bpm_int,
         C_yr3_bpm_ext = C_bpm_ext,
         C_yr3_age = C_age)

#### make version of data which excludes participants who answered
#### "refuse" or "dont_know" to sex question, set male as reference level

sex_meddata <- meddata %>% 
  filter(sex!="refuse",
         sex!="dont_know") %>%
  mutate(sex = fct_relevel(sex, "male"))

#### make version of data which excludes participants who are GD

nogd_meddata <- meddata %>%
  filter(genderid != "gd")

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

##### Bargraph of LES vs DERS by gender ####
ggplot(yr4data, 
       aes(x=total_bad_le,y=ders_total, fill=genderid)) +
  # aes(x=total_bad_le,y=cbcl_int)) +
  geom_point(aes(color=genderid, shape = genderid)) +
  # geom_bar(stat="summary",fun="mean",
  #          position=position_dodge2(preserve = "single"),
  #          color="black",width=.7,alpha=.9) +
  geom_smooth(aes(linetype=genderid),method="lm",color="black", se=FALSE) +
  scale_linetype_manual(values = c("cis_boy"="31",
                                   "cis_girl"="11",
                                   "gd"="solid")) +
  scale_shape_manual(values=c(21,22,23)) +
  # scale_fill_grey(start=0.9,end=0) +
  scale_colour_grey(start=0.9,end=0) +
  scale_fill_manual(values=c("grey40","grey85","black")) +
  # scale_color_manual(values=c("grey40","grey85","black")) +
  # # scale_fill_grey(start=0.9,end=0) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 1),
                     limits=c(-.5,15.5)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(25,150,by=25),
                     limits=c(25,150)) +
                     # breaks=seq(0,120,by=20),
                     # limits=c(0,120)) +
  guides(
    line = guide_legend(override.aes = list(size = 2)),
    fill = guide_legend(override.aes = list(size = 2)) 
  ) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_ders_bygender.tiff",width=14,height=6,unit="in",path="figures")
# ggsave("les_vs_ders_bar_bygender.tiff",width=9,height=6,unit="in",path="figures")
# ggsave("les_vs_ders_scatter_bygender.tiff",width=9,height=6,unit="in",path="figures")

##### Bargraph of LES vs CBCL internalizing by gender ####
ggplot(yr4data, 
       aes(x=total_bad_le,y=cbcl_int, fill=genderid)) +
       # aes(x=total_bad_le,y=cbcl_int)) +
  # geom_point(aes(color=genderid, shape = genderid)) +
  geom_bar(stat="summary",fun="mean",
           position=position_dodge2(preserve = "single"),
           color="black",width=.7,alpha=.9) +
  geom_smooth(aes(linetype=genderid),method="lm",color="black", se=FALSE) +
  scale_linetype_manual(values = c("cis_boy"="31",
                                   "cis_girl"="11",
                                   "gd"="solid")) +
  scale_shape_manual(values=c(21,22,23)) +
  # scale_fill_grey(start=0.9,end=0) +
  scale_colour_grey(start=0.9,end=0) +
  scale_fill_manual(values=c("grey40","grey85","black")) +
  # scale_color_manual(values=c("grey40","grey85","black")) +
  # # scale_fill_grey(start=0.9,end=0) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 1),
                     limits=c(-.5,15.5)) +
  scale_y_continuous(expand = c(0,0),
                     # breaks=seq(30,90,by=10),
                     # limits=c(30,90)) +
                     breaks=seq(0,90,by=10),
                     limits=c(0,90)) +
  guides(
    line = guide_legend(override.aes = list(size = 2)),
    fill = guide_legend(override.aes = list(size = 2)) 
  ) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_cbclint_bygender.tiff",width=14,height=6,unit="in",path="figures")
# ggsave("les_vs_cbclint_bar_bygender.tiff",width=9,height=6,unit="in",path="figures")
# ggsave("les_vs_cbclint_scatter_bygender.tiff",width=9,height=6,unit="in",path="figures")

##### Bargraph of LES vs CBCL externalizing by gender ####
ggplot(yr4data, 
       aes(x=total_bad_le,y=cbcl_ext, fill=genderid)) +
  # aes(x=total_bad_le,y=cbcl_int)) +
  # geom_point(aes(color=genderid, shape = genderid)) +
  geom_bar(stat="summary",fun="mean",
           position=position_dodge2(preserve = "single"),
           color="black",width=.9) +
  geom_smooth(aes(linetype=genderid),method="lm",color="black", se=FALSE) +
  scale_linetype_manual(values = c("cis_boy"="31",
                                   "cis_girl"="11",
                                   "gd"="solid")) +
  scale_shape_manual(values=c(21,22,23)) +
  # scale_fill_grey(start=0.9,end=0) +
  scale_colour_grey(start=0.9,end=0) +
  scale_fill_manual(values=c("grey40","grey85","black")) +
  # scale_color_manual(values=c("grey40","grey85","black")) +
  # # scale_fill_grey(start=0.9,end=0) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 1),
                     limits=c(-.5,15.5)) +
  scale_y_continuous(expand = c(0,0),
                     # breaks=seq(30,90,by=10),
                     # limits=c(30,90)) +
                        breaks=seq(0,90,by=10),
                        limits=c(0,90)) +
  guides(
    shape = guide_legend(override.aes = list(size = 2)),
    fill = guide_legend(override.aes = list(size = 2)) 
  ) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_cbclext_bygender.tiff",width=14,height=6,unit="in",path="figures")
# ggsave("les_vs_cbclext_bar_bygender.tiff",width=9,height=6,unit="in",path="figures")
# ggsave("les_vs_cbclext_scatter_bygender.tiff",width=9,height=6,unit="in",path="figures")

##### Bargraph of LES vs BPM internalizing by gender ####
ggplot(yr4data, 
       aes(x=total_bad_le,y=bpm_int, fill=genderid)) +
  # aes(x=total_bad_le,y=bpm_int)) +
  # geom_point(aes(color=genderid, shape = genderid)) +
  geom_bar(stat="summary",fun="mean",
           position=position_dodge2(preserve = "single"),
           color="black",width=.7,alpha=.9) +
  geom_smooth(aes(linetype=genderid),method="lm",color="black", se=FALSE) +
  scale_linetype_manual(values = c("cis_boy"="31",
                                   "cis_girl"="11",
                                   "gd"="solid")) +
  scale_shape_manual(values=c(21,22,23)) +
  # scale_fill_grey(start=0.9,end=0) +
  scale_colour_grey(start=0.9,end=0) +
  scale_fill_manual(values=c("grey40","grey85","black")) +
  # scale_color_manual(values=c("grey40","grey85","black")) +
  # # scale_fill_grey(start=0.9,end=0) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 1),
                     limits=c(-.5,15.5)) +
  scale_y_continuous(expand = c(0,0),
                     # breaks=seq(40,80,by=5),
                     # limits=c(45,80)) +
                     breaks=seq(0,80,by=10),
                     limits=c(0,80)) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_bpmint_bygender.tiff",width=14,height=6,unit="in",path="figures")
# ggsave("les_vs_bpmint_bar_bygender.tiff",width=9,height=6,unit="in",path="figures")
# ggsave("les_vs_bpmint_scatter_bygender.tiff",width=9,height=6,unit="in",path="figures")

##### Bargraph of LES vs BPM externalizing by gender ####
ggplot(yr4data, 
       aes(x=total_bad_le,y=bpm_ext, fill=genderid)) +
  # aes(x=total_bad_le,y=bpm_int)) +
  # geom_point(aes(color=genderid, shape = genderid)) +
  geom_bar(stat="summary",fun="mean",
           position=position_dodge2(preserve = "single"),
           color="black",width=.7,alpha=.9) +
  geom_smooth(aes(linetype=genderid),method="lm",color="black", se=FALSE) +
  scale_linetype_manual(values = c("cis_boy"="31",
                                   "cis_girl"="11",
                                   "gd"="solid")) +
  scale_shape_manual(values=c(21,22,23)) +
  # scale_fill_grey(start=0.9,end=0) +
  scale_colour_grey(start=0.9,end=0) +
  scale_fill_manual(values=c("grey40","grey85","black")) +
  # scale_color_manual(values=c("grey40","grey85","black")) +
  # # scale_fill_grey(start=0.9,end=0) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 1),
                     limits=c(-.5,15.5)) +
  scale_y_continuous(expand = c(0,0),
                     # breaks=seq(40,80,by=5),
                     # limits=c(45,80)) +
                        breaks=seq(0,80,by=10),
                        limits=c(0,80)) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_bpmext_bygender.tiff",width=14,height=6,unit="in",path="figures")
# ggsave("les_vs_bpmext_bar_bygender.tiff",width=9,height=6,unit="in",path="figures")
# ggsave("les_vs_bpmext_scatter_bygender.tiff",width=9,height=6,unit="in",path="figures")

##### Scatterplots of DERS vs CBCL by gender ####
outcome_list <- c("cbcl_int","cbcl_ext")
cbcl_ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  cbcl_ders_plot <-
    ggplot(aes(x=ders_total,y=.data[[outcome]],
                               # color=genderid,
                               linetype=genderid,
                               shape = genderid,
                               fill = genderid
                               ),data=yr4data) +
    geom_point(alpha=.6) +
    geom_smooth(method="lm",
                se=FALSE,
                # linewidth=1.75,
                color="black") +
    # geom_smooth(aes(color=genderid),method="lm",
    #             se=FALSE,
    #             linewidth=1) +    
    # geom_hline(yintercept=70,linetype='dashed') +
    scale_linetype_manual(values = c("cis_boy"="31",
                                     "cis_girl"="11",
                                     "gd"="solid")) +
    scale_shape_manual(values=c(21,22,23)) +
    scale_fill_manual(values=c("grey40","grey85","black")) +
    # scale_fill_grey(start=0.9,end=0) +
    scale_colour_grey(start=0.9,end=0) +
    scale_x_continuous(expand = c(0,0),
                       breaks=seq(25,150,25),
                       limits = c(25,155)) +
    scale_y_continuous(expand = c(0,0),
                       breaks=seq(30,90,10),
                       limits = c(30,90)) +
    theme_classic() +
    guides(
      shape = guide_legend(override.aes = list(size = 2)),
      fill = guide_legend(override.aes = list(size = 2)) 
    )
  cbcl_ders_plot_list[[outcome]] <- cbcl_ders_plot
  # Save plot
  # ggsave(paste0("ders_vs_",outcome,"_bygender.tiff"),
  #        width=7,height=5,units = "in",path="figures")
}

##### Scatterplots of DERS vs BPM by gender ####
outcome_list <- c("bpm_int","bpm_ext")
bpm_ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  bpm_ders_plot <- ggplot(aes(x=ders_total,y=.data[[outcome]],
                              # color=genderid,
                              linetype=genderid,
                              shape = genderid,
                              fill = genderid
  ),data=yr4data) +
    geom_point(alpha=.6) +
    geom_smooth(method="lm",
                se=FALSE,
                # linewidth=1.75,
                color="black") +
    # geom_smooth(aes(color=genderid),method="lm",
    #             se=FALSE,
    #             linewidth=1) +    
    # geom_hline(yintercept=70,linetype='dashed') +
    scale_linetype_manual(values = c("cis_boy"="31",
                                     "cis_girl"="11",
                                     "gd"="solid")) +
    scale_shape_manual(values=c(21,22,23)) +
    scale_fill_manual(values=c("grey40","grey85","black")) +
    # scale_fill_grey(start=0.9,end=0) +
    scale_colour_grey(start=0.9,end=0) +
    # scale_x_continuous(expand = c(0,0),
    #                    breaks=seq(25,150,25),
    #                    limits = c(25,155)) +
    # scale_y_continuous(expand = c(0,0),
    #                    breaks=seq(20,90,10),
    #                    limits = c(20,90)) +
    scale_x_continuous(expand = c(0,0),
                       breaks=seq(25,150,25),
                       limits = c(25,155)) +
    scale_y_continuous(expand = c(0,0),
                       breaks=seq(40,80,10),
                       limits = c(40,80)) +
  theme_classic() +
    guides(
      shape = guide_legend(override.aes = list(size = 2)),
      fill = guide_legend(override.aes = list(size = 2)) 
    )
  bpm_ders_plot_list[[outcome]] <- bpm_ders_plot
  # Save plot
  # ggsave(paste0("ders_vs_",outcome,"_bygender.tiff"),
  #        width=7,height=5,units = "in",path="figures")
}


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

### Mixed effect linear regression to determine whether DERS differs based ####
### on LES, using age as fixed effect covariate and site as random intercept
#### DERS ~ LES + age + (1|site) ####
# DERS scores differ significantly based on LES (p = 1.71e-11)
ders_les_age_reg <- lmer(C_yr4_ders_total ~ C_yr4_total_bad_le + C_yr4_age + (1|site),
# ders_les_age_reg <- lmer(C_yr4_ders_total ~ C_yr3_total_bad_le + C_yr4_age + (1|site),
# ders_les_age_reg <- lmer(C_log_yr4_ders_total ~ C_log_yr4_total_bad_le + C_yr4_age + (1|site),
                         data=meddata)
summary(ders_les_age_reg)
rsq(ders_les_age_reg,adj=TRUE)

### Mixed effect linear regression to determine whether CBCL or BPM differ ####
### based on LES and/or DERS, using age as fixed effect covariate and site as random 
### intercept
#### CBCL internalizing ~ LES + age + (1|site) ####
cbcl_int_les_age_reg <- lmer(C_yr4_cbcl_int ~ C_yr4_total_bad_le + C_yr4_age + (1|site),
# cbcl_int_les_age_reg <- lmer(C_yr4_cbcl_int ~ C_yr3_total_bad_le + C_yr4_age + (1|site),
# cbcl_int_les_age_reg <- lmer(C_log_yr4_cbcl_int ~ C_log_yr4_total_bad_le + C_yr4_age + (1|site),
                           data=meddata)
summary(cbcl_int_les_age_reg)
#### CBCL externalizing ~ LES + age + (1|site) ####
cbcl_ext_les_age_reg <- lmer(C_yr4_cbcl_ext ~ C_yr4_total_bad_le + C_yr4_age + (1|site),
# cbcl_ext_les_age_reg <- lmer(C_yr4_cbcl_ext ~ C_yr3_total_bad_le + C_yr4_age + (1|site),
# cbcl_ext_les_age_reg <- lmer(C_log_yr4_cbcl_ext ~ C_log_yr4_total_bad_le + C_yr4_age + (1|site),
                           data=meddata)
summary(cbcl_ext_les_age_reg)
#### BPM internalizing ~ LES + age + (1|site) ####
bpm_int_les_age_reg <- lmer(C_yr4_bpm_int ~ C_yr4_total_bad_le + C_yr4_age + (1|site),
 # bpm_int_les_age_reg <- lmer(C_yr4_bpm_int ~ C_yr3_total_bad_le + C_yr4_age + (1|site),
 # bpm_int_les_age_reg <- lmer(C_log_yr4_bpm_int ~ C_log_yr4_total_bad_le + C_yr4_age + (1|site),
                             data=meddata)
summary(bpm_int_les_age_reg)
#### BPM externalizing ~ LES + age + (1|site) ####
bpm_ext_les_age_reg <- lmer(C_yr4_bpm_ext ~ C_yr4_total_bad_le + C_yr4_age + (1|site),
 # bpm_ext_les_age_reg <- lmer(C_yr4_bpm_ext ~ C_yr3_total_bad_le + C_yr4_age + (1|site),
 # bpm_ext_les_age_reg <- lmer(C_log_yr4_bpm_ext ~ C_log_yr4_total_bad_le + C_yr4_age + (1|site),
                             data=meddata)
summary(bpm_ext_les_age_reg)
#### CBCL internalizing ~ DERS + age + (1|site) ####
cbcl_int_ders_age_reg <- lmer(C_yr4_cbcl_int ~ C_yr4_ders_total + C_yr4_age + (1|site),
 # cbcl_int_ders_age_reg <- lmer(C_yr4_cbcl_int ~ C_yr3_ders_total + C_yr4_age + (1|site),
 # cbcl_int_ders_age_reg <- lmer(C_log_yr4_cbcl_int ~ C_log_yr4_ders_total + C_yr4_age + (1|site),
                           data=meddata)
summary(cbcl_int_ders_age_reg)
#### CBCL externalizing ~ DERS + age + (1|site) ####
cbcl_ext_ders_age_reg <- lmer(C_yr4_cbcl_ext ~ C_yr4_ders_total + C_yr4_age + (1|site),
 # cbcl_ext_ders_age_reg <- lmer(C_yr4_cbcl_ext ~ C_yr3_ders_total + C_yr4_age + (1|site),
 # cbcl_ext_ders_age_reg <- lmer(C_log_yr4_cbcl_ext ~ C_log_yr4_ders_total + C_yr4_age + (1|site),
                             data=meddata)
summary(cbcl_ext_ders_age_reg)
#### BPM internalizing ~ DERS + age + (1|site) ####
bpm_int_ders_age_reg <- lmer(C_yr4_bpm_int ~ C_yr4_ders_total + C_yr4_age + (1|site),
  # bpm_int_ders_age_reg <- lmer(C_yr4_bpm_int ~ C_yr3_ders_total + C_yr4_age + (1|site),
  # bpm_int_ders_age_reg <- lmer(C_log_yr4_bpm_int ~ C_log_yr4_ders_total + C_yr4_age + (1|site),
                            data=meddata)
summary(bpm_int_ders_age_reg)
#### BPM externalizing ~ DERS + age + (1|site) ####
bpm_ext_ders_age_reg <- lmer(C_yr4_bpm_ext ~ C_yr4_ders_total + C_yr4_age + (1|site),
  # bpm_ext_ders_age_reg <- lmer(C_yr4_bpm_ext ~ C_yr3_ders_total + C_yr4_age + (1|site),
  # bpm_ext_ders_age_reg <- lmer(C_log_yr4_bpm_ext ~ C_log_yr4_ders_total + C_yr4_age + (1|site),
                            data=meddata)
summary(bpm_ext_ders_age_reg)
#### CBCL internalizing ~ LES + DERS + age + (1|site) ####
cbcl_int_les_ders_age_reg <- lmer(C_yr4_cbcl_int ~ C_yr4_total_bad_le + C_yr4_ders_total + C_yr4_age + (1|site),
# cbcl_int_les_ders_age_reg <- lmer(C_yr4_cbcl_int ~ C_yr3_total_bad_le + C_yr3_ders_total + C_yr4_age + (1|site),
# cbcl_int_les_ders_age_reg <- lmer(C_log_yr4_cbcl_int ~ C_log_yr4_total_bad_le + C_log_yr4_ders_total + C_yr4_age + (1|site),
                              data=meddata)
summary(cbcl_int_les_ders_age_reg)
BIC(cbcl_int_les_ders_age_reg)
rsq(cbcl_int_les_ders_age_reg, adj=TRUE)
#### CBCL externalizing ~ LES + DERS + age + (1|site) ####
cbcl_ext_les_ders_age_reg <- lmer(C_yr4_cbcl_ext ~ C_yr4_total_bad_le + C_yr4_ders_total + C_yr4_age + (1|site),
# cbcl_ext_les_ders_age_reg <- lmer(C_yr4_cbcl_ext ~ C_yr3_total_bad_le + C_yr3_ders_total + C_yr4_age + (1|site),
# cbcl_ext_les_ders_age_reg <- lmer(C_log_yr4_cbcl_ext ~ C_log_yr4_total_bad_le + C_log_yr4_ders_total + C_yr4_age + (1|site),
                              data=meddata)
summary(cbcl_ext_les_ders_age_reg)
rsq(cbcl_ext_les_ders_age_reg, adj=TRUE)
#### BPM internalizing ~ LES + DERS + age + (1|site) ####
bpm_int_les_ders_age_reg <- lmer(C_yr4_bpm_int ~ C_yr4_total_bad_le + C_yr4_ders_total + C_yr4_age + (1|site),
# bpm_int_les_ders_age_reg <- lmer(C_yr4_bpm_int ~ C_yr3_total_bad_le + C_yr3_ders_total + C_yr4_age + (1|site),
# bpm_int_les_ders_age_reg <- lmer(C_log_yr4_bpm_int ~ C_log_yr4_total_bad_le + C_log_yr4_ders_total + C_yr4_age + (1|site),
                              data=meddata)
summary(bpm_int_les_ders_age_reg)
rsq(bpm_int_les_ders_age_reg, adj=TRUE)
#### BPM externalizing ~ LES + DERS + age + (1|site) ####
bpm_ext_les_ders_age_reg <- lmer(C_yr4_bpm_ext ~ C_yr4_total_bad_le + C_yr4_ders_total + C_yr4_age + (1|site),
# bpm_ext_les_ders_age_reg <- lmer(C_yr4_bpm_ext ~ C_yr3_total_bad_le + C_yr3_ders_total + C_yr4_age + (1|site),
# bpm_ext_les_ders_age_reg <- lmer(C_log_yr4_bpm_ext ~ C_log_yr4_total_bad_le + C_log_yr4_ders_total + C_yr4_age + (1|site),
                              data=meddata)
summary(bpm_ext_les_ders_age_reg)
rsq(bpm_ext_les_ders_age_reg, adj=TRUE)



## STEP TWO: MODERATING EFFECTS OF GENDER OR SEX ####
### Mixed effect linear regression to determine whether gender moderates ####
### relationship between DERS and LES, use age as fixed effect covariate and
### site as random intercept
#### DERS ~ LES*gender + age + (1|site) ####
ders_les_gendercisboy_reg <- lmer(C_yr4_ders_total ~ C_yr4_total_bad_le*genderid_refcisboy + C_yr4_age + (1|site),
# ders_les_gendercisboy_reg <- lmer(C_yr4_ders_total ~ C_yr3_total_bad_le*genderid_refcisboy + C_yr4_age + (1|site),
# ders_les_gendercisboy_reg <- lmer(C_log_yr4_ders_total ~ C_log_yr4_total_bad_le*genderid_refcisboy + C_yr4_age + (1|site),
                         data=meddata)
summary(ders_les_gendercisboy_reg)
anova(ders_les_gendercisboy_reg)
rsq(ders_les_gendercisboy_reg,adj=TRUE)
### Mixed effect linear regression to determine whether sex moderates ####
### relationship between DERS and LES, use age as fixed effect covariate and
### site as random intercept
#### DERS ~ LES*sex + age + (1|site) ####
# Relationship between DERS and LES does not differ significantly based on sex
ders_les_sex_reg <- lmer(C_yr4_ders_total ~ C_yr4_total_bad_le*sex + C_yr4_age + (1|site),
# ders_les_sex_reg <- lmer(C_yr4_ders_total ~ C_yr3_total_bad_le*sex + C_yr4_age + (1|site),
# ders_les_sex_reg <- lmer(C_log_yr4_ders_total ~ C_log_yr4_total_bad_le*sex + C_yr4_age + (1|site),
                                  data=sex_meddata)
summary(ders_les_sex_reg)
rsq(ders_les_sex_reg, adj=TRUE)
### Mixed effect linear regression to determine whether gender moderates ####
### relationship between LES, DERS, and CBCL or BPM using age as fixed effect
### covariate and site as random intercept
#### CBCL internalizing ~ LES*gender + DERS*gender + age + (1|site) ####
cbcl_int_les_gendercisboy_reg <- 
  lmer(C_yr4_cbcl_int ~ C_yr4_total_bad_le*genderid_refcisboy + C_yr4_ders_total*genderid_refcisboy +
  # lmer(C_yr4_cbcl_int ~ C_yr3_total_bad_le*genderid_refcisboy + C_yr3_ders_total*genderid_refcisboy +
  # lmer(C_log_yr4_cbcl_int ~ C_log_yr4_total_bad_le*genderid_refcisboy + C_log_yr4_ders_total*genderid_refcisboy +
       C_yr4_age + (1|site),  
       data=meddata, REML=FALSE)
summary(cbcl_int_les_gendercisboy_reg)
anova(cbcl_int_les_gendercisboy_reg)
rsq(cbcl_int_les_gendercisboy_reg,adj=TRUE)
#### CBCL externalizing ~ LES*gender + DERS*gender + age + (1|site) ####
cbcl_ext_les_gendercisboy_reg <- 
  lmer(C_yr4_cbcl_ext ~ C_yr4_total_bad_le*genderid_refcisboy + C_yr4_ders_total*genderid_refcisboy +
   # lmer(C_yr4_cbcl_ext ~ C_yr3_total_bad_le*genderid_refcisboy + C_yr3_ders_total*genderid_refcisboy +
   # lmer(C_log_yr4_cbcl_ext ~ C_log_yr4_total_bad_le*genderid_refcisboy + C_log_yr4_ders_total*genderid_refcisboy +
         C_yr4_age + (1|site),  
       data=meddata, REML=FALSE)
summary(cbcl_ext_les_gendercisboy_reg)
anova(cbcl_ext_les_gendercisboy_reg)
rsq(bpm_ext_les_gendercisboy_reg,adj=TRUE)
#### BPM internalizing ~ LES*gender + DERS*gender + age + (1|site) ####
bpm_int_les_gendercisboy_reg <- 
  lmer(C_yr4_bpm_int ~ C_yr4_total_bad_le*genderid_refcisboy + C_yr4_ders_total*genderid_refcisboy +
   # lmer(C_yr4_bpm_int ~ C_yr3_total_bad_le*genderid_refcisboy + C_yr3_ders_total*genderid_refcisboy +
   # lmer(C_log_yr4_bpm_int ~ C_log_yr4_total_bad_le*genderid_refcisboy + C_log_yr4_ders_total*genderid_refcisboy +
         C_yr4_age + (1|site),  
       data=meddata, REML=FALSE)
summary(bpm_int_les_gendercisboy_reg)
anova(bpm_int_les_gendercisboy_reg)
rsq(bpm_int_les_gendercisboy_reg,adj=TRUE)
#### BPM externalizing ~ LES*gender + DERS*gender + age + (1|site) ####
bpm_ext_les_gendercisboy_reg <- 
  lmer(C_yr4_bpm_ext ~ C_yr4_total_bad_le*genderid_refcisboy + C_yr4_ders_total*genderid_refcisboy +
   # lmer(C_yr4_bpm_ext ~ C_yr3_total_bad_le*genderid_refcisboy + C_yr3_ders_total*genderid_refcisboy +
   # lmer(C_log_yr4_bpm_ext ~ C_log_yr4_total_bad_le*genderid_refcisboy + C_log_yr4_ders_total*genderid_refcisboy +
         C_yr4_age + (1|site),  
       data=meddata, REML=FALSE)
summary(bpm_ext_les_gendercisboy_reg)
anova(bpm_ext_les_gendercisboy_reg)
rsq(bpm_ext_les_gendercisboy_reg,adj=TRUE)

### Mixed effect linear regression to determine whether sex moderates ####
### relationship between LES, DERS, and CBCL or BPM using age as fixed effect
### covariate and site as random intercept
#### CBCL internalizing ~ LES*sex + DERS*sex + age + (1|site) ####
cbcl_int_les_sex_reg <- 
  lmer(C_yr4_cbcl_int ~ C_yr4_total_bad_le*sex + C_yr4_ders_total*sex + C_yr4_age + (1|site),
  # lmer(C_yr4_cbcl_int ~ C_yr3_total_bad_le*sex + C_yr3_ders_total*sex + C_yr4_age + (1|site),
  # lmer(C_log_yr4_cbcl_int ~ C_log_yr4_total_bad_le*sex + C_log_yr4_ders_total*sex + C_yr4_age + (1|site),
       data=sex_meddata, REML=FALSE)
summary(cbcl_int_les_sex_reg)
anova(cbcl_int_les_sex_reg)
rsq(cbcl_int_les_sex_reg,adj=TRUE)
#### CBCL externalizing ~ LES*sex + DERS*sex + age + (1|site) ####
cbcl_ext_les_sex_reg <-
  lmer(C_yr4_cbcl_ext ~ C_yr4_total_bad_le*sex + C_yr4_ders_total*sex + C_yr4_age + (1|site),
   # lmer(C_yr4_cbcl_ext ~ C_yr3_total_bad_le*sex + C_yr3_ders_total*sex + C_yr4_age + (1|site),
   # lmer(C_log_yr4_cbcl_ext ~ C_log_yr4_total_bad_le*sex + C_log_yr4_ders_total*sex + C_yr4_age + (1|site),
       data=sex_meddata, REML=FALSE)
summary(cbcl_ext_les_sex_reg)
anova(cbcl_ext_les_sex_reg)
rsq(bpm_ext_les_sex_reg,adj=TRUE)
#### BPM internalizing ~ LES*sex + DERS*sex + age + (1|site) ####
bpm_int_les_sex_reg <- 
  lmer(C_yr4_bpm_int ~ C_yr4_total_bad_le*sex + C_yr4_ders_total*sex + C_yr4_age + (1|site),
  # lmer(C_yr4_bpm_int ~ C_yr3_total_bad_le*sex + C_yr3_ders_total*sex + C_yr4_age + (1|site),
  # lmer(C_log_yr4_bpm_int ~ C_log_yr4_total_bad_le*sex + C_log_yr4_ders_total*sex + C_yr4_age + (1|site),
       data=sex_meddata, REML=FALSE)
summary(bpm_int_les_sex_reg)
anova(bpm_int_les_sex_reg)
rsq(bpm_int_les_sex_reg,adj=TRUE)
#### BPM externalizing ~ LES*sex + DERS*sex + age + (1|site) ####
bpm_ext_les_sex_reg <-
  lmer(C_yr4_bpm_ext ~ C_yr4_total_bad_le*sex + C_yr4_ders_total*sex + C_yr4_age + (1|site),
   # lmer(C_yr4_bpm_ext ~ C_yr3_total_bad_le*sex + C_yr3_ders_total*sex + C_yr4_age + (1|site),
   # lmer(C_log_yr4_bpm_ext ~ C_log_yr4_total_bad_le*sex + C_log_yr4_ders_total*sex + C_yr4_age + (1|site),
       data=sex_meddata, REML=FALSE)
summary(bpm_ext_les_sex_reg)
anova(bpm_ext_les_sex_reg)
rsq(bpm_ext_les_sex_reg,adj=TRUE)

## STEP THREE: MEDIATING EFFECT OF ER ON CBCL OR BPM ~ LES #### 


### Simple mediation model for CBCL internalizing ####
cbclint_model <- 
  ' # direct effect
        C_yr4_cbcl_int~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_cbcl_int~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)'
cbclint_model <- sem(cbclint_model, 
                             data = meddata, 
                             meanstructure = TRUE,
                             se = "robust.cluster",
                             # group = "genderid",
                             cluster = "site")
summary(cbclint_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclint_model, boot.ci.type = "bca.simple")

### Simple mediation model for CBCL externalizing ####
cbclext_model <- 
  ' # direct effect
        C_yr4_cbcl_ext~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_cbcl_ext~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)'
cbclext_model <- sem(cbclext_model, 
                     data = meddata, 
                     meanstructure = TRUE,
                     se = "robust.cluster",
                     # group = "genderid",
                     cluster = "site")
summary(cbclext_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclext_model, boot.ci.type = "bca.simple")

### Simple mediation model for bpm internalizing ####
bpmint_model <- 
  ' # direct effect
        C_yr4_bpm_int~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_int~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)'
bpmint_model <- sem(bpmint_model, 
                     data = meddata, 
                     meanstructure = TRUE,
                     se = "robust.cluster",
                     # group = "genderid",
                     cluster = "site")
summary(bpmint_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmint_model, boot.ci.type = "bca.simple")

### Simple mediation model for bpm externalizing ####
bpmext_model <- 
  ' # direct effect
        C_yr4_bpm_ext~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_ext~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)'
bpmext_model <- sem(bpmext_model, 
                     data = meddata, 
                     meanstructure = TRUE,
                     se = "robust.cluster",
                     # group = "genderid",
                     cluster = "site")
summary(bpmext_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmext_model, boot.ci.type = "bca.simple")

## STEP FOUR: MODERATING EFFECT OF GENDER OR SEX ON MEDIATION ####
### Moderated mediation model for CBCL internalizing based on gender ####
#### Constrained gender model by group
cbclint_constrained_gender_model <- 
  ' # direct effect
        C_yr4_cbcl_int~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_cbcl_int~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
# ' # direct effect
#         C_yr4_cbcl_int~ c*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_cbcl_int~ b*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
# ' # direct effect
#         C_log_yr4_cbcl_int~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_cbcl_int~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
cbclint_constrained_gender_model <- sem(cbclint_constrained_gender_model, 
                     data = meddata, 
                     meanstructure = TRUE,
                     se = "robust.cluster",
                     group = "genderid",
                     cluster = "site")
summary(cbclint_constrained_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclint_constrained_gender_model, boot.ci.type = "bca.simple")

#### Unconstrained gender model by group
cbclint_unconstrained_gender_model <- 
  ' # direct effect
        C_yr4_cbcl_int~ c(c1,c2,c3)*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ c(a1,a2,a3)*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_cbcl_int~ c(b1,b2,b3)*C_yr4_ders_total
      # indirect effect (a*b)
        ab1 := a1*b1
        ab2 := a2*b2
        ab3 := a3*b3
      # total effect
        total1 := c1 + (a1*b1)
        total2 := c2 + (a2*b2)
        total3 := c3 + (a3*b3)
'
# ' # direct effect
#         C_yr4_cbcl_int~ c(c1,c2,c3)*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ c(a1,a2,a3)*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_cbcl_int~ c(b1,b2,b3)*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#         ab3 := a3*b3
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
#         total3 := c3 + (a3*b3)
# '
#   ' # direct effect
#         C_log_yr4_cbcl_int~ c(c1,c2,c3)*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ c(a1,a2,a3)*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_cbcl_int~ c(b1,b2,b3)*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#         ab3 := a3*b3
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
#         total3 := c3 + (a3*b3)
# '
cbclint_unconstrained_gender_model <- sem(cbclint_unconstrained_gender_model, 
                                        data = meddata, 
                                        meanstructure = TRUE,
                                        se = "robust.cluster",
                                        group = "genderid",
                                        cluster = "site")
summary(cbclint_unconstrained_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclint_unconstrained_gender_model, boot.ci.type = "bca.simple")

#### Does the unconstrained model fit better than the constrained model?
anova(cbclint_constrained_gender_model,cbclint_unconstrained_gender_model)

#### Does the constrained model fit better than the model without gender?
anova(cbclint_model,cbclint_constrained_gender_model)

#### Dummy-coded gender model (not by group)
cbclint_dummy_gender_model <- 
  ' # direct effect
        C_yr4_cbcl_int~ c*C_yr4_total_bad_le + gender_cisgirl + gender_gd + C_yr4_age + C_yr4_total_bad_le*gender_cisgirl + C_yr4_total_bad_le*gender_gd
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + gender_cisgirl + gender_gd + C_yr4_age + C_yr4_total_bad_le*gender_cisgirl + C_yr4_total_bad_le*gender_gd
      # indirect effect
        C_yr4_cbcl_int~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
cbclint_dummy_gender_model <- sem(cbclint_dummy_gender_model, 
                                          data = meddata, 
                                          meanstructure = TRUE,
                                          se = "robust.cluster",
                                          # group = "genderid",
                                          cluster = "site")
summary(cbclint_dummy_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclint_dummy_gender_model, boot.ci.type = "bca.simple")

### Moderated mediation model for CBCL externalizing based on gender ####
#### Constrained gender model by group
cbclext_constrained_gender_model <- 
  ' # direct effect
        C_yr4_cbcl_ext~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_cbcl_ext~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
# ' # direct effect
#         C_yr4_cbcl_ext~ c*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_cbcl_ext~ b*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
# ' # direct effect
#         C_log_yr4_cbcl_ext~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_cbcl_ext~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
cbclext_constrained_gender_model <- sem(cbclext_constrained_gender_model, 
                                        data = meddata, 
                                        meanstructure = TRUE,
                                        se = "robust.cluster",
                                        group = "genderid",
                                        cluster = "site")
summary(cbclext_constrained_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclext_constrained_gender_model, boot.ci.type = "bca.simple")

#### Unconstrained gender model by group
cbclext_unconstrained_gender_model <- 
  ' # direct effect
        C_yr4_cbcl_ext~ c(c1,c2,c3)*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ c(a1,a2,a3)*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_cbcl_ext~ c(b1,b2,b3)*C_yr4_ders_total
      # indirect effect (a*b)
        ab1 := a1*b1
        ab2 := a2*b2
        ab3 := a3*b3
      # total effect
        total1 := c1 + (a1*b1)
        total2 := c2 + (a2*b2)
        total3 := c3 + (a3*b3)
'
# ' # direct effect
#         C_yr4_cbcl_ext~ c(c1,c2,c3)*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ c(a1,a2,a3)*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_cbcl_ext~ c(b1,b2,b3)*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#         ab3 := a3*b3
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
#         total3 := c3 + (a3*b3)
# '
# ' # direct effect
#         C_log_yr4_cbcl_ext~ c(c1,c2,c3)*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ c(a1,a2,a3)*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_cbcl_ext~ c(b1,b2,b3)*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#         ab3 := a3*b3
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
#         total3 := c3 + (a3*b3)
# '
cbclext_unconstrained_gender_model <- sem(cbclext_unconstrained_gender_model, 
                                          data = meddata, 
                                          meanstructure = TRUE,
                                          se = "robust.cluster",
                                          group = "genderid",
                                          cluster = "site")
summary(cbclext_unconstrained_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclext_unconstrained_gender_model, boot.ci.type = "bca.simple")

#### Does the unconstrained model fit better than the constrained model?
anova(cbclext_constrained_gender_model,cbclext_unconstrained_gender_model)

#### Does the constrained model fit better than the model without gender?
anova(cbclext_model,cbclext_constrained_gender_model)

#### Dummy-coded gender model (not by group)
cbclext_dummy_gender_model <- 
  ' # direct effect
        C_yr4_cbcl_ext~ c*C_yr4_total_bad_le + gender_cisgirl + gender_gd + C_yr4_age + C_yr4_total_bad_le*gender_cisgirl + C_yr4_total_bad_le*gender_gd
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + gender_cisgirl + gender_gd + C_yr4_age + C_yr4_total_bad_le*gender_cisgirl + C_yr4_total_bad_le*gender_gd
      # indirect effect
        C_yr4_cbcl_ext~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
cbclext_dummy_gender_model <- sem(cbclext_dummy_gender_model, 
                                  data = meddata, 
                                  meanstructure = TRUE,
                                  se = "robust.cluster",
                                  # group = "genderid",
                                  cluster = "site")
summary(cbclext_dummy_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclext_dummy_gender_model, boot.ci.type = "bca.simple")

### Moderated mediation model for BPM internalizing based on gender ####
#### Constrained gender model by group
bpmint_constrained_gender_model <- 
  ' # direct effect
        C_yr4_bpm_int~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_int~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
# ' # direct effect
#         C_yr4_bpm_int~ c*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_bpm_int~ b*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
# ' # direct effect
#         C_log_yr4_bpm_int~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_bpm_int~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
bpmint_constrained_gender_model <- sem(bpmint_constrained_gender_model, 
                                        data = meddata, 
                                        meanstructure = TRUE,
                                        se = "robust.cluster",
                                        group = "genderid",
                                        cluster = "site")
summary(bpmint_constrained_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmint_constrained_gender_model, boot.ci.type = "bca.simple")

#### Unconstrained gender model by group
bpmint_unconstrained_gender_model <- 
  ' # direct effect
        C_yr4_bpm_int~ c(c1,c2,c3)*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ c(a1,a2,a3)*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_int~ c(b1,b2,b3)*C_yr4_ders_total
      # indirect effect (a*b)
        ab1 := a1*b1
        ab2 := a2*b2
        ab3 := a3*b3
      # total effect
        total1 := c1 + (a1*b1)
        total2 := c2 + (a2*b2)
        total3 := c3 + (a3*b3)
'
# ' # direct effect
#         C_yr4_bpm_int~ c(c1,c2,c3)*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ c(a1,a2,a3)*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_bpm_int~ c(b1,b2,b3)*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#         ab3 := a3*b3
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
#         total3 := c3 + (a3*b3)
# '
# ' # direct effect
#         C_log_yr4_bpm_int~ c(c1,c2,c3)*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ c(a1,a2,a3)*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_bpm_int~ c(b1,b2,b3)*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#         ab3 := a3*b3
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
#         total3 := c3 + (a3*b3)
# '
bpmint_unconstrained_gender_model <- sem(bpmint_unconstrained_gender_model, 
                                          data = meddata, 
                                          meanstructure = TRUE,
                                          se = "robust.cluster",
                                          group = "genderid",
                                          cluster = "site")
summary(bpmint_unconstrained_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmint_unconstrained_gender_model, boot.ci.type = "bca.simple")

#### Does the unconstrained model fit better than the constrained model?
anova(bpmint_constrained_gender_model,bpmint_unconstrained_gender_model)

#### Does the constrained model fit better than the model without gender?
anova(bpmint_model,bpmint_constrained_gender_model)

#### Which gender groups are different? Use Z test to find out. 
##### For (a) LES -> ER
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.821 - 0.797)/(sqrt((0.250^2)+(0.185)^2)) = 0.07716882
# cis boy (beta1) vs gd (beta2):
#     Z = (0.821 - 1.601)/(sqrt((0.250^2)+(0.662)^2)) = -1.102267
# cis girl (beta1) vs gd (beta2):
#     Z = (0.797 - 1.601)/(sqrt((0.185^2)+(0.662)^2)) = -1.169686
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = 0.07716882, so pnorm(-abs(0.07716882))*2 = 0.9384892
# cis boy vs gd: Z = -1.102267, so pnorm(-abs(-1.102267))*2 = 0.2703456
# cis girl vs gd: Z = -1.169686, so pnorm(-abs(-1.169686))*2 = 0.2421274
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.9384892,0.2703456,0.2421274),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p = .938
# cis boy vs gd: p = .406
# cis girl vs gd: .406

##### For (b) ER -> BPM int
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.049 - 0.066)/(sqrt((0.010^2)+(0.010)^2)) = -1.202082
# cis boy (beta1) vs gd (beta2):
#     Z = (0.049 - 0.068)/(sqrt((0.010^2)+(0.029)^2)) = -0.6193823
# cis girl (beta1) vs gd (beta2):
#     Z = (0.066 - 0.068)/(sqrt((0.010^2)+(0.029)^2)) = -0.06519814
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -1.202082, so pnorm(-abs(-1.202082))*2 = 0.2293318
# cis boy vs gd: Z = -0.6193823, so pnorm(-abs(-0.6193823))*2 = 0.5356645
# cis girl vs gd: Z = -0.06519814, so pnorm(-abs(-0.06519814))*2 = 0.9480162
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.2293318,0.5356645,0.9480162),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p = .688
# cis boy vs gd: p = .803
# cis girl vs gd: .948

##### For direct (c') LES -> BPM int
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.471 - 0.730)/(sqrt((0.059^2)+(0.050)^2)) = -3.348982
# cis boy (beta1) vs gd (beta2):
#     Z = (0.471 - 0.864)/(sqrt((0.059^2)+(0.210)^2)) = -1.801672
# cis girl (beta1) vs gd (beta2):
#     Z = (0.730 - 0.864)/(sqrt((0.050^2)+(0.210)^2)) = -0.620743
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -3.348982, so pnorm(-abs(-3.348982))*2 = 0.0008110906
# cis boy vs gd: Z = -1.801672, so pnorm(-abs(-1.801672))*2 = 0.07159703
# cis girl vs gd: Z = -0.620743, so pnorm(-abs(-0.620743))*2 = 0.5347687
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.0008110906,0.07159703,0.5347687),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p = .002
# cis boy vs gd: p = .107
# cis girl vs gd: .535

##### For indirect (ab) LES -ER-> BPM int
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.040 - 0.053)/(sqrt((0.015^2)+(0.016)^2)) = -0.592749
# cis boy (beta1) vs gd (beta2):
#     Z = (0.040 - 0.109)/(sqrt((0.015^2)+(0.072)^2)) = -0.9381896
# cis girl (beta1) vs gd (beta2):
#     Z = (0.053 - 0.109)/(sqrt((0.016^2)+(0.072)^2)) = -0.7592566
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -0.592749, so pnorm(-abs(-0.592749))*2 = 0.5533491
# cis boy vs gd: Z = -0.9381896, so pnorm(-abs(-0.9381896))*2 = 0.348147
# cis girl vs gd: Z = -0.7592566, so pnorm(-abs(-0.7592566))*2 = 0.4476991
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.5533491,0.348147,0.4476991),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p = .553
# cis boy vs gd: p = .553
# cis girl vs gd: .553

#### Dummy-coded gender model (not by group)
bpmint_dummy_gender_model <- 
  ' # direct effect
        C_yr4_bpm_int~ c*C_yr4_total_bad_le + gender_cisgirl + gender_gd + C_yr4_age + C_yr4_total_bad_le*gender_cisgirl + C_yr4_total_bad_le*gender_gd
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + gender_cisgirl + gender_gd + C_yr4_age + C_yr4_total_bad_le*gender_cisgirl + C_yr4_total_bad_le*gender_gd
      # indirect effect
        C_yr4_bpm_int~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
bpmint_dummy_gender_model <- sem(bpmint_dummy_gender_model, 
                                  data = meddata, 
                                  meanstructure = TRUE,
                                  se = "robust.cluster",
                                  # group = "genderid",
                                  cluster = "site")
summary(bpmint_dummy_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmint_dummy_gender_model, boot.ci.type = "bca.simple")

### Moderated mediation model for BPM externalizing based on gender ####
#### Constrained gender model by group
bpmext_constrained_gender_model <- 
  ' # direct effect
        C_yr4_bpm_ext~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_ext~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
# ' # direct effect
#         C_yr4_bpm_ext~ c*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_bpm_ext~ b*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
# ' # direct effect
#         C_log_yr4_bpm_ext~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_bpm_ext~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
bpmext_constrained_gender_model <- sem(bpmext_constrained_gender_model, 
                                        data = meddata, 
                                        meanstructure = TRUE,
                                        se = "robust.cluster",
                                        group = "genderid",
                                        cluster = "site")
summary(bpmext_constrained_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmext_constrained_gender_model, boot.ci.type = "bca.simple")

#### Unconstrained gender model by group
bpmext_unconstrained_gender_model <- 
  ' # direct effect
        C_yr4_bpm_ext~ c(c1,c2,c3)*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ c(a1,a2,a3)*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_ext~ c(b1,b2,b3)*C_yr4_ders_total
      # indirect effect (a*b)
        ab1 := a1*b1
        ab2 := a2*b2
        ab3 := a3*b3
      # total effect
        total1 := c1 + (a1*b1)
        total2 := c2 + (a2*b2)
        total3 := c3 + (a3*b3)
'
# ' # direct effect
#         C_yr4_bpm_ext~ c(c1,c2,c3)*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ c(a1,a2,a3)*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_bpm_ext~ c(b1,b2,b3)*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#         ab3 := a3*b3
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
#         total3 := c3 + (a3*b3)
# '
# ' # direct effect
#         C_log_yr4_bpm_ext~ c(c1,c2,c3)*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ c(a1,a2,a3)*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_bpm_ext~ c(b1,b2,b3)*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#         ab3 := a3*b3
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
#         total3 := c3 + (a3*b3)
# '
bpmext_unconstrained_gender_model <- sem(bpmext_unconstrained_gender_model, 
                                          data = meddata, 
                                          meanstructure = TRUE,
                                          se = "robust.cluster",
                                          group = "genderid",
                                          cluster = "site")
summary(bpmext_unconstrained_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmext_unconstrained_gender_model, boot.ci.type = "bca.simple")

#### Does the unconstrained model fit better than the constrained model?
anova(bpmext_constrained_gender_model,bpmext_unconstrained_gender_model)

#### Does the constrained model fit better than the model without gender?
anova(bpmext_model,bpmext_constrained_gender_model)

#### Dummy-coded gender model (not by group)
bpmext_dummy_gender_model <- 
  ' # direct effect
        C_yr4_bpm_ext~ c*C_yr4_total_bad_le + gender_cisgirl + gender_gd + C_yr4_age + C_yr4_total_bad_le*gender_cisgirl + C_yr4_total_bad_le*gender_gd
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + gender_cisgirl + gender_gd + C_yr4_age + C_yr4_total_bad_le*gender_cisgirl + C_yr4_total_bad_le*gender_gd
      # indirect effect
        C_yr4_bpm_ext~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
bpmext_dummy_gender_model <- sem(bpmext_dummy_gender_model, 
                                  data = meddata, 
                                  meanstructure = TRUE,
                                  se = "robust.cluster",
                                  # group = "genderid",
                                  cluster = "site")
summary(bpmext_dummy_gender_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmext_dummy_gender_model, boot.ci.type = "bca.simple")

### Moderated mediation model for CBCL internalizing based on sex ####
#### Constrained sex model by group
cbclint_constrained_sex_model <- 
  ' # direct effect
        C_yr4_cbcl_int~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_cbcl_int~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
# ' # direct effect
#         C_yr4_cbcl_int~ c*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_cbcl_int~ b*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
# ' # direct effect
#         C_log_yr4_cbcl_int~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_cbcl_int~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
cbclint_constrained_sex_model <- sem(cbclint_constrained_sex_model, 
                                        data = sex_meddata, 
                                        meanstructure = TRUE,
                                        se = "robust.cluster",
                                        group = "sex",
                                        cluster = "site")
summary(cbclint_constrained_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclint_constrained_sex_model, boot.ci.type = "bca.simple")

#### Unconstrained gender model by group
cbclint_unconstrained_sex_model <- 
  ' # direct effect
        C_yr4_cbcl_int~ c(c1,c2)*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ c(a1,a2)*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_cbcl_int~ c(b1,b2)*C_yr4_ders_total
      # indirect effect (a*b)
        ab1 := a1*b1
        ab2 := a2*b2
      # total effect
        total1 := c1 + (a1*b1)
        total2 := c2 + (a2*b2)
'
# ' # direct effect
#         C_yr4_cbcl_int~ c(c1,c2)*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ c(a1,a2)*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_cbcl_int~ c(b1,b2)*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
# '
# ' # direct effect
#         C_log_yr4_cbcl_int~ c(c1,c2)*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ c(a1,a2)*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_cbcl_int~ c(b1,b2)*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
# '
cbclint_unconstrained_sex_model <- sem(cbclint_unconstrained_sex_model, 
                                          data = sex_meddata, 
                                          meanstructure = TRUE,
                                          se = "robust.cluster",
                                          group = "sex",
                                          cluster = "site")
summary(cbclint_unconstrained_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclint_unconstrained_sex_model, boot.ci.type = "bca.simple")

#### Does the unconstrained model fit better than the constrained model?
anova(cbclint_constrained_sex_model,cbclint_unconstrained_sex_model)

#### Which sex groups are different? Use Z test to find out. 
##### For (a) LES -> ER
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.830 - 1.047)/(sqrt((0.253^2)+(0.201)^2)) = -0.671566
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -0.671566, so pnorm(-abs(-0.671566))*2 = 0.50186
# So final p-values rounded to three places are:
# male vs female: p = .502

##### For (b) ER -> CBCL int
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.297 - 0.350)/(sqrt((0.015^2)+(0.019)^2)) = -2.18941
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -2.18941, so pnorm(-abs(-2.18941))*2 = 0.02856705
# So final p-values rounded to three places are:
# male vs female: p = .029

##### For direct (c') LES -> CBCL int
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.276 - 0.515)/(sqrt((0.119^2)+(0.106)^2)) = -1.499708
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -1.499708, so pnorm(-abs(-1.499708))*2 = 0.1336901
# So final p-values rounded to three places are:
# male vs female: p = .134

##### For indirect (ab) LES -ER-> CBCL int
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.247 - 0.367)/(sqrt((0.049^2)+(0.079)^2)) = -1.290845
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -1.290845, so pnorm(-abs(-1.290845))*2 = 0.1967574
# So final p-values rounded to three places are:
# male vs female: p = .197

#### Dummy-coded sex model (not by group)
cbclint_dummy_sex_model <- 
  ' # direct effect
        C_yr4_cbcl_int~ c*C_yr4_total_bad_le + sex_female + C_yr4_age + C_yr4_total_bad_le*sex_female
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + sex_female + C_yr4_age + C_yr4_total_bad_le*sex_female
      # indirect effect
        C_yr4_cbcl_int~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
cbclint_dummy_sex_model <- sem(cbclint_dummy_sex_model, 
                                  data = sex_meddata, 
                                  meanstructure = TRUE,
                                  se = "robust.cluster",
                                  # group = "genderid",
                                  cluster = "site")
summary(cbclint_dummy_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclint_dummy_sex_model, boot.ci.type = "bca.simple")

### Moderated mediation model for CBCL externalizing based on sex ####
#### Constrained sex model by group
cbclext_constrained_sex_model <- 
  ' # direct effect
        C_yr4_cbcl_ext~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_cbcl_ext~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
# ' # direct effect
#         C_yr4_cbcl_ext~ c*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_cbcl_ext~ b*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
# ' # direct effect
#         C_log_yr4_cbcl_ext~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_cbcl_ext~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
cbclext_constrained_sex_model <- sem(cbclext_constrained_sex_model, 
                                     data = sex_meddata, 
                                     meanstructure = TRUE,
                                     se = "robust.cluster",
                                     group = "sex",
                                     cluster = "site")
summary(cbclext_constrained_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclext_constrained_sex_model, boot.ci.type = "bca.simple")

#### Unconstrained gender model by group
cbclext_unconstrained_sex_model <- 
  ' # direct effect
        C_yr4_cbcl_ext~ c(c1,c2)*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ c(a1,a2)*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_cbcl_ext~ c(b1,b2)*C_yr4_ders_total
      # indirect effect (a*b)
        ab1 := a1*b1
        ab2 := a2*b2
      # total effect
        total1 := c1 + (a1*b1)
        total2 := c2 + (a2*b2)
'
# ' # direct effect
#         C_yr4_cbcl_ext~ c(c1,c2)*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ c(a1,a2)*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_cbcl_ext~ c(b1,b2)*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
# '
# ' # direct effect
#         C_log_yr4_cbcl_ext~ c(c1,c2)*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ c(a1,a2)*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_cbcl_ext~ c(b1,b2)*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
# '
cbclext_unconstrained_sex_model <- sem(cbclext_unconstrained_sex_model, 
                                       data = sex_meddata, 
                                       meanstructure = TRUE,
                                       se = "robust.cluster",
                                       group = "sex",
                                       cluster = "site")
summary(cbclext_unconstrained_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclext_unconstrained_sex_model, boot.ci.type = "bca.simple")

#### Does the unconstrained model fit better than the constrained model?
anova(cbclext_constrained_sex_model,cbclext_unconstrained_sex_model)

#### Dummy-coded sex model (not by group)
cbclext_dummy_sex_model <- 
  ' # direct effect
        C_yr4_cbcl_ext~ c*C_yr4_total_bad_le + sex_female + C_yr4_age + C_yr4_total_bad_le*sex_female
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + sex_female + C_yr4_age + C_yr4_total_bad_le*sex_female
      # indirect effect
        C_yr4_cbcl_ext~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
cbclext_dummy_sex_model <- sem(cbclext_dummy_sex_model, 
                               data = sex_meddata, 
                               meanstructure = TRUE,
                               se = "robust.cluster",
                               # group = "genderid",
                               cluster = "site")
summary(cbclext_dummy_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(cbclext_dummy_sex_model, boot.ci.type = "bca.simple")

### Moderated mediation model for BPM internalizing based on sex ####
#### Constrained sex model by group
bpmint_constrained_sex_model <- 
  ' # direct effect
        C_yr4_bpm_int~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_int~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
# ' # direct effect
#         C_yr4_bpm_int~ c*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_bpm_int~ b*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
# ' # direct effect
#         C_log_yr4_bpm_int~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_bpm_int~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
bpmint_constrained_sex_model <- sem(bpmint_constrained_sex_model, 
                                     data = sex_meddata, 
                                     meanstructure = TRUE,
                                     se = "robust.cluster",
                                     group = "sex",
                                     cluster = "site")
summary(bpmint_constrained_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmint_constrained_sex_model, boot.ci.type = "bca.simple")

#### Unconstrained gender model by group
bpmint_unconstrained_sex_model <- 
  ' # direct effect
        C_yr4_bpm_int~ c(c1,c2)*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ c(a1,a2)*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_int~ c(b1,b2)*C_yr4_ders_total
      # indirect effect (a*b)
        ab1 := a1*b1
        ab2 := a2*b2
      # total effect
        total1 := c1 + (a1*b1)
        total2 := c2 + (a2*b2)
'
# ' # direct effect
#         C_yr4_bpm_int~ c(c1,c2)*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ c(a1,a2)*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_bpm_int~ c(b1,b2)*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
# '
# ' # direct effect
#         C_log_yr4_bpm_int~ c(c1,c2)*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ c(a1,a2)*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_bpm_int~ c(b1,b2)*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
# '
bpmint_unconstrained_sex_model <- sem(bpmint_unconstrained_sex_model, 
                                       data = sex_meddata, 
                                       meanstructure = TRUE,
                                       se = "robust.cluster",
                                       group = "sex",
                                       cluster = "site")
summary(bpmint_unconstrained_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmint_unconstrained_sex_model, boot.ci.type = "bca.simple")

#### Does the unconstrained model fit better than the constrained model?
anova(bpmint_constrained_sex_model,bpmint_unconstrained_sex_model)

#### Which sex groups are different? Use Z test to find out. 
##### For (a) LES -> ER
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.830 - 1.047)/(sqrt((0.253^2)+(0.201)^2)) = -0.671566
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -0.671566, so pnorm(-abs(-0.671566))*2 = 0.50186
# So final p-values rounded to three places are:
# male vs female: p = .502

##### For (b) ER -> BPM int
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.051 - 0.082)/(sqrt((0.009^2)+(0.009)^2)) = -2.43559
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -2.43559, so pnorm(-abs(-2.43559))*2 = 0.01486753
# So final p-values rounded to three places are:
# male vs female: p = .015

##### For direct (c') LES -> BPM int
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.475 - 0.802)/(sqrt((0.060^2)+(0.062)^2)) = -3.790047
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z 0.0001506188
# male vs female: Z = -3.790047, so pnorm(-abs(-3.790047))*2 = 0.1336901
# So final p-values rounded to three places are:
# male vs female: p < .001

##### For indirect (ab) LES -ER-> BPM int
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.042 - 0.086)/(sqrt((0.015^2)+(0.021)^2)) = -1.704965
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -1.704965, so pnorm(-abs(-1.704965))*2 = 0.08820095
# So final p-values rounded to three places are:
# male vs female: p = .088

#### Dummy-coded sex model (not by group)
bpmint_dummy_sex_model <- 
  ' # direct effect
        C_yr4_bpm_int~ c*C_yr4_total_bad_le + sex_female + C_yr4_age + C_yr4_total_bad_le*sex_female
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + sex_female + C_yr4_age + C_yr4_total_bad_le*sex_female
      # indirect effect
        C_yr4_bpm_int~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
bpmint_dummy_sex_model <- sem(bpmint_dummy_sex_model, 
                               data = sex_meddata, 
                               meanstructure = TRUE,
                               se = "robust.cluster",
                               # group = "genderid",
                               cluster = "site")
summary(bpmint_dummy_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmint_dummy_sex_model, boot.ci.type = "bca.simple")

### Moderated mediation model for BPM externalizing based on sex ####
#### Constrained sex model by group
bpmext_constrained_sex_model <- 
  ' # direct effect
        C_yr4_bpm_ext~ c*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_ext~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
# ' # direct effect
#         C_yr4_bpm_ext~ c*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_bpm_ext~ b*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
# ' # direct effect
#         C_log_yr4_bpm_ext~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_bpm_ext~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)
# '
bpmext_constrained_sex_model <- sem(bpmext_constrained_sex_model, 
                                     data = sex_meddata, 
                                     meanstructure = TRUE,
                                     se = "robust.cluster",
                                     group = "sex",
                                     cluster = "site")
summary(bpmext_constrained_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmext_constrained_sex_model, boot.ci.type = "bca.simple")

#### Unconstrained sex model by group
bpmext_unconstrained_sex_model <- 
  ' # direct effect
        C_yr4_bpm_ext~ c(c1,c2)*C_yr4_total_bad_le + C_yr4_age
      # mediator
        C_yr4_ders_total ~ c(a1,a2)*C_yr4_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_ext~ c(b1,b2)*C_yr4_ders_total
      # indirect effect (a*b)
        ab1 := a1*b1
        ab2 := a2*b2
      # total effect
        total1 := c1 + (a1*b1)
        total2 := c2 + (a2*b2)
'
# ' # direct effect
#         C_yr4_bpm_ext~ c(c1,c2)*C_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_yr3_ders_total ~ c(a1,a2)*C_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_yr4_bpm_ext~ c(b1,b2)*C_yr3_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
# '
# ' # direct effect
#         C_log_yr4_bpm_ext~ c(c1,c2)*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ c(a1,a2)*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_bpm_ext~ c(b1,b2)*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab1 := a1*b1
#         ab2 := a2*b2
#       # total effect
#         total1 := c1 + (a1*b1)
#         total2 := c2 + (a2*b2)
# '
bpmext_unconstrained_sex_model <- sem(bpmext_unconstrained_sex_model, 
                                       data = sex_meddata, 
                                       meanstructure = TRUE,
                                       se = "robust.cluster",
                                       group = "sex",
                                       cluster = "site")
summary(bpmext_unconstrained_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmext_unconstrained_sex_model, boot.ci.type = "bca.simple")

#### Does the unconstrained model fit better than the constrained model?
anova(bpmext_constrained_sex_model,bpmext_unconstrained_sex_model)

#### Dummy-coded gender model (not by group)
bpmext_dummy_sex_model <- 
  ' # direct effect
        C_yr4_bpm_ext~ c*C_yr4_total_bad_le + sex_female + C_yr4_age + C_yr4_total_bad_le*sex_female
      # mediator
        C_yr4_ders_total ~ a*C_yr4_total_bad_le  + sex_female + C_yr4_age + C_yr4_total_bad_le*sex_female
      # indirect effect
        C_yr4_bpm_ext~ b*C_yr4_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)
'
bpmext_dummy_sex_model <- sem(bpmext_dummy_sex_model, 
                               data = sex_meddata, 
                               meanstructure = TRUE,
                               se = "robust.cluster",
                               # group = "genderid",
                               cluster = "site")
summary(bpmext_dummy_sex_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmext_dummy_sex_model, boot.ci.type = "bca.simple")
