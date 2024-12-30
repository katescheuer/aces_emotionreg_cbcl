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

### Prevent use of scientific notation ####
options(scipen=999)

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

### Prepare gender data for analysis ####
#### Identify gender groups ####
genderdata <- gish_y_gi %>%
  # keep only data from year 3 and 4 follow-up visits
  # Before this step, n should be 49083. After this step, n should be 15064.
  filter(eventname=="3_year_follow_up_y_arm_1"|
         eventname=="4_year_follow_up_y_arm_1") %>%
  # convert numeric sex values to human-readable character strings
  mutate(sex_details = case_when(
                kbi_sex_assigned_at_birth==1 ~ "male",
                kbi_sex_assigned_at_birth==2 ~ "female",
                kbi_sex_assigned_at_birth==777 ~ "dont_know",
                kbi_sex_assigned_at_birth==999 ~ "refuse"
                )) %>%
  # set "don't know" or "refuse" to be NA for sex
  mutate(sex = case_when(
    sex_details=="male" ~ "male",
    sex_details=="female" ~ "female",
    sex_details=="dont_know" ~ NA_character_,
    sex_details=="refuse" ~ NA_character_
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

  # keep only columns relevant to analysis
  select(src_subject_id,eventname,
         sex,sex_details,sex_female,
         gender,trans,gender_details,genderid,
         gender_cisgirl,gender_gd,gender_cisboy
         ) %>%
  # remove subjects who refused to answer and/or did not understand gender
  # or trans questions. Before this step, n should be 15064. After this step,
  # n should be 14495.
  filter(genderid!="refuse",
         genderid!="dont_understand") %>%

  # make genderid a factor (automatically uses cisboy as reference)
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
         cbcl_scr_syn_internal_t,
         cbcl_scr_syn_external_t
         ) %>%
  # rename subscale columns to be more human-readable and shorter
  rename(cbcl_ext = cbcl_scr_syn_external_t,
         cbcl_int = cbcl_scr_syn_internal_t
         ) %>%
  # add column with log-transformed CBCL internalizing values
  mutate(log_cbcl_int = log(cbcl_int)) %>%
  # add column with log-transformed CBCL externalizing values
  mutate(log_cbcl_ext = log(cbcl_ext)) 

### Prepare BPM data for analysis ####
bpmdata <- mh_y_bpm %>%
  # select only columns relevant to analysis
  select(src_subject_id,eventname,
         bpm_y_scr_internal_t,
         bpm_y_scr_external_t
  ) %>%
  # rename subscale columns to be more human-readable and shorter
  rename(bpm_ext = bpm_y_scr_external_t,
         bpm_int = bpm_y_scr_internal_t
  ) %>%
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
  # he/she can remain in control of his/her behaviors"
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
  # add column for log-transformed version of total number of bad events (need
  # to add 1 because log(0) = -Inf)
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
  # Z-score continuous variables
  mutate(across(
    c(age, ders_total, total_bad_le, 
      cbcl_int, cbcl_ext,
      bpm_int, bpm_ext,
      log_ders_total, log_total_bad_le, 
      log_cbcl_int, log_cbcl_ext,
      log_bpm_int, log_bpm_ext),
    ~ as.numeric(scale(.)),
    .names = "Z_{.col}"
  )) %>%
  # Grand-mean center continuous variables
  mutate(across(
    c(age, ders_total, total_bad_le, 
      cbcl_int, cbcl_ext,
      bpm_int, bpm_ext,
      log_ders_total, log_total_bad_le, 
      log_cbcl_int, log_cbcl_ext,
      log_bpm_int, log_bpm_ext),
    ~ as.numeric(misty::center(.,type = c("CGM"))),
    .names = "C_{.col}"
  )) %>%
  # make genderid, sex, and site factors rather than characters
  mutate(across(c(genderid, sex, site), as.factor)) %>%
  # make males comparison level for sex
  mutate(sex = relevel(sex, ref="male"))

### Get general overview of all data ####
#### Get raw number and percentage for each gender group and data collection year ####
alldata %>% 
  group_by(eventname) %>% 
  count(genderid) %>% 
  mutate(percentage = n / sum(n) * 100)

#### Create separate data frame for just data from year 4 follow-up visit ####
# n should be 4612
yr4data <- alldata %>% filter(eventname=="4_year_follow_up_y_arm_1")

#### Create separate data frame for just data from year 3 follow-up visit ####
# n should be 9883
yr3data <- alldata %>% filter(eventname=="3_year_follow_up_y_arm_1")

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
analysis_data %>%
  group_by(gender_details) %>%
  count()

#### Determine how many subjects in each combination of gender and sex group ####
analysis_data %>%
  group_by(genderid, sex) %>%
  count()

### Combine year 4 and year 3 data to have option to use year 3 as x variable ####
#### Make combined year 3 and year 4 data for analysis ####
analysis_data <- yr4data %>%
  # add year 4 specifier
  rename(
    yr4_age = age,
    yr4_total_bad_le = total_bad_le,
    yr4_ders_total = ders_total,
    yr4_cbcl_int = cbcl_int,
    yr4_cbcl_ext = cbcl_ext,
    yr4_bpm_int = bpm_int,
    yr4_bpm_ext = bpm_ext,
    log_yr4_total_bad_le = log_total_bad_le,
    log_yr4_ders_total = log_ders_total,
    log_yr4_cbcl_int = log_cbcl_int,
    log_yr4_cbcl_ext = log_cbcl_ext,
    log_yr4_bpm_int = log_bpm_int,
    log_yr4_bpm_ext = log_bpm_ext,
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
    Z_yr4_age = Z_age,
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
    C_yr4_age = C_age
    ) %>%
  # add year 3 data
  left_join(select(yr3data, 
                   c(src_subject_id,
                     age,
                     total_bad_le,
                     ders_total,
                     cbcl_int,
                     cbcl_ext,
                     bpm_int,
                     bpm_ext,
                     log_total_bad_le,
                     log_ders_total,
                     log_cbcl_int,
                     log_cbcl_ext,
                     log_bpm_int,
                     log_bpm_ext,
                     Z_age,
                     Z_total_bad_le,Z_ders_total,
                     Z_cbcl_int,Z_cbcl_ext,
                     Z_bpm_int,Z_bpm_ext,
                     C_age,
                     C_total_bad_le,C_ders_total,
                     C_cbcl_int,C_cbcl_ext,
                     C_bpm_int,C_bpm_ext,
                     C_log_total_bad_le,C_log_ders_total,
                     C_log_cbcl_int,C_log_cbcl_ext,
                     C_log_bpm_int,C_log_bpm_ext
                   )),
            by="src_subject_id") %>%
  # name year 3 data to have year 3 specifier
  rename(
        yr3_age = age,
        yr3_total_bad_le = total_bad_le,
        yr3_ders_total = ders_total,
        yr3_cbcl_int = cbcl_int,
        yr3_cbcl_ext = cbcl_ext,
        yr3_bpm_int = bpm_int,
        yr3_bpm_ext = bpm_ext,
        log_yr3_total_bad_le = log_total_bad_le,
        log_yr3_ders_total = log_ders_total,
        log_yr3_cbcl_int = log_cbcl_int,
        log_yr3_cbcl_ext = log_cbcl_ext,
        log_yr3_bpm_int = log_bpm_int,
        log_yr3_bpm_ext = log_bpm_ext,
        Z_yr3_total_bad_le = Z_total_bad_le,
         Z_yr3_ders_total = Z_ders_total,
         Z_yr3_cbcl_int = Z_cbcl_int,
         Z_yr3_cbcl_ext = Z_cbcl_ext,
         Z_yr3_bpm_int = Z_bpm_int,
         Z_yr3_bpm_ext = Z_bpm_ext,
         Z_yr3_age = Z_age,
        C_yr3_total_bad_le = C_total_bad_le,
         C_yr3_ders_total = C_ders_total,
         C_yr3_cbcl_int = C_cbcl_int,
         C_yr3_cbcl_ext = C_cbcl_ext,
         C_yr3_bpm_int = C_bpm_int,
         C_yr3_bpm_ext = C_bpm_ext,
         C_yr3_age = C_age,
        C_log_yr3_total_bad_le = C_log_total_bad_le,
        C_log_yr3_ders_total = C_log_ders_total,
        C_log_yr3_cbcl_int = C_log_cbcl_int,
        C_log_yr3_cbcl_ext = C_log_cbcl_ext,
        C_log_yr3_bpm_int = C_log_bpm_int,
        C_log_yr3_bpm_ext = C_log_bpm_ext,
        ) %>%
# remove subjects without LES or DERS in year 3 or without CBCL or BPM in year 4
# before this step, n should be 4612, and after this step, n should be 3763
    filter(!is.na(yr3_ders_total), !is.na(yr3_total_bad_le),
           !is.na(yr4_cbcl_int), !is.na(yr4_cbcl_ext),
           !is.na(yr4_bpm_int), !is.na(yr4_bpm_ext))


#### See type of each column ####
str(analysis_data)

#### See all unique values for each column ####
map(analysis_data,unique)

## BASIC STATS ####

### Get summary stats for each variable ####
sumstats <- 
  analysis_data %>%
  group_by(genderid) %>%
  # group_by(gender_details) %>%
  # filter(!is.na(sex)) %>%
  # group_by(sex) %>%
  # to get number of people who answered don't know or refuse for sex
  # group_by(sex_details) %>%
  # count()
  summarise(
    n = n(),
    across(
      c("yr4_age","yr3_total_bad_le","yr3_ders_total",
        "yr4_bpm_int","yr4_cbcl_int","yr4_bpm_ext","yr4_cbcl_ext"),
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
walk(c("yr3_total_bad_le", "log_yr3_total_bad_le", 
       "yr3_ders_total", "log_yr3_ders_total", 
       "yr4_bpm_int","yr4_cbcl_int",
       "yr4_bpm_ext","yr4_cbcl_ext",
       "log_yr4_bpm_int","log_yr4_cbcl_int",
       "log_yr4_bpm_ext","log_yr4_cbcl_ext"), 
     ~ {
       cat("Variable:", .x, "\n")
       print(ad.test(analysis_data[[.x]]))
     })

### Create basic histogram for each variable ####
# Store histograms in list in case want to view later
variable_histograms <- 
  map(c("yr3_total_bad_le", "log_yr3_total_bad_le", 
        "yr3_ders_total", "log_yr3_ders_total", 
        "yr4_bpm_int", "log_yr4_bpm_int", 
        "yr4_cbcl_int", "log_yr4_cbcl_int", 
        "yr4_bpm_ext", "log_yr4_bpm_ext",
        "yr4_cbcl_ext", "log_yr4_cbcl_ext"), 
      # note that !!sym(.x) turns the variables in the list above into arguments
      # that can be passed to ggplot
     ~ ggplot(analysis_data, aes(x = !!sym(.x))) + 
         geom_histogram() +
         ggtitle(.x)
             )
# Print all histograms from stored list
print(variable_histograms)

### Create correlation matrix for all variables from year 4 data ####
# Make correlation matrix
# All variables are significantly correlated.
corrmat <- 
  analysis_data %>%
  # select relevant columns
  select(c(yr4_age,
           yr3_total_bad_le,yr3_ders_total,
           yr4_bpm_int,yr4_cbcl_int,
           yr4_bpm_ext,yr4_cbcl_ext)) %>% 
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
ggplot(analysis_data, 
       aes(x=yr3_total_bad_le,y=yr3_ders_total, fill=genderid)) +
  # aes(x=total_bad_le,y=cbcl_int)) +
  geom_point(aes(color=genderid, shape = genderid),size=2) +
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
                     breaks = seq(0,16, by = 1),
                     limits=c(-.5,16.5)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(20,130,10),
                     limits = c(25,131)) +
  # breaks=seq(0,120,by=20),
  # limits=c(0,120)) +
  # guides(
  #   line = guide_legend(override.aes = list(size = 2)),
  #   fill = guide_legend(override.aes = list(size = 2)) 
  # ) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_ders_bygender.tiff",width=14,height=6,unit="in",path="figures")
# ggsave("les_vs_ders_bar_bygender.tiff",width=9,height=6,unit="in",path="figures")
# ggsave("les_vs_ders_scatter_bygender.tiff",width=8.5,height=6,unit="in",path="figures")

##### Bargraph of LES vs CBCL internalizing by gender ####
ggplot(analysis_data, 
       aes(x=yr3_total_bad_le,y=yr4_cbcl_int, fill=genderid)) +
       # aes(x=total_bad_le,y=cbcl_int)) +
  geom_point(aes(color=genderid, shape = genderid),size=2) +
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
                     breaks = seq(0,16, by = 1),
                     limits=c(-.5,16.5)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(30,90,by=10),
                     limits=c(30,90)) +
                     # breaks=seq(0,90,by=10),
                     # limits=c(0,90)) +
  guides(
    line = guide_legend(override.aes = list(size = 2)),
    fill = guide_legend(override.aes = list(size = 2)) 
  ) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_cbclint_bygender.tiff",width=14,height=6,unit="in",path="figures")
# ggsave("les_vs_cbclint_bar_bygender.tiff",width=9,height=6,unit="in",path="figures")
# ggsave("les_vs_cbclint_scatter_bygender.tiff",width=8.5,height=6,unit="in",path="figures")

##### Bargraph of LES vs CBCL externalizing by gender ####
ggplot(analysis_data, 
       aes(x=yr3_total_bad_le,y=yr4_cbcl_ext, fill=genderid)) +
  # aes(x=total_bad_le,y=cbcl_int)) +
  geom_point(aes(color=genderid, shape = genderid), size=2) +
  # geom_bar(stat="summary",fun="mean",
  #          position=position_dodge2(preserve = "single"),
  #          color="black",width=.9) +
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
                     breaks = seq(0,16, by = 1),
                     limits=c(-.5,16.5)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(30,90,by=10),
                     limits=c(30,90)) +
                        # breaks=seq(0,90,by=10),
                        # limits=c(0,90)) +
  guides(
    shape = guide_legend(override.aes = list(size = 2)),
    fill = guide_legend(override.aes = list(size = 2)) 
  ) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_cbclext_bygender.tiff",width=14,height=6,unit="in",path="figures")
# ggsave("les_vs_cbclext_bar_bygender.tiff",width=9,height=6,unit="in",path="figures")
# ggsave("les_vs_cbclext_scatter_bygender.tiff",width=8.5,height=6,unit="in",path="figures")

##### Bargraph of LES vs BPM internalizing by gender ####
ggplot(analysis_data, 
       aes(x=yr3_total_bad_le,y=yr4_bpm_int, fill=genderid)) +
  # aes(x=total_bad_le,y=bpm_int)) +
  geom_point(aes(color=genderid, shape = genderid),size=2) +
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
                     breaks = seq(0,16, by = 1),
                     limits=c(-.5,16.5)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(40,75,by=5),
                     limits=c(49,76)) +
                     # breaks=seq(0,80,by=10),
                     # limits=c(0,80)) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_bpmint_bygender.tiff",width=14,height=6,unit="in",path="figures")
# ggsave("les_vs_bpmint_bar_bygender.tiff",width=9,height=6,unit="in",path="figures")
# ggsave("les_vs_bpmint_scatter_bygender.tiff",width=8.5,height=6,unit="in",path="figures")

##### Bargraph of LES vs BPM externalizing by gender ####
ggplot(analysis_data, 
       aes(x=yr3_total_bad_le,y=yr4_bpm_ext, fill=genderid)) +
  # aes(x=total_bad_le,y=bpm_int)) +
  geom_point(aes(color=genderid, shape = genderid), size=2) +
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
                     breaks = seq(0,16, by = 1),
                     limits=c(-.5,16.5)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(40,75,by=5),
                     limits=c(49,76)) +
                        # breaks=seq(0,80,by=10),
                        # limits=c(0,80)) +
  theme_classic()
# Save bar graph
# ggsave("les_vs_bpmext_bygender.tiff",width=14,height=6,unit="in",path="figures")
# ggsave("les_vs_bpmext_bar_bygender.tiff",width=9,height=6,unit="in",path="figures")
# ggsave("les_vs_bpmext_scatter_bygender.tiff",width=8.5,height=6,unit="in",path="figures")

##### Scatterplots of DERS vs CBCL by gender ####
outcome_list <- c("yr4_cbcl_int","yr4_cbcl_ext")
cbcl_ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  cbcl_ders_plot <-
    ggplot(aes(x=yr3_ders_total,y=.data[[outcome]],
                               # color=genderid,
                               linetype=genderid,
                               shape = genderid,
                               fill = genderid
                               ),data=analysis_data) +
    geom_point(alpha=.6, size = 2) +
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
                       breaks=seq(20,130,10),
                       limits = c(25,131)) +
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
         # width=8.3,height=6,units = "in",path="figures")
}

##### Scatterplots of DERS vs BPM by gender ####
outcome_list <- c("yr4_bpm_int","yr4_bpm_ext")
bpm_ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  bpm_ders_plot <- ggplot(aes(x=yr3_ders_total,y=.data[[outcome]],
                              # color=genderid,
                              linetype=genderid,
                              shape = genderid,
                              fill = genderid
  ),data=analysis_data) +
    geom_point(alpha=.6, size=2) +
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
                       breaks=seq(20,130,10),
                       limits = c(25,131)) +
    scale_y_continuous(expand = c(0,0),
                       breaks=seq(40,75,by=5),
                       limits=c(49,76)) +
    theme_classic() +
    guides(
      shape = guide_legend(override.aes = list(size = 2)),
      fill = guide_legend(override.aes = list(size = 2)) 
    )
  bpm_ders_plot_list[[outcome]] <- bpm_ders_plot
  # Save plot
  # ggsave(paste0("ders_vs_",outcome,"_bygender.tiff"),
  #        width=8.3,height=6,units = "in",path="figures")
}


## STEP ONE: BASIC GROUP DIFFERENCES AND REGRESSION ####

### Kruskal-Wallis (non-parametric version of one-way ANOVA) and Dunn test ie ####
### pairwise Wilcoxon tests to determine whether variables differ based on 
### gender
#### Age (year 4) 
kruskal.test(yr4_age ~ genderid, data = analysis_data)
# kruskal.test(yr4_age ~ gender_details, data = analysis_data)
#### LES (year 3) 
kruskal.test(yr3_total_bad_le ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr3_total_bad_le, analysis_data$genderid, method = "bh")
# kruskal.test(yr3_total_bad_le ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr3_total_bad_le, analysis_data$gender_details, method = "bh")
#### DERS (year 3) 
kruskal.test(yr3_ders_total ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr3_ders_total, analysis_data$genderid, method = "bh")
# kruskal.test(yr3_ders_total ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr3_ders_total, analysis_data$gender_details, method = "bh")
#### CBCL internalizing (year 4) 
kruskal.test(yr4_cbcl_int ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr4_cbcl_int, analysis_data$genderid, method = "bh")
# kruskal.test(yr4_cbcl_int ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr4_cbcl_int, analysis_data$gender_details, method = "bh")
#### CBCL externalizing (year 4)
kruskal.test(yr4_cbcl_ext ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr4_cbcl_ext, analysis_data$genderid, method = "bh")
# kruskal.test(yr4_cbcl_ext ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr4_cbcl_ext, analysis_data$gender_details, method = "bh")
#### BPM internalizing (year 4) 
kruskal.test(yr4_bpm_int ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr4_bpm_int, analysis_data$genderid, method = "bh")
# kruskal.test(yr4_bpm_int ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr4_bpm_int, analysis_data$gender_details, method = "bh")
#### BPM externalizing (year 4)
kruskal.test(yr4_bpm_ext ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr4_bpm_ext, analysis_data$genderid, method = "bh")
# kruskal.test(yr4_bpm_ext ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr4_bpm_ext, analysis_data$gender_details, method = "bh")


### Mann-Whitney U (non-parametric version of two-sample t-test) to determine ####
### whether variables differ based on sex
#### Age (year 4) 
wilcox.test(yr4_age ~ sex, data = analysis_data)
#### LES (year 3) does differ significantly based on sex (p = 0.01768)
wilcox.test(yr3_total_bad_le ~ sex, data = analysis_data)
#### DERS (year 3) does differ significantly based on sex (p = 5.757e-16)
wilcox.test(yr3_ders_total ~ sex, data = analysis_data)
#### CBCL internalizing (year 4) 
wilcox.test(yr4_cbcl_int ~ sex, data = analysis_data)
#### CBCL externalizing (year 4)
wilcox.test(yr4_cbcl_ext ~ sex, data = analysis_data)
#### BPM internalizing (year 4)
wilcox.test(yr4_bpm_int ~ sex, data = analysis_data)
#### BPM externalizing (year 4) 
wilcox.test(yr4_bpm_ext ~ sex, data = analysis_data)

### Mixed effect linear regression to determine whether DERS differs based ####
### on LES, using age as fixed effect covariate and site as random intercept
#### DERS ~ LES + age + (1|site) ####
ders_les_age_reg <- lmer(C_yr4_ders_total ~ C_yr3_total_bad_le + C_yr4_age + (1|site),
# ders_les_age_reg <- lmer(C_log_yr4_ders_total ~ C_log_yr4_total_bad_le + C_yr4_age + (1|site),
                         data=analysis_data)
summary(ders_les_age_reg)
rsq(ders_les_age_reg,adj=TRUE)

### Mixed effect linear regression to determine whether CBCL or BPM differ ####
### based on LES and/or DERS, using age as fixed effect covariate and site as random 
### intercept
#### CBCL internalizing ~ LES + age + (1|site) ####
cbcl_int_les_age_reg <- lmer(C_yr4_cbcl_int ~ C_yr3_total_bad_le + C_yr4_age + (1|site),
# cbcl_int_les_age_reg <- lmer(C_log_yr4_cbcl_int ~ C_log_yr3_total_bad_le + C_yr4_age + (1|site),
                           data=analysis_data)
summary(cbcl_int_les_age_reg)
#### CBCL externalizing ~ LES + age + (1|site) ####
cbcl_ext_les_age_reg <- lmer(C_yr4_cbcl_ext ~ C_yr3_total_bad_le + C_yr4_age + (1|site),
# cbcl_ext_les_age_reg <- lmer(C_log_yr4_cbcl_ext ~ C_log_yr3_total_bad_le + C_yr4_age + (1|site),
                           data=analysis_data)
summary(cbcl_ext_les_age_reg)
#### BPM internalizing ~ LES + age + (1|site) ####
 bpm_int_les_age_reg <- lmer(C_yr4_bpm_int ~ C_yr3_total_bad_le + C_yr4_age + (1|site),
 # bpm_int_les_age_reg <- lmer(C_log_yr4_bpm_int ~ C_log_yr3_total_bad_le + C_yr4_age + (1|site),
                             data=analysis_data)
summary(bpm_int_les_age_reg)
#### BPM externalizing ~ LES + age + (1|site) ####
 bpm_ext_les_age_reg <- lmer(C_yr4_bpm_ext ~ C_yr3_total_bad_le + C_yr4_age + (1|site),
 # bpm_ext_les_age_reg <- lmer(C_log_yr4_bpm_ext ~ C_log_yr3_total_bad_le + C_yr4_age + (1|site),
                             data=analysis_data)
summary(bpm_ext_les_age_reg)
#### CBCL internalizing ~ DERS + age + (1|site) ####
 cbcl_int_ders_age_reg <- lmer(C_yr4_cbcl_int ~ C_yr3_ders_total + C_yr4_age + (1|site),
 # cbcl_int_ders_age_reg <- lmer(C_log_yr4_cbcl_int ~ C_log_yr3_ders_total + C_yr4_age + (1|site),
                           data=analysis_data)
summary(cbcl_int_ders_age_reg)
#### CBCL externalizing ~ DERS + age + (1|site) ####
 cbcl_ext_ders_age_reg <- lmer(C_yr4_cbcl_ext ~ C_yr3_ders_total + C_yr4_age + (1|site),
 # cbcl_ext_ders_age_reg <- lmer(C_log_yr4_cbcl_ext ~ C_log_yr3_ders_total + C_yr4_age + (1|site),
                             data=analysis_data)
summary(cbcl_ext_ders_age_reg)
#### BPM internalizing ~ DERS + age + (1|site) ####
bpm_int_ders_age_reg <- lmer(C_yr4_bpm_int ~ C_yr3_ders_total + C_yr4_age + (1|site),
# bpm_int_ders_age_reg <- lmer(C_log_yr4_bpm_int ~ C_log_yr3_ders_total + C_yr4_age + (1|site),
                            data=analysis_data)
summary(bpm_int_ders_age_reg)
#### BPM externalizing ~ DERS + age + (1|site) ####
bpm_ext_ders_age_reg <- lmer(C_yr4_bpm_ext ~ C_yr3_ders_total + C_yr4_age + (1|site),
# bpm_ext_ders_age_reg <- lmer(C_log_yr4_bpm_ext ~ C_log_yr3_ders_total + C_yr4_age + (1|site),
                            data=analysis_data)
summary(bpm_ext_ders_age_reg)
#### CBCL internalizing ~ LES + DERS + age + (1|site) ####
cbcl_int_les_ders_age_reg <- lmer(C_yr4_cbcl_int ~ C_yr3_total_bad_le + C_yr3_ders_total + C_yr4_age + (1|site),
# cbcl_int_les_ders_age_reg <- lmer(C_log_yr4_cbcl_int ~ C_log_yr3_total_bad_le + C_log_yr3_ders_total + C_yr4_age + (1|site),
                              data=analysis_data)
summary(cbcl_int_les_ders_age_reg)
rsq(cbcl_int_les_ders_age_reg, adj=TRUE)
#### CBCL externalizing ~ LES + DERS + age + (1|site) ####
cbcl_ext_les_ders_age_reg <- lmer(C_yr4_cbcl_ext ~ C_yr3_total_bad_le + C_yr3_ders_total + C_yr4_age + (1|site),
# cbcl_ext_les_ders_age_reg <- lmer(C_log_yr4_cbcl_ext ~ C_log_yr3_total_bad_le + C_log_yr3_ders_total + C_yr4_age + (1|site),
                              data=analysis_data)
summary(cbcl_ext_les_ders_age_reg)
rsq(cbcl_ext_les_ders_age_reg, adj=TRUE)
#### BPM internalizing ~ LES + DERS + age + (1|site) ####
bpm_int_les_ders_age_reg <- lmer(C_yr4_bpm_int ~ C_yr3_total_bad_le + C_yr3_ders_total + C_yr4_age + (1|site),
# bpm_int_les_ders_age_reg <- lmer(C_log_yr4_bpm_int ~ C_log_yr3_total_bad_le + C_log_yr3_ders_total + C_yr4_age + (1|site),
                              data=analysis_data)
summary(bpm_int_les_ders_age_reg)
rsq(bpm_int_les_ders_age_reg, adj=TRUE)
#### BPM externalizing ~ LES + DERS + age + (1|site) ####
bpm_ext_les_ders_age_reg <- lmer(C_yr4_bpm_ext ~ C_yr3_total_bad_le + C_yr3_ders_total + C_yr4_age + (1|site),
# bpm_ext_les_ders_age_reg <- lmer(C_log_yr4_bpm_ext ~ C_log_yr3_total_bad_le + C_log_yr3_ders_total + C_yr4_age + (1|site),
                              data=analysis_data)
summary(bpm_ext_les_ders_age_reg)
rsq(bpm_ext_les_ders_age_reg, adj=TRUE)

## STEP TWO: MODERATING EFFECTS OF GENDER OR SEX ####
### Mixed effect linear regression to determine whether gender moderates ####
### relationship between DERS and LES, use age as fixed effect covariate and
### site as random intercept
#### DERS ~ LES*gender + age + (1|site) ####
ders_les_gendercisboy_reg <- lmer(C_yr4_ders_total ~ C_yr3_total_bad_le*genderid_refcisboy + C_yr4_age + (1|site),
# ders_les_gendercisboy_reg <- lmer(C_log_yr4_ders_total ~ C_log_yr3_total_bad_le*genderid_refcisboy + C_yr4_age + (1|site),
                         data=analysis_data)
summary(ders_les_gendercisboy_reg)
anova(ders_les_gendercisboy_reg)
rsq(ders_les_gendercisboy_reg,adj=TRUE)
### Mixed effect linear regression to determine whether sex moderates ####
### relationship between DERS and LES, use age as fixed effect covariate and
### site as random intercept
#### DERS ~ LES*sex + age + (1|site) ####
ders_les_sex_reg <- lmer(C_yr4_ders_total ~ C_yr3_total_bad_le*sex + C_yr4_age + (1|site),
# ders_les_sex_reg <- lmer(C_log_yr4_ders_total ~ C_log_yr4_total_bad_le*sex + C_yr4_age + (1|site),
                                  data=analysis_data)
summary(ders_les_sex_reg)
rsq(ders_les_sex_reg, adj=TRUE)
### Mixed effect linear regression to determine whether gender moderates ####
### relationship between LES, DERS, and CBCL or BPM using age as fixed effect
### covariate and site as random intercept
#### CBCL internalizing ~ LES*gender + DERS*gender + age + (1|site) ####
cbcl_int_les_gendercisboy_reg <- 
  lmer(C_yr4_cbcl_int ~ C_yr3_total_bad_le*genderid_refcisboy + C_yr3_ders_total*genderid_refcisboy +
  # lmer(C_log_yr4_cbcl_int ~ C_log_yr4_total_bad_le*genderid_refcisboy + C_log_yr4_ders_total*genderid_refcisboy +
       C_yr4_age + (1|site),  
       data=analysis_data, REML=FALSE)
summary(cbcl_int_les_gendercisboy_reg)
anova(cbcl_int_les_gendercisboy_reg)
rsq(cbcl_int_les_gendercisboy_reg,adj=TRUE)
#### CBCL externalizing ~ LES*gender + DERS*gender + age + (1|site) ####
cbcl_ext_les_gendercisboy_reg <- 
   lmer(C_yr4_cbcl_ext ~ C_yr3_total_bad_le*genderid_refcisboy + C_yr3_ders_total*genderid_refcisboy +
   # lmer(C_log_yr4_cbcl_ext ~ C_log_yr4_total_bad_le*genderid_refcisboy + C_log_yr4_ders_total*genderid_refcisboy +
         C_yr4_age + (1|site),  
       data=analysis_data, REML=FALSE)
summary(cbcl_ext_les_gendercisboy_reg)
anova(cbcl_ext_les_gendercisboy_reg)
rsq(cbcl_ext_les_gendercisboy_reg,adj=TRUE)
#### BPM internalizing ~ LES*gender + DERS*gender + age + (1|site) ####
bpm_int_les_gendercisboy_reg <- 
   lmer(C_yr4_bpm_int ~ C_yr3_total_bad_le*genderid_refcisboy + C_yr3_ders_total*genderid_refcisboy +
   # lmer(C_log_yr4_bpm_int ~ C_log_yr4_total_bad_le*genderid_refcisboy + C_log_yr4_ders_total*genderid_refcisboy +
         C_yr4_age + (1|site),  
       data=analysis_data, REML=FALSE)
summary(bpm_int_les_gendercisboy_reg)
anova(bpm_int_les_gendercisboy_reg)
rsq(bpm_int_les_gendercisboy_reg,adj=TRUE)

bpm_int_les_gendercisgirl_reg <- 
  lmer(C_yr4_bpm_int ~ C_yr3_total_bad_le*genderid_refcisgirl + C_yr3_ders_total*genderid_refcisgirl +
         # lmer(C_log_yr4_bpm_int ~ C_log_yr4_total_bad_le*genderid_refcisgirl + C_log_yr4_ders_total*genderid_refcisgirl +
         C_yr4_age + (1|site),  
       data=analysis_data, REML=FALSE)
summary(bpm_int_les_gendercisgirl_reg)
anova(bpm_int_les_gendercisgirl_reg)
rsq(bpm_int_les_gendercisgirl_reg,adj=TRUE)
#### BPM externalizing ~ LES*gender + DERS*gender + age + (1|site) ####
bpm_ext_les_gendercisboy_reg <- 
   lmer(C_yr4_bpm_ext ~ C_yr3_total_bad_le*genderid_refcisboy + C_yr3_ders_total*genderid_refcisboy +
   # lmer(C_log_yr4_bpm_ext ~ C_log_yr4_total_bad_le*genderid_refcisboy + C_log_yr4_ders_total*genderid_refcisboy +
         C_yr4_age + (1|site),  
       data=analysis_data, REML=FALSE)
summary(bpm_ext_les_gendercisboy_reg)
anova(bpm_ext_les_gendercisboy_reg)
rsq(bpm_ext_les_gendercisboy_reg,adj=TRUE)

### Mixed effect linear regression to determine whether sex moderates ####
### relationship between LES, DERS, and CBCL or BPM using age as fixed effect
### covariate and site as random intercept
#### CBCL internalizing ~ LES*sex + DERS*sex + age + (1|site) ####
cbcl_int_les_sex_reg <- 
  lmer(C_yr4_cbcl_int ~ C_yr3_total_bad_le*sex + C_yr3_ders_total*sex + C_yr4_age + (1|site),
  # lmer(C_log_yr4_cbcl_int ~ C_log_yr4_total_bad_le*sex + C_log_yr4_ders_total*sex + C_yr4_age + (1|site),
       data=analysis_data, REML=FALSE)
summary(cbcl_int_les_sex_reg)
rsq(cbcl_int_les_sex_reg,adj=TRUE)
#### CBCL externalizing ~ LES*sex + DERS*sex + age + (1|site) ####
cbcl_ext_les_sex_reg <-
   lmer(C_yr4_cbcl_ext ~ C_yr3_total_bad_le*sex + C_yr3_ders_total*sex + C_yr4_age + (1|site),
   # lmer(C_log_yr4_cbcl_ext ~ C_log_yr4_total_bad_le*sex + C_log_yr4_ders_total*sex + C_yr4_age + (1|site),
       data=analysis_data, REML=FALSE)
summary(cbcl_ext_les_sex_reg)
rsq(cbcl_ext_les_sex_reg,adj=TRUE)
#### BPM internalizing ~ LES*sex + DERS*sex + age + (1|site) ####
bpm_int_les_sex_reg <- 
  lmer(C_yr4_bpm_int ~ C_yr3_total_bad_le*sex + C_yr3_ders_total*sex + C_yr4_age + (1|site),
  # lmer(C_log_yr4_bpm_int ~ C_log_yr4_total_bad_le*sex + C_log_yr4_ders_total*sex + C_yr4_age + (1|site),
       data=analysis_data, REML=FALSE)
summary(bpm_int_les_sex_reg)
rsq(bpm_int_les_sex_reg,adj=TRUE)
#### BPM externalizing ~ LES*sex + DERS*sex + age + (1|site) ####
bpm_ext_les_sex_reg <-
   lmer(C_yr4_bpm_ext ~ C_yr3_total_bad_le*sex + C_yr3_ders_total*sex + C_yr4_age + (1|site),
   # lmer(C_log_yr4_bpm_ext ~ C_log_yr4_total_bad_le*sex + C_log_yr4_ders_total*sex + C_yr4_age + (1|site),
       data=analysis_data, REML=FALSE)
summary(bpm_ext_les_sex_reg)
rsq(bpm_ext_les_sex_reg,adj=TRUE)

## STEP THREE: MEDIATING EFFECT OF ER ON CBCL OR BPM ~ LES #### 


### Simple mediation model for CBCL internalizing ####
cbclint_model <-
' # direct effect
      C_yr4_cbcl_int~ c*C_yr3_total_bad_le + C_yr4_age
    # mediator
      C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
    # indirect effect
      C_yr4_cbcl_int~ b*C_yr3_ders_total
    # indirect effect (a*b)
      ab := a*b
    # total effect
      total := c + (a*b)'
# cbclint_model <-
#   ' # direct effect
#         C_log_yr4_cbcl_int~ c*C_log_yr3_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr3_ders_total ~ a*C_log_yr3_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_cbcl_int~ b*C_log_yr3_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)'
cbclint_model <- sem(cbclint_model, 
                             data = analysis_data, 
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
      C_yr4_cbcl_ext~ c*C_yr3_total_bad_le + C_yr4_age
    # mediator
      C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
    # indirect effect
      C_yr4_cbcl_ext~ b*C_yr3_ders_total
    # indirect effect (a*b)
      ab := a*b
    # total effect
      total := c + (a*b)'
# cbclext_model <-
#   ' # direct effect
#         C_log_yr4_cbcl_ext~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_cbcl_ext~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)'
cbclext_model <- sem(cbclext_model, 
                     data = analysis_data, 
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
        C_yr4_bpm_int~ c*C_yr3_total_bad_le + C_yr4_age
      # mediator
        C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_int~ b*C_yr3_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)'
# bpmint_model <-
#   ' # direct effect
#         C_log_yr4_bpm_int~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_bpm_int~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)'
bpmint_model <- sem(bpmint_model, 
                     data = analysis_data, 
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
        C_yr4_bpm_ext~ c*C_yr3_total_bad_le + C_yr4_age
      # mediator
        C_yr3_ders_total ~ a*C_yr3_total_bad_le  + C_yr4_age
      # indirect effect
        C_yr4_bpm_ext~ b*C_yr3_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)'
# bpmext_model <-
#   ' # direct effect
#         C_log_yr4_bpm_ext~ c*C_log_yr4_total_bad_le + C_yr4_age
#       # mediator
#         C_log_yr4_ders_total ~ a*C_log_yr4_total_bad_le  + C_yr4_age
#       # indirect effect
#         C_log_yr4_bpm_ext~ b*C_log_yr4_ders_total
#       # indirect effect (a*b)
#         ab := a*b
#       # total effect
#         total := c + (a*b)'
bpmext_model <- sem(bpmext_model, 
                     data = analysis_data, 
                     meanstructure = TRUE,
                     se = "robust.cluster",
                     # group = "genderid",
                     cluster = "site")
summary(bpmext_model, fit.measures=T, 
        standardized=F, ci=TRUE, rsquare=TRUE)
parameterEstimates(bpmext_model, boot.ci.type = "bca.simple")


## STEP FOUR: MODERATING EFFECT OF GENDER OR SEX ON MEDIATION ####

### Moderated mediation model (Hayes model 15) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### internalizing 
cbcl_int_gender_model15 <- PROCESS(
  analysis_data,
  y = "C_yr4_cbcl_int",
  x = "C_yr3_total_bad_le",
  meds = c("C_yr3_ders_total"),
  # y = "C_log_yr4_cbcl_int",
  # x = "C_log_yr3_total_bad_le",
  # meds = c("C_log_yr3_ders_total"),
  mods = c("genderid_refcisboy"),
  covs = c("C_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

### Moderated mediation model (Hayes model 15) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### externalizing 
cbcl_ext_gender_model15 <- PROCESS(
  analysis_data,
  y = "C_yr4_cbcl_ext",
  x = "C_yr3_total_bad_le",
  meds = c("C_yr3_ders_total"),
  # y = "C_log_yr4_cbcl_ext",
  # x = "C_log_yr3_total_bad_le",
  # meds = c("C_log_yr3_ders_total"),
  mods = c("genderid_refcisboy"),
  covs = c("C_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

### Moderated mediation model (Hayes model 15) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and BPM
### internalizing 
bpm_int_gender_model15 <- PROCESS(
  analysis_data,
  y = "C_yr4_bpm_int",
  x = "C_yr3_total_bad_le",
  meds = c("C_yr3_ders_total"),
  # y = "C_log_yr4_bpm_int",
  # x = "C_log_yr3_total_bad_le",
  # meds = c("C_log_yr3_ders_total"),
  mods = c("genderid_refcisboy"),
  covs = c("C_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

# for untransformed data:
# are the conditional direct effects [c'] of X on Y significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.19444 -0.59639 )/(sqrt((0.06742^2)+(0.06987)^2)) = -4.139796
# cis boy (beta1) vs gd (beta2):
#     Z = (0.19444 -0.72997)/(sqrt((0.06742^2)+(0.17668)^2)) = -2.831896
# cis girl (beta1) vs gd (beta2):
#     Z = (0.59639 -0.72997)/(sqrt((0.06987^2)+(0.17668)^2)) = -0.7030756
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -4.139796, so pnorm(-abs(-4.139796))*2 = 0.007837924
# cis boy vs gd: Z = -2.831896, so pnorm(-abs(-2.831896))*2 = 0.03444077
# cis girl vs gd: Z = -0.7030756, so pnorm(-abs(-0.7030756))*2 = 0.4750891
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.00003476148,0.00462729,0.4820086),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p < .001
# cis boy vs gd: p = .007
# cis girl vs gd: .482

# for untransformed data:
# are the indirect effects [ab] of X on Y via M significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.03178   -0.03062   )/(sqrt((0.00920^2)+(0.00958)^2)) = 0.08733504
# cis boy (beta1) vs gd (beta2):
#     Z = (0.03178   -0.04321  )/(sqrt((0.00920^2)+(0.02450)^2)) = -0.4367529
# cis girl (beta1) vs gd (beta2):
#     Z = (0.03062   -0.04321  )/(sqrt((0.00958^2)+(0.02450)^2)) = -0.4785909
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = 0.08733504, so pnorm(-abs(0.08733504))*2 = 0.007837924
# cis boy vs gd: Z = -0.4367529, so pnorm(-abs(-0.4367529))*2 = 0.03444077
# cis girl vs gd: Z = -0.4785909, so pnorm(-abs(-0.4785909))*2 = 0.4750891
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.9304052,0.6622906,0.6322297),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p = .930
# cis boy vs gd: p = .930
# cis girl vs gd: .930

# for untransformed data:
# are the effects [b] of M on Y significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.04281    -0.04124    )/(sqrt((0.00746^2)+(0.00867)^2)) = 0.1372656
# cis boy (beta1) vs gd (beta2):
#     Z = (0.04281    -0.05821   )/(sqrt((0.00746^2)+(0.02188)^2)) = -0.6661824
# cis girl (beta1) vs gd (beta2):
#     Z = (0.04124    -0.05821   )/(sqrt((0.00867^2)+(0.02188)^2)) = -0.7210491
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = 0.1372656, so pnorm(-abs(0.1372656))*2 = 0.8908209
# cis boy vs gd: Z = -0.6661824, so pnorm(-abs(-0.6661824))*2 = 0.5052945
# cis girl vs gd: Z = -0.7210491, so pnorm(-abs(-0.7210491))*2 = 0.4708793
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.8908209,0.5052945,0.4708793),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p = .891
# cis boy vs gd: p = .758
# cis girl vs gd: .758

# for log transformed data:
# are the conditional direct effects [c'] of X on Y significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.01070  -0.03186  )/(sqrt((0.00363^2)+(0.00388)^2)) = -3.982449
# cis boy (beta1) vs gd (beta2):
#     Z = (0.01070  -0.03747 )/(sqrt((0.00363^2)+(0.01158)^2)) = -2.205903
# cis girl (beta1) vs gd (beta2):
#     Z = (0.03186  -0.03747 )/(sqrt((0.00388^2)+(0.01158)^2)) = -0.4593567
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -3.982449, so pnorm(-abs(-3.982449))*2 = 0.00006820876
# cis boy vs gd: Z = -2.205903, so pnorm(-abs(-2.205903))*2 = 0.02739079
# cis girl vs gd: Z = -0.4593567, so pnorm(-abs(-0.4593567))*2 = 0.645978
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.00006820876,0.02739079,0.645978),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p < .001
# cis boy vs gd: p = .041
# cis girl vs gd: .646

# for log transformed data:
# are the indirect effects [ab] of X on Y via M significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.00111    -0.00114    )/(sqrt((0.00042^2)+(0.00043)^2)) = -0.04990997
# cis boy (beta1) vs gd (beta2):
#     Z = (0.00111    -0.00145   )/(sqrt((0.00042^2)+(0.00091)^2)) = -0.3392376
# cis girl (beta1) vs gd (beta2):
#     Z = (0.00114    -0.00145   )/(sqrt((0.00043^2)+(0.00091)^2)) = -0.3080044
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -0.04990997, so pnorm(-abs(-0.04990997))*2 = 0.9601941
# cis boy vs gd: Z = -0.3392376, so pnorm(-abs(-0.3392376))*2 = 0.7344307
# cis girl vs gd: Z = -0.3080044, so pnorm(-abs(-0.3080044))*2 = 0.758079
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.9601941,0.7344307,0.758079),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p = .960
# cis boy vs gd: p = .960
# cis girl vs gd: .960

# for log transformed data:
# are the effects [b] of M on Y significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.04444     -0.04565    )/(sqrt((0.00753^2)+(0.00851)^2)) = -0.1064846
# cis boy (beta1) vs gd (beta2):
#     Z = (0.04444     -0.05834    )/(sqrt((0.00753^2)+(0.02303)^2)) = -0.5736744
# cis girl (beta1) vs gd (beta2):
#     Z = (0.04565    -0.05834    )/(sqrt((0.00851^2)+(0.02303)^2)) = -0.516862
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -0.1064846, so pnorm(-abs(-0.1064846))*2 = 0.9151979
# cis boy vs gd: Z = -0.5736744, so pnorm(-abs(-0.5736744))*2 = 0.5661882
# cis girl vs gd: Z = -0.516862, so pnorm(-abs(-0.516862))*2 = 0.6052525
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.9151979,0.5661882,0.6052525),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p = .915
# cis boy vs gd: p = .908
# cis girl vs gd: .908

### Moderated mediation model (Hayes model 15) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and BPM
### externalizing 
bpm_ext_gender_model15 <- PROCESS(
  analysis_data,
  y = "C_yr4_bpm_ext",
  x = "C_yr3_total_bad_le",
  meds = c("C_yr3_ders_total"),
  # y = "C_log_yr4_bpm_ext",
  # x = "C_log_yr3_total_bad_le",
  # meds = c("C_log_yr3_ders_total"),
  mods = c("genderid_refcisboy"),
  covs = c("C_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

### Moderated mediation model (Hayes model 15) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### internalizing 
cbcl_int_sex_model15 <- PROCESS(
  analysis_data,
  y = "C_yr4_cbcl_int",
  x = "C_yr3_total_bad_le",
  meds = c("C_yr3_ders_total"),
  # y = "C_log_yr4_cbcl_int",
  # x = "C_log_yr3_total_bad_le",
  # meds = c("C_log_yr3_ders_total"),
  mods = c("sex"),
  covs = c("C_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

### Moderated mediation model (Hayes model 15) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### externalizing 
cbcl_ext_sex_model15 <- PROCESS(
  analysis_data,
  y = "C_yr4_cbcl_ext",
  x = "C_yr3_total_bad_le",
  meds = c("C_yr3_ders_total"),
  # y = "C_log_yr4_cbcl_ext",
  # x = "C_log_yr3_total_bad_le",
  # meds = c("C_log_yr3_ders_total"),
  mods = c("sex"),
  covs = c("C_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

### Moderated mediation model (Hayes model 15) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and BPM
### internalizing 
bpm_int_sex_model15 <- PROCESS(
  analysis_data,
  y = "C_yr4_bpm_int",
  x = "C_yr3_total_bad_le",
  meds = c("C_yr3_ders_total"),
  # y = "C_log_yr4_bpm_int",
  # x = "C_log_yr3_total_bad_le",
  # meds = c("C_log_yr3_ders_total"),
  mods = c("sex"),
  covs = c("C_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

# for untransformed data:
# are the conditional direct effects [c'] of X on Y significantly 
# different? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.20097  -0.68083 )/(sqrt((0.06889^2)+(0.06677)^2)) = -5.001781
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -5.001781, so pnorm(-abs(-5.001781))*2 = 5.68031e-07, or
# < 0.001 rounded to three places

# for untransformed data:
# are the conditional indirect effects [ab] of X through M on Y significantly 
# different? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.03300  -0.04107  )/(sqrt((0.00904^2)+(0.01133)^2)) = -0.556763
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -0.556763, so pnorm(-abs(-0.556763))*2 = 0.5776894, or
# 0.578 rounded to three places

# for untransformed data:
# are the conditional effects [b] of M on Y significantly 
# different? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.04309   -0.05363  )/(sqrt((0.00760^2)+(0.00826)^2)) = -0.939024
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -0.939024, so pnorm(-abs(-0.939024))*2 = 0.3477184, or
# 0.348 rounded to three places

# for log transformed data:
# are the conditional direct effects [c'] of X on Y significantly 
# different? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.01133   -0.03633 )/(sqrt((0.00370^2)+(0.00377)^2)) = -4.732769
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -4.732769, so pnorm(-abs(-4.732769))*2 = 0.000002214775, or
# < 0.001 rounded to three places

# for log transformed data:
# are the conditional indirect effects [ab] of X through M on Y significantly 
# different? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.00119   -0.00151   )/(sqrt((0.00042^2)+(0.00052)^2)) = -0.478733
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -0.478733, so pnorm(-abs(-0.478733))*2 = 0.6321286, or
# 0.632 rounded to three places

# for log transformed data:
# are the conditional effects [b] of M on Y significantly 
# different? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.04481    -0.05692  )/(sqrt((0.00767^2)+(0.00817)^2)) = -1.080657
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -1.080657, so pnorm(-abs(-1.080657))*2 = 0.2798497, or
# 0.280 rounded to three places

### Moderated mediation model (Hayes model 15) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and BPM
### externalizing 
bpm_ext_sex_model15 <- PROCESS(
  analysis_data,
  y = "C_yr4_bpm_ext",
  x = "C_yr3_total_bad_le",
  meds = c("C_yr3_ders_total"),
  # y = "C_log_yr4_bpm_ext",
  # x = "C_log_yr3_total_bad_le",
  # meds = c("C_log_yr3_ders_total"),
  mods = c("sex"),
  covs = c("C_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    "x-y",
    "m-y"
    # "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)
