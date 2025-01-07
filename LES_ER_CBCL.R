### Set working directory ####
setwd("C:/Users/Kate Scheuer/OneDrive - UW/Desktop/Lab/aces_emotionreg_cbcl")

### Load libraries ####
library(tidyverse) # for dplyr and associated functions
# /!\ added FSA to run post-hoc Dunn tests after Kruskal-Wallis tests
library(FSA) #for function Dunn tests ie dunnTest()
# /!\ added rsq to get variance explained for mixed effect linear regression models
library(nortest) #for function Anderson-Darling tests ie ad.test
library(psych) #for correlation matrices ie corr.test()
library(lavaan) #for SEM
# /!\ added bruceR package for conditional process modeling used for moderated
# /!\ mediation only (still using lavaan package for sem for basic mediation)
library(bruceR) #for conditional process modeling

# /!\ got sick of scientific notation when working with very small p values
### Prevent use of scientific notation ####
options(scipen=999)

## PREP DATA ####

### Read in raw data ####
#### Gender data ####
# /!\ switched from read.csv to read_csv, no need for "header = T"
gish_y_gi <- read_csv("data/gish_y_gi.csv")

#### DERS-P for emotion regulation ####
# /!\ switched from read.csv to read_csv, no need for "header = T"
mh_p_ders <- read_csv("data/mh_p_ders.csv")

#### CBCL for parent-report psychopathology symptoms ####
# /!\ switched from read.csv to read_csv, no need for "header = T"
mh_p_cbcl <- read_csv("data/mh_p_cbcl.csv")

#### BPM for youth-report psychopathology symptoms ####
# /!\ added youth-report measure of internalizing and externalizing ie BPM-Y
# /!\ in addition to using parent-report measure ie CBCL
mh_y_bpm <- read_csv("data/mh_y_bpm.csv")

#### Longitudinal tracking data ####
# /!\ switched from read.csv to read_csv, no need for "header = T"
abcd_y_lt <- read_csv("data/abcd_y_lt.csv")

#### LES (youth-reported) ####
#### LES for exposure to negative life events
# /!\ switched from read.csv to read_csv, no need for "header = T"
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
# /!\ instead of using one data frame for gender analysis and a separate one
# /!\ for sex data (because some youth did provide gender but said "don't know"
# /!\ or "refuse" for sex), decided to use one combined data frame but have one
# /!\ column for responses including "don't know" and "refuse" ie "sex_details"
# /!\ column created on line 61 above as well as another column called "sex"
# /!\ with NAs for "don't know" and "refuse" created below
  # set "don't know" or "refuse" to be NA for sex
  mutate(sex = case_when(
    sex_details=="male" ~ "male",
    sex_details=="female" ~ "female",
    sex_details=="dont_know" ~ NA_character_,
    sex_details=="refuse" ~ NA_character_
  )) %>%
# /!\ set males to be reference level for sex to make interpretation easier
  # make "male" reference level for sex
  mutate(sex = relevel(as.factor(sex), ref="male")) %>%
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
# /!\ added a column for more granular gender identity groups (trans boys, trans
# /!\ girls, nonbinary youth) which are combined into "GD" group for main analysis
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
# /!\ combine trans boys, trans girls, and nonbinary youth into one "GD" group
# /!\ due to small sample size
  # combine all not cis groups due to sample size to make one gender diverse group (gd)
  mutate(genderid = case_when(
                      gender_details=="trans_boy" ~ "gd",
                      gender_details=="trans_girl" ~ "gd",
                      gender_details=="nb" ~ "gd",
                      gender_details=="cis_boy" ~ "cis_boy",
                      gender_details=="cis_girl" ~ "cis_girl"
                      )) %>%
# /!\ don't select columns that aren't actually used in analysis
  # keep only columns relevant to analysis
  select(src_subject_id,eventname,
         sex,sex_details,
         genderid,gender_details
         ) %>%
  # remove subjects who refused to answer and/or did not understand gender
  # or trans questions. Before this step, n should be 15064. After this step,
  # n should be 14495.
  filter(genderid!="refuse",
         genderid!="dont_understand") %>%
# /!\ make new column that is the same as genderid but is now a factor and is
# /!\ named to clearly indicate cis boys are reference level
  # make genderid a factor (automatically uses cisboy as reference)
  mutate(genderid_refcisboy = as.factor(genderid)) %>%
# /!\ make new column that is the same as genderid_refcisboy but uses cis girls
# /!\ as reference level rather than cis boys. this will become relevant when
# /!\ trying to get differences between all three gender groups in the basic
# /!\ moderation analysis below (step two)
  # make another column that uses cis_girl as reference instead
  mutate(genderid_refcisgirl = relevel(genderid_refcisboy,"cis_girl"))

#### Count number of subjects per gender group per data collection year ####
genderdata %>% 
  group_by(eventname) %>% 
  count(genderid)

### Prepare CBCL data for analysis ####
cbcldata <- mh_p_cbcl %>%
# /!\ only using internalizing and externalizing subscales from CBCL (no longer 
# /!\ also using total problems subscale per Lili's suggestion)
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

# /!\ Load BPM-Y ie youth-report data and prep for analysis
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
# /!\ more explicitly spelled out reverse scoring scheme item below
  # add column to reverse score "my child pays attention to how he/she feels"
  mutate(rev_ders_attn_awareness_p = 
           case_when(ders_attn_awareness_p == 1 ~ 5,
                     ders_attn_awareness_p == 2 ~ 4,
                     ders_attn_awareness_p == 3 ~ 3,
                     ders_attn_awareness_p == 4 ~ 2,
                     ders_attn_awareness_p == 5 ~ 1)) %>%
# /!\ more explicitly spelled out reverse scoring scheme item below
  # add column to reverse score "my child is attentive to his/her feelings"
  mutate(rev_ders_feelings_attentive_p = 
           case_when(ders_feelings_attentive_p == 1 ~ 5,
                     ders_feelings_attentive_p == 2 ~ 4,
                     ders_feelings_attentive_p == 3 ~ 3,
                     ders_feelings_attentive_p == 4 ~ 2,
                     ders_feelings_attentive_p == 5 ~ 1)) %>%
# /!\ more explicitly spelled out reverse scoring scheme item below
  # add column to reverse score "my child cares about what he/she is feeling"
  mutate(rev_ders_feelings_care_p = 
           case_when(ders_feelings_care_p == 1 ~ 5,
                     ders_feelings_care_p == 2 ~ 4,
                     ders_feelings_care_p == 3 ~ 3,
                     ders_feelings_care_p == 4 ~ 2,
                     ders_feelings_care_p == 5 ~ 1)) %>%
# /!\ more explicitly spelled out reverse scoring scheme item below
  # add column to reverse score "when my child is upset, he/she acknowledges
  # his/her emotions"
  mutate(rev_ders_upset_ack_p = 
           case_when(ders_upset_ack_p == 1 ~ 5,
                     ders_upset_ack_p == 2 ~ 4,
                     ders_upset_ack_p == 3 ~ 3,
                     ders_upset_ack_p == 4 ~ 2,
                     ders_upset_ack_p == 5 ~ 1)) %>%
# /!\ more explicitly spelled out reverse scoring scheme item below
  # add column to reverse score "my child is clear about his/her feelings"
  mutate(rev_ders_clear_feelings_p = 
           case_when(ders_clear_feelings_p == 1 ~ 5,
                     ders_clear_feelings_p == 2 ~ 4,
                     ders_clear_feelings_p == 3 ~ 3,
                     ders_clear_feelings_p == 4 ~ 2,
                     ders_clear_feelings_p == 5 ~ 1)) %>%
# /!\ more explicitly spelled out reverse scoring scheme item below
  # add column to reverse score "my child knows exactly how he/she is feeling"
  mutate(rev_ders_feelings_know_p = 
           case_when(ders_feelings_know_p == 1 ~ 5,
                     ders_feelings_know_p == 2 ~ 4,
                     ders_feelings_know_p == 3 ~ 3,
                     ders_feelings_know_p == 4 ~ 2,
                     ders_feelings_know_p == 5 ~ 1)) %>%
# /!\ more explicitly spelled out reverse scoring scheme item below
  # add column to reverse score "when my child is upset, he/she feels like
  # he/she can remain in control of his/her behaviors"
    mutate(rev_ders_upset_behavior_control_p = 
             case_when(ders_upset_behavior_control_p == 1 ~ 5,
                       ders_upset_behavior_control_p == 2 ~ 4,
                       ders_upset_behavior_control_p == 3 ~ 3,
                       ders_upset_behavior_control_p == 4 ~ 2,
                       ders_upset_behavior_control_p == 5 ~ 1)) %>%
# /!\ more explicitly spelled out reverse scoring scheme item below
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
# /!\ add visual checkpoint to see if there are NA or errors. this is probably
# /!\ a good place for unit testing (I think?).
# see all unique values (can visually check for NA or errors)
map(dersdata,unique)
# /!\ add one more visual checkpoint in case eye didn't catch an NA on the last
# /!\ step. again probably a good place for unit testing - I should probably 
# /!\ learn how to do that
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
# /!\ n for alldata should be 14495
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
# /!\ Kate suggested leaving variables on original scale (not Z scoring) but
# /!\ conditional process modeling command PROCESS() from bruceR package said
# /!\ it was important for interpreting interactions ie moderation to have
# /!\ variables centered, so I grand-mean centered continuous variables. Not
# /!\ sure if this is the right move or not
  # Grand-mean center continuous variables
  mutate(across(
    c(age, ders_total, total_bad_le, 
      cbcl_int, cbcl_ext,
      bpm_int, bpm_ext,
      log_ders_total, log_total_bad_le, 
      log_cbcl_int, log_cbcl_ext,
      log_bpm_int, log_bpm_ext),
    # Z-score continuous variables
    ~ as.numeric(scale(.,center=TRUE,scale=TRUE)),
    # Grand-mean center continuous variables
    # ~ as.numeric(misty::center(.,type = c("CGM"))),
    .names = "Z_{.col}"
  )) %>%
  # make genderid, sex, and site factors rather than characters
  mutate(across(c(genderid, sex, site), as.factor))

### Get general overview of all data ####
#### Get raw number and percentage for each gender group and data collection year ####
alldata %>% 
  group_by(eventname) %>% 
  count(genderid) %>% 
  mutate(percentage = n / sum(n) * 100)

#### Create separate data frame for just data from year 4 follow-up visit ####
# /!\ n for yr4data should be 4612
yr4data <- alldata %>% filter(eventname=="4_year_follow_up_y_arm_1")

#### Create separate data frame for just data from year 3 follow-up visit ####
# /!\ n for yr3data should be 9883
yr3data <- alldata %>% filter(eventname=="3_year_follow_up_y_arm_1")

# /!\ per Kate's suggestion, figure out how many youth changed their gender from
# /!\ year three to year four (as justification for using data from year four
# /!\ follow-up visit rather than year three even though not all of year four
# /!\ has been released yet)
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

# /!\ Put everything into one streamlined data frame for all subsequent analysis
# /!\ NOTE: main analysis now uses LES (ie negative life event) and DERS-P (ie 
# /!\ emotion regulation difficulties) data from year three visits and uses age, 
# /!\ gender, and CBCL (ie parent-report internalizing and externalizing) and 
# /!\ BPM-Y (ie youth-report internalizing and externalizing) from year four 
# /!\ visits to take advantage of longitudinal structure of ABCD data and to 
# /!\ more robustly show mediation (which technically requires temporal separation)
### Combine year 4 and year 3 data to have option to use year 3 as x variable ####
#### Make combined year 3 and year 4 data for analysis ####
analysis_data <- yr4data %>%
# /!\ starting with year four data, mark columns as coming from year four
# /!\ because columns from year three will have the same names. There has to be
# /!\ a more efficient way to do this, but I don't know what it is (and I'm kind
# /!\ of afraid to tinker with a new function at this point, but maybe I just
# /!\ need to get over that lol)
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
    Z_yr4_age = Z_age
  ) %>%
# /!\ add in year three data. as far as I know, data needs to be in this format
# /!\ ie wide for linear regression commands and other analysis commands to work
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
                     Z_log_total_bad_le,Z_log_ders_total,
                     Z_log_cbcl_int,Z_log_cbcl_ext,
                     Z_log_bpm_int,Z_log_bpm_ext
                   )),
            by="src_subject_id") %>%
# /!\ make year three columns all include year three in name to be more clear
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
    Z_log_yr3_total_bad_le = Z_log_total_bad_le,
    Z_log_yr3_ders_total = Z_log_ders_total,
    Z_log_yr3_cbcl_int = Z_log_cbcl_int,
    Z_log_yr3_cbcl_ext = Z_log_cbcl_ext,
    Z_log_yr3_bpm_int = Z_log_bpm_int,
    Z_log_yr3_bpm_ext = Z_log_bpm_ext,
  ) %>%
# /!\ only keep subjects with year three LES, year three DERS-P, year four BPM-Y,
# /!\ and year four CBCL data. okay for variables to be missing in other years
# /!\ eg okay if year *four* LES data is NA
  # remove subjects without LES or DERS in year 3 or without CBCL or BPM in year 4
# /!\ before this step, n should be 4612, and after this step, n should be 3763
  filter(!is.na(yr3_ders_total), !is.na(yr3_total_bad_le),
         !is.na(yr4_cbcl_int), !is.na(yr4_cbcl_ext),
         !is.na(yr4_bpm_int), !is.na(yr4_bpm_ext))

#### See type of each column ####
# /!\ visual check to make sure type of each column is correct
str(analysis_data)

#### See all unique values for each column ####
# /!\ visual check to make sure everything looks okay ie no NA in columns used
# /!\ in analyses (okay for NA to be in some columns, see note on lines 510-511),
# /!\ check for min/max and general reasonableness of values
map(analysis_data,unique)

# /!\ find number of subjects with more granular gender groups ie trans boy,
# /!\ trans girl, nonbinary, cis boy, or cis girl
#### Determine how many subjects in each more detailed gender group ####
analysis_data %>%
  group_by(gender_details) %>%
  count()

# /!\ find number of subjects in each gender group and sex group. note that 
# /!\ most GD youth are assigned female at birth
#### Determine how many subjects in each combination of gender and sex group ####
analysis_data %>%
  group_by(genderid, sex) %>%
  count()

## BASIC STATS ####

### Get summary stats for each variable ####
sumstats <- 
  analysis_data %>%
  group_by(genderid) %>%
# /!\ next six lines are all options to comment in or out depending on what 
# /!\ specific summary stats are needed eg need stats for broader gender groups
# /!\ (cis boy, cis girl, GD) for one table, need stats for more granular gender
# /!\ groups (trans boy, trans girl, nonbinary, cis boy, cis girl) for different
# /!\ table, need stats for sex groups for another table, etc
  # group_by(gender_details) %>%
  # filter(!is.na(sex)) %>%
  # group_by(sex) %>%
  # to get number of people who answered don't know or refuse for sex
  # group_by(sex_details) %>%
  # count()
# /!\ summarise works much nicer than "summary" function and gave me more
# /!\ flexibility in what I wanted in output compared to "describe" function 
# /!\ (but may I was just using describe wrong)
  summarise(
    n = n(),
    across(
      c("yr4_age","yr3_total_bad_le","yr3_ders_total",
        "yr4_bpm_int","yr4_cbcl_int","yr4_bpm_ext","yr4_cbcl_ext"),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
# /!\ lines 575 to 586 are just so output on line 587 is in nice format to be
# /!\ transferred to table in manuscript. there are R packages for making 
# /!\ publication-ready tables which I should probably eventually learn to use
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
# /!\ need to round now that we're not using scientific notation
round(corrmat$p.adj,5)

## PLOTS ####

##### Graph of LES vs DERS by gender ####
# /!\ make scatterplot of year three LES vs year three DERS-P, which is a little
# /!\ weird because LES is technically count data and should be in a bar graph,
# /!\ but Jen suggested I use a scatterplot with trend lines, and I agree with
# /!\ her that it looks better
ggplot(analysis_data, 
       aes(x=yr3_total_bad_le,y=yr3_ders_total, fill=genderid)) +
  geom_point(aes(color=genderid, shape = genderid),size=2) +
# /!\ add regression/trend lines for each gender group
  geom_smooth(aes(linetype=genderid),method="lm",color="black", se=FALSE) +
  scale_linetype_manual(values = c("cis_boy"="31",
                                   "cis_girl"="11",
                                   "gd"="solid")) +
  scale_shape_manual(values=c(21,22,23)) +
  scale_colour_grey(start=0.9,end=0) +
  scale_fill_manual(values=c("grey40","grey85","black")) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0,16, by = 1),
                     limits=c(-.5,16.5)) +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(20,130,10),
                     limits = c(25,131)) +
  guides(
    shape = guide_legend(override.aes = list(size = 3)),
    line = guide_legend(override.aes = list(size = 2))
  ) +
  theme_classic() +
# /!\ include line below only when saving graph to be used exclusively for
# /!\ legend because the line below increases the width of the legend so it's
# /!\ easier to see the different line patterns between groups
  theme(legend.key.width = unit(0.5, "in"))
# Save graph
# /!\ to save one figure with legend to be cropped and used for all panels in
# /!\ figures 1 and 2
# ggsave("scatterplot_legend.tiff",width=9,height=6,unit="in",path="figures")
# /!\ comment out line 680 above which increases width of legend and save as 
# /!\ normal for figure 2
# ggsave("les_vs_ders_scatter_bygender.tiff",width=8.5,height=6,unit="in",path="figures")

# /!\ make scatterplot of year three LES vs year four CBCL scores based
# /!\ on gender groups for figure 1
##### Graphs of LES vs CBCL by gender ####
outcome_list <- c("yr4_cbcl_int","yr4_cbcl_ext")
cbcl_les_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  cbcl_les_plot <-
    ggplot(aes(x=yr3_total_bad_le,y=.data[[outcome]],
               fill = genderid
    ),data=analysis_data) +
    geom_point(aes(color=genderid, shape = genderid), size=2) +
    geom_smooth(aes(linetype=genderid),method="lm",color="black", se=FALSE) +
    scale_linetype_manual(values = c("cis_boy"="31",
                                     "cis_girl"="11",
                                     "gd"="solid")) +
    scale_shape_manual(values=c(21,22,23)) +
    scale_colour_grey(start=0.9,end=0) +
    scale_fill_manual(values=c("grey40","grey85","black")) +
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(0,16, by = 1),
                       limits=c(-.5,16.5)) +
    scale_y_continuous(expand = c(0,0),
                       breaks=seq(30,90,by=10),
                       limits=c(30,90)) +
    theme_classic()
  cbcl_les_plot_list[[outcome]] <- cbcl_les_plot
  # Save plot
  # ggsave(paste0("les_vs_",outcome,"_bygender.tiff"),
  # width=8.5,height=6,units = "in",path="figures")
}

# /!\ make scatterplot of year three LES vs year four BPM-Y scores based
# /!\ on gender groups for figure 1
##### Graphs of LES vs CBCL by gender ####
outcome_list <- c("yr4_bpm_int","yr4_bpm_ext")
bpm_les_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  bpm_les_plot <-
    ggplot(aes(x=yr3_total_bad_le,y=.data[[outcome]],
               fill = genderid
    ),data=analysis_data) +
    geom_point(aes(color=genderid, shape = genderid),size=2) +
    geom_smooth(aes(linetype=genderid),method="lm",color="black", se=FALSE) +
    scale_linetype_manual(values = c("cis_boy"="31",
                                     "cis_girl"="11",
                                     "gd"="solid")) +
    scale_shape_manual(values=c(21,22,23)) +
    scale_colour_grey(start=0.9,end=0) +
    scale_fill_manual(values=c("grey40","grey85","black")) +
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(0,16, by = 1),
                       limits=c(-.5,16.5)) +
    scale_y_continuous(expand = c(0,0),
                       breaks=seq(40,75,by=5),
                       limits=c(49,76)) +
    theme_classic()
  bpm_les_plot_list[[outcome]] <- bpm_les_plot
  # Save plot
  # ggsave(paste0("les_vs_",outcome,"_bygender.tiff"),
  # width=8.5,height=6,units = "in",path="figures")
}

##### Graphs of DERS vs CBCL by gender ####
outcome_list <- c("yr4_cbcl_int","yr4_cbcl_ext")
cbcl_ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  cbcl_ders_plot <-
    ggplot(aes(x=yr3_ders_total,y=.data[[outcome]],
                               linetype=genderid,
                               shape = genderid,
                               fill = genderid
                               ),data=analysis_data) +
    geom_point(alpha=.6, size = 2) +
    geom_smooth(method="lm",
                se=FALSE,
                color="black") +
    scale_linetype_manual(values = c("cis_boy"="31",
                                     "cis_girl"="11",
                                     "gd"="solid")) +
    scale_shape_manual(values=c(21,22,23)) +
    scale_fill_manual(values=c("grey40","grey85","black")) +
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

##### Graphs of DERS vs BPM by gender ####
outcome_list <- c("yr4_bpm_int","yr4_bpm_ext")
bpm_ders_plot_list <- list()
for (outcome in outcome_list) {
  # Create plot
  bpm_ders_plot <- ggplot(aes(x=yr3_ders_total,y=.data[[outcome]],
                              linetype=genderid,
                              shape = genderid,
                              fill = genderid
  ),data=analysis_data) +
    geom_point(alpha=.6, size=2) +
    geom_smooth(method="lm",
                se=FALSE,
                color="black") +
    scale_linetype_manual(values = c("cis_boy"="31",
                                     "cis_girl"="11",
                                     "gd"="solid")) +
    scale_shape_manual(values=c(21,22,23)) +
    scale_fill_manual(values=c("grey40","grey85","black")) +
    scale_colour_grey(start=0.9,end=0) +
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

# /!\ for broad gender groups (ie genderid), p = 0.1441
# /!\ for more granular gender groups (ie gender_details), p = 0.3413
#### Age (year 4)
kruskal.test(yr4_age ~ genderid, data = analysis_data)
# kruskal.test(yr4_age ~ gender_details, data = analysis_data)

# /!\ for broad gender groups (ie genderid), p = 0.000008609. cis girls and GD
# /!\ are significantly different from cis boys, but no significant difference
# /!\ between GD and cis girls
# /!\ for more granular gender groups (ie gender_details), p = 0.00008648. only
# /!\ significant differences are between nonbinary and cis boys and between
# /!\ nonbinary and cis girls
#### LES (year 3) 
kruskal.test(yr3_total_bad_le ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr3_total_bad_le, analysis_data$genderid, method = "bh")
# kruskal.test(yr3_total_bad_le ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr3_total_bad_le, analysis_data$gender_details, method = "bh")

# /!\ for broad gender groups (ie genderid), p = 0.0000000000006128. all groups
# /!\ are significantly different
# /!\ for more granular gender groups (ie gender_details), p = 0.00000000001118. 
# /!\ only significant differences are between cis boy and cis girl and between 
# /!\ nonbinary and cis girl
#### DERS (year 3) 
kruskal.test(yr3_ders_total ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr3_ders_total, analysis_data$genderid, method = "bh")
# kruskal.test(yr3_ders_total ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr3_ders_total, analysis_data$gender_details, method = "bh")

# /!\ for broad gender groups (ie genderid), p = 0.000000000000001124. only 
# /!\ significant differences are between GD and cis boys and between GD and
# /!\ cis girls
# /!\ for more granular gender groups (ie gender_details), p = 0.00000000000001058 
# /!\ only significant differences are between nonbinary and cis boy, between
# /!\ nonbinary and cis girl, between trans boy and cis boy, and between trans
# /!\ boy and cis girl
#### CBCL internalizing (year 4) 
kruskal.test(yr4_cbcl_int ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr4_cbcl_int, analysis_data$genderid, method = "bh")
# kruskal.test(yr4_cbcl_int ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr4_cbcl_int, analysis_data$gender_details, method = "bh")

# /!\ for broad gender groups (ie genderid), p = 0.000000165. all groups 
# /!\ significantly different
# /!\ for more granular gender groups (ie gender_details), p = 0.0000003991 
# /!\ only significant differences are between cis boys and cis girls, between
# /!\ nonbinary and cis boy, betweenn nonbinary and cis girl, between trans boy 
# /!\ and cis boy, and between trans boy and cis girl
#### CBCL externalizing (year 4)
kruskal.test(yr4_cbcl_ext ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr4_cbcl_ext, analysis_data$genderid, method = "bh")
# kruskal.test(yr4_cbcl_ext ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr4_cbcl_ext, analysis_data$gender_details, method = "bh")

# /!\ for broad gender groups (ie genderid), p < 0.00000000000000022. all groups 
# /!\ significantly different
# /!\ for more granular gender groups (ie gender_details), p < 0.00000000000000022 
# /!\ all comparisons significant *except* between nonbinary and trans boys,
# /!\ between trans girls and cis boys, and between trans girls and cis girls
#### BPM internalizing (year 4) 
# kruskal.test(yr4_bpm_int ~ genderid, data = analysis_data)
# dunnTest(analysis_data$yr4_bpm_int, analysis_data$genderid, method = "bh")
# kruskal.test(yr4_bpm_int ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr4_bpm_int, analysis_data$gender_details, method = "bh")

# /!\ for broad gender groups (ie genderid), p = 0.0000000002072 all groups 
# /!\ significantly different
# /!\ for more granular gender groups (ie gender_details), p = 0.000000004543 
# /!\ only significant differences are between cis boys and cis girls, between
# /!\ nonbinary and cis boy, betweenn nonbinary and cis girl, between trans boy 
# /!\ and cis boy, and between trans boy and cis girl
#### BPM externalizing (year 4)
kruskal.test(yr4_bpm_ext ~ genderid, data = analysis_data)
dunnTest(analysis_data$yr4_bpm_ext, analysis_data$genderid, method = "bh")
# kruskal.test(yr4_bpm_ext ~ gender_details, data = analysis_data)
# dunnTest(analysis_data$yr4_bpm_ext, analysis_data$gender_details, method = "bh")


### Mann-Whitney U (non-parametric version of two-sample t-test) to determine ####
### whether variables differ based on sex

# /!\ no difference based on sex, p = 0.1584
#### Age (year 4) 
wilcox.test(yr4_age ~ sex, data = analysis_data)

# /!\ significant difference based on sex, p = 0.01035
#### LES (year 3) 
wilcox.test(yr3_total_bad_le ~ sex, data = analysis_data)

# /!\ significant difference based on sex, p = 0.000000005503
#### DERS (year 3) 
wilcox.test(yr3_ders_total ~ sex, data = analysis_data)

# /!\ significant difference based on sex, p = 0.02859
#### CBCL internalizing (year 4) 
wilcox.test(yr4_cbcl_int ~ sex, data = analysis_data)

# /!\ significant difference based on sex, p = 0.02453
#### CBCL externalizing (year 4)
wilcox.test(yr4_cbcl_ext ~ sex, data = analysis_data)

# /!\ significant difference based on sex, p = 0.00000000004388
#### BPM internalizing (year 4)
wilcox.test(yr4_bpm_int ~ sex, data = analysis_data)

# /!\ significant difference based on sex, p = 0.000005854
#### BPM externalizing (year 4) 
wilcox.test(yr4_bpm_ext ~ sex, data = analysis_data)

### Mixed effect linear regression to determine whether DERS differs based ####
### on LES, using age as fixed effect covariate and site as random intercept
#### DERS ~ LES + age + (1|site) ####
ders_les_age_reg <- lmer(Z_yr4_ders_total ~ Z_yr3_total_bad_le + Z_yr4_age + (1|site),
# /!\ all regressions are repeated with log transformed data for sensitivity analysis
# ders_les_age_reg <- lmer(Z_log_yr4_ders_total ~ Z_log_yr4_total_bad_le + Z_yr4_age + (1|site),
                         data=analysis_data)
# /!\ LES is significant, untransformed: p = 0.00000000115, log transformed:0.000000179 
summary(ders_les_age_reg)

### Mixed effect linear regression to determine whether CBCL or BPM differ ####
### based on LES and/or DERS, using age as fixed effect covariate and site as random 
### intercept
#### First check that all pairs of variables are related (with covariates of ####
#### age and site) to determine whether mediation analysis is reasonable
##### CBCL internalizing ~ LES + age + (1|site) ####
cbcl_int_les_age_reg <- lmer(Z_yr4_cbcl_int ~ Z_yr3_total_bad_le + Z_yr4_age + (1|site),
# /!\ all regressions are repeated with log transformed data for sensitivity analysis
# cbcl_int_les_age_reg <- lmer(Z_log_yr4_cbcl_int ~ Z_log_yr3_total_bad_le + Z_yr4_age + (1|site),
                           data=analysis_data)
# /!\ LES is significant, untransformed: p = 0.000000000000103, log transformed: 0.0000000037  
summary(cbcl_int_les_age_reg)

##### CBCL externalizing ~ LES + age + (1|site) ####
cbcl_ext_les_age_reg <- lmer(Z_yr4_cbcl_ext ~ Z_yr3_total_bad_le + Z_yr4_age + (1|site),
# /!\ all regressions are repeated with log transformed data for sensitivity analysis
# cbcl_ext_les_age_reg <- lmer(Z_log_yr4_cbcl_ext ~ Z_log_yr3_total_bad_le + Z_yr4_age + (1|site),
                           data=analysis_data)
# /!\ LES is significant, untransformed: p < 0.0000000000000002, log transformed:0.000000000000158 
summary(cbcl_ext_les_age_reg)

##### BPM internalizing ~ LES + age + (1|site) ####
 bpm_int_les_age_reg <- lmer(Z_yr4_bpm_int ~ Z_yr3_total_bad_le + Z_yr4_age + (1|site),
 # /!\ all regressions are repeated with log transformed data for sensitivity analysis
# bpm_int_les_age_reg <- lmer(Z_log_yr4_bpm_int ~ Z_log_yr3_total_bad_le + Z_yr4_age + (1|site),
                             data=analysis_data)
# /!\ LES is significant, untransformed: p<0.0000000000000002, log transformed:<0.0000000000000002 
summary(bpm_int_les_age_reg)

##### BPM externalizing ~ LES + age + (1|site) ####
 bpm_ext_les_age_reg <- lmer(Z_yr4_bpm_ext ~ Z_yr3_total_bad_le + Z_yr4_age + (1|site),
 # /!\ all regressions are repeated with log transformed data for sensitivity analysis
# bpm_ext_les_age_reg <- lmer(Z_log_yr4_bpm_ext ~ Z_log_yr3_total_bad_le + Z_yr4_age + (1|site),
                             data=analysis_data)
# /!\ LES is significant, untransformed:p=0.000000000000000793, log transformed:0.0000000000000436 
summary(bpm_ext_les_age_reg)

##### CBCL internalizing ~ DERS + age + (1|site) ####
 cbcl_int_ders_age_reg <- lmer(Z_yr4_cbcl_int ~ Z_yr3_ders_total + Z_yr4_age + (1|site),
 # /!\ all regressions are repeated with log transformed data for sensitivity analysis
# cbcl_int_ders_age_reg <- lmer(Z_log_yr4_cbcl_int ~ Z_log_yr3_ders_total + Z_yr4_age + (1|site),
                           data=analysis_data)
# /!\ DERS is significant, untransformed:<0.0000000000000002, log transformed:<0.0000000000000002  
summary(cbcl_int_ders_age_reg)

##### CBCL externalizing ~ DERS + age + (1|site) ####
 cbcl_ext_ders_age_reg <- lmer(Z_yr4_cbcl_ext ~ Z_yr3_ders_total + Z_yr4_age + (1|site),
 # /!\ all regressions are repeated with log transformed data for sensitivity analysis
# cbcl_ext_ders_age_reg <- lmer(Z_log_yr4_cbcl_ext ~ Z_log_yr3_ders_total + Z_yr4_age + (1|site),
                             data=analysis_data)
# /!\ DERS is significant, untransformed:<0.0000000000000002, log transformed:<0.0000000000000002  
summary(cbcl_ext_ders_age_reg)

##### BPM internalizing ~ DERS + age + (1|site) ####
bpm_int_ders_age_reg <- lmer(Z_yr4_bpm_int ~ Z_yr3_ders_total + Z_yr4_age + (1|site),
# /!\ all regressions are repeated with log transformed data for sensitivity analysis
# bpm_int_ders_age_reg <- lmer(Z_log_yr4_bpm_int ~ Z_log_yr3_ders_total + Z_yr4_age + (1|site),
                            data=analysis_data)
# /!\ DERS is significant, untransformed:<0.0000000000000002, log transformed:<0.0000000000000002  
summary(bpm_int_ders_age_reg)

##### BPM externalizing ~ DERS + age + (1|site) ####
bpm_ext_ders_age_reg <- lmer(Z_yr4_bpm_ext ~ Z_yr3_ders_total + Z_yr4_age + (1|site),
# /!\ all regressions are repeated with log transformed data for sensitivity analysis
# bpm_ext_ders_age_reg <- lmer(Z_log_yr4_bpm_ext ~ Z_log_yr3_ders_total + Z_yr4_age + (1|site),
                            data=analysis_data)
# /!\ DERS is significant, untransformed:<0.0000000000000002, log transformed:<0.0000000000000002  
summary(bpm_ext_ders_age_reg)

#### Second include both LES and DERS in same model of psychopathology #### 
#### symptoms to get variance explained for full linear regression model which
#### does not include sex or gender as a covariate
##### CBCL internalizing ~ LES + DERS + age + (1|site) ####
cbcl_int_les_ders_age_reg <- lmer(Z_yr4_cbcl_int ~ Z_yr3_total_bad_le + Z_yr3_ders_total + Z_yr4_age + (1|site),
# /!\ all regressions are repeated with log transformed data for sensitivity analysis
# cbcl_int_les_ders_age_reg <- lmer(Z_log_yr4_cbcl_int ~ Z_log_yr3_total_bad_le + Z_log_yr3_ders_total + Z_yr4_age + (1|site),
                              data=analysis_data)
# /!\ LES is significant, untransformed:p=0.00000000623, log transformed: 0.000000543
# /!\ DERS is significant, untransformed:p=<0.0000000000000002, log transformed:<0.0000000000000002  
summary(cbcl_int_les_ders_age_reg)

##### CBCL externalizing ~ LES + DERS + age + (1|site) ####
cbcl_ext_les_ders_age_reg <- lmer(Z_yr4_cbcl_ext ~ Z_yr3_total_bad_le + Z_yr3_ders_total + Z_yr4_age + (1|site),
# /!\ all regressions are repeated with log transformed data for sensitivity analysis
# cbcl_ext_les_ders_age_reg <- lmer(Z_log_yr4_cbcl_ext ~ Z_log_yr3_total_bad_le + Z_log_yr3_ders_total + Z_yr4_age + (1|site),
                              data=analysis_data)
# /!\ LES is significant, untransformed:p=0.00000000000345, log transformed:0.0000000000549  
# /!\ DERS is significant, untransformed:<0.0000000000000002, log transformed:<0.0000000000000002  
summary(cbcl_ext_les_ders_age_reg)

##### BPM internalizing ~ LES + DERS + age + (1|site) ####
bpm_int_les_ders_age_reg <- lmer(Z_yr4_bpm_int ~ Z_yr3_total_bad_le + Z_yr3_ders_total + Z_yr4_age + (1|site),
# /!\ all regressions are repeated with log transformed data for sensitivity analysis
# bpm_int_les_ders_age_reg <- lmer(Z_log_yr4_bpm_int ~ Z_log_yr3_total_bad_le + Z_log_yr3_ders_total + Z_yr4_age + (1|site),
                              data=analysis_data)
# /!\ LES is significant, untransformed:p<0.0000000000000002, log transformed:<0.0000000000000002  
# /!\ DERS is significant, untransformed:0.00000000000000343, log transformed:<0.0000000000000002  
summary(bpm_int_les_ders_age_reg)

##### BPM externalizing ~ LES + DERS + age + (1|site) ####
bpm_ext_les_ders_age_reg <- lmer(Z_yr4_bpm_ext ~ Z_yr3_total_bad_le + Z_yr3_ders_total + Z_yr4_age + (1|site),
# /!\ all regressions are repeated with log transformed data for sensitivity analysis
# bpm_ext_les_ders_age_reg <- lmer(Z_log_yr4_bpm_ext ~ Z_log_yr3_total_bad_le + Z_log_yr3_ders_total + Z_yr4_age + (1|site),
                              data=analysis_data)
# /!\ LES is significant, untransformed:p=0.000000000000337, log transformed:0.00000000000122  
# /!\ DERS is significant, untransformed:<0.0000000000000002, log transformed:<0.0000000000000002  
summary(bpm_ext_les_ders_age_reg)

# /!\ Lili suggested more cleanly defining and tested moderators (gender or
# /!\ sex) vs mediators (DERS ie emotion regulation), so I added step 2 below
# /!\ for testing moderation only (ignoring mediation), then step 3 for testing
# /!\ mediation only (ignoring gender or sex), and finally step 4 for testing
# /!\ moderated mediation
## STEP TWO: MEDIATING EFFECT OF ER ON CBCL OR BPM ~ LES #### 

### Simple mediation model for CBCL internalizing ####
cbclint_model <-
' # direct effect
      Z_yr4_cbcl_int~ c*Z_yr3_total_bad_le + Z_yr4_age
    # mediator
      Z_yr3_ders_total ~ a*Z_yr3_total_bad_le  + Z_yr4_age
    # indirect effect
      Z_yr4_cbcl_int~ b*Z_yr3_ders_total
    # indirect effect (a*b)
      ab := a*b
    # total effect
      total := c + (a*b)'
# /!\ all models are repeated with log transformed data for sensitivity analysis
# cbclint_model <-
#   ' # direct effect
#         Z_log_yr4_cbcl_int~ c*Z_log_yr3_total_bad_le + Z_yr4_age
#       # mediator
#         Z_log_yr3_ders_total ~ a*Z_log_yr3_total_bad_le  + Z_yr4_age
#       # indirect effect
#         Z_log_yr4_cbcl_int~ b*Z_log_yr3_ders_total
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
# /!\ all paths significant suggesting partial mediation
# /!\ path a: untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path b: p = untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path c' (direct): p = untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path ab (indirect): p = untransformed: p < 0.001, log transformed: p < 0.001
parameterEstimates(cbclint_model, boot.ci.type = "bca.simple")

### Simple mediation model for CBCL externalizing ####
cbclext_model <-
' # direct effect
      Z_yr4_cbcl_ext~ c*Z_yr3_total_bad_le + Z_yr4_age
    # mediator
      Z_yr3_ders_total ~ a*Z_yr3_total_bad_le  + Z_yr4_age
    # indirect effect
      Z_yr4_cbcl_ext~ b*Z_yr3_ders_total
    # indirect effect (a*b)
      ab := a*b
    # total effect
      total := c + (a*b)'
# /!\ all models are repeated with log transformed data for sensitivity analysis
# cbclext_model <-
#   ' # direct effect
#         Z_log_yr4_cbcl_ext~ c*Z_log_yr4_total_bad_le + Z_yr4_age
#       # mediator
#         Z_log_yr4_ders_total ~ a*Z_log_yr4_total_bad_le  + Z_yr4_age
#       # indirect effect
#         Z_log_yr4_cbcl_ext~ b*Z_log_yr4_ders_total
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
# /!\ all paths significant suggesting partial mediation
# /!\ path a: untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path b: p = untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path c' (direct): p = untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path ab (indirect): p = untransformed: p < 0.001, log transformed: p < 0.001
parameterEstimates(cbclext_model, boot.ci.type = "bca.simple")

### Simple mediation model for BPM internalizing ####
bpmint_model <-
  ' # direct effect
        Z_yr4_bpm_int~ c*Z_yr3_total_bad_le + Z_yr4_age
      # mediator
        Z_yr3_ders_total ~ a*Z_yr3_total_bad_le  + Z_yr4_age
      # indirect effect
        Z_yr4_bpm_int~ b*Z_yr3_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)'
# /!\ all models are repeated with log transformed data for sensitivity analysis
# bpmint_model <-
#   ' # direct effect
#         Z_log_yr4_bpm_int~ c*Z_log_yr4_total_bad_le + Z_yr4_age
#       # mediator
#         Z_log_yr4_ders_total ~ a*Z_log_yr4_total_bad_le  + Z_yr4_age
#       # indirect effect
#         Z_log_yr4_bpm_int~ b*Z_log_yr4_ders_total
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
# /!\ all paths significant suggesting partial mediation
# /!\ path a: untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path b: p = untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path c' (direct): p = untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path ab (indirect): p = untransformed: p < 0.001, log transformed: p < 0.001
parameterEstimates(bpmint_model, boot.ci.type = "bca.simple")

### Simple mediation model for BPM externalizing ####
bpmext_model <-
  ' # direct effect
        Z_yr4_bpm_ext~ c*Z_yr3_total_bad_le + Z_yr4_age
      # mediator
        Z_yr3_ders_total ~ a*Z_yr3_total_bad_le  + Z_yr4_age
      # indirect effect
        Z_yr4_bpm_ext~ b*Z_yr3_ders_total
      # indirect effect (a*b)
        ab := a*b
      # total effect
        total := c + (a*b)'
# /!\ all models are repeated with log transformed data for sensitivity analysis
# bpmext_model <-
#   ' # direct effect
#         Z_log_yr4_bpm_ext~ c*Z_log_yr4_total_bad_le + Z_yr4_age
#       # mediator
#         Z_log_yr4_ders_total ~ a*Z_log_yr4_total_bad_le  + Z_yr4_age
#       # indirect effect
#         Z_log_yr4_bpm_ext~ b*Z_log_yr4_ders_total
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
# /!\ all paths significant suggesting partial mediation
# /!\ path a: untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path b: p = untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path c' (direct): p = untransformed: p < 0.001, log transformed: p < 0.001
# /!\ path ab (indirect): p = untransformed: p < 0.001, log transformed: p < 0.001
parameterEstimates(bpmext_model, boot.ci.type = "bca.simple")


# /!\ trying to figure out how to get a p-value or actual statistical test to 
# /!\ compare gender groups was kind of a nightmare - 
# /!\ I could have done it with SEM but the explanation got really complicated
# /!\ really fast, so I did some digging and based this setup of first SEM then
# /!\ process modeling on a paper that was published a few years ago in JAACAP
# /!\ and used ABCD data.
## STEP THREE: MODERATING EFFECT OF GENDER OR SEX ON MEDIATION ####

### Moderated mediation model (Hayes model 59) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### internalizing 
# /!\ overall interactions: untransformed: p = .467, log transformed: .700 
# /!\ LES*gender: untransformed: p = .350, log transformed: p = .437
# /!\ DERS*gender: untransformed: p = .586, log transformed: p = .822
cbcl_int_gender_model15 <- PROCESS(
  analysis_data,
  y = "Z_yr4_cbcl_int",
  x = "Z_yr3_total_bad_le",
  meds = c("Z_yr3_ders_total"),
# /!\ all models are repeated with log transformed data for sensitivity analysis
  # y = "Z_log_yr4_cbcl_int",
  # x = "Z_log_yr3_total_bad_le",
  # meds = c("Z_log_yr3_ders_total"),
  mods = c("genderid_refcisboy"),
  covs = c("Z_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    # "x-y"
    # "m-y"
    "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

### Moderated mediation model (Hayes model 59) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### externalizing 
# /!\ overall interactions: untransformed: p = .946, log transformed: .975 
# /!\ LES*gender: untransformed: p = .740, log transformed: p = .961
# /!\ DERS*gender: untransformed: p = .910, log transformed: p = .833
cbcl_ext_gender_model15 <- PROCESS(
  analysis_data,
  y = "Z_yr4_cbcl_ext",
  x = "Z_yr3_total_bad_le",
  meds = c("Z_yr3_ders_total"),
# /!\ all models are repeated with log transformed data for sensitivity analysis
  # y = "Z_log_yr4_cbcl_ext",
  # x = "Z_log_yr3_total_bad_le",
  # meds = c("Z_log_yr3_ders_total"),
  mods = c("genderid_refcisboy"),
  covs = c("Z_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    # "x-y"
    # "m-y"
    "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

### Moderated mediation model (Hayes model 59) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and BPM
### internalizing 
# /!\ overall interactions: untransformed: p <.001, log transformed: <.001 
# /!\ LES*gender: untransformed: p <.001, log transformed: p = <.001
# /!\ DERS*gender: untransformed: p = .770, log transformed: p = .848
bpm_int_gender_model15 <- PROCESS(
  analysis_data,
  y = "Z_yr4_bpm_int",
  x = "Z_yr3_total_bad_le",
  meds = c("Z_yr3_ders_total"),
# /!\ all models are repeated with log transformed data for sensitivity analysis
  # y = "Z_log_yr4_bpm_int",
  # x = "Z_log_yr3_total_bad_le",
  # meds = c("Z_log_yr3_ders_total"),
  mods = c("genderid_refcisboy"),
  covs = c("Z_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    # "x-y"
    # "m-y"
    "all"
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
#     Z = (0.06840  -0.20980   )/(sqrt((0.02372^2)+(0.02458)^2)) = -4.139505
# cis boy (beta1) vs gd (beta2):
#     Z = (0.06840  -0.25678  )/(sqrt((0.02372^2)+(0.06215)^2)) = -2.831818
# cis girl (beta1) vs gd (beta2):
#     Z = (0.20980   -0.25678  )/(sqrt((0.02458^2)+(0.06215)^2)) = -0.7029344
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -4.139505, so pnorm(-abs(-4.139505))*2 = 0.0000348056
# cis boy vs gd: Z = -2.831818, so pnorm(-abs(-2.831818))*2 = 0.004628418
# cis girl vs gd: Z = -0.7029344, so pnorm(-abs(-0.7029344))*2 = 0.4820966
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.0000348056,0.004628418,0.4820966),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p < .001
# cis boy vs gd: p = .007
# cis girl vs gd: .482

# for untransformed data:
# are the conditional indirect effects [ab] of X on Y via M significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.00933   -0.01097   )/(sqrt((0.00408^2)+(0.00403)^2)) = -0.2859761
# cis boy (beta1) vs gd (beta2):
#     Z = (0.00933   -0.03023   )/(sqrt((0.00408^2)+(0.02138)^2)) = -0.9602212
# cis girl (beta1) vs gd (beta2):
#     Z = (0.01097   -0.03023   )/(sqrt((0.00403^2)+(0.02138)^2)) = -0.8852527
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -0.2859761, so pnorm(-abs(-0.2859761))*2 = 0.7748964
# cis boy vs gd: Z = -0.9602212, so pnorm(-abs(-0.9602212))*2 = 0.3369439
# cis girl vs gd: Z = -0.8852527, so pnorm(-abs(-0.8852527))*2 = 0.3760204
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.7748964,0.3369439,0.3760204),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p = .775
# cis boy vs gd: p = .564
# cis girl vs gd: .564

# for log transformed data:
# are the conditional direct effects [c'] of X on Y significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.06707    -0.19967   )/(sqrt((0.02273^2)+(0.02429 )^2)) = -3.985997
# cis boy (beta1) vs gd (beta2):
#     Z = (0.06707    -0.23480  )/(sqrt((0.02273^2)+(0.07259)^2)) = -2.205073
# cis girl (beta1) vs gd (beta2):
#     Z = (0.19967   -0.23480  )/(sqrt((0.02429 ^2)+(0.07259)^2)) = -0.4589389
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -3.985997, so pnorm(-abs(-3.985997))*2 = 0.00006719736
# cis boy vs gd: Z = -2.205073, so pnorm(-abs(-2.205073))*2 = 0.02744897
# cis girl vs gd: Z = -0.4589389, so pnorm(-abs(-0.4589389))*2 = 0.646278
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.00006719736,0.02744897,0.646278),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p < .001
# cis boy vs gd: p = .041
# cis girl vs gd: .646

# for log transformed data:
# are the conditional indirect effects [ab] of X on Y via M significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# cis boy (beta1) vs cis girl (beta2): 
#     Z = (0.00345    -0.00946    )/(sqrt((0.00330^2)+(0.00381)^2)) = -1.192355
# cis boy (beta1) vs gd (beta2):
#     Z = (0.00345    -0.02850    )/(sqrt((0.00330^2)+(0.02103)^2)) = -1.176756
# cis girl (beta1) vs gd (beta2):
#     Z = (0.00946    -0.02850    )/(sqrt((0.00381^2)+(0.02103)^2)) = -0.890871
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# cis boy vs cis girl: Z = -1.192355, so pnorm(-abs(-1.192355))*2 = 0.2331221
# cis boy vs gd: Z = -1.176756, so pnorm(-abs(-1.176756))*2 = 0.2392929
# cis girl vs gd: Z = -0.890871, so pnorm(-abs(-0.890871))*2 = 0.3729984
# Finally, we need to fdr correct for multiple tests:
# p.adjust(c(0.2331221,0.2392929,0.3729984),method="fdr")
# So final p-values rounded to three places are:
# cis boy vs cis girl: p = .359
# cis boy vs gd: p = .359
# cis girl vs gd: .373

### Moderated mediation model (Hayes model 59) to test whether gender ####
### moderates mediating effect of DERS on relationship between LES and BPM
### externalizing 
# /!\ overall interactions: untransformed: p = .670, log transformed: .244 
# /!\ LES*gender: untransformed: p = .434, log transformed: p = .099
# /!\ DERS*gender: untransformed: p = .751, log transformed: p = .748
bpm_ext_gender_model15 <- PROCESS(
  analysis_data,
  y = "Z_yr4_bpm_ext",
  x = "Z_yr3_total_bad_le",
  meds = c("Z_yr3_ders_total"),
# /!\ all models are repeated with log transformed data for sensitivity analysis
  # y = "Z_log_yr4_bpm_ext",
  # x = "Z_log_yr3_total_bad_le",
  # meds = c("Z_log_yr3_ders_total"),
  mods = c("genderid_refcisboy"),
  covs = c("Z_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    # "x-y"
    # "m-y"
    "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

### Moderated mediation model (Hayes model 59) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### internalizing 
# /!\ overall interactions: untransformed: p = .061, log transformed: .169 
# /!\ LES*sex: untransformed: p = .096, log transformed: p = .144
# /!\ DERS*sex: untransformed: p = .128, log transformed: p = .270
cbcl_int_sex_model15 <- PROCESS(
  analysis_data,
  y = "Z_yr4_cbcl_int",
  x = "Z_yr3_total_bad_le",
  meds = c("Z_yr3_ders_total"),
# /!\ all models are repeated with log transformed data for sensitivity analysis
  # y = "Z_log_yr4_cbcl_int",
  # x = "Z_log_yr3_total_bad_le",
  # meds = c("Z_log_yr3_ders_total"),
  mods = c("sex"),
  covs = c("Z_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    # "x-y"
    # "m-y"
    "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

# for log transformed data:
# are the conditional indirect effects [ab] of X on Y via M significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.00776      -0.03787       )/(sqrt((0.00907^2)+(0.00960)^2)) = -2.279851
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -2.279851, so pnorm(-abs(-2.279851))*2 = 0.02261653
# So final p-values rounded to three places for male vs female is p = .023

### Moderated mediation model (Hayes model 59) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and CBCL
### externalizing 
# /!\ overall interactions: untransformed: p = .896, log transformed: .974 
# /!\ LES*sex: untransformed: p = .719, log transformed: p = .886
# /!\ DERS*sex: untransformed: p = .739, log transformed: p = .865
cbcl_ext_sex_model15 <- PROCESS(
  analysis_data,
  y = "Z_yr4_cbcl_ext",
  x = "Z_yr3_total_bad_le",
  meds = c("Z_yr3_ders_total"),
# /!\ all models are repeated with log transformed data for sensitivity analysis
  # y = "Z_log_yr4_cbcl_ext",
  # x = "Z_log_yr3_total_bad_le",
  # meds = c("Z_log_yr3_ders_total"),
  mods = c("sex"),
  covs = c("Z_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    # "x-y"
    # "m-y"
    "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

# for log transformed data:
# are the conditional indirect effects [ab] of X on Y via M significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.00873      -0.03959        )/(sqrt((0.01024^2)+(0.00996)^2)) = -2.160319
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -2.160319, so pnorm(-abs(-2.160319))*2 = 0.03074798
# So final p-values rounded to three places for male vs female is p = .031

### Moderated mediation model (Hayes model 59) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and BPM
### internalizing 
# /!\ overall interactions: untransformed: p <.001, log transformed: <.001 
# /!\ LES*sex: untransformed: p <.001, log transformed: p <.001
# /!\ DERS*sex: untransformed: p = .348, log transformed: p = .280
bpm_int_sex_model15 <- PROCESS(
  analysis_data,
  y = "Z_yr4_bpm_int",
  x = "Z_yr3_total_bad_le",
  meds = c("Z_yr3_ders_total"),
# /!\ all models are repeated with log transformed data for sensitivity analysis
  # y = "Z_log_yr4_bpm_int",
  # x = "Z_log_yr3_total_bad_le",
  # meds = c("Z_log_yr3_ders_total"),
  mods = c("sex"),
  covs = c("Z_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    # "x-y"
    # "m-y"
    "all"
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
# male (beta1) vs female (beta2): 
#     Z = (0.07070   -0.23950    )/(sqrt((0.02423^2)+(0.02349)^2)) = -5.001898
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -5.001898, so pnorm(-abs(-5.001898))*2 = 0.0000005676863
# So final p-values rounded to three places for male vs female is p < .001

# for untransformed data:
# are the conditional indirect effects [ab] of X on Y via M significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.00874    -0.01777     )/(sqrt((0.00395^2)+(0.00494)^2)) = -1.42766
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -1.42766, so pnorm(-abs(-1.42766))*2 = 0.1533897
# So final p-values rounded to three places for male vs female is p = .153

# for log transformed data:
# are the conditional direct effects [c'] of X on Y significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.07099    -0.22767     )/(sqrt((0.02322^2)+(0.02366)^2)) = -4.726306
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -4.726306, so pnorm(-abs(-4.726306))*2 = 0.00000228641
# So final p-values rounded to three places for male vs female is p < .001

# for log transformed data:
# are the conditional indirect effects [ab] of X on Y via M significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.00274     -0.01558      )/(sqrt((0.00331^2)+(0.00459)^2)) = -2.268955
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -2.268955, so pnorm(-abs(-2.268955))*2 = 0.02327106
# So final p-values rounded to three places for male vs female is p = .023

### Moderated mediation model (Hayes model 59) to test whether sex ####
### moderates mediating effect of DERS on relationship between LES and BPM
### externalizing 
# /!\ overall interactions: untransformed: p = .397, log transformed: .102 
# /!\ LES*sex: untransformed: p = .252, log transformed: p = .048
# /!\ DERS*sex: untransformed: p = .532, log transformed: p = .493
bpm_ext_sex_model15 <- PROCESS(
  analysis_data,
  y = "Z_yr4_bpm_ext",
  x = "Z_yr3_total_bad_le",
  meds = c("Z_yr3_ders_total"),
# /!\ all models are repeated with log transformed data for sensitivity analysis
  # y = "Z_log_yr4_bpm_ext",
  # x = "Z_log_yr3_total_bad_le",
  # meds = c("Z_log_yr3_ders_total"),
  mods = c("sex"),
  covs = c("Z_yr4_age"),
  hlm.re.m = "site",
  hlm.re.y = "site",
  mod.path = c(
    # "x-y"
    # "m-y"
    "all"
  ),
  cov.path = c("both"),
  nsim = 1000,
  seed = 1234,
  center = FALSE,
  std = FALSE,
  digits = 5)

# for log transformed data:
# are the conditional indirect effects [ab] of X on Y via M significant different for
# different groups? Use Z test to find out. 
# Z = (beta1 - beta2 / (sqrt(SE1^2 + SE2^2)))
# male (beta1) vs female (beta2): 
#     Z = (0.02074      -0.09292       )/(sqrt((0.02304^2)+(0.02338)^2)) = -2.198949
# to go from Z score to p-value, find probability of being outside absolute value
# of Z score (because don't know if beta1 is smaller or larger than beta2) and 
# then multiply that by 2 because two-tailed test. Can use default settings of
# mean = 0 and sd = 1 in pnorm function because that is true of Z scores
# male vs female: Z = -2.198949, so pnorm(-abs(-2.198949))*2 = 0.02788155
# So final p-values rounded to three places for male vs female is p = .028