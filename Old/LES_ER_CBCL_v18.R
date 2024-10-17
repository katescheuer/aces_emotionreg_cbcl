setwd("C:/Users/Kate Scheuer/OneDrive - UW/Desktop/Lab/aces_emotionreg_cbcl")

library(tidyverse)
library(lme4)
library(lmerTest)
library(partR2)
library(psych)
library(lavaan)
library(ggpattern)
library(misty)

gish_y_gi <- read.csv("gish_y_gi.csv",header=T)
mh_p_ders <- read.csv("mh_p_ders.csv",header=T)
mh_p_cbcl <- read.csv("mh_p_cbcl.csv",header=T)
abcd_y_lt <- read.csv("abcd_y_lt.csv",header=T)
mh_y_le <- read.csv("mh_y_le.csv",header=T)

genderdata <- gish_y_gi %>%
          filter(eventname=="3_year_follow_up_y_arm_1"|
                 eventname=="4_year_follow_up_y_arm_1") %>%
          select(c(src_subject_id,eventname,
                   kbi_sex_assigned_at_birth,
                   kbi_gender,kbi_y_trans_id)) %>%
          mutate(sex = case_when(kbi_sex_assigned_at_birth==1 ~ "male",
                                 kbi_sex_assigned_at_birth==2 ~ "female",
                                 kbi_sex_assigned_at_birth==777 ~ "dont_know",
                                 kbi_sex_assigned_at_birth==999 ~ "refuse")) %>%
          mutate(gender = case_when(kbi_gender==1 ~ "boy",
                                    kbi_gender==2 ~ "girl",
                                    kbi_gender==3 ~ "nb",
                                    kbi_gender==4 ~ "dont_understand",
                                    kbi_gender==777 ~ "refuse"
          )) %>%
          mutate(trans = case_when(kbi_y_trans_id==1 ~ "yes",
                                   kbi_y_trans_id==2 ~ "maybe",
                                   kbi_y_trans_id==3 ~ "no",
                                   kbi_y_trans_id==4 ~ "dont_understand",
                                   kbi_y_trans_id==777 ~ "refuse"
          )) %>%
          mutate(genderid = case_when(kbi_gender==777 ~ "refuse",
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
          select(src_subject_id,eventname,sex,gender,trans,genderid) %>%
          filter(
                 # genderid!="NA",
                 genderid!="refuse",
                 genderid!="dont_understand")
genderdata %>% group_by(eventname) %>% count(genderid)

cbcldata <- mh_p_cbcl %>%
              select(src_subject_id,eventname,
                     cbcl_scr_syn_totprob_t,
                     cbcl_scr_syn_internal_t,cbcl_scr_syn_external_t,
                     cbcl_scr_syn_aggressive_t,cbcl_scr_syn_attention_t,
                     cbcl_scr_syn_anxdep_t,cbcl_scr_syn_rulebreak_t,
                     cbcl_scr_syn_social_t,cbcl_scr_syn_somatic_t,
                     cbcl_scr_syn_thought_t,cbcl_scr_syn_withdep_t) %>%
              rename(cbcl_total = cbcl_scr_syn_totprob_t,
                     cbcl_ext = cbcl_scr_syn_external_t,
                     cbcl_int = cbcl_scr_syn_internal_t,
                     cbcl_agg = cbcl_scr_syn_aggressive_t,
                     cbcl_attn = cbcl_scr_syn_attention_t,
                     cbcl_anxdep = cbcl_scr_syn_anxdep_t,
                     cbcl_rule = cbcl_scr_syn_rulebreak_t,
                     cbcl_social = cbcl_scr_syn_social_t,
                     cbcl_somatic = cbcl_scr_syn_somatic_t,
                     cbcl_thought = cbcl_scr_syn_thought_t,
                     cbcl_withdep = cbcl_scr_syn_withdep_t) %>%
            mutate(logcbcl_total = log(cbcl_total)) %>%
            mutate(logcbcl_int = log(cbcl_int)) %>%
            mutate(logcbcl_ext = log(cbcl_ext)) %>%
            select(src_subject_id,eventname,
                   cbcl_total,cbcl_int,cbcl_ext,
                   logcbcl_total,logcbcl_int,logcbcl_ext
                   # cbcl_agg,cbcl_attn,cbcl_anxdep,cbcl_rule,
                   # cbcl_social,cbcl_somatic,cbcl_thought,cbcl_withdep
                   )
            # filter(cbcl_total!="NA")


dersdata <- mh_p_ders %>%
              filter(rowSums(select(., all_of(c("ders_attn_awareness_p",
                            "ders_feelings_attentive_p","ders_feelings_care_p",
                            "ders_upset_ack_p","ders_clear_feelings_p",
                            "ders_feelings_know_p","ders_upset_difficulty_p",
                            "ders_upset_focus_p","ders_upset_concentrate_p",
                            "ders_upset_fixation_p","ders_emotion_overwhelm_p",
                            "ders_upset_control_p","ders_upset_out_control_p",
                            "ders_upset_behavior_p","ders_upset_lose_control_p",
                            "ders_upset_behavior_control_p","ders_upset_angry_p",
                            "ders_upset_embarrassed_p","ders_upset_ashamed_p",
                            "ders_upset_weak_p","ders_upset_guilty_p",
                            "ders_upset_irritation_p","ders_upset_time_p",
                            "ders_upset_depressed_p","ders_upset_feel_better_p",
                            "ders_upset_esteem_p","ders_upset_long_time_better_p",
                            "ders_upset_emotion_overwhelm_p","ders_upset_better_p"))) == 777) == 0) %>%
              mutate(rev_ders_attn_awareness_p = 5 + 1 - ders_attn_awareness_p) %>%
              mutate(rev_ders_feelings_attentive_p = 5 + 1 - ders_feelings_attentive_p) %>%
              mutate(rev_ders_feelings_care_p = 5 + 1 - ders_feelings_care_p) %>%
              mutate(rev_ders_upset_ack_p = 5 + 1 - ders_upset_ack_p) %>%
              mutate(rev_ders_clear_feelings_p = 5 + 1 - ders_clear_feelings_p) %>%
              mutate(rev_ders_feelings_know_p = 5 + 1 - ders_feelings_know_p) %>%
              mutate(rev_ders_upset_behavior_control_p = 5 + 1 - ders_upset_behavior_control_p) %>%
              mutate(rev_ders_upset_better_p = 5 + 1 - ders_upset_better_p) %>%
              mutate(factor6awareness = rowSums(across(all_of(
                c("rev_ders_attn_awareness_p","rev_ders_feelings_attentive_p",
                  "rev_ders_feelings_care_p","rev_ders_upset_ack_p"))))) %>%
              mutate(factor6clarity = rowSums(across(all_of(
                c("rev_ders_clear_feelings_p","rev_ders_feelings_know_p"))))) %>%
              mutate(factor6goals = rowSums(across(all_of(
                c("ders_upset_difficulty_p","ders_upset_focus_p",
                  "ders_upset_concentrate_p","ders_upset_fixation_p"))))) %>%
              mutate(factor6impulse = rowSums(across(all_of(
                c("ders_emotion_overwhelm_p","ders_upset_control_p",
                  "ders_upset_out_control_p","ders_upset_behavior_p",
                  "ders_upset_lose_control_p","rev_ders_upset_behavior_control_p"))))) %>%
              mutate(factor6nonaccept = rowSums(across(all_of(
                c("ders_upset_angry_p","ders_upset_embarrassed_p",
                  "ders_upset_ashamed_p","ders_upset_weak_p",
                  "ders_upset_guilty_p","ders_upset_irritation_p"))))) %>%
              mutate(factor6strategies = rowSums(across(all_of(
                c("ders_upset_time_p","ders_upset_depressed_p",
                  "ders_upset_feel_better_p","ders_upset_esteem_p",
                  "ders_upset_long_time_better_p","ders_upset_emotion_overwhelm_p",
                  "rev_ders_upset_better_p"))))) %>%
              mutate(factor4attuned = rowSums(across(all_of(
                c("rev_ders_attn_awareness_p","rev_ders_feelings_attentive_p",
                  "rev_ders_feelings_care_p","rev_ders_upset_ack_p",
                  "rev_ders_clear_feelings_p","rev_ders_feelings_know_p"))))) %>%
              mutate(factor4distracted = rowSums(across(all_of(
                c("ders_upset_difficulty_p","ders_upset_focus_p",
                  "ders_upset_concentrate_p","ders_upset_fixation_p"))))) %>%
              mutate(factor4catastrophizing = rowSums(across(all_of(
                c("ders_emotion_overwhelm_p","ders_upset_control_p",
                  "ders_upset_out_control_p","ders_upset_behavior_p",
                  "ders_upset_lose_control_p","ders_upset_time_p",
                  "ders_upset_depressed_p","ders_upset_feel_better_p",
                  "ders_upset_long_time_better_p","rev_ders_upset_better_p"))))) %>%
              mutate(factor4neg_secondary = rowSums(across(all_of(
                c("ders_upset_angry_p","ders_upset_embarrassed_p",
                  "ders_upset_ashamed_p","ders_upset_weak_p",
                  "ders_upset_guilty_p","ders_upset_irritation_p",
                  "ders_upset_esteem_p"))))) %>%
              # original six factor solution with all items included
              mutate(ders6total = rowSums(across(all_of(c("factor6awareness",
                                                          "factor6clarity",
                                                          "factor6goals",
                                                          "factor6impulse",
                                                          "factor6nonaccept",
                                                          "factor6strategies"))))) %>%
              mutate(logders6total = log(ders6total)) %>%
              # four factor solution, does not include two items with low factor loading based on Crumbly 2023
              mutate(ders4total = rowSums(across(all_of(c("factor4attuned",
                                                          "factor4distracted",
                                                          "factor4catastrophizing",
                                                          "factor4neg_secondary"))))) %>%
              mutate(logders4total = log(ders4total)) %>%
              select(src_subject_id,eventname,factor6awareness,factor6clarity,
                     factor6goals,factor6impulse,factor6nonaccept,factor6strategies,
                     factor4attuned,factor4distracted,factor4catastrophizing,
                     factor4neg_secondary,
                     ders6total,logders6total,ders4total,logders4total)
dersdata %>% group_by(eventname) %>% summary()

ledata <- mh_y_le %>% 
            select(src_subject_id,eventname,ple_y_ss_total_bad) %>%
            rename(total_bad_le = ple_y_ss_total_bad) %>%
            mutate(log_total_bad_le = log(total_bad_le+1)) %>%
            mutate(lowhighle = if_else(total_bad_le<5,"low","high"))
ledata %>% group_by(eventname) %>% summary()


alldata <- genderdata %>%
            left_join(select(abcd_y_lt,c(src_subject_id,eventname,
                                          site_id_l,interview_age)),
                          by=c("src_subject_id","eventname")) %>%
            rename(age = interview_age, site = site_id_l) %>%
            left_join(dersdata,by=c("src_subject_id","eventname")) %>%
            left_join(ledata,by=c("src_subject_id","eventname")) %>%
            left_join(cbcldata,by=c("src_subject_id","eventname")) %>%
            mutate(gender_cisgirl = if_else(genderid=="cis_girl",1,0)) %>%
            mutate(gender_gd = if_else(genderid=="gd",1,0)) %>%
            mutate(gender_cisboy = if_else(genderid=="cis_boy",1,0)) %>%
            mutate(sex_male = if_else(sex=="male",1,0)) %>%
            mutate(sex_female = if_else(sex=="female",1,0)) %>%
            # cluster mean center variables
            mutate(gender_cisgirl_CMC = as.numeric(center(gender_cisgirl,type="CWC",
                                               cluster=site))) %>%
            mutate(gender_gd_CMC = as.numeric(center(gender_gd,type="CWC",
                                                          cluster=site))) %>%
            mutate(gender_cisboy_CMC = as.numeric(center(gender_cisboy,type="CWC",
                                                          cluster=site))) %>%
            mutate(sex_male_CMC = as.numeric(center(sex_male,type="CWC",
                                                    cluster=site))) %>%
            mutate(sex_female_CMC = as.numeric(center(sex_female,type="CWC",
                                                    cluster=site))) %>%
            mutate(age_CMC = as.numeric(center(age,type="CWC",
                                                                    cluster=site))) %>%
            mutate(ders6total_CMC = as.numeric(center(ders6total,type="CWC",
                                                      cluster=site))) %>%
            mutate(total_bad_le_CMC = as.numeric(center(total_bad_le,type="CWC",
                                                      cluster=site))) %>%
            mutate(cbcl_total_CMC = as.numeric(center(cbcl_total,type="CWC",
                                                      cluster=site))) %>%
            mutate(cbcl_int_CMC = as.numeric(center(cbcl_int,type="CWC",
                                                      cluster=site))) %>%
            mutate(cbcl_ext_CMC = as.numeric(center(cbcl_ext,type="CWC",
                                                    cluster=site))) %>%
            mutate(logders6total_CMC = as.numeric(center(logders6total,type="CWC",
                                                      cluster=site))) %>%
            mutate(log_total_bad_le_CMC = as.numeric(center(log_total_bad_le,type="CWC",
                                                        cluster=site))) %>%
            mutate(logcbcl_total_CMC = as.numeric(center(logcbcl_total,type="CWC",
                                                      cluster=site))) %>%
            mutate(logcbcl_int_CMC = as.numeric(center(logcbcl_int,type="CWC",
                                                    cluster=site))) %>%
            mutate(logcbcl_ext_CMC = as.numeric(center(logcbcl_ext,type="CWC",
                                                    cluster=site))) %>%
            # Z score variables
            mutate(age_ZCMC = as.numeric(scale(age_CMC,scale=TRUE))) %>%
            mutate(ders6total_ZCMC = as.numeric(scale(ders6total_CMC,scale=TRUE))) %>%
            mutate(total_bad_le_ZCMC = as.numeric(scale(total_bad_le_CMC,scale=TRUE))) %>%
            mutate(cbcl_total_ZCMC = as.numeric(scale(cbcl_total_CMC,scale=TRUE))) %>%
            mutate(cbcl_int_ZCMC = as.numeric(scale(cbcl_int_CMC,scale=TRUE))) %>%
            mutate(cbcl_ext_ZCMC = as.numeric(scale(cbcl_ext_CMC,scale=TRUE))) %>%
            mutate(logders6total_ZCMC = as.numeric(scale(logders6total_CMC,scale=TRUE))) %>%
            mutate(log_total_bad_le_ZCMC = as.numeric(scale(log_total_bad_le_CMC,
                                                            scale=TRUE))) %>%
            mutate(logcbcl_total_ZCMC = as.numeric(scale(logcbcl_total_CMC,scale=TRUE))) %>%
            mutate(logcbcl_int_ZCMC = as.numeric(scale(logcbcl_int_CMC,scale=TRUE))) %>%
            mutate(logcbcl_ext_ZCMC = as.numeric(scale(logcbcl_ext_CMC,scale=TRUE))) 

map(alldata,unique)

alldata %>% group_by(eventname) %>% count(genderid) %>% mutate(percentage = n / sum(n) * 100)

yr4data <- alldata %>% filter(eventname=="4_year_follow_up_y_arm_1")
yr3data <- alldata %>% filter(eventname=="3_year_follow_up_y_arm_1")

yr4data %>% summary()
yr4data %>% group_by(genderid) %>% count()

### are variables normally distributed? no, none of these are normally distributed
shapiro.test(yr4data$total_bad_le)
shapiro.test(yr4data$log_total_bad_le)
shapiro.test(yr4data$ders6total)
shapiro.test(yr4data$logders6total)
shapiro.test(yr4data$ders4total)
shapiro.test(yr4data$logders4total)
shapiro.test(yr4data$cbcl_total)
shapiro.test(yr4data$logcbcl_total)
shapiro.test(yr4data$cbcl_int)
shapiro.test(yr4data$logcbcl_int)
shapiro.test(yr4data$cbcl_ext)
shapiro.test(yr4data$logcbcl_ext)

ggplot(aes(x=total_bad_le),data=yr4data) +
  geom_histogram()
# ggplot(aes(x=log_total_bad_le),data=yr4data) +
#   geom_histogram()
ggplot(aes(x=ders6total),data=yr4data) +
  geom_histogram()
ggplot(aes(x=log(logders6total)),data=yr4data) +
  geom_histogram() #sig diff from normal but actually looks pretty good once transformed
ggplot(aes(x=ders6total),data=yr4data) +
  geom_histogram()
ggplot(aes(x=log(logders6total)),data=yr4data) +
  geom_histogram() #sig diff from normal but actually looks pretty good once transformed
ggplot(aes(x=cbcl_total),data=yr4data) +
  geom_histogram()
ggplot(aes(x=log(cbcl_total)),data=yr4data) +
  geom_histogram()
ggplot(aes(x=cbcl_int),data=yr4data) +
  geom_histogram()
ggplot(aes(x=log(cbcl_int)),data=yr4data) +
  geom_histogram()
ggplot(aes(x=cbcl_ext),data=yr4data) +
  geom_histogram()
ggplot(aes(x=log(cbcl_ext)),data=yr4data) +
  geom_histogram()


yr4data %>% group_by(eventname) %>% count(genderid) %>% mutate(percentage = n / sum(n) * 100)
yr4data$genderid <- as.factor(yr4data$genderid)
yr4data$site <- as.factor(yr4data$site)
# yr4data$lowhighle <- as.factor(yr4data$lowhighle)

corrmat <- yr4data %>% select(c(total_bad_le,ders6total,cbcl_total,cbcl_int,cbcl_ext)) %>% corr.test(adjust="BH")
corrmat$r
corrmat$p.adj

kruskal.test(age ~ genderid, data = yr4data)
pairwise.wilcox.test(yr4data$age, yr4data$genderid,
                     p.adjust.method = "BH")
yr4data %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    n = n()
  )
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

ggplot(aes(x=total_bad_le),data=yr4data) +
  geom_histogram()
ggplot(aes(x=log_total_bad_le),data=yr4data) +
  geom_histogram()

ggplot(yr4data, aes(x = total_bad_le,fill=genderid)) +
  geom_histogram(position="dodge")
ggplot(yr4data, aes(x = total_bad_le,fill=genderid)) +
  geom_density(alpha=0.5,adjust=1.5) +
  scale_x_continuous(expand = c(0,0),limits = c(0,15),breaks=seq(0,15,1)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,0.3),breaks=seq(0,0.3,0.05)) +
  scale_fill_discrete(labels=c("Cis boy","Cis girl", "GD")) +
  labs(x="Negative Life Events",y="Proportion of Subjects",fill="Gender") +
  theme_classic()
df_proportions <- yr4data %>%
  group_by(genderid, total_bad_le) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  complete(total_bad_le, genderid, fill = list(n = 0, prop = 0)) %>% # Ensure all combinations are present
  mutate(proportion = replace_na(proportion,0))
ggplot(df_proportions, aes(x=total_bad_le,y=proportion, fill=genderid)) +
  # geom_col_pattern(aes(pattern=genderid),position="dodge") +
  # geom_bar_pattern(aes(
  # geom_col_pattern(aes(
  #                      pattern=genderid,
  #                      # pattern_angle = genderid,
  #                      # binwidth=1.5
  #                      pattern_spacing = genderid,
  #                      fill=genderid
  #                      # bins=15
  #                      ),
  #                      # pattern_spacing = 0.02,
  #                      # pattern_spacing = 0.01,
  #                      fill="white",
  #                      color="black",
  #                      pattern_fill="black",
  #                      pattern_color="black",
  #                      # binwidth=1,
  #                      # bins=15,
  #                      position="dodge"
  #                      # position=position_dodge(width=0.7)
  #                      # width=0.7
  #                      ) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_x_continuous(expand = c(0,0),breaks = seq(min(df_proportions$total_bad_le),
      max(df_proportions$total_bad_le), by = 1)) + # X-axis ticks by 1
  scale_y_continuous(expand = c(0,0),breaks=seq(0,0.3,by=0.05),limits=c(0,0.30)) +
  # scale_pattern_manual(values=c("weave","crosshatch","stripe")) +
  # scale_pattern_type_manual(values=c("hexagonal","triangle","pythagorean")) +
  # scale_pattern_density_manual(values=c(0.1,0.3,0.5)) +
  scale_fill_manual(values=c("white","darkgray","black")) +
  # scale_fill_discrete(labels=c("Cis boy","Cis girl", "GD")) +
  # labs(x="Number of Negative Life Events",y="Proportion of Subjects",pattern="Gender") +
  theme_classic()
# ggsave("SuppFig1A.tiff",width=7,height=3,unit="in")


ggplot(aes(x=genderid,y=total_bad_le),data=yr4data) +
  # geom_violin(aes(fill=genderid)) +
  geom_boxplot() 
yr4data %>%
  summarise(
    mean_total_bad_le = mean(total_bad_le, na.rm = TRUE),
    sd_total_bad_le = sd(total_bad_le, na.rm = TRUE),
    min_total_bad_le = min(total_bad_le, na.rm = TRUE),
    max_total_bad_le = max(total_bad_le, na.rm = TRUE),
    median_total_bad_le = median(total_bad_le, na.rm = TRUE),
    n = n()
  )
yr4data %>%
  group_by(genderid) %>%
  summarise(
    mean_total_bad_le = mean(total_bad_le, na.rm = TRUE),
    sd_total_bad_le = sd(total_bad_le, na.rm = TRUE),
    min_total_bad_le = min(total_bad_le, na.rm = TRUE),
    max_total_bad_le = max(total_bad_le, na.rm = TRUE),
    median_total_bad_le = median(total_bad_le, na.rm = TRUE),
    n = n()
  )

kruskal.test(total_bad_le ~ genderid, data = yr4data)
pairwise.wilcox.test(yr4data$total_bad_le, yr4data$genderid,
                     p.adjust.method = "BH")


ggplot(aes(x=ders6total),data=yr4data) +
  geom_histogram()
ggplot(aes(x=logders6total),data=yr4data) +
  geom_histogram()
ggplot(aes(x=genderid,y=ders6total,fill=genderid),data=yr4data) +
  geom_boxplot()
kruskal.test(ders6total ~ genderid, data = yr4data)
pairwise.wilcox.test(yr4data$ders6total, yr4data$genderid,
                     p.adjust.method = "BH")
ggplot(yr4data, aes(x = ders6total,fill=genderid)) +
  geom_density(alpha=0.5,adjust=.9) +
  scale_x_continuous(expand = c(0,0),limits=c(0,145),breaks=seq(0,145,5)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,0.031),breaks=seq(0,0.03,0.005)) +
  scale_fill_discrete(labels=c("Cis boy","Cis girl", "GD")) +
  labs(x="Emotion Regulation Difficulties",y="Density",fill="Gender") +
  theme_classic()
df_proportions <- yr4data %>%
  mutate(ders6total_bin = cut(ders6total, breaks = seq(0, max(ders6total,na.rm=TRUE)+10, by = 10), right = FALSE)) %>% # Create bins
  group_by(genderid, ders6total_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  complete(ders6total_bin, genderid, fill = list(n = 0, prop = 0)) %>% # Ensure all combinations are present
  mutate(proportion = replace_na(proportion,0))
ggplot(df_proportions, aes(x = ders6total_bin,y=proportion,fill=genderid)) +
  # geom_col_pattern(aes(
  #             pattern=genderid,
  #             # pattern_angle = genderid,
  #             # binwidth=1.5
  #             pattern_spacing = genderid,
  #             fill=genderid
  #             # bins=15
  #           ),
  #           # pattern_spacing = 0.02,
  #           # pattern_spacing = 0.01,
  #           fill="white",
  #           color="black",
  #           pattern_fill="black",
  #           pattern_color="black",
  #           # binwidth=1,
  #           # bins=15,
  #           position="dodge"
  #           # position=position_dodge(width=0.7)
  #           # width=0.7
  #           ) +
          geom_bar(stat="identity",position="dodge",color="black") +
          scale_fill_manual(values=c("white","darkgray","black")) +
          scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
                                      "70-79","80-89","90-99","100-109","110-119","120-129",
                                      "130-139")) +
          scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
                                                        0.3,
                                                        by=0.05),limits = c(0,0.3)) +
            theme_classic()
# ggsave("SuppFig1B.tiff",width=8,height=3,unit="in")
  # geom_bar(stat="identity",position="dodge") + 
  # scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
  #                             "70-79","80-89","90-99","100-109","110-119","120-129",
  #                             "130-139")) +
  # scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
  #                                               max(df_proportions$proportion),
  #                                               by=0.05)) + 
  # scale_fill_discrete(labels=c("Cis boy","Cis girl", "GD")) +
  # labs(x="Emotion Regulation Difficulties",y="Proportion of Subjects",fill="Gender") +
  # theme_classic() +
  # theme(axis.text.x = element_text(angle = 60, hjust=.9))
yr4data %>%
  summarise(
    mean_ders6total = mean(ders6total, na.rm = TRUE),
    sd_ders6total = sd(ders6total, na.rm = TRUE),
    min_ders6total = min(ders6total, na.rm = TRUE),
    max_ders6total = max(ders6total, na.rm = TRUE),
    median_ders6total = median(ders6total, na.rm = TRUE),
    n = n()
  )
yr4data %>%
  group_by(genderid) %>%
  summarise(
    mean_ders6total = mean(ders6total, na.rm = TRUE),
    sd_ders6total = sd(ders6total, na.rm = TRUE),
    min_ders6total = min(ders6total, na.rm = TRUE),
    max_ders6total = max(ders6total, na.rm = TRUE),
    median_ders6total = median(ders6total, na.rm = TRUE),
    n = n()
  )

ggplot(aes(x=cbcl_total),data=yr4data) +
  geom_histogram()
ggplot(aes(x=genderid,y=cbcl_total,fill=genderid),data=yr4data) +
  geom_boxplot()
kruskal.test(cbcl_total ~ genderid, data = yr4data)
pairwise.wilcox.test(yr4data$cbcl_total, yr4data$genderid,
                     p.adjust.method = "BH")
ggplot(yr4data, aes(x = cbcl_total,fill=genderid)) +
  geom_density(alpha=0.5,adjust=1) +
  scale_x_continuous(expand = c(0,0),breaks=(seq(0,100,10)),limits = c(0,100)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,0.04),breaks=seq(0,0.04,0.005)) +
  scale_fill_discrete(labels=c("Cis boy","Cis girl", "GD")) +
  labs(x="CBCL Total Problems",y="Density",fill="Gender") +
  theme_classic()
df_proportions <- yr4data %>%
  mutate(cbcl_total_bin = cut(cbcl_total, breaks = seq(0, 100, by = 10), right = FALSE)) %>% # Create bins
  group_by(genderid, cbcl_total_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  complete(cbcl_total_bin, genderid, fill = list(n = 0, prop = 0)) %>% # Ensure all combinations are present
  mutate(proportion = replace_na(proportion,0))
ggplot(df_proportions, aes(x = cbcl_total_bin,y=proportion,fill=genderid)) +
  # geom_bar(stat="identity",position="dodge") + 
  # scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
  #                           "70-79","80-89","90-99")) +
  # scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
  #                                               max(df_proportions$proportion),
  #                                               by=0.05)) + 
  # scale_fill_discrete(labels=c("Cis boy","Cis girl", "GD")) +
  # labs(x="CBCL Total Problems",y="Proportion of Subjects",fill="Gender") +
  # theme_classic()
# ggplot(df_proportions, aes(x = ders6total_bin,y=proportion,fill=genderid)) +
  # geom_col_pattern(aes(
  #   pattern=genderid,
  #   # pattern_angle = genderid,
  #   # binwidth=1.5
  #   pattern_spacing = genderid,
  #   fill=genderid
  #   # bins=15
  # ),
  # # pattern_spacing = 0.02,
  # # pattern_spacing = 0.01,
  # fill="white",
  # color="black",
  # pattern_fill="black",
  # pattern_color="black",
  # # binwidth=1,
  # # bins=15,
  # position="dodge"
  # # position=position_dodge(width=0.7)
  # # width=0.7
  # ) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_fill_manual(values=c("white","darkgray","black")) +
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
                            "70-79","80-89","90-99")) +
  scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
                                                0.4,
                                                by=0.05),limits = c(0,0.4)) + 
  # scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
  #                           "70-79","80-89","90-99","100-109","110-119","120-129",
  #                           "130-139")) +
  # scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
  #                                               max(df_proportions$proportion),
  #                                               by=0.05)) +
  theme_classic()
# ggsave("SuppFig1C.tiff",width=5.45,height=3.5,unit="in")
yr4data %>%
  summarise(
    mean_cbcl_total = mean(cbcl_total, na.rm = TRUE),
    sd_cbcl_total = sd(cbcl_total, na.rm = TRUE),
    min_cbcl_total = min(cbcl_total, na.rm = TRUE),
    max_cbcl_total = max(cbcl_total, na.rm = TRUE),
    median_cbcl_total = median(cbcl_total, na.rm = TRUE),
    n = n()
  )
yr4data %>%
  group_by(genderid) %>%
  summarise(
    mean_cbcl_total = mean(cbcl_total, na.rm = TRUE),
    sd_cbcl_total = sd(cbcl_total, na.rm = TRUE),
    min_cbcl_total = min(cbcl_total, na.rm = TRUE),
    max_cbcl_total = max(cbcl_total, na.rm = TRUE),
    median_cbcl_total = median(cbcl_total, na.rm = TRUE),
    n = n()
  )

ggplot(aes(x=cbcl_int),data=yr4data) +
  geom_histogram()
ggplot(aes(x=genderid,y=cbcl_int,fill=genderid),data=yr4data) +
  geom_boxplot()
kruskal.test(cbcl_int ~ genderid, data = yr4data)
pairwise.wilcox.test(yr4data$cbcl_int, yr4data$genderid,
                     p.adjust.method = "BH")
ggplot(yr4data, aes(x = cbcl_int,fill=genderid)) +
  geom_density(alpha=0.5,adjust=1) +
  scale_x_continuous(expand = c(0,0),breaks=(seq(0,100,10)),limits = c(0,100)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,0.05),breaks=seq(0,0.05,0.005)) +
  scale_fill_discrete(labels=c("Cis boy","Cis girl", "GD")) +
  labs(x="CBCL Internalizing",y="Density",fill="Gender") +
  theme_classic()
df_proportions <- yr4data %>%
  mutate(cbcl_int_bin = cut(cbcl_int, breaks = seq(0, 100, by = 10), right = FALSE)) %>% # Create bins
  group_by(genderid, cbcl_int_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  complete(cbcl_int_bin, genderid, fill = list(n = 0, prop = 0)) %>% # Ensure all combinations are present
  mutate(proportion = replace_na(proportion,0))
ggplot(df_proportions, aes(x = cbcl_int_bin,y=proportion,fill=genderid)) +
  # geom_col_pattern(aes(
  #   pattern=genderid,
  #   # pattern_angle = genderid,
  #   # binwidth=1.5
  #   pattern_spacing = genderid,
  #   fill=genderid
  #   # bins=15
  # ),
  # # pattern_spacing = 0.02,
  # # pattern_spacing = 0.01,
  # fill="white",
  # color="black",
  # pattern_fill="black",
  # pattern_color="black",
  # # binwidth=1,
  # # bins=15,
  # position="dodge"
  # # position=position_dodge(width=0.7)
  # # width=0.7
  # ) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_fill_manual(values=c("white","darkgray","black")) +
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
                            "70-79","80-89","90-99")) +
  scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
                                                0.4,
                                                by=0.05),limits = c(0,0.4)) +
  # scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
  #                           "70-79","80-89","90-99","100-109","110-119","120-129",
  #                           "130-139")) +
  # scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
  #                                               max(df_proportions$proportion),
  #                                               by=0.05)) +
  theme_classic()
# ggsave("SuppFig1D.tiff",width=5.45,height=3.5,unit="in")
# geom_bar(stat="identity",position="dodge") + 
#   scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
#                             "70-79","80-89","90-99")) +
#   scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
#                                                 max(df_proportions$proportion),
#                                                 by=0.05)) + 
#   scale_fill_discrete(labels=c("Cis boy","Cis girl", "GD")) +
#   labs(x="CBCL Internalizing",y="Proportion of Subjects",fill="Gender") +
#   theme_classic()
yr4data %>%
  summarise(
    mean_cbcl_int = mean(cbcl_int, na.rm = TRUE),
    sd_cbcl_int = sd(cbcl_int, na.rm = TRUE),
    min_cbcl_int = min(cbcl_int, na.rm = TRUE),
    max_cbcl_int = max(cbcl_int, na.rm = TRUE),
    median_cbcl_int = median(cbcl_int, na.rm = TRUE),
    n = n()
  )
yr4data %>%
  group_by(genderid) %>%
  summarise(
    mean_cbcl_int = mean(cbcl_int, na.rm = TRUE),
    sd_cbcl_int = sd(cbcl_int, na.rm = TRUE),
    min_cbcl_int = min(cbcl_int, na.rm = TRUE),
    max_cbcl_int = max(cbcl_int, na.rm = TRUE),
    median_cbcl_int = median(cbcl_int, na.rm = TRUE),
    n = n()
  )

ggplot(aes(x=cbcl_ext),data=yr4data) +
  geom_histogram()
ggplot(aes(x=genderid,y=cbcl_ext,fill=genderid),data=yr4data) +
  geom_boxplot()
kruskal.test(cbcl_ext ~ genderid, data = yr4data)
pairwise.wilcox.test(yr4data$cbcl_ext, yr4data$genderid,
                     p.adjust.method = "BH")
ggplot(yr4data, aes(x = cbcl_ext,fill=genderid)) +
  geom_density(alpha=0.5,adjust=1) +
  scale_x_continuous(expand = c(0,0),breaks=(seq(0,100,10)),limits = c(0,100)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,0.09),breaks=seq(0,0.09,0.005)) +
  scale_fill_discrete(labels=c("Cis boy","Cis girl", "GD")) +
  labs(x="CBCL Externalizing",y="Density",fill="Gender") +
  theme_classic()
df_proportions <- yr4data %>%
  mutate(cbcl_ext_bin = cut(cbcl_ext, breaks = seq(0, 100, by = 10), right = FALSE)) %>% # Create bins
  group_by(genderid, cbcl_ext_bin) %>%
  count() %>%
  group_by(genderid) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  complete(cbcl_ext_bin, genderid, fill = list(n = 0, prop = 0)) %>% # Ensure all combinations are present
  mutate(proportion = replace_na(proportion,0))
ggplot(df_proportions, aes(x = cbcl_ext_bin,y=proportion,fill=genderid)) +
  # geom_col_pattern(aes(
  #   pattern=genderid,
  #   # pattern_angle = genderid,
  #   # binwidth=1.5
  #   pattern_spacing = genderid,
  #   fill=genderid
  #   # bins=15
  # ),
  # # pattern_spacing = 0.02,
  # # pattern_spacing = 0.01,
  # fill="white",
  # color="black",
  # pattern_fill="black",
  # pattern_color="black",
  # # binwidth=1,
  # # bins=15,
  # position="dodge"
  # # position=position_dodge(width=0.7)
  # # width=0.7
  # ) +
  geom_bar(stat="identity",position="dodge",color="black") +
  scale_fill_manual(values=c("white","darkgray","black")) +
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
                            "70-79","80-89","90-99")) +
  scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
                                                0.4,
                                                by=0.05),limits = c(0,0.401)) + 
  # scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
  #                           "70-79","80-89","90-99","100-109","110-119","120-129",
  #                           "130-139")) +
  # scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
  #                                               max(df_proportions$proportion),
  #                                               by=0.05)) +
  theme_classic()
ggsave("SuppFig1E.tiff",width=5.45,height=3.5,unit="in")
# geom_bar(stat="identity",position="dodge") + 
#   scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
#                             "70-79","80-89","90-99")) +
#   scale_y_continuous(expand = c(0,0),breaks=seq(min(df_proportions$proportion),
#                                                 max(df_proportions$proportion),
#                                                 by=0.05)) + 
#   scale_fill_discrete(labels=c("Cis boy","Cis girl", "GD")) +
#   labs(x="CBCL Externalizing",y="Proportion of Subjects",fill="Gender") +
#   theme_classic()
yr4data %>%
  summarise(
    mean_cbcl_ext = mean(cbcl_ext, na.rm = TRUE),
    sd_cbcl_ext = sd(cbcl_ext, na.rm = TRUE),
    min_cbcl_ext = min(cbcl_ext, na.rm = TRUE),
    max_cbcl_ext = max(cbcl_ext, na.rm = TRUE),
    median_cbcl_ext = median(cbcl_ext, na.rm = TRUE),
    n = n()
  )
yr4data %>%
  group_by(genderid) %>%
  summarise(
    mean_cbcl_ext = mean(cbcl_ext, na.rm = TRUE),
    sd_cbcl_ext = sd(cbcl_ext, na.rm = TRUE),
    min_cbcl_ext = min(cbcl_ext, na.rm = TRUE),
    max_cbcl_ext = max(cbcl_ext, na.rm = TRUE),
    median_cbcl_ext = median(cbcl_ext, na.rm = TRUE),
    n = n()
  )

### scatter plots of ders-p or les vs cbcl scores
### without gender
ggplot(aes(x=ders6total,y=cbcl_total),data=yr4data) +
  geom_point(size=1) +
  geom_smooth(method="lm",se=FALSE,size=1.25,color="black") +
  scale_x_continuous(expand = c(0,0),breaks=seq(25,150,25),limits = c(25,150)) + 
  scale_y_continuous(expand = c(0,0),breaks=seq(0,100,20),limits = c(0,100)) + 
  theme_classic()
ggsave("ders_vs_cbcltotal_nogender.tiff",width=6,height=6,units = "in")

ggplot(aes(x=ders6total,y=cbcl_int),data=yr4data) +
  geom_point(size=1) +
  geom_smooth(method="lm",se=FALSE,size=1.25,color="black") +
  scale_x_continuous(expand = c(0,0),breaks=seq(25,150,25),limits = c(25,150)) + 
  scale_y_continuous(expand = c(0,0),breaks=seq(0,100,20),limits = c(0,100)) + 
  theme_classic()
ggsave("ders_vs_cbclint_nogender.tiff",width=6,height=6,units = "in")

ggplot(aes(x=ders6total,y=cbcl_ext),data=yr4data) +
  geom_point(size=1) +
  geom_smooth(method="lm",se=FALSE,size=1.25,color="black") +
  scale_x_continuous(expand = c(0,0),breaks=seq(25,150,25),limits = c(25,150)) + 
  scale_y_continuous(expand = c(0,0),breaks=seq(0,100,20),limits = c(0,100)) + 
  theme_classic()
ggsave("ders_vs_cbclext_nogender.tiff",width=6,height=6,units = "in")

les_vs_cbcl_summary_stats <- yr4data %>% 
  group_by(total_bad_le) %>% 
  summarise(cbcl_total=mean(cbcl_total,na.rm=TRUE),
            cbcl_int=mean(cbcl_int,na.rm=TRUE),
            cbcl_ext=mean(cbcl_ext,na.rm=TRUE),
            n=n(),
            .groups="drop") %>%
  complete(total_bad_le)
ggplot(aes(x=total_bad_le,y=cbcl_total),data=les_vs_cbcl_summary_stats) +
  geom_bar(stat="identity",position="dodge",color="black",width=0.75) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,80,10),limits = c(0,80)) +
  scale_x_continuous(breaks=seq(0,15,1),expand=c(0,0)) +
  theme_classic()
ggsave("les_vs_cbcltotal_nogender.tiff",width=6,height=6,units = "in")

ggplot(aes(x=total_bad_le,y=cbcl_int),data=les_vs_cbcl_summary_stats) +
  geom_bar(stat="identity",position="dodge",color="black",width=0.75) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,80,10),limits = c(0,80)) +
  scale_x_continuous(breaks=seq(0,15,1),expand=c(0,0)) +
  theme_classic()
ggsave("les_vs_cbclint_nogender.tiff",width=6,height=6,units = "in")

ggplot(aes(x=total_bad_le,y=cbcl_ext),data=les_vs_cbcl_summary_stats) +
  geom_bar(stat="identity",position="dodge",color="black",width=0.75) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,80,10),limits = c(0,80)) +
  scale_x_continuous(breaks=seq(0,15,1),expand=c(0,0)) +
  theme_classic()
ggsave("les_vs_cbclext_nogender.tiff",width=6,height=6,units = "in")

### with gender
ggplot(aes(x=ders6total,y=cbcl_total,
           color=genderid,shape=genderid, linetype=genderid),data=yr4data) +
  # geom_point(alpha=0.3,size=1) +
  geom_point(size=2) +
  # geom_smooth(method="lm",se=FALSE,size=1.25) +
  geom_smooth(method="lm",se=FALSE) +
  # scale_color_manual(values=c("gray","gray1","black")) +
  scale_x_continuous(expand = c(0,0),breaks=seq(25,150,25),limits = c(25,150)) + 
  scale_y_continuous(expand = c(0,0),breaks=seq(0,100,20),limits = c(0,100)) + 
  theme_classic() +
  # theme(legend.position="none")
  theme(legend.key.width=unit(3,"cm"))
ggsave("ders_vs_cbcltotal_gender.tiff",width=6,height=6,units = "in")

ggplot(aes(x=ders6total,y=cbcl_int,
           color=genderid,shape=genderid,linetype=genderid),data=yr4data) +
  geom_point(alpha=0.3,size=1) +
  geom_smooth(method="lm",se=FALSE,size=1.25) +
  scale_x_continuous(expand = c(0,0),breaks=seq(25,150,25),limits = c(25,150)) + 
  scale_y_continuous(expand = c(0,0),breaks=seq(0,100,20),limits = c(0,100)) + 
  theme_classic() +
  theme(legend.position="none")
ggsave("ders_vs_cbclint_gender.tiff",width=6,height=6,units = "in")

ggplot(aes(x=ders6total,y=cbcl_ext,
           color=genderid,shape=genderid,linetype=genderid),data=yr4data) +
  geom_point(alpha=0.3,size=1) +
  geom_smooth(method="lm",se=FALSE,size=1.25) +
  scale_x_continuous(expand = c(0,0),breaks=seq(25,150,25),limits = c(25,150)) + 
  scale_y_continuous(expand = c(0,0),breaks=seq(0,100,20),limits = c(0,100)) + 
  theme_classic() +
  theme(legend.position="none")
ggsave("ders_vs_cbclext_gender.tiff",width=6,height=6,units = "in")

les_vs_cbcl_summary_stats <- yr4data %>% 
                              group_by(total_bad_le,genderid) %>% 
                              summarise(cbcl_total=mean(cbcl_total,na.rm=TRUE),
                                        cbcl_int=mean(cbcl_int,na.rm=TRUE),
                                        cbcl_ext=mean(cbcl_ext,na.rm=TRUE),
                                        n=n(),
                                        .groups="drop") %>%
                              complete(total_bad_le,genderid)
ggplot(aes(x=total_bad_le,y=cbcl_total,fill=genderid),data=les_vs_cbcl_summary_stats) +
  geom_bar(stat="identity",position="dodge",color="black",width=0.75) +
  # scale_fill_manual(values=c("white","darkgray","black")) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,80,10),limits = c(0,80)) +
  scale_x_continuous(breaks=seq(0,15,1),expand=c(0,0)) +
  theme_classic() + 
  theme(legend.position="none")
ggsave("les_vs_cbcltotal_gender.tiff",width=6,height=6,units = "in")

ggplot(aes(x=total_bad_le,y=cbcl_int,fill=genderid),data=les_vs_cbcl_summary_stats) +
  geom_bar(stat="identity",position="dodge",color="black",width=0.75) +
  # scale_fill_manual(values=c("white","darkgray","black")) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,80,10),limits = c(0,80)) +
  scale_x_continuous(breaks=seq(0,15,1),expand=c(0,0)) +
  theme_classic() +
  theme(legend.position="none")
ggsave("les_vs_cbclint_gender.tiff",width=6,height=6,units = "in")

ggplot(aes(x=total_bad_le,y=cbcl_ext,fill=genderid),data=les_vs_cbcl_summary_stats) +
  geom_bar(stat="identity",position="dodge",color="black",width=0.75) +
  # scale_fill_manual(values=c("white","darkgray","black")) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,80,10),limits = c(0,80)) +
  scale_x_continuous(breaks=seq(0,15,1),expand=c(0,0)) +
  theme_classic() +
  theme(legend.position="none")
ggsave("les_vs_cbclext_gender.tiff",width=6,height=6,units = "in")

### mediation models with lavaan  ####
meddata <- yr4data %>% 
              select(src_subject_id,
                     total_bad_le_ZCMC,ders6total_ZCMC,
                     cbcl_total_ZCMC,cbcl_int_ZCMC,cbcl_ext_ZCMC,
                     log_total_bad_le_ZCMC,logders6total_ZCMC,logcbcl_total_ZCMC,
                     logcbcl_ext_ZCMC,logcbcl_int_ZCMC,
                     age_ZCMC,site,
                     gender_cisgirl_CMC,gender_gd_CMC,gender_cisboy_CMC,
                     sex_male_CMC,sex_female_CMC,
                     genderid,sex) %>%
              rename(age = age_ZCMC,
                     cisgirl = gender_cisgirl_CMC,
                     gd = gender_gd_CMC,
                     cisboy = gender_cisboy_CMC,
                     LES = total_bad_le_ZCMC,
                     DERS = ders6total_ZCMC,
                     totalCBCL = cbcl_total_ZCMC,
                     intCBCL = cbcl_int_ZCMC,
                     extCBCL = cbcl_ext_ZCMC,
                     logLES = log_total_bad_le_ZCMC,
                     logDERS = logders6total_ZCMC,
                     logtotalCBCL = logcbcl_total_ZCMC,
                     logintCBCL = logcbcl_int_ZCMC,
                     logextCBCL = logcbcl_ext_ZCMC)
              # mutate(gender_cisgirl = if_else(genderid=="cis_girl",1,0),
              #        gender_gd = if_else(genderid=="gd",1,0)) %>%
              # left_join(select(yr3data,c("src_subject_id","total_bad_le_ZCMC")),
              #           by=c("src_subject_id")) %>%
              # rename(yr3LES = total_bad_le_ZCMC)

sexmeddata <- meddata %>%
                filter(sex!="refuse",sex!="dont_know")

### establishing relationships between all pairs of variables before mediation
#do life events affect emotion regulation - yes
DERS_le_reg <- lmer(DERS ~ LES + age + (1|site),data=meddata)
summary(DERS_le_reg)
# AIC(DERS_le_reg)
# partR2(DERS_le_reg,partvars=c("LES","age"),R2_type="marginal",nboot=10)

#do life events affect cbcl - yes to all
cbcltotal_le_reg <- lmer(totalCBCL ~ LES + age + (1|site),data=meddata)
summary(cbcltotal_le_reg)
# AIC(cbcltotal_le_reg)
# partR2(cbcltotal_le_reg,partvars=c("LES","age"),R2_type="marginal",nboot=10)

cbclint_le_reg <- lmer(intCBCL ~ LES + age + (1|site),data=meddata)
summary(cbclint_le_reg)
# AIC(cbclint_le_reg)
# partR2(cbclint_le_reg,partvars=c("LES","age"),R2_type="marginal",nboot=10)

cbclext_le_reg <- lmer(extCBCL ~ LES + age + (1|site),data=meddata)
summary(cbclext_le_reg)
# AIC(cbclext_le_reg)
# partR2(cbclext_le_reg,partvars=c("LES","age"),R2_type="marginal",nboot=10)

#does emotion regulation affect cbcl - yes to all
cbcltotal_DERS_reg <- lmer(totalCBCL ~ DERS + age + (1|site),data=meddata)
summary(cbcltotal_DERS_reg)
# AIC(cbcltotal_DERS_reg)
# partR2(cbcltotal_DERS_reg,partvars=c("DERS","age"),R2_type="marginal",nboot=10)

cbclint_DERS_reg <- lmer(intCBCL ~ DERS + age + (1|site),data=meddata)
summary(cbclint_DERS_reg)
# AIC(cbclint_DERS_reg)
# partR2(cbclint_DERS_reg,partvars=c("DERS","age"),R2_type="marginal",nboot=10)

cbclext_DERS_reg <- lmer(extCBCL ~ DERS + age + (1|site),data=meddata)
summary(cbclext_DERS_reg)
# AIC(cbclext_DERS_reg)
# partR2(cbclext_DERS_reg,partvars=c("DERS","age"),R2_type="marginal",nboot=10)


### not transformed variables (all year 4)
### total problems cbcl
nogendertotalmodel <- ' # direct effect
             totalCBCL~ c*LES + age
           # mediator
             DERS ~ a*LES  + age
           # indirect effect
             totalCBCL~ b*DERS
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)         '
nogendertotalfit <- sem(nogendertotalmodel, data = meddata, meanstructure = TRUE,
                        se = "robust.cluster",
                        cluster = "site"
                        )
summary(nogendertotalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(nogendertotalfit, boot.ci.type = "bca.simple")


# gender
# using cis boys as the comparison group
gendertotalmodel_compcisboy <- ' # direct effect 
             totalCBCL~ c*LES + cisgirl + gd + age + LES:cisgirl + LES:gd
           # mediator 
             DERS ~ a*LES  + cisgirl + gd + age + LES:cisgirl + LES:gd
           # indirect effect 
             totalCBCL~ b*DERS
           # indirect effect w mediator (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
'
# using cis girls as the comparison group
gendertotalmodel_compcisgirl <- ' # direct effect 
             totalCBCL~ c*LES + cisboy + gd + age + LES:cisboy + LES:gd
           # mediator 
             DERS ~ a*LES  + cisboy + gd + age + LES:cisboy + LES:gd
           # indirect effect 
             totalCBCL~ b*DERS
           # indirect effect w mediator (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
'
gendertotalfit_compcisboy <- sem(gendertotalmodel_compcisboy, data = meddata, 
                                 meanstructure = TRUE,
                                 se = "robust.cluster",
                                 cluster = "site")
summary(gendertotalfit_compcisboy, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(gendertotalmodel_compcisboy, boot.ci.type = "bca.simple")

gendertotalfit_compcisgirl <- sem(gendertotalmodel_compcisgirl, data = meddata, 
                                 meanstructure = TRUE,
                                 se = "robust.cluster",
                                 cluster = "site")
summary(gendertotalfit_compcisgirl, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(gendertotalfit_compcisgirl, boot.ci.type = "bca.simple")

# sex
sextotalmodel <- ' # direct effect
             totalCBCL~ c*LES + sex_female_CMC + age + LES:sex_female_CMC
           # mediator
             DERS ~ a*LES  + sex_female_CMC + age + LES:sex_female_CMC
           # indirect effect
             totalCBCL~ b*DERS
           # indirect effect w mediator (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
'
sextotalfit <- sem(sextotalmodel, data = sexmeddata, meanstructure = TRUE,
                      se = "robust.cluster",
                      cluster = "site"
                   )
summary(sextotalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(sextotalfit, boot.ci.type = "bca.simple")

### internalizing cbcl
nogenderintmodel <- ' # direct effect
             intCBCL~ c*LES + age
           # mediator
             DERS ~ a*LES  + age
           # indirect effect
             intCBCL~ b*DERS
           # indirect effect (a*b)
             ab := a*b
           # int effect
             int := c + (a*b)         '
nogenderintfit <- sem(nogenderintmodel, data = meddata, meanstructure = TRUE,
                        se = "robust.cluster",
                        cluster = "site"
)
summary(nogenderintfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(nogenderintfit, boot.ci.type = "bca.simple")

# gender
# using cis boys as the comparison group
genderintmodel_compcisboy <- ' 
            # direct effect 
             intCBCL~ c*LES + cisgirl + gd + age + LES:cisgirl + LES:gd
           # mediator 
             DERS ~ a*LES  + cisgirl + gd + age + LES:cisgirl + LES:gd
           # indirect effect 
             intCBCL~ b*DERS
           # indirect effect w mediator (a*b)
             ab := a*b
           # int effect
             int := c + (a*b)
'
# using cis girls as the comparison group
genderintmodel_compcisgirl <- ' # direct effect 
             intCBCL~ c*LES + cisboy + gd + age + LES:cisboy + LES:gd
           # mediator 
             DERS ~ a*LES  + cisboy + gd + age + LES:cisboy + LES:gd
           # indirect effect 
             intCBCL~ b*DERS
           # indirect effect w mediator (a*b)
             ab := a*b
           # int effect
             int := c + (a*b)
'
genderintfit_compcisboy <- sem(genderintmodel_compcisboy, data = meddata, 
                                 meanstructure = TRUE,
                                 se = "robust.cluster",
                                 cluster = "site")
summary(genderintfit_compcisboy, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(genderintfit_compcisboy, boot.ci.type = "bca.simple")

genderintfit_compcisgirl <- sem(genderintmodel_compcisgirl, data = meddata, 
                                  meanstructure = TRUE,
                                  se = "robust.cluster",
                                  cluster = "site")
summary(genderintfit_compcisgirl, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(genderintfit_compcisgirl, boot.ci.type = "bca.simple")

# sex
sexintmodel <- ' # direct effect
             intCBCL~ c*LES + sex_female_CMC + age + LES:sex_female_CMC
           # mediator
             DERS ~ a*LES  + sex_female_CMC + age + LES:sex_female_CMC
           # indirect effect
             intCBCL~ b*DERS
           # indirect effect w mediator (a*b)
             ab := a*b
           # int effect
             int := c + (a*b)
'
sexintfit <- sem(sexintmodel, data = sexmeddata, meanstructure = TRUE,
                   se = "robust.cluster",
                   cluster = "site"
)
summary(sexintfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(sexintfit, boot.ci.type = "bca.simple")

### externalizing cbcl
nogenderextmodel <- ' # direct effect
             extCBCL~ c*LES + age
           # mediator
             DERS ~ a*LES  + age
           # indirect effect
             extCBCL~ b*DERS
           # indirect effect (a*b)
             ab := a*b
           # ext effect
             ext := c + (a*b)         '
nogenderextfit <- sem(nogenderextmodel, data = meddata, meanstructure = TRUE,
                        se = "robust.cluster",
                        cluster = "site"
)
summary(nogenderextfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(nogenderextfit, boot.ci.type = "bca.simple")

# gender
# using cis boys as the comparison group
genderextmodel_compcisboy <- ' # direct effect 
             extCBCL~ c*LES + cisgirl + gd + age + LES:cisgirl + LES:gd
           # mediator 
             DERS ~ a*LES  + cisgirl + gd + age + LES:cisgirl + LES:gd
           # indirect effect 
             extCBCL~ b*DERS
           # indirect effect w mediator (a*b)
             ab := a*b
           # ext effect
             ext := c + (a*b)
'
# using cis girls as the comparison group
genderextmodel_compcisgirl <- ' # direct effect 
             extCBCL~ c*LES + cisboy + gd + age + LES:cisboy + LES:gd
           # mediator 
             DERS ~ a*LES  + cisboy + gd + age + LES:cisboy + LES:gd
           # indirect effect 
             extCBCL~ b*DERS
           # indirect effect w mediator (a*b)
             ab := a*b
           # ext effect
             ext := c + (a*b)
'
genderextfit_compcisboy <- sem(genderextmodel_compcisboy, data = meddata, 
                                 meanstructure = TRUE,
                                 se = "robust.cluster",
                                 cluster = "site")
summary(genderextfit_compcisboy, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(genderextfit_compcisboy, boot.ci.type = "bca.simple")

genderextfit_compcisgirl <- sem(genderextmodel_compcisgirl, data = meddata, 
                                  meanstructure = TRUE,
                                  se = "robust.cluster",
                                  cluster = "site")
summary(genderextfit_compcisgirl, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(genderextfit_compcisgirl, boot.ci.type = "bca.simple")

# sex
sexextmodel <- ' # direct effect
             extCBCL~ c*LES + sex_female_CMC + age + LES:sex_female_CMC
           # mediator
             DERS ~ a*LES  + sex_female_CMC + age + LES:sex_female_CMC
           # indirect effect
             extCBCL~ b*DERS
           # indirect effect w mediator (a*b)
             ab := a*b
           # ext effect
             ext := c + (a*b)
'
sexextfit <- sem(sexextmodel, data = sexmeddata, meanstructure = TRUE,
                   se = "robust.cluster",
                   cluster = "site"
)
summary(sexextfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
parameterEstimates(sexextfit, boot.ci.type = "bca.simple")


# ### log transformed variables (all yr 4) ####
# ### total problems cbcl
# nogendertotalmodel <- ' # direct effect
#              logtotalCBCL~ c*logLES + c1*age
#            # mediator
#              logDERS ~ a*logLES  + a1*age
#            # indirect effect
#              logtotalCBCL~ b*logDERS
#            # indirect effect (a*b)
#              ab := a*b
#            # total effect
#              total := c + (a*b)         '
# nogendertotalfit <- sem(nogendertotalmodel, data = meddata, meanstructure = TRUE,
#                         # se = "boot",
#                         # bootstrap = 500,
#                         se = "robust.cluster",
#                         cluster = "site"
# )
# summary(nogendertotalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# # semPaths(object = nogendertotalfit, whatLabels = "std") #make diagram with standardized coefficients on edges
# 
# # unconstrained gender
# gendertotalmodel <- ' # direct effect 
#              logtotalCBCL~ c("c1","c2","c3")*logLES + c("m1","m2","m3")*age
#            # mediator 
#              logDERS ~ c("a1","a2","a3")*logLES  + c("n1","n2","n3")*age
#            # indirect effect 
#              logtotalCBCL~ c("b1","b2","b3")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#              a3b3 := a3*b3
#            # total effect
#              total1 := c1 + (a1*b1)
#              total2 := c2 + (a2*b2)
#              total3 := c3 + (a3*b3)
# '
# gendertotalfit <- sem(gendertotalmodel, data = meddata, meanstructure = TRUE,
#                       # se = "boot",
#                       # bootstrap = 500,
#                       group="genderid",
#                       se = "robust.cluster",
#                       cluster = "site")
# summary(gendertotalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained gender
# gender_constall_totalmodel <- ' # direct effect 
#              logtotalCBCL~ c("c1","c1","c1")*logLES + c("m1","m1","m1")*age
#            # mediator 
#              logDERS ~ c("a1","a1","a1")*logLES  + c("n1","n1","n1")*age
#            # indirect effect 
#              logtotalCBCL~ c("b1","b1","b1")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#              a3b3 := a1*b1
#            # total effect
#              total1 := c1 + (a1*b1)
#              total2 := c1 + (a1*b1)
#              total3 := c1 + (a1*b1)
# '
# gender_constall_totalfit <- sem(gender_constall_totalmodel, data = meddata, meanstructure = TRUE,
#                                 # se = "boot",
#                                 # bootstrap = 500,
#                                 group="genderid",
#                                 se = "robust.cluster",
#                                 cluster = "site")
# summary(gender_constall_totalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of gender
# anova(gendertotalfit, gender_constall_totalfit)
# 
# # unconstrained sex
# sextotalmodel <- ' # direct effect
#              logtotalCBCL~ c("c1","c2")*logLES + c("m1","m2")*age
#            # mediator
#              logDERS ~ c("a1","a2")*logLES  + c("n1","n2")*age
#            # indirect effect
#              logtotalCBCL~ c("b1","b2")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#            # total effect
#              total1 := c1 + (a1*b1)
#              total2 := c2 + (a2*b2)
# '
# sextotalfit <- sem(sextotalmodel, data = sexmeddata, meanstructure = TRUE,
#                    # se = "boot",
#                    # bootstrap = 500,
#                    group="sex",
#                    se = "robust.cluster",
#                    cluster = "site"
# )
# summary(sextotalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained sex
# sex_constall_totalmodel <- ' # direct effect 
#              logtotalCBCL~ c("c1","c1")*logLES + c("m1","m1")*age
#            # mediator 
#              logDERS ~ c("a1","a1")*logLES  + c("n1","n1")*age
#            # indirect effect 
#              logtotalCBCL~ c("b1","b1")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#            # total effect
#              total1 := c1 + (a1*b1)
#              total2 := c1 + (a1*b1)
# '
# sex_constall_totalfit <- sem(sex_constall_totalmodel, data = sexmeddata, meanstructure = TRUE,
#                              # se = "boot",
#                              # bootstrap = 500,
#                              group="sex",
#                              se = "robust.cluster",
#                              cluster = "site")
# summary(sex_constall_totalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of sex
# anova(sextotalfit, sex_constall_totalfit)
# 
# #do the constrained models for gender and sex significantly differ? no
# anova(gender_constall_totalfit,sex_constall_totalfit)
# 
# ### internalizing cbcl
# nogenderintmodel <- ' # direct effect
#              logintCBCL~ c*logLES + c1*age
#            # mediator
#              logDERS ~ a*logLES  + m1*age
#            # indirect effect
#              logintCBCL~ b*logDERS
#            # indirect effect (a*b)
#              ab := a*b
#            # total effect
#              total := c + (a*b)         '
# nogenderintfit <- sem(nogenderintmodel, data = meddata, meanstructure = TRUE,
#                       # se = "boot", 
#                       # bootstrap = 500
#                       se = "robust.cluster",
#                       cluster = "site")
# summary(nogenderintfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# # unconstrained
# genderintmodel <- ' # direct effect 
#              logintCBCL~ c("c1","c2","c3")*logLES + c("m1","m2","m3")*age
#            # mediator 
#              logDERS ~ c("a1","a2","a3")*logLES  + c("n1","n2","n3")*age
#            # indirect effect 
#              logintCBCL~ c("b1","b2","b3")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#              a3b3 := a3*b3
#            # int effect
#              int1 := c1 + (a1*b1)
#              int2 := c2 + (a2*b2)
#              int3 := c3 + (a3*b3)
# '
# genderintfit <- sem(genderintmodel, data = meddata, meanstructure = TRUE,
#                     # se = "boot",
#                     # bootstrap = 500,
#                     group="genderid",
#                     se = "robust.cluster",
#                     cluster = "site")
# summary(genderintfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained
# gender_constall_intmodel <- ' # direct effect 
#              logintCBCL~ c("c1","c1","c1")*logLES + c("m1","m1","m1")*age
#            # mediator 
#              logDERS ~ c("a1","a1","a1")*logLES  + c("n1","n1","n1")*age
#            # indirect effect 
#              logintCBCL~ c("b1","b1","b1")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#              a3b3 := a1*b1
#            # int effect
#              int1 := c1 + (a1*b1)
#              int2 := c1 + (a1*b1)
#              int3 := c1 + (a1*b1)
# '
# gender_constall_intfit <- sem(gender_constall_intmodel, data = meddata, meanstructure = TRUE,
#                               # se = "boot",
#                               # bootstrap = 500,
#                               group="genderid",
#                               se = "robust.cluster",
#                               cluster = "site")
# summary(gender_constall_intfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of gender
# anova(genderintfit, gender_constall_intfit)
# 
# # unconstrained sex
# sexintmodel <- ' # direct effect
#              logintCBCL~ c("c1","c2")*logLES + c("m1","m2")*age
#            # mediator
#              logDERS ~ c("a1","a2")*logLES  + c("n1","n2")*age
#            # indirect effect
#              logintCBCL~ c("b1","b2")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#            # int effect
#              int1 := c1 + (a1*b1)
#              int2 := c2 + (a2*b2)
# '
# sexintfit <- sem(sexintmodel, data = sexmeddata, meanstructure = TRUE,
#                  # se = "boot",
#                  # bootstrap = 500,
#                  group="sex",
#                  se = "robust.cluster",
#                  cluster = "site"
# )
# summary(sexintfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained sex
# sex_constall_intmodel <- ' # direct effect 
#              logintCBCL~ c("c1","c1")*logLES + c("m1","m1")*age
#            # mediator 
#              logDERS ~ c("a1","a1")*logLES  + c("n1","n1")*age
#            # indirect effect 
#              logintCBCL~ c("b1","b1")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#            # int effect
#              int1 := c1 + (a1*b1)
#              int2 := c1 + (a1*b1)
# '
# sex_constall_intfit <- sem(sex_constall_intmodel, data = sexmeddata, meanstructure = TRUE,
#                            # se = "boot",
#                            # bootstrap = 500,
#                            group="sex",
#                            se = "robust.cluster",
#                            cluster = "site")
# summary(sex_constall_intfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of sex
# anova(sexintfit, sex_constall_intfit)
# 
# #do the constrained models for gender and sex significantly differ? no
# anova(gender_constall_intfit,sex_constall_intfit)
# 
# ### externalizing cbcl
# nogenderextmodel <- ' # direct effect
#              logextCBCL~ c*logLES + c1*age
#            # mediator
#              logDERS ~ a*logLES  + m1*age
#            # indirect effect
#              logextCBCL~ b*logDERS
#            # indirect effect (a*b)
#              ab := a*b
#            # total effect
#              total := c + (a*b)         '
# nogenderextfit <- sem(nogenderextmodel, data = meddata, meanstructure = TRUE,
#                       # se = "boot", 
#                       # bootstrap = 500,
#                       se = "robust.cluster",
#                       cluster = "site")
# summary(nogenderextfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# # unconstrained
# genderextmodel <- ' # direct effect 
#              logextCBCL~ c("c1","c2","c3")*logLES + c("m1","m2","m3")*age
#            # mediator 
#              logDERS ~ c("a1","a2","a3")*logLES  + c("n1","n2","n3")*age
#            # indirect effect 
#              logextCBCL~ c("b1","b2","b3")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#              a3b3 := a3*b3
#            # ext effect
#              ext1 := c1 + (a1*b1)
#              ext2 := c2 + (a2*b2)
#              ext3 := c3 + (a3*b3)
# '
# genderextfit <- sem(genderextmodel, data = meddata, meanstructure = TRUE,
#                     # se = "boot",
#                     # bootstrap = 500,
#                     group="genderid",
#                     se = "robust.cluster",
#                     cluster = "site")
# summary(genderextfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained
# gender_constall_extmodel <- ' # direct effect 
#              logextCBCL~ c("c1","c1","c1")*logLES + c("m1","m1","m1")*age
#            # mediator 
#              logDERS ~ c("a1","a1","a1")*logLES  + c("n1","n1","n1")*age
#            # indirect effect 
#              logextCBCL~ c("b1","b1","b1")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#              a3b3 := a1*b1
#            # ext effect
#              ext1 := c1 + (a1*b1)
#              ext2 := c1 + (a1*b1)
#              ext3 := c1 + (a1*b1)
# '
# gender_constall_extfit <- sem(gender_constall_extmodel, data = meddata, meanstructure = TRUE,
#                               # se = "boot",
#                               # bootstrap = 500,
#                               group="genderid",
#                               se = "robust.cluster",
#                               cluster = "site")
# summary(gender_constall_extfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of gender
# anova(genderextfit, gender_constall_extfit)
# 
# # unconstrained sex
# sexextmodel <- ' # direct effect
#              logextCBCL~ c("c1","c2")*logLES + c("m1","m2")*age
#            # mediator
#              logDERS ~ c("a1","a2")*logLES  + c("n1","n2")*age
#            # indirect effect
#              logextCBCL~ c("b1","b2")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#            # ext effect
#              ext1 := c1 + (a1*b1)
#              ext2 := c2 + (a2*b2)
# '
# sexextfit <- sem(sexextmodel, data = sexmeddata, meanstructure = TRUE,
#                  # se = "boot",
#                  # bootstrap = 500,
#                  group="sex",
#                  se = "robust.cluster",
#                  cluster = "site"
# )
# summary(sexextfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained sex
# sex_constall_extmodel <- ' # direct effect 
#              logextCBCL~ c("c1","c1")*logLES + c("m1","m1")*age
#            # mediator 
#              logDERS ~ c("a1","a1")*logLES  + c("n1","n1")*age
#            # indirect effect 
#              logextCBCL~ c("b1","b1")*logDERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#            # ext effect
#              ext1 := c1 + (a1*b1)
#              ext2 := c1 + (a1*b1)
# '
# sex_constall_extfit <- sem(sex_constall_extmodel, data = sexmeddata, meanstructure = TRUE,
#                            # se = "boot",
#                            # bootstrap = 500,
#                            group="sex",
#                            se = "robust.cluster",
#                            cluster = "site")
# summary(sex_constall_extfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of sex
# anova(sexextfit, sex_constall_extfit)
# 
# #do the constrained models for gender and sex significantly differ? no
# anova(gender_constall_extfit,sex_constall_extfit)
# 
# 
# ### not transformed variables (yr3 year 3, DERS and CBCL year 4)
# ### total problems cbcl
# nogendertotalmodel <- ' # direct effect
#              totalCBCL~ c*yr3LES + c1*age
#            # mediator
#              DERS ~ a*yr3LES  + a1*age
#            # indirect effect
#              totalCBCL~ b*DERS
#            # indirect effect (a*b)
#              ab := a*b
#            # total effect
#              total := c + (a*b)         '
# nogendertotalfit <- sem(nogendertotalmodel, data = meddata, meanstructure = TRUE,
#                         # se = "boot",
#                         # bootstrap = 500,
#                         se = "robust.cluster",
#                         cluster = "site"
# )
# summary(nogendertotalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# # semPaths(object = nogendertotalfit, whatLabels = "std") #make diagram with standardized coefficients on edges
# 
# # unconstrained gender
# gendertotalmodel <- ' # direct effect 
#              totalCBCL~ c("c1","c2","c3")*yr3LES + c("m1","m2","m3")*age
#            # mediator 
#              DERS ~ c("a1","a2","a3")*yr3LES  + c("n1","n2","n3")*age
#            # indirect effect 
#              totalCBCL~ c("b1","b2","b3")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#              a3b3 := a3*b3
#            # total effect
#              total1 := c1 + (a1*b1)
#              total2 := c2 + (a2*b2)
#              total3 := c3 + (a3*b3)
# '
# gendertotalfit <- sem(gendertotalmodel, data = meddata, meanstructure = TRUE,
#                       # se = "boot",
#                       # bootstrap = 500,
#                       group="genderid",
#                       se = "robust.cluster",
#                       cluster = "site")
# summary(gendertotalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained gender
# gender_constall_totalmodel <- ' # direct effect 
#              totalCBCL~ c("c1","c1","c1")*yr3LES + c("m1","m1","m1")*age
#            # mediator 
#              DERS ~ c("a1","a1","a1")*yr3LES  + c("n1","n1","n1")*age
#            # indirect effect 
#              totalCBCL~ c("b1","b1","b1")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#              a3b3 := a1*b1
#            # total effect
#              total1 := c1 + (a1*b1)
#              total2 := c1 + (a1*b1)
#              total3 := c1 + (a1*b1)
# '
# gender_constall_totalfit <- sem(gender_constall_totalmodel, data = meddata, meanstructure = TRUE,
#                                 # se = "boot",
#                                 # bootstrap = 500,
#                                 group="genderid",
#                                 se = "robust.cluster",
#                                 cluster = "site")
# summary(gender_constall_totalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of gender
# anova(gendertotalfit, gender_constall_totalfit)
# 
# #do the model without gender and the constrained models for gender significantly differ? no
# anova(nogendertotalfit,gender_constall_totalfit)
# 
# # unconstrained sex
# sextotalmodel <- ' # direct effect
#              totalCBCL~ c("c1","c2")*yr3LES + c("m1","m2")*age
#            # mediator
#              DERS ~ c("a1","a2")*yr3LES  + c("n1","n2")*age
#            # indirect effect
#              totalCBCL~ c("b1","b2")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#            # total effect
#              total1 := c1 + (a1*b1)
#              total2 := c2 + (a2*b2)
# '
# sextotalfit <- sem(sextotalmodel, data = sexmeddata, meanstructure = TRUE,
#                    # se = "boot",
#                    # bootstrap = 500,
#                    group="sex",
#                    se = "robust.cluster",
#                    cluster = "site"
# )
# summary(sextotalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained sex
# sex_constall_totalmodel <- ' # direct effect 
#              totalCBCL~ c("c1","c1")*yr3LES + c("m1","m1")*age
#            # mediator 
#              DERS ~ c("a1","a1")*yr3LES  + c("n1","n1")*age
#            # indirect effect 
#              totalCBCL~ c("b1","b1")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#            # total effect
#              total1 := c1 + (a1*b1)
#              total2 := c1 + (a1*b1)
# '
# sex_constall_totalfit <- sem(sex_constall_totalmodel, data = sexmeddata, meanstructure = TRUE,
#                              # se = "boot",
#                              # bootstrap = 500,
#                              group="sex",
#                              se = "robust.cluster",
#                              cluster = "site")
# summary(sex_constall_totalfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of sex
# anova(sextotalfit, sex_constall_totalfit)
# 
# #do the constrained models for gender and sex significantly differ? no
# anova(gender_constall_totalfit,sex_constall_totalfit)
# 
# ### internalizing cbcl
# nogenderintmodel <- ' # direct effect
#              intCBCL~ c*yr3LES + c1*age
#            # mediator
#              DERS ~ a*yr3LES  + m1*age
#            # indirect effect
#              intCBCL~ b*DERS
#            # indirect effect (a*b)
#              ab := a*b
#            # total effect
#              total := c + (a*b)         '
# nogenderintfit <- sem(nogenderintmodel, data = meddata, meanstructure = TRUE,
#                       # se = "boot", 
#                       # bootstrap = 500
#                       se = "robust.cluster",
#                       cluster = "site")
# summary(nogenderintfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# # unconstrained
# genderintmodel <- ' # direct effect 
#              intCBCL~ c("c1","c2","c3")*yr3LES + c("m1","m2","m3")*age
#            # mediator 
#              DERS ~ c("a1","a2","a3")*yr3LES  + c("n1","n2","n3")*age
#            # indirect effect 
#              intCBCL~ c("b1","b2","b3")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#              a3b3 := a3*b3
#            # int effect
#              int1 := c1 + (a1*b1)
#              int2 := c2 + (a2*b2)
#              int3 := c3 + (a3*b3)
# '
# genderintfit <- sem(genderintmodel, data = meddata, meanstructure = TRUE,
#                     # se = "boot",
#                     # bootstrap = 500,
#                     group="genderid",
#                     se = "robust.cluster",
#                     cluster = "site")
# summary(genderintfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained
# gender_constall_intmodel <- ' # direct effect 
#              intCBCL~ c("c1","c1","c1")*yr3LES + c("m1","m1","m1")*age
#            # mediator 
#              DERS ~ c("a1","a1","a1")*yr3LES  + c("n1","n1","n1")*age
#            # indirect effect 
#              intCBCL~ c("b1","b1","b1")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#              a3b3 := a1*b1
#            # int effect
#              int1 := c1 + (a1*b1)
#              int2 := c1 + (a1*b1)
#              int3 := c1 + (a1*b1)
# '
# gender_constall_intfit <- sem(gender_constall_intmodel, data = meddata, meanstructure = TRUE,
#                               # se = "boot",
#                               # bootstrap = 500,
#                               group="genderid",
#                               se = "robust.cluster",
#                               cluster = "site")
# summary(gender_constall_intfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of gender
# anova(genderintfit, gender_constall_intfit)
# 
# #do the model without gender and the constrained models for gender significantly differ? no
# anova(nogenderintfit,gender_constall_intfit)
# 
# # unconstrained sex
# sexintmodel <- ' # direct effect
#              intCBCL~ c("c1","c2")*yr3LES + c("m1","m2")*age
#            # mediator
#              DERS ~ c("a1","a2")*yr3LES  + c("n1","n2")*age
#            # indirect effect
#              intCBCL~ c("b1","b2")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#            # int effect
#              int1 := c1 + (a1*b1)
#              int2 := c2 + (a2*b2)
# '
# sexintfit <- sem(sexintmodel, data = sexmeddata, meanstructure = TRUE,
#                  # se = "boot",
#                  # bootstrap = 500,
#                  group="sex",
#                  se = "robust.cluster",
#                  cluster = "site"
# )
# summary(sexintfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained sex
# sex_constall_intmodel <- ' # direct effect 
#              intCBCL~ c("c1","c1")*yr3LES + c("m1","m1")*age
#            # mediator 
#              DERS ~ c("a1","a1")*yr3LES  + c("n1","n1")*age
#            # indirect effect 
#              intCBCL~ c("b1","b1")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#            # int effect
#              int1 := c1 + (a1*b1)
#              int2 := c1 + (a1*b1)
# '
# sex_constall_intfit <- sem(sex_constall_intmodel, data = sexmeddata, meanstructure = TRUE,
#                            # se = "boot",
#                            # bootstrap = 500,
#                            group="sex",
#                            se = "robust.cluster",
#                            cluster = "site")
# summary(sex_constall_intfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of sex
# anova(sexintfit, sex_constall_intfit)
# 
# #do the constrained models for gender and sex significantly differ? no
# anova(gender_constall_intfit,sex_constall_intfit)
# 
# ### externalizing cbcl
# nogenderextmodel <- ' # direct effect
#              extCBCL~ c*yr3LES + c1*age
#            # mediator
#              DERS ~ a*yr3LES  + m1*age
#            # indirect effect
#              extCBCL~ b*DERS
#            # indirect effect (a*b)
#              ab := a*b
#            # total effect
#              total := c + (a*b)         '
# nogenderextfit <- sem(nogenderextmodel, data = meddata, meanstructure = TRUE,
#                       # se = "boot", 
#                       # bootstrap = 500,
#                       se = "robust.cluster",
#                       cluster = "site")
# summary(nogenderextfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# # unconstrained
# genderextmodel <- ' # direct effect 
#              extCBCL~ c("c1","c2","c3")*yr3LES + c("m1","m2","m3")*age
#            # mediator 
#              DERS ~ c("a1","a2","a3")*yr3LES  + c("n1","n2","n3")*age
#            # indirect effect 
#              extCBCL~ c("b1","b2","b3")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#              a3b3 := a3*b3
#            # ext effect
#              ext1 := c1 + (a1*b1)
#              ext2 := c2 + (a2*b2)
#              ext3 := c3 + (a3*b3)
# '
# genderextfit <- sem(genderextmodel, data = meddata, meanstructure = TRUE,
#                     # se = "boot",
#                     # bootstrap = 500,
#                     group="genderid",
#                     se = "robust.cluster",
#                     cluster = "site")
# summary(genderextfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained
# gender_constall_extmodel <- ' # direct effect 
#              extCBCL~ c("c1","c1","c1")*yr3LES + c("m1","m1","m1")*age
#            # mediator 
#              DERS ~ c("a1","a1","a1")*yr3LES  + c("n1","n1","n1")*age
#            # indirect effect 
#              extCBCL~ c("b1","b1","b1")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#              a3b3 := a1*b1
#            # ext effect
#              ext1 := c1 + (a1*b1)
#              ext2 := c1 + (a1*b1)
#              ext3 := c1 + (a1*b1)
# '
# gender_constall_extfit <- sem(gender_constall_extmodel, data = meddata, meanstructure = TRUE,
#                               # se = "boot",
#                               # bootstrap = 500,
#                               group="genderid",
#                               se = "robust.cluster",
#                               cluster = "site")
# summary(gender_constall_extfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of gender
# anova(genderextfit, gender_constall_extfit)
# 
# #do the model without gender and the constrained models for gender significantly differ? no
# anova(nogenderextfit,gender_constall_extfit)
# 
# # unconstrained sex
# sexextmodel <- ' # direct effect
#              extCBCL~ c("c1","c2")*yr3LES + c("m1","m2")*age
#            # mediator
#              DERS ~ c("a1","a2")*yr3LES  + c("n1","n2")*age
#            # indirect effect
#              extCBCL~ c("b1","b2")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a2*b2
#            # ext effect
#              ext1 := c1 + (a1*b1)
#              ext2 := c2 + (a2*b2)
# '
# sexextfit <- sem(sexextmodel, data = sexmeddata, meanstructure = TRUE,
#                  # se = "boot",
#                  # bootstrap = 500,
#                  group="sex",
#                  se = "robust.cluster",
#                  cluster = "site"
# )
# summary(sexextfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# 
# #all constrained sex
# sex_constall_extmodel <- ' # direct effect 
#              extCBCL~ c("c1","c1")*yr3LES + c("m1","m1")*age
#            # mediator 
#              DERS ~ c("a1","a1")*yr3LES  + c("n1","n1")*age
#            # indirect effect 
#              extCBCL~ c("b1","b1")*DERS
#            # indirect effect w mediator (a*b)
#              a1b1 := a1*b1
#              a2b2 := a1*b1
#            # ext effect
#              ext1 := c1 + (a1*b1)
#              ext2 := c1 + (a1*b1)
# '
# sex_constall_extfit <- sem(sex_constall_extmodel, data = sexmeddata, meanstructure = TRUE,
#                            # se = "boot",
#                            # bootstrap = 500,
#                            group="sex",
#                            se = "robust.cluster",
#                            cluster = "site")
# summary(sex_constall_extfit, fit.measures=T, standardized=T, ci=TRUE, rsquare=TRUE)
# #fully constrained and fully unconstrained models are not sig diff so no effect of sex
# anova(sexextfit, sex_constall_extfit)
# 
# #do the constrained models for gender and sex significantly differ? no
# anova(gender_constall_extfit,sex_constall_extfit)
# 
# 
# ### standard regression without gender ####
# 
# #how much does site matter?
# site_ders6total_reg <- lmer(ders6total ~ (1|site),data=yr4data)
# variances <- as.data.frame(VarCorr(site_ders6total_reg))
# cluster_var = variances[1,'vcov']
# resid_var = variances[2,'vcov']
# ICC_Y2 <- cluster_var/(cluster_var + resid_var)
# ICC_Y2 #site explains 0.0110 variance in ders6total
# 
# site_le_reg <- lmer(total_bad_le ~ (1|site),data=yr4data)
# variances <- as.data.frame(VarCorr(site_le_reg))
# cluster_var = variances[1,'vcov']
# resid_var = variances[2,'vcov']
# ICC_Y2 <- cluster_var/(cluster_var + resid_var)
# ICC_Y2 #site explains 0.0106 variance in total_bad_le
# 
# site_cbcl_total_reg <- lmer(cbcl_total ~ (1|site),data=yr4data)
# variances <- as.data.frame(VarCorr(site_cbcl_total_reg))
# cluster_var = variances[1,'vcov']
# resid_var = variances[2,'vcov']
# ICC_Y2 <- cluster_var/(cluster_var + resid_var)
# ICC_Y2 #site explains 0.0081 variance in cbcl_total
# 
# site_cbcl_int_reg <- lmer(cbcl_int ~ (1|site),data=yr4data)
# variances <- as.data.frame(VarCorr(site_cbcl_int_reg))
# cluster_var = variances[1,'vcov']
# resid_var = variances[2,'vcov']
# ICC_Y2 <- cluster_var/(cluster_var + resid_var)
# ICC_Y2 #site explains 0.0050 variance in cbcl_int
# 
# site_cbcl_ext_reg <- lmer(cbcl_ext ~ (1|site),data=yr4data)
# variances <- as.data.frame(VarCorr(site_cbcl_ext_reg))
# cluster_var = variances[1,'vcov']
# resid_var = variances[2,'vcov']
# ICC_Y2 <- cluster_var/(cluster_var + resid_var)
# ICC_Y2 #site explains 0.0072 variance in cbcl_ext
# 
# #do life events affect emotion regulation - yes
# ders6total_le_reg <- lmer(ders6total ~ total_bad_le + age + (1|site),data=yr4data)
# summary(ders6total_le_reg)
# AIC(ders6total_le_reg)
# partR2(ders6total_le_reg,partvars=c("total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# #do life events affect cbcl - yes to all
# cbcltotal_le_reg <- lmer(cbcl_total ~ total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotal_le_reg)
# AIC(cbcltotal_le_reg)
# partR2(cbcltotal_le_reg,partvars=c("total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclint_le_reg <- lmer(cbcl_int ~ total_bad_le + age + (1|site),data=yr4data)
# summary(cbclint_le_reg)
# AIC(cbclint_le_reg)
# partR2(cbclint_le_reg,partvars=c("total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclext_le_reg <- lmer(cbcl_ext ~ total_bad_le + age + (1|site),data=yr4data)
# summary(cbclext_le_reg)
# AIC(cbclext_le_reg)
# partR2(cbclext_le_reg,partvars=c("total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# #does emotion regulation affect cbcl - yes to all
# cbcltotal_ders6total_reg <- lmer(cbcl_total ~ ders6total + age + (1|site),data=yr4data)
# summary(cbcltotal_ders6total_reg)
# AIC(cbcltotal_ders6total_reg)
# partR2(cbcltotal_ders6total_reg,partvars=c("ders6total","age"),R2_type="marginal",nboot=10)
# 
# cbclint_ders6total_reg <- lmer(cbcl_int ~ ders6total + age + (1|site),data=yr4data)
# summary(cbclint_ders6total_reg)
# AIC(cbclint_ders6total_reg)
# partR2(cbclint_ders6total_reg,partvars=c("ders6total","age"),R2_type="marginal",nboot=10)
# 
# cbclext_ders6total_reg <- lmer(cbcl_ext ~ ders6total + age + (1|site),data=yr4data)
# summary(cbclext_ders6total_reg)
# AIC(cbclext_ders6total_reg)
# partR2(cbclext_ders6total_reg,partvars=c("ders6total","age"),R2_type="marginal",nboot=10)
# 
# #do emotion regulation and life events interact to affect cbcl - no to all
# cbcltotal_le_ders6total_reg <- lmer(cbcl_total ~ ders6total*total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotal_le_ders6total_reg) #main effect ders and le and age but no interaction
# #no interaction so use just additive model
# cbcltotal_le_ders6total_reg <- lmer(cbcl_total ~ ders6total+total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotal_le_ders6total_reg) #main effect ders and le 
# AIC(cbcltotal_le_ders6total_reg) #main effect ders and le 
# partR2(cbcltotal_le_ders6total_reg,partvars=c("ders6total","total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclint_le_ders6total_reg <- lmer(cbcl_int ~ ders6total*total_bad_le + age + (1|site),data=yr4data)
# summary(cbclint_le_ders6total_reg) #main effect ders only and not le or age
# #no interaction so use just additive model
# cbclint_le_ders6total_reg <- lmer(cbcl_int ~ ders6total+total_bad_le + age + (1|site),data=yr4data)
# summary(cbclint_le_ders6total_reg) #main effect ders and le but not age
# AIC(cbclint_le_ders6total_reg) #main effect ders and le but not age
# partR2(cbclint_le_ders6total_reg,partvars=c("ders6total","total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclext_le_ders6total_reg <- lmer(cbcl_ext ~ ders6total*total_bad_le + age + (1|site),data=yr4data)
# summary(cbclext_le_ders6total_reg) #main effect ders and le and age but no interaction
# #no interaction so use just additive model
# cbclext_le_ders6total_reg <- lmer(cbcl_ext ~ ders6total+total_bad_le + age + (1|site),data=yr4data)
# summary(cbclext_le_ders6total_reg) #main effect ders and le and age 
# AIC(cbclext_le_ders6total_reg) #main effect ders and le and age 
# partR2(cbclext_le_ders6total_reg,partvars=c("ders6total","total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# ### standard regression adding in gender ####
# #does gender affect age - gd 1.1893 months younger than cis boys p = 0.0251
# age_gender_reg <- lmer(age ~ genderid +(1|site),data=yr4data)
# summary(age_gender_reg)
# 
# #does gender affect life events - same as KW results above
# le_gender_reg <- lmer(total_bad_le ~ genderid + age + (1|site),data=yr4data)
# summary(le_gender_reg)
# 
# #does gender affect emotional regulation problems - same as KW results above
# ders6total_gender_reg <- lmer(ders6total ~ genderid + age + (1|site),data=yr4data)
# summary(ders6total_gender_reg)
# 
# #does gender affect cbcl - same as KW results above
# cbcltotal_gender_reg <- lmer(cbcl_total ~ genderid + age + (1|site),data=yr4data)
# summary(cbcltotal_gender_reg)
# cbclint_gender_reg <- lmer(cbcl_int ~ genderid + age + (1|site),data=yr4data)
# summary(cbclint_gender_reg)
# cbclext_gender_reg <- lmer(cbcl_ext ~ genderid + age + (1|site),data=yr4data)
# summary(cbclext_gender_reg)
# 
# #do gender and life events interact to affect emotion regulation - yes
# ders6total_gender_le_reg <- lmer(ders6total ~ genderid*total_bad_le + age + (1|site),data=yr4data)
# summary(ders6total_gender_le_reg) #main effects cis girl and gd and le but no interactions
# #no interaction so just do additive model
# ders6total_gender_le_reg <- lmer(ders6total ~ genderid+total_bad_le + age + (1|site),data=yr4data)
# summary(ders6total_gender_le_reg) #main effects cis girl and gd and le
# AIC(ders6total_gender_le_reg) #main effects cis girl and gd and le
# partR2(ders6total_gender_le_reg,partvars=c("genderid","total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# #do gender and life events interact to affect cbcl - main effect but no interactions to all
# cbcltotal_gender_le_reg <- lmer(cbcl_total ~ genderid*total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotal_gender_le_reg) #main effect gd and le and age but not cis girl or interaction
# #no interaction so use just additive model
# cbcltotal_gender_le_reg <- lmer(cbcl_total ~ genderid+total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotal_gender_le_reg) #main effect gd and le and age but not cis girl
# AIC(cbcltotal_gender_le_reg) #main effect gd and le and age but not cis girl
# partR2(cbcltotal_gender_le_reg,partvars=c("genderid","total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclint_gender_le_reg <- lmer(cbcl_int ~ genderid*total_bad_le + age + (1|site),data=yr4data)
# summary(cbclint_gender_le_reg) #main effect gd and le but not cis girl or age or interaction
# #no interaction so use just additive model
# cbclint_gender_le_reg <- lmer(cbcl_int ~ genderid+total_bad_le + age + (1|site),data=yr4data)
# summary(cbclint_gender_le_reg) #main effect gd and cis girl and le but not age 
# AIC(cbclint_gender_le_reg) #main effect gd and cis girl and le but not age 
# partR2(cbclint_gender_le_reg,partvars=c("genderid","total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclext_gender_le_reg <- lmer(cbcl_ext ~ genderid*total_bad_le + age + (1|site),data=yr4data)
# summary(cbclext_gender_le_reg) #main effect gd and cis girl and le and age but not interaction
# #no interaction so use just additive model
# cbclext_gender_le_reg <- lmer(cbcl_ext ~ genderid+total_bad_le + age + (1|site),data=yr4data)
# summary(cbclext_gender_le_reg) #main effect gd and cis girl and le and age 
# AIC(cbclext_gender_le_reg) #main effect gd and cis girl and le and age 
# partR2(cbclext_gender_le_reg,partvars=c("genderid","total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# #do gender and emotion regulation interact to affect cbcl - main effects but no interactions to all
# cbcltotalgender_ders6total_reg <- lmer(cbcl_total ~ genderid*ders6total + age + (1|site),data=yr4data)
# summary(cbcltotalgender_ders6total_reg) #main effect gd and ders but not cis girl or interaction
# #no interaction so use just additive model
# cbcltotalgender_ders6total_reg <- lmer(cbcl_total ~ genderid+ders6total + age + (1|site),data=yr4data)
# summary(cbcltotalgender_ders6total_reg) #main effect gd and ders but not cis girl or interaction
# AIC(cbcltotalgender_ders6total_reg) #main effect gd and ders but not cis girl or interaction
# partR2(cbcltotalgender_ders6total_reg,partvars=c("genderid","ders6total","age"),R2_type="marginal",nboot=10)
# 
# cbclintgender_ders6total_reg <- lmer(cbcl_int ~ genderid*ders6total + age + (1|site),data=yr4data)
# summary(cbclintgender_ders6total_reg) #main effect ders but not cis girl or gd or interaction
# #no interaction so use just additive model
# cbclintgender_ders6total_reg <- lmer(cbcl_int ~ genderid+ders6total + age + (1|site),data=yr4data)
# summary(cbclintgender_ders6total_reg) #main effect ders but not cis girl or gd or interaction
# AIC(cbclintgender_ders6total_reg) #main effect ders but not cis girl or gd or interaction
# partR2(cbclintgender_ders6total_reg,partvars=c("genderid","ders6total","age"),R2_type="marginal",nboot=10)
# 
# cbclextgender_ders6total_reg <- lmer(cbcl_ext ~ genderid*ders6total + age + (1|site),data=yr4data)
# summary(cbclextgender_ders6total_reg) #main effect ders but not cis girl or gd or interaction
# #no interaction so use just additive model
# cbclextgender_ders6total_reg <- lmer(cbcl_ext ~ genderid+ders6total + age + (1|site),data=yr4data)
# summary(cbclextgender_ders6total_reg) #main effect ders but not cis girl or gd or interaction
# AIC(cbclextgender_ders6total_reg) #main effect ders but not cis girl or gd or interaction
# partR2(cbclextgender_ders6total_reg,partvars=c("genderid","ders6total","age"),R2_type="marginal",nboot=10)
# 
# #do gender, emotion regulation and life events affect cbcl - 
# cbcltotalgender_le_ders6total_reg <- lmer(cbcl_total ~ genderid + ders6total + total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotalgender_le_ders6total_reg) #cis girl, gd, ders, and le but not age significant
# AIC(cbcltotalgender_le_ders6total_reg) #cis girl, gd, ders, and le but not age significant
# partR2(cbcltotalgender_le_ders6total_reg,partvars=c("genderid","ders6total","total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclintgender_le_ders6total_reg <- lmer(cbcl_int ~ genderid + ders6total + total_bad_le + age + (1|site),data=yr4data)
# summary(cbclintgender_le_ders6total_reg) #cis girl, gd, ders, and le but not age significant
# AIC(cbclintgender_le_ders6total_reg) #cis girl, gd, ders, and le but not age significant
# partR2(cbclintgender_le_ders6total_reg,partvars=c("genderid","ders6total","total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclextgender_le_ders6total_reg <- lmer(cbcl_ext ~ genderid + ders6total + total_bad_le + age + (1|site),data=yr4data)
# summary(cbclextgender_le_ders6total_reg) #ders and le and age but not cis girl or gd significant
# AIC(cbclextgender_le_ders6total_reg) #ders and le and age but not cis girl or gd significant
# partR2(cbclextgender_le_ders6total_reg,partvars=c("genderid","ders6total","total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# 
# ### standard regression log transformed les, ders and cbcl, without gender ####
# #do life events affect emotion regulation - yes
# logders6total_logle_reg <- lmer(logders6total ~ log_total_bad_le + age + (1|site),data=yr4data)
# summary(logders6total_logle_reg)
# AIC(logders6total_logle_reg)
# partR2(logders6total_logle_reg,partvars=c("log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# #do life events affect cbcl - yes to all
# cbcltotal_logle_reg <- lmer(logcbcl_total ~ log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotal_logle_reg)
# AIC(cbcltotal_logle_reg)
# partR2(cbcltotal_logle_reg,partvars=c("log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclint_logle_reg <- lmer(logcbcl_int ~ log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclint_logle_reg)
# AIC(cbclint_logle_reg)
# partR2(cbclint_logle_reg,partvars=c("log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclext_logle_reg <- lmer(logcbcl_ext ~ log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclext_logle_reg)
# AIC(cbclext_logle_reg)
# partR2(cbclext_logle_reg,partvars=c("log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# #does emotion regulation affect cbcl - yes to all
# cbcltotal_logders6total_reg <- lmer(logcbcl_total ~ logders6total + age + (1|site),data=yr4data)
# summary(cbcltotal_logders6total_reg)
# AIC(cbcltotal_logders6total_reg)
# partR2(cbcltotal_logders6total_reg,partvars=c("logders6total","age"),R2_type="marginal",nboot=10)
# 
# cbclint_logders6total_reg <- lmer(logcbcl_int ~ logders6total + age + (1|site),data=yr4data)
# summary(cbclint_logders6total_reg)
# AIC(cbclint_logders6total_reg)
# partR2(cbclint_logders6total_reg,partvars=c("logders6total","age"),R2_type="marginal",nboot=10)
# 
# cbclext_logders6total_reg <- lmer(logcbcl_ext ~ logders6total + age + (1|site),data=yr4data)
# summary(cbclext_logders6total_reg)
# AIC(cbclext_logders6total_reg)
# partR2(cbclext_logders6total_reg,partvars=c("logders6total","age"),R2_type="marginal",nboot=10)
# 
# #do emotion regulation and life events interact to affect cbcl
# cbcltotal_le_logders6total_reg <- lmer(logcbcl_total ~ logders6total*log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotal_le_logders6total_reg) #no sig ixn
# cbcltotal_le_logders6total_reg <- lmer(logcbcl_total ~ logders6total+log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotal_le_logders6total_reg) #main effect ders and le 
# AIC(cbcltotal_le_logders6total_reg) #main effect ders and le 
# partR2(cbcltotal_le_logders6total_reg,partvars=c("logders6total","log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# 
# cbclint_le_logders6total_reg <- lmer(logcbcl_int ~ logders6total*log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclint_le_logders6total_reg) #sig ixn 
# cbclint_le_logders6total_reg <- lmer(logcbcl_int ~ logders6total+log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclint_le_logders6total_reg) #main effect ders and le but not age
# AIC(cbclint_le_logders6total_reg) #main effect ders and le but not age
# partR2(cbclint_le_logders6total_reg,partvars=c("logders6total","log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclext_le_logders6total_reg <- lmer(logcbcl_ext ~ logders6total*log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclext_le_logders6total_reg) #sig ixn
# cbclext_le_logders6total_reg <- lmer(logcbcl_ext ~ logders6total+log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclext_le_logders6total_reg) #main effect ders and le and age 
# AIC(cbclext_le_logders6total_reg) #main effect ders and le and age 
# partR2(cbclext_le_logders6total_reg,partvars=c("logders6total","log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# ### standard regression log transformed les, ders and cbcl, adding in gender ####
# #does gender affect age - gd 1.1893 months younger than cis boys p = 0.0251
# age_gender_reg <- lmer(age ~ genderid +(1|site),data=yr4data)
# summary(age_gender_reg)
# 
# #does gender affect life events - same as KW results above
# le_gender_reg <- lmer(log_total_bad_le ~ genderid + age + (1|site),data=yr4data)
# summary(le_gender_reg)
# 
# #does gender affect emotional regulation problems - same as KW results above
# logders6total_gender_reg <- lmer(logders6total ~ genderid + age + (1|site),data=yr4data)
# summary(logders6total_gender_reg)
# 
# #does gender affect cbcl - same as KW results above
# cbcltotal_gender_reg <- lmer(logcbcl_total ~ genderid + age + (1|site),data=yr4data)
# summary(cbcltotal_gender_reg)
# cbclint_gender_reg <- lmer(logcbcl_int ~ genderid + age + (1|site),data=yr4data)
# summary(cbclint_gender_reg)
# cbclext_gender_reg <- lmer(logcbcl_ext ~ genderid + age + (1|site),data=yr4data)
# summary(cbclext_gender_reg)
# 
# #do gender and life events interact to affect emotion regulation - yes
# logders6total_gender_le_reg <- lmer(logders6total ~ genderid*log_total_bad_le + age + (1|site),data=yr4data)
# summary(logders6total_gender_le_reg) #main effects cis girl and gd and le but no interactions
# #no interaction so just do additive model
# logders6total_gender_le_reg <- lmer(logders6total ~ genderid+log_total_bad_le + age + (1|site),data=yr4data)
# summary(logders6total_gender_le_reg) #main effects cis girl and gd and le
# AIC(logders6total_gender_le_reg) #main effects cis girl and gd and le
# partR2(logders6total_gender_le_reg,partvars=c("genderid","log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# #do gender and life events interact to affect cbcl - main effect but no interactions to all
# cbcltotal_gender_le_reg <- lmer(logcbcl_total ~ genderid*log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotal_gender_le_reg) #main effect gd and le and age but not cis girl or interaction
# #no interaction so use just additive model
# cbcltotal_gender_le_reg <- lmer(logcbcl_total ~ genderid+log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotal_gender_le_reg) #main effect gd and le and age but not cis girl
# AIC(cbcltotal_gender_le_reg) #main effect gd and le and age but not cis girl
# partR2(cbcltotal_gender_le_reg,partvars=c("genderid","log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclint_gender_le_reg <- lmer(logcbcl_int ~ genderid*log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclint_gender_le_reg) #main effect gd and le but not cis girl or age or interaction
# #no interaction so use just additive model
# cbclint_gender_le_reg <- lmer(logcbcl_int ~ genderid+log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclint_gender_le_reg) #main effect gd and le but not cis girl or age 
# AIC(cbclint_gender_le_reg) #main effect gd and le but not cis girl or age 
# partR2(cbclint_gender_le_reg,partvars=c("genderid","log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclext_gender_le_reg <- lmer(logcbcl_ext ~ genderid*log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclext_gender_le_reg) #main effect gd and cis girl and le and age but not interaction
# #no interaction so use just additive model
# cbclext_gender_le_reg <- lmer(logcbcl_ext ~ genderid+log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclext_gender_le_reg) #main effect gd and cis girl and le and age 
# AIC(cbclext_gender_le_reg) #main effect gd and cis girl and le and age 
# partR2(cbclext_gender_le_reg,partvars=c("genderid","log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# #do gender and emotion regulation interact to affect cbcl - main effects but no interactions to all
# cbcltotalgender_logders6total_reg <- lmer(logcbcl_total ~ genderid*logders6total + age + (1|site),data=yr4data)
# summary(cbcltotalgender_logders6total_reg) #main effect ders but not gd or cis girl or interaction
# #no interaction so use just additive model
# cbcltotalgender_logders6total_reg <- lmer(logcbcl_total ~ genderid+logders6total + age + (1|site),data=yr4data)
# summary(cbcltotalgender_logders6total_reg) #main effect gd and ders and cis girl
# AIC(cbcltotalgender_logders6total_reg) #main effect gd and ders but not cis girl 
# partR2(cbcltotalgender_logders6total_reg,partvars=c("genderid","logders6total","age"),R2_type="marginal",nboot=10)
# 
# cbclintgender_logders6total_reg <- lmer(logcbcl_int ~ genderid*logders6total + age + (1|site),data=yr4data)
# summary(cbclintgender_logders6total_reg) #main effect ders but not cis girl or gd or interaction
# #no interaction so use just additive model
# cbclintgender_logders6total_reg <- lmer(logcbcl_int ~ genderid+logders6total + age + (1|site),data=yr4data)
# summary(cbclintgender_logders6total_reg) #main effect ders and cis girl and gd 
# AIC(cbclintgender_logders6total_reg) #main effect ders and cis girl and gd 
# partR2(cbclintgender_logders6total_reg,partvars=c("genderid","logders6total","age"),R2_type="marginal",nboot=10)
# 
# cbclextgender_logders6total_reg <- lmer(logcbcl_ext ~ genderid*logders6total + age + (1|site),data=yr4data)
# summary(cbclextgender_logders6total_reg) 
# #no interaction so use just additive model
# cbclextgender_logders6total_reg <- lmer(logcbcl_ext ~ genderid+logders6total + age + (1|site),data=yr4data)
# summary(cbclextgender_logders6total_reg) #main effect ders but not cis girl or gd 
# AIC(cbclextgender_logders6total_reg) #main effect ders but not cis girl or gd 
# partR2(cbclextgender_logders6total_reg,partvars=c("genderid","logders6total","age"),R2_type="marginal",nboot=10)
# 
# #do gender, emotion regulation and life events affect cbcl - 
# cbcltotalgender_le_logders6total_reg <- lmer(logcbcl_total ~ genderid + logders6total + log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbcltotalgender_le_logders6total_reg) #cis girl, gd, ders, and le and age significant
# AIC(cbcltotalgender_le_logders6total_reg) #cis girl, gd, ders, and le and age significant
# partR2(cbcltotalgender_le_logders6total_reg,partvars=c("genderid","logders6total","log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclintgender_le_logders6total_reg <- lmer(logcbcl_int ~ genderid + logders6total + log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclintgender_le_logders6total_reg) #cis girl, gd, ders, and le but not age significant
# AIC(cbclintgender_le_logders6total_reg) #cis girl, gd, ders, and le but not age significant
# partR2(cbclintgender_le_logders6total_reg,partvars=c("genderid","logders6total","log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# cbclextgender_le_logders6total_reg <- lmer(logcbcl_ext ~ genderid + logders6total + log_total_bad_le + age + (1|site),data=yr4data)
# summary(cbclextgender_le_logders6total_reg) #ders and le and age and cis girl but not gd significant
# AIC(cbclextgender_le_logders6total_reg) #ders and le and age and cis girl but not gd significant
# partR2(cbclextgender_le_logders6total_reg,partvars=c("genderid","logders6total","log_total_bad_le","age"),R2_type="marginal",nboot=10)
# 
# 
