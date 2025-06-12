library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(cowplot)



ESPRIT_full <- read_excel("/Users/zanwynia/Desktop/Datasets/ESPRIT_labels.xlsx")



###----------------------------------------------------------------------------
#Adding labels to participants in each cohort

table(ESPRIT_full$`Which group will participant be joining?`)

ESPRIT_full <- ESPRIT_full %>% 
  rename(cohort = `Which group will participant be joining?`,
         age = `Current Age:`,
         gender = `What is your current gender identity?`,
         race = `Which race do you identify with most?`,
         hispanic = `Are you of Hispanic, Latino, or Spanish origin?`, 
         edu = `What is your education level:`,
         employ = `What is your employment status?`,
         married = `What is your current marital status?`) %>%
  mutate(cohort = case_when(
    cohort == "May 14th - July 2nd (Tuesdays at 10am)" ~ "Cohort 1", 
    cohort == "May 30th - July 25nd (Thursdays at 5pm)" ~ "Cohort 2", 
    cohort == "September 24th - November 12th (Tuesdays, at 10am)" ~ "Cohort 3", 
    cohort == "September 26th - November 14th (Thursdays at 5pm)" ~ "Cohort 4"
  )) %>% 
  mutate(fg_change = case_when(
    cohort == "Cohort 1" ~ "Cohort 1&2",
    cohort == "Cohort 2" ~ "Cohort 1&2",
    cohort == "Cohort 3" ~ "Cohort 3&4",
    cohort == "Cohort 4" ~ "Cohort 3&4"
  ))


###-----------------------------------------------------------------------------
#Subsetting data so it's only people who were actually in cohort (i.e. getting rid 
#of all the people who were screened out)

ESPRIT_full <- ESPRIT_full %>% 
  rename(ids = `Record ID`)

participant_ids <- ESPRIT_full %>% 
  filter(!is.na(cohort)) %>% 
  pull(ids) %>% 
  unique()



ESPRIT_full <- ESPRIT_full %>% 
  filter(ids %in% participant_ids)


ESPRIT_full$ids <- as.factor(ESPRIT_full$ids)

unique(ESPRIT_full$ids)


ESPRIT_full <- ESPRIT_full %>% 
  group_by(ids) %>% 
  fill(cohort, .direction = "downup") %>% 
  ungroup() %>% 
  group_by(ids) %>% 
  fill(fg_change, .direction = "downup") %>% 
  ungroup()


#Checking to make sure cohort and id filling in happened correctly
ESPRIT_full %>%
  filter(ids == 67) %>%
  select(ids, cohort, `Event Name`) %>%
  arrange(`Event Name`)



###-----------------------------------------------------------------------------
#Creating new variables in the dataset that properly score our outcomes (e.g. PEG)

#PEG
ESPRIT_full <- ESPRIT_full %>% 
  rename(intensity = `What number best describes your pain on average in the past week?`,
         enjoyment = `What number best describes how, during the past week, pain has interfered with your enjoyment of life?`,
         activity = `What number best describes how, during the past week, pain has interfered with your general activity?`)

#Intensity, enjoyment, and activity currently stored as a character variable because of values at 
#10 and 0
ESPRIT_full <- ESPRIT_full %>% 
  mutate(across(c(intensity, enjoyment, activity), ~ case_when(
    .x == "10 - pain as bad as you can imagine" ~ "10", 
    .x == "0 - no pain" ~ "0", 
    TRUE ~ .x
  ))) %>% 
  mutate(across(c(intensity, enjoyment, activity), as.numeric))


ESPRIT_full$peg <- (ESPRIT_full$intensity + ESPRIT_full$enjoyment + ESPRIT_full$activity)/3

summary(ESPRIT_full$peg)

#PROMIS-Global Health Physical 

ESPRIT_full <- ESPRIT_full %>% 
  rename(promis_global01 = `In general, would you say your health is:`,
         promis_global02 = `In general, would you say your quality of life is:`,
         promis_global03 = `In general, how would you rate your physical health?`,
         promis_global04 = `In general, how would you rate your mental health, including your mood and your ability to think?`,
         promis_global05 = `In general, how would you rate your satisfaction with your social activities and relationships?`,
         promis_global09 = `In general, please rate how well you carry out your usual social activities and roles. (This includes activities at home, at work and in your community, and responsibilities as a parent, child, spouse, employee, friend, etc.)`,
         promis_global06 = `To what extent are you able to carry out your everyday physical activities such as walking, climbing stairs, carrying groceries, or moving a chair?`,
         promis_global10 = `In the past 7 days  How often have you been bothered by emotional problems such as feeling anxious, depressed or irritable?`,
         promis_global08 = `In the past 7 days  How would you rate your fatigue on average?`)


#Creating a _num version of PROMIS variables so we can keep both the character version and the numeric
#version of the variable. Will do that for other variables as well
col_name_num <- function(df, column_prefix, suffix="_num"){
  df %>% 
    mutate(across(starts_with(column_prefix), .names = paste0("{col}", suffix)))
}

ESPRIT_full <- col_name_num(ESPRIT_full, "promis_global")


#Now defining coding schemes

poor_to_excellent <- c("Poor" = 1, "Fair" = 2, "Good" = 3, "Very good" = 4, "Excellent" = 5)
not_at_all <- c("Not at all" = 1, "A little" = 2, "Moderately" = 3, "Mostly" = 4, "Completely" = 5)
severity <- c("Very severe" = 1, "Severe" = 2, "Moderate" = 3, "Mild" = 4, "None" = 5)
frequency <- c("Always" = 1, "Often" = 2, "Sometimes" = 3, "Rarely" = 4, "Never" = 5)


ESPRIT_full <- ESPRIT_full %>% 
  mutate(
    across(c(promis_global01_num, promis_global02_num, promis_global03_num, promis_global04_num, 
             promis_global05_num, promis_global09_num), 
           ~ poor_to_excellent[.x]),
    promis_global06_num = not_at_all[promis_global06_num], 
    promis_global08_num = severity[promis_global08_num],
    promis_global10_num = frequency[promis_global10_num]
  )



ESPRIT_full$promis_physical <- ESPRIT_full$promis_global03_num + ESPRIT_full$promis_global06_num + ESPRIT_full$promis_global08_num

ESPRIT_full$promis_mental <- ESPRIT_full$promis_global04_num + ESPRIT_full$promis_global05_num + ESPRIT_full$promis_global09_num + ESPRIT_full$promis_global10_num

ESPRIT_full <- ESPRIT_full %>% 
  rename(promis_general = promis_global01_num, 
         promis_quality = promis_global02_num)


#PHQ-8
ESPRIT_full <- ESPRIT_full %>% 
  rename(
    phq8_1 = `Little interest or pleasure in doing things`,
    phq8_2 = `Feeling down, depressed, or hopeless`,
    phq8_3 = `Trouble falling or staying asleep, or sleeping too much`, 
    phq8_4 = `Feeling tired or having little energy`, 
    phq8_5 = `Poor appetite or overeating`, 
    phq8_6 = `Feeling bad about yourself -- or that you are a failure or have let yourself or your family down`, 
    phq8_7 = `Trouble concentrating on things, such as reading the newspaper or watching television`, 
    phq8_8 = `Moving or speaking so slowly that other people could have noticed? Or the opposite -- being so fidgety or restless that you have been moving around a lot more than usual`
  )


ESPRIT_full <- col_name_num(ESPRIT_full, "phq8")

days <- c("0- Not at all" = 0, "1- Several days" = 1, "2- More than half the days" = 2, 
          "3- Nearly every day" = 3)

ESPRIT_full <- ESPRIT_full %>% 
  mutate(across(c(phq8_1_num, phq8_2_num, phq8_3_num, phq8_4_num, phq8_5_num, phq8_6_num, phq8_7_num, 
                  phq8_8_num), ~days[.x]))


ESPRIT_full$depression <- ESPRIT_full$phq8_1_num + ESPRIT_full$phq8_2_num + ESPRIT_full$phq8_3_num + ESPRIT_full$phq8_4_num + ESPRIT_full$phq8_5_num + ESPRIT_full$phq8_6_num + ESPRIT_full$phq8_7_num + ESPRIT_full$phq8_8_num

summary(ESPRIT_full$depression)

#GAD 7 

ESPRIT_full <- ESPRIT_full %>% 
  rename(
    gad1 = `Feeling nervous, anxious, or on edge`, 
    gad2 = `Not being able to stop or control worrying`, 
    gad3 = `Worrying too much about different things`, 
    gad4 = `Trouble relaxing`, 
    gad5 = `Being so restless that it's hard to sit still`, 
    gad6 = `Becoming easily annoyed or irritable`, 
    gad7 = `Feeling afraid as if something awful might happen`,
    anx_difficulty = `If you checked off any problems, how difficult have these made it for you to do your work, take care of things at home, or get along with other people?`
  )


days_gad <- c("Not at all" = 0, "Several days" = 1, "More than half the days" = 2, "Nearly every day" = 3)

ESPRIT_full <- col_name_num(ESPRIT_full, "gad")

ESPRIT_full <- ESPRIT_full %>% 
  mutate(across(c(gad1_num, gad2_num, gad3_num, gad4_num, gad5_num, gad6_num, gad7_num), 
                ~days_gad[.x]))

ESPRIT_full$anxiety <- ESPRIT_full$gad1_num + ESPRIT_full$gad2_num + ESPRIT_full$gad3_num + ESPRIT_full$gad4_num + ESPRIT_full$gad5_num + ESPRIT_full$gad6_num + ESPRIT_full$gad7_num


summary(ESPRIT_full$anxiety)


#TSK-11

ESPRIT_full <- ESPRIT_full %>% 
  rename(
    tsk11_1 = `I'm afraid that I might injure myself if I exercise`, 
    tsk11_2 = `If I were to try to overcome it, my pain would increase`, 
    tsk11_3 = `My body is telling me I have something dangerously wrong`, 
    tsk11_4 = `People aren't taking my medical condition seriously enough`, 
    tsk11_5 = `My accident/injury/problem has put my body at risk for the rest of my life`, 
    tsk11_6 = `Pain always means I have injured my body`, 
    tsk11_7 = `Simply being careful that I do not make any unnecessary movements is the safest thing I can do to prevent my pain from worsening`, 
    tsk11_8 = `I wouldn't have this much pain if there weren't something potentially dangerous going on in my body`, 
    tsk11_9 = `Pain lets me know when to stop exercising so that I don't injure myself`, 
    tsk11_10 = `I can't do all the things normal people do because it's too easy for me to get injured`,
    tsk11_11 = `No one should have to exercise when he/she is in pain`
  )


ESPRIT_full <- col_name_num(ESPRIT_full, "tsk11")

agree <- c("Strongly disagree" = 1, "Disagree" = 2, "Agree" = 3, "Strongly agree" = 4)

ESPRIT_full <- ESPRIT_full %>% 
  mutate(across(c(tsk11_1_num, tsk11_2_num, tsk11_3_num, tsk11_4_num, tsk11_5_num, 
                  tsk11_6_num, tsk11_7_num, tsk11_8_num, tsk11_9_num, tsk11_10_num,
                  tsk11_11_num), ~agree[.x]))


ESPRIT_full$kinesophobia <- ESPRIT_full$tsk11_1_num +ESPRIT_full$tsk11_2_num + ESPRIT_full$tsk11_3_num + ESPRIT_full$tsk11_4_num + ESPRIT_full$tsk11_5_num + ESPRIT_full$tsk11_6_num + ESPRIT_full$tsk11_7_num + ESPRIT_full$tsk11_8_num + ESPRIT_full$tsk11_9_num + ESPRIT_full$tsk11_10_num + ESPRIT_full$tsk11_11_num

summary(ESPRIT_full$kinesophobia)


#Concerns about pain scale (CAP)
ESPRIT_full <- ESPRIT_full %>% 
  rename(
    cap1 = `My pain is more than I can manage.`, 
    cap2 = `Because of my pain, I will never be happy again.`, 
    cap3 = `Because of my pain, my life is terrible.`, 
    cap4 = `My life will only get worse because of my pain.`, 
    cap5 = `In the past 7 days, how often did you keep thinking about how much it hurts?`, 
    cap6 = `In the past 7 days, how often did you have trouble thinking of anything other than your pain?`
  )

ESPRIT_full <- col_name_num(ESPRIT_full, "cap")

freq_flip <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Always" = 5)

ESPRIT_full <- ESPRIT_full %>% 
  mutate(across(c(cap1_num, cap2_num, cap3_num, cap4_num, cap5_num, cap6_num), 
                ~freq_flip[.x]))


ESPRIT_full$painconcern <- ESPRIT_full$cap1_num + ESPRIT_full$cap2_num + ESPRIT_full$cap3_num + ESPRIT_full$cap4_num + ESPRIT_full$cap5_num + ESPRIT_full$cap6_num


summary(ESPRIT_full$painconcern)



#Life Engagement Test 

ESPRIT_full <- ESPRIT_full %>% 
  rename(
    let1 = `There is not enough purpose in my life.`,
    let2 = `To me, the things I do are all worthwhile.`,
    let3 = `Most of what I do seems trivial and unimportant to me.`,
    let4 = `I value my activities a lot.`, 
    let5 = `I don't care very much about the things I do.`, 
    let6 = `I have lots of reasons for living.`, 
  )

ESPRIT_full <- col_name_num(ESPRIT_full, "let")

agree_let <- c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3, "Agree" = 4, "Strongly agree" = 5)
agree_let_flip <- c("Strongly disagree" = 5, "Disagree" = 4, "Neutral" = 3, "Agree" = 2, "Strongly agree" = 1)

ESPRIT_full <- ESPRIT_full %>% 
  mutate(across(c(let2_num, let4_num, let6_num), ~agree_let[.x]),
         across(c(let1_num, let3_num, let5_num), ~agree_let_flip[.x]))


ESPRIT_full$engagement <- ESPRIT_full$let1_num + ESPRIT_full$let2_num + ESPRIT_full$let3_num + ESPRIT_full$let4_num + ESPRIT_full$let5_num + ESPRIT_full$let6_num

summary(ESPRIT_full$engagement)

#Widespread Pain Index 

table(ESPRIT_full$`Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=a. Shoulder girdle, left)`)

ESPRIT_full <- ESPRIT_full %>% 
  rename(
    wpi_severity1 = `Fatigue or tiredness through the day`, 
    wpi_severity2 = `Waking up tired or unrefreshed`, 
    wpi_severity3 = `Trouble thinking or remembering`,
    shoulder_girdle_left = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=a. Shoulder girdle, left)`,
    shoulder_girdle_right = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=b. Shoulder girdle, right)`,
    upper_arm_left = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=c. Upper arm, left)`,
    upper_arm_right = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=d. Upper arm, right)`,
    lower_arm_left = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=e. Lower arm, left)`,
    lower_arm_right = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=f. Lower arm, right)`,
    hip_left = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=g. Hip (buttock) left)`,
    hip_right = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=h. Hip (buttock) right)`,
    upper_leg_left = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=i. Upper leg, left)`,
    upper_leg_right = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=j. Upper leg, right)`,
    lower_leg_left = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=k. Lower leg, left)`,
    lower_leg_right = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=l. Lower leg, right)`,
    jaw_left = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=m. Jaw, left)`,
    jaw_right = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=n. Jaw, right)`,
    chest = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=o, Chest)`, 
    abdomen = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=p. Abdomen)`, 
    upper_back = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=q. Upper back)`,
    lower_back = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=r. Lower back)`, 
    neck = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=s. Neck)`
  )


pain_vars <- c("shoulder_girdle_left", "shoulder_girdle_right", "upper_arm_left", 
               "upper_arm_right", "lower_arm_left", "lower_arm_right", 
               "hip_left", "hip_right", "upper_leg_left", "upper_leg_right", 
               "lower_leg_left", "lower_leg_right", "jaw_left", "jaw_right", 
               "chest", "abdomen", "upper_back", "lower_back", "neck")

# Recode all at once
ESPRIT_full <- ESPRIT_full %>% 
  mutate(across(all_of(pain_vars), 
                ~case_when(. == "Checked" ~ 1, 
                           . == "Unchecked" ~ 0,
                           TRUE ~ as.numeric(.))))

ESPRIT_full <- ESPRIT_full %>% 
  rowwise() %>% 
  mutate(total_pain_sites = sum(c_across(all_of(pain_vars)), na.rm = TRUE)) %>% 
  ungroup()

ESPRIT_full <- col_name_num(ESPRIT_full, "wpi")

problem <- c("No problem" = 0, "Slight or mild problems" = 1, "Moderate problems" = 2, 
             "Severe problems" = 3)

ESPRIT_full <- ESPRIT_full %>% 
  mutate(across(c(wpi_severity1_num, wpi_severity2_num, wpi_severity3_num), ~problem[.x]))


ESPRIT_full$WPI <- ESPRIT_full$wpi_severity1_num + ESPRIT_full$wpi_severity2_num + ESPRIT_full$wpi_severity3_num

summary(ESPRIT_full$WPI)

#PODS

ESPRIT_full <- ESPRIT_full %>% 
  rename(
    pods1 = `Opioid medicines have caused me to lose interest in my usual activities.`,
    pods2 = `Opioid medicines have caused me to have trouble concentrating or remembering.`,
    pods3 = `Opioid medicines have caused me to feel slowed down, sluggish or sedated.`,
    pods4 = `Opioid medicines have caused me to feel depressed, down or anxious`,
    pods5 = `How often have side effects of opioid medicine interfered with your work, family, or social responsibilities?`,
    pods6 = `How often did opioid medicine make it hard for you to think clearly?`, 
    pods7 = `In the past year, about how many times did opiate medicines make you sleepy or less alert when you were driving, operating machinery, or doing something else where you needed to be alert?`,
    pods8 = `In the past month, considering the side effects of opiate medicines you experienced, how bothersome were these side effects?`
  )


ESPRIT_full <- col_name_num(ESPRIT_full, "pod")
freq_opioid <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Always or almost every day" = 5)
times_opioid <- c("Never" = 1, "Once or twice" = 2, "Three or more times" = 3)
bothersome <- c("Not at all bothersome" = 1, "A little bothersome" = 2, "Moderately bothersome" = 3, 
                "Very bothersome" = 4, "Extremely bothersome" = 5)

ESPRIT_full <- ESPRIT_full %>% 
  mutate(across(c(pods1_num, pods2_num, pods3_num, pods4_num), ~agree_let[.x]),
         across(c(pods5_num, pods6_num), ~freq_opioid[.x]),
         pods7_num = times_opioid[pods7_num], 
         pods8_num = bothersome[pods8_num])

ESPRIT_full$opioid <- ESPRIT_full$pods1_num + ESPRIT_full$pods2_num + ESPRIT_full$pods3_num + ESPRIT_full$pods4_num + ESPRIT_full$pods5_num + ESPRIT_full$pods6_num + ESPRIT_full$pods7_num + ESPRIT_full$pods8_num

summary(ESPRIT_full$opioid)

#Global impressions of change 

ESPRIT_full <- ESPRIT_full %>% 
  rename(
    gic2a = `Physical health in general now?`, 
    gic2b = `Emotional problems (such as feeling anxious, depressed, or irritable) now?`,
    gic2 = `Pain severity now?`
  )

ESPRIT_full <- col_name_num(ESPRIT_full, "gic")

improvement <- c("Much better" = 1, "Slightly better" = 2, "About the same" = 3, "Slightly worse" = 4, 
                 "Much worse" = 5)


ESPRIT_full <- ESPRIT_full %>% 
  mutate(across(c(gic2a_num, gic2b_num, gic2_num), ~improvement[.x]))

summary(ESPRIT_full$gic2_num)

###----------------------------------------------------------------------------
#Chronic pain attributions 

ESPRIT_full <- ESPRIT_full %>% 
  rename(CPA_brain = `To what extent do you believe your pain is or was due to mind or brain processes`,
         CPA_str = `To what extent do you believe your pain is or was due to structural issues in your body?...141`)


###----------------------------------------------------------------------------
#Making dataset that is just baseline values and post-treatment values 
bl_post <- ESPRIT_full %>% 
  filter(`Event Name` %in% c("2 month follow-up", "Baseline", "4 month follow-up"))


#Making a wide dataset for change scores
bl_post_wide <- bl_post %>% 
  pivot_wider(
    id_cols = c(ids, cohort, fg_change),
    names_from = `Event Name`,
    values_from = c(peg, promis_physical, promis_mental, 
                    promis_general, promis_quality, kinesophobia, 
                    opioid, depression, anxiety, engagement, painconcern, intensity, age, 
                    race, gender, hispanic, activity, enjoyment, edu, employ, married, shoulder_girdle_left, 
                    shoulder_girdle_right, upper_arm_left, upper_arm_right, lower_arm_left, lower_arm_right, 
                    hip_left, hip_right, upper_leg_left, upper_leg_right, lower_leg_left, lower_leg_right, 
                    jaw_left, jaw_right, chest, abdomen, upper_back, lower_back, neck, WPI, CPA_brain, CPA_str)
  )

bl_post_wide <- bl_post_wide %>% 
  mutate(fg_change = factor(fg_change, levels = c("Cohort 1&2", "Cohort 3&4")))


bl_post_wide<-bl_post_wide %>% 
  mutate(
    peg_change_2month = peg_Baseline - `peg_2 month follow-up`,
    peg_change_4month = peg_Baseline - `peg_4 month follow-up`,
    promis_physical_change_2month = `promis_physical_2 month follow-up` - promis_physical_Baseline,
    promis_physical_change_4month = `promis_physical_4 month follow-up` - promis_physical_Baseline,
    promis_mental_change_2month = `promis_mental_2 month follow-up` - promis_mental_Baseline,
    promis_mental_change_4month = `promis_mental_4 month follow-up` - promis_mental_Baseline,
    promis_general_change_2month = `promis_general_2 month follow-up` - promis_general_Baseline,
    promis_general_change_4month = `promis_general_4 month follow-up` - promis_general_Baseline,
    promis_quality_change_2month = `promis_quality_2 month follow-up` - promis_quality_Baseline,
    promis_quality_change_4month = `promis_quality_4 month follow-up` - promis_quality_Baseline,
    kinesophobia_change_2month = kinesophobia_Baseline - `kinesophobia_2 month follow-up`,
    kinesophobia_change_4month = kinesophobia_Baseline - `kinesophobia_4 month follow-up`,
    opioid_change_2month = opioid_Baseline - `opioid_2 month follow-up`,
    opioid_change_4month = opioid_Baseline - `opioid_4 month follow-up`,
    depression_change_2month = depression_Baseline - `depression_2 month follow-up`,
    depression_change_4month = depression_Baseline - `depression_4 month follow-up`,
    anxiety_change_2month = anxiety_Baseline - `anxiety_2 month follow-up`,
    anxiety_change_4month = anxiety_Baseline - `anxiety_4 month follow-up`,
    engagement_change_2month = `engagement_2 month follow-up` - engagement_Baseline,
    engagement_change_4month = `engagement_4 month follow-up` - engagement_Baseline,
    painconcern_change_2month = painconcern_Baseline - `painconcern_2 month follow-up`,
    painconcern_change_4month = painconcern_Baseline - `painconcern_4 month follow-up`,
    intensity_change_2month = intensity_Baseline - `intensity_2 month follow-up`,
    intensity_change_4month = intensity_Baseline - `intensity_4 month follow-up`,
    activity_change_2month = activity_Baseline - `activity_2 month follow-up`,
    activity_change_4month = activity_Baseline - `activity_4 month follow-up`,
    enjoyment_change_2month = enjoyment_Baseline - `enjoyment_2 month follow-up`,
    enjoyment_change_4month = enjoyment_Baseline - `enjoyment_4 month follow-up`,
    WPI_change_2month = WPI_Baseline - `WPI_2 month follow-up`,
    WPI_change_4month = WPI_Baseline - `WPI_4 month follow-up`,
    CPA_brain_change_2month = `CPA_brain_2 month follow-up` - CPA_brain_Baseline,
    CPA_brain_change_4month = `CPA_brain_4 month follow-up` - CPA_brain_Baseline,
    CPA_str_change_2month = CPA_str_Baseline- `CPA_str_2 month follow-up`,
    CPA_str_change_4month =  CPA_str_Baseline - `CPA_str_4 month follow-up`
  )


bl_post_wide$painsite_Baseline <- rowSums(bl_post_wide[, c("upper_arm_left_Baseline", "upper_arm_right_Baseline", "shoulder_girdle_left_Baseline", 
                                                           "shoulder_girdle_right_Baseline", "lower_arm_left_Baseline", "lower_arm_right_Baseline",
                                                           "hip_left_Baseline", "hip_right_Baseline", "upper_leg_left_Baseline", "upper_leg_right_Baseline", 
                                                           "lower_leg_left_Baseline", "lower_leg_right_Baseline", "jaw_left_Baseline", "jaw_right_Baseline", 
                                                           "chest_Baseline", "abdomen_Baseline", "upper_back_Baseline", "lower_back_Baseline", "neck_Baseline")])

bl_post_wide$painsite_2_month_followup <- rowSums(bl_post_wide[, c("shoulder_girdle_left_2 month follow-up", "shoulder_girdle_right_2 month follow-up", 
                                                                   "upper_arm_left_2 month follow-up", "upper_arm_right_2 month follow-up", "lower_arm_left_2 month follow-up",
                                                                   "lower_arm_right_2 month follow-up", "hip_left_2 month follow-up", "hip_right_2 month follow-up", 
                                                                   "upper_leg_left_2 month follow-up", "upper_leg_right_2 month follow-up", "lower_leg_left_2 month follow-up",
                                                                   "lower_leg_right_2 month follow-up", "jaw_left_2 month follow-up", "jaw_right_2 month follow-up", "abdomen_2 month follow-up", 
                                                                   "chest_2 month follow-up", "upper_back_2 month follow-up", "lower_back_2 month follow-up", "neck_2 month follow-up")])

bl_post_wide$painsite_4_month_followup <- rowSums(bl_post_wide[, c("shoulder_girdle_left_4 month follow-up", "shoulder_girdle_right_4 month follow-up", 
                                                                   "upper_arm_left_4 month follow-up", "upper_arm_right_4 month follow-up", "lower_arm_left_4 month follow-up",
                                                                   "lower_arm_right_4 month follow-up", "hip_left_4 month follow-up", "hip_right_4 month follow-up", 
                                                                   "upper_leg_left_4 month follow-up", "upper_leg_right_4 month follow-up", "lower_leg_left_4 month follow-up",
                                                                   "lower_leg_right_4 month follow-up", "jaw_left_4 month follow-up", "jaw_right_4 month follow-up", "abdomen_4 month follow-up", 
                                                                   "chest_4 month follow-up", "upper_back_4 month follow-up", "lower_back_4 month follow-up", "neck_4 month follow-up")])



bl_post_wide$painsite_change_2_month <- bl_post_wide$painsite_Baseline - bl_post_wide$painsite_2_month_followup

bl_post_wide$painsite_change_4_month <- bl_post_wide$painsite_Baseline - bl_post_wide$painsite_4_month_followup

###----------------------------------------------------------------------------
#Making graphs

#Function for making summary dataframes for the outcome of interest
summary_df <- function(df, outcome){
  outcome_df <- df %>% 
    group_by(`Event Name`, fg_change) %>% 
    summarize(mean_outcome = mean({{outcome}}, na.rm=TRUE), 
              se_outcome = sd({{outcome}}, na.rm=TRUE)/sqrt(n()),
              n=n()) %>% 
    mutate(`Event Name` = case_when(
      `Event Name` == "2 month follow-up" ~ "Post Treatment", 
      `Event Name` == "4 month follow-up" ~ "2-Month Follow-up",
      `Event Name` == "Baseline" ~ "Baseline"
    )) %>% 
    mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "Post Treatment", "2-Month Follow-up")))
  
  return(outcome_df)
    
}

#function for plotting
line_function <- function(data, ylab="", title="", legend_pos = ""){
  
  ggplot(data, aes(x = `Event Name`, y=mean_outcome, color=fg_change, group = fg_change))+
    geom_line(aes(x = as.numeric(factor(`Event Name`)) +
                    case_when(
                      fg_change == "Cohort 1&2" ~ -0.05,
                      fg_change == "Cohort 3&4" ~ 0.05
                    )),
              linewidth = 0.5)  + 
    geom_point(aes(x = as.numeric(factor(`Event Name`)) +
                     case_when(
                       fg_change == "Cohort 1&2" ~ -0.05,
                       fg_change == "Cohort 3&4" ~ 0.05
                     )),
               size = 4) +
    theme_minimal() +
    geom_errorbar(aes(ymin = mean_outcome - se_outcome, 
                      ymax = mean_outcome + se_outcome,
                      x = as.numeric(factor(`Event Name`)) +
                        case_when(
                          fg_change == "Cohort 1&2" ~ -0.05,
                          fg_change == "Cohort 3&4" ~ 0.05
                        )),
                  width = 0.25, alpha = 0.9, size = 0.8) +
    scale_color_brewer(palette = "Set1") +  
    labs(title = title,
         subtitle = "",
         x = "",
         y = ylab, 
         color = "",
    ) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 15),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.position = legend_pos,
      legend.text = element_text(size = 15)
    ) +  
    scale_x_continuous(
      breaks = 1:3,
      labels=c("Baseline", "Post-tx", "FU")
    )
}

#Did PEG split by group instead of cohort, the rest of the secondary outcomes right now are split by cohort
peg_df <- summary_df(bl_post, peg)

peg_line <- line_function(peg_df, "PEG Score", "", "right")

print(peg_line)


###----------------------------------------------------------------------------
#Intensity
intensity_df <- summary_df(bl_post, intensity)


intensity_line <- line_function(intensity_df, "Pain Intensity", "") 

###----------------------------------------------------------------------------
#PROMIS General 
promis_general_df <- summary_df(bl_post, promis_general)


promis_general_line <- line_function(promis_general_df, "PROMIS General Health", "") + 
  scale_y_continuous(breaks=2:4, limits = c(2,3.5))
###----------------------------------------------------------------------------
#PROMIS Quality of Life
promis_quality_df <- summary_df(bl_post, promis_quality)

promis_quality_line <- line_function(promis_quality_df, "PROMIS Quality of Life", "", "bottom") + 
  scale_y_continuous(breaks=2:4, limits = c(2,3.5))

###----------------------------------------------------------------------------
#PROMIS Physical Health
promis_physical_df <- summary_df(bl_post, promis_physical)

promis_physical_line <- line_function(promis_physical_df, "PROMIS Physical Health", "") 

###----------------------------------------------------------------------------
#PROMIS Mental Health
promis_mental_df <- summary_df(bl_post, promis_mental)

promis_mental_line <- line_function(promis_mental_df, "PROMIS Mental Health", "")

###----------------------------------------------------------------------------
#PHQ
depression_df <- summary_df(bl_post, depression)

depression_line <- line_function(depression_df, "PHQ-9", "")

###----------------------------------------------------------------------------
#GAD
anxiety_df <- summary_df(bl_post, anxiety)

anxiety_line <- line_function(anxiety_df, "GAD-7", "")

###----------------------------------------------------------------------------
#Concerns about pain
painconcern_df <- summary_df(bl_post, painconcern)

painconcern_line <- line_function(painconcern_df, "CAP-6", "")

###----------------------------------------------------------------------------  
#Life Engagement 
engagement_df <- summary_df(bl_post, engagement)

engagement_line <- line_function(engagement_df, "LET", "")
###----------------------------------------------------------------------------
#TSK
kinesophobia_df <- summary_df(bl_post, kinesophobia)

kinesophobia_line <- line_function(kinesophobia_df, "TSK-11", "", "bottom")

###----------------------------------------------------------------------------
#Brain attributions 
brainattr_df <- summary_df(bl_post, CPA_brain)

brainattr_line <- line_function(brainattr_df, "Brain Attributions", "")
#Structural attributions
strattr_df <- summary_df(bl_post, CPA_str)

strattr_line <- line_function(strattr_df, "Structural Attributions", "")
###----------------------------------------------------------------------------
#Opioid 
opioid_df <- summary_df(bl_post, opioid)

opioid_line <- line_function(opioid_df, "POD", "")
###----------------------------------------------------------------------------
#Combined plot 
combined_primary <- peg_line + intensity_line + plot_annotation(tag_levels = 'A')
print(combined_primary)


combined_secondary <- depression_line + promis_physical_line + brainattr_line + strattr_line +
  promis_quality_line + engagement_line + plot_annotation(tag_levels = 'A') +
  plot_layout(ncol = 3, nrow = 2) &  # & applies theme to all plots
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 11)
  )

ggsave("Figure 2.pdf", width =12, height = 8, dpi = 300)

