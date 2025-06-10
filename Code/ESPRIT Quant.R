library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(performance)
library(lme4)
library(lmerTest)
library(car)
library(effsize)
library(effectsize)


ESPRIT_unedited <- read_excel("/Users/zanwynia/Desktop/Datasets/ESPRIT_labels.xlsx")


ESPRIT <- ESPRIT_unedited %>% 
  rename(cohort = `Which group will participant be joining?`,
         age = `Current Age:`,
         gender = `What is your current gender identity?`,
         race = `Which race do you identify with most?`,
         hispanic = `Are you of Hispanic, Latino, or Spanish origin?`, 
         edu = `What is your education level:`,
         employ = `What is your employment status?`,
         married = `What is your current marital status?`, 
         ids = `Record ID`, 
         suicide = `Have you tried to hurt or kill yourself in the past five years? (This includes non-suicidal self-harm, such as cutting yourself on purpose. If you have intentionally hurt yourself in the past five years, please answer yes).`)


#Descrpitive statistics 

# First, create a separate dataframe with just the pain ratings from Baseline
baseline_pain <- ESPRIT %>%
  filter(`Event Name` == "Baseline") %>%
  select(ids, intensity = `What number best describes your pain on average in the past week?`, 
         enjoyment = `What number best describes how, during the past week, pain has interfered with your enjoyment of life?`,
         activity = `What number best describes how, during the past week, pain has interfered with your general activity?`,)


baseline_pain$intensity[baseline_pain$intensity == "10 - pain as bad as you can imagine"] <- 10
baseline_pain$enjoyment[baseline_pain$enjoyment == "10 - pain as bad as you can imagine"] <- 10
baseline_pain$activity[baseline_pain$activity == "10 - pain as bad as you can imagine"] <- 10


baseline_pain$intensity[baseline_pain$intensity == "0 - no pain"] <- 0
baseline_pain$enjoyment[baseline_pain$enjoyment == "0 - no pain"] <- 0
baseline_pain$activity[baseline_pain$activity == "0 - no pain"] <- 0

baseline_pain$intensity <- as.numeric(baseline_pain$intensity)
baseline_pain$enjoyment <- as.numeric(baseline_pain$enjoyment)
baseline_pain$activity <- as.numeric(baseline_pain$activity)

# Then, get all the demographic info from Eligibility and Consent
demographic_info <- ESPRIT %>%
  filter(`Event Name` == "Eligibility and Consent") %>%
  mutate(
    depression =  `Which if any of the following have you ever been diagnosed with? (choice=Depression)`, 
    anxiety = `Which if any of the following have you ever been diagnosed with? (choice=Anxiety)`, 
    schizo = `Which if any of the following have you ever been diagnosed with? (choice=Schizophrenia or Psychosis)`, 
    bipolar = `Which if any of the following have you ever been diagnosed with? (choice=Bipolar disorder or mania)`, 
    SUD = `Which if any of the following have you ever been diagnosed with? (choice=Substance use disorder)`, 
    PTSD = `Which if any of the following have you ever been diagnosed with? (choice=Post-traumatic stress disorder (PTSD))`,
    bp_history = `How long has back or neck pain been an ongoing problem for you (in years)?`
  ) %>% 
  select(ids, age, gender, race, hispanic, edu, married, employ, suicide, depression, anxiety, schizo, bipolar, SUD, PTSD, bp_history,
         `Enrollment Status`)


# Join them together to get one row per participant
baseline_wide <- demographic_info %>%
  left_join(baseline_pain, by = "ids") %>% 
  drop_na(intensity) %>% 
  slice(-1) %>% 
  mutate(peg = (intensity+ enjoyment + activity)/3)

baseline_wide <- baseline_wide %>% 
  mutate(across(c(depression, anxiety, schizo, bipolar, SUD, PTSD),
                ~ case_when(.x == "Checked" ~ 1, .x == "Unchecked" ~ 0), 
                .names="{col}_num"))

baseline_wide$any_mental <- ifelse(rowSums(baseline_wide[, c("depression_num", "anxiety_num", 
                                                             "schizo_num", "bipolar_num", 
                                                             "SUD_num", "PTSD_num")],
                                           na.rm=TRUE) > 0, "yes", "no")

table(baseline_wide$`Enrollment Status`)

table(baseline_wide$suicide)

table(baseline_wide$any_mental)
prop.table(table(baseline_wide$any_mental))

table(baseline_wide$depression)
prop.table(table(baseline_wide$depression))
table(baseline_wide$anxiety)
prop.table(table(baseline_wide$anxiety))
table(baseline_wide$schizo)
prop.table(table(baseline_wide$schizo))
table(baseline_wide$bipolar)
prop.table(table(baseline_wide$bipolar))
table(baseline_wide$SUD)
prop.table(table(baseline_wide$SUD))
table(baseline_wide$PTSD)
prop.table(table(baseline_wide$PTSD))


summary(baseline_wide$age)
sd(baseline_wide$age, na.rm = TRUE)

table(baseline_wide$gender)
prop.table(table(baseline_wide$gender))

table(baseline_wide$race)
prop.table(table(baseline_wide$race))

table(baseline_wide$hispanic)
prop.table(table(baseline_wide$hispanic))

table(baseline_wide$edu)
prop.table(table(baseline_wide$edu))

table(baseline_wide$married)
prop.table(table(baseline_wide$married))

table(baseline_wide$employ)
prop.table(table(baseline_wide$employ))


summary(baseline_wide$intensity)
sd(baseline_wide$intensity, na.rm = TRUE)

print(baseline_wide$ids)

#Record ID 67 accidentally put in 2002 years, I think she meant that it started in 2002 so 
#I will put 23 years 
baseline_wide$bp_history[baseline_wide$bp_history == 2002] <- 23 

summary(baseline_wide$bp_history)
sd(baseline_wide$bp_history, na.rm=TRUE)


#Creating cohort and cohort grouping variables
ESPRIT_full <- ESPRIT %>% 
  mutate(cohort = case_when(
    cohort == "May 14th - July 2nd (Tuesdays at 10am)" ~ "Cohort 1", 
    cohort == "May 30th - July 25nd (Thursdays at 5pm)" ~ "Cohort 2", 
    cohort == "September 24th - November 12th (Tuesdays, at 10am)" ~ "Cohort 3", 
    cohort == "September 26th - November 14th (Thursdays at 5pm)" ~ "Cohort 4"
  )) %>% 
  mutate(fg_change = case_when(
    cohort == "Cohort 1" ~ "Group 1",
    cohort == "Cohort 2" ~ "Group 1",
    cohort == "Cohort 3" ~ "Group 2",
    cohort == "Cohort 4" ~ "Group 2"
  ))

table(ESPRIT_full$fg_change)


table(ESPRIT_full$cohort)


#Test record entered by Leon, taking it out
ESPRIT_full <- ESPRIT_full %>% 
  slice(-c(1,2))

###-----------------------------------------------------------------------------
#Subsetting data so it's only people who were actually in cohort (i.e. getting rid 
#of all the people who were screened out)


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
  filter(`Event Name` %in% c("Eligibility and Consent", "2 month follow-up", "Baseline", 
                             "4 month follow-up"))



#Making a wide dataset for change scores
bl_post_wide <- bl_post %>% 
  pivot_wider(
    id_cols = c(ids, cohort, fg_change),
    names_from = `Event Name`,
    values_from = c(peg, promis_physical, promis_mental, 
                    promis_global01_num, promis_global02_num, kinesophobia, 
                    opioid, depression, anxiety, engagement, painconcern, intensity, age, 
                    race, gender, hispanic, activity, enjoyment, edu, employ, married, shoulder_girdle_left, 
                    shoulder_girdle_right, upper_arm_left, upper_arm_right, lower_arm_left, lower_arm_right, 
                    hip_left, hip_right, upper_leg_left, upper_leg_right, lower_leg_left, lower_leg_right, 
                    jaw_left, jaw_right, chest, abdomen, upper_back, lower_back, neck, WPI, CPA_brain, CPA_str)
  )

bl_post_wide <- bl_post_wide %>% 
  select(-`peg_Eligibility and Consent`, -`promis_physical_Eligibility and Consent`, 
         -`promis_mental_Eligibility and Consent`, -`promis_global01_num_Eligibility and Consent`,
         -`promis_global02_num_Eligibility and Consent`, -`kinesophobia_Eligibility and Consent`, 
         -`opioid_Eligibility and Consent`, -`depression_Eligibility and Consent`, -`anxiety_Eligibility and Consent`,
         -`engagement_Eligibility and Consent`, -`painconcern_Eligibility and Consent`, -`intensity_Eligibility and Consent`,
         -age_Baseline, -`age_2 month follow-up`, -race_Baseline, -`race_2 month follow-up`, 
         -gender_Baseline, -`gender_2 month follow-up`, -hispanic_Baseline, -`hispanic_2 month follow-up`, -edu_Baseline,
         -`edu_2 month follow-up`, -married_Baseline, -`married_2 month follow-up`, -employ_Baseline, -`employ_2 month follow-up`,
         -`shoulder_girdle_left_Eligibility and Consent`, -`shoulder_girdle_right_Eligibility and Consent`, -`upper_arm_left_Eligibility and Consent`, 
         -`upper_arm_right_Eligibility and Consent`, -`lower_arm_left_Eligibility and Consent`, -`lower_arm_right_Eligibility and Consent`,
         -`hip_left_Eligibility and Consent`, -`hip_right_Eligibility and Consent`, -`upper_leg_left_Eligibility and Consent`, 
         -`upper_leg_right_Eligibility and Consent`, -`lower_leg_left_Eligibility and Consent`, -`lower_leg_right_Eligibility and Consent`, 
         -`jaw_left_Eligibility and Consent`, -`jaw_right_Eligibility and Consent`, -`chest_Eligibility and Consent`, -`abdomen_Eligibility and Consent`,
         -`upper_back_Eligibility and Consent`, -`lower_back_Eligibility and Consent`, -`neck_Eligibility and Consent`, -`enjoyment_Eligibility and Consent`, 
         -`activity_Eligibility and Consent`, -`WPI_Eligibility and Consent`, -`CPA_brain_Eligibility and Consent`, -`CPA_str_Eligibility and Consent`
         
  ) %>% 
  rename(age = `age_Eligibility and Consent`,
         race = `race_Eligibility and Consent`, 
         gender = `gender_Eligibility and Consent`,
         hispanic = `hispanic_Eligibility and Consent`,
         edu = `edu_Eligibility and Consent`, 
         married = `married_Eligibility and Consent`, 
         employ = `employ_Eligibility and Consent`) %>% 
  mutate(
    phys = case_when(
      cohort == "Cohort 1" ~ "No Phys", 
      cohort == "Cohort 2" ~ "No Phys", 
      cohort == "Cohort 3" ~ "No Phys", 
      cohort == "Cohort 4" ~"Phys"
    )) %>% 
  mutate(fg_change = factor(fg_change, levels = c("Group 1", "Group 2")))


bl_post_wide<-bl_post_wide %>% 
  mutate(
    peg_change_2month = peg_Baseline - `peg_2 month follow-up`,
    peg_change_4month = peg_Baseline - `peg_4 month follow-up`,
    promis_physical_change_2month = `promis_physical_2 month follow-up` - promis_physical_Baseline,
    promis_physical_change_4month = `promis_physical_4 month follow-up` - promis_physical_Baseline,
    promis_mental_change_2month = `promis_mental_2 month follow-up` - promis_mental_Baseline,
    promis_mental_change_4month = `promis_mental_4 month follow-up` - promis_mental_Baseline,
    promis_general_change_2month = `promis_global01_num_2 month follow-up` - promis_global01_num_Baseline,
    promis_general_change_4month = `promis_global01_num_4 month follow-up` - promis_global01_num_Baseline,
    promis_quality_change_2month = `promis_global02_num_2 month follow-up` - promis_global02_num_Baseline,
    promis_quality_change_4month = `promis_global02_num_4 month follow-up` - promis_global02_num_Baseline,
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
    CPA_str_change_2month = `CPA_str_2 month follow-up` - CPA_str_Baseline,
    CPA_str_change_4month = `CPA_str_4 month follow-up` - CPA_str_Baseline
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

###-----------------------------------------------------------------------------
#How many people completed post-treatment and 2-month follow-up

twomonthcomplete <- bl_post_wide %>% 
  drop_na(`peg_2 month follow-up`)

fourmonthcomplete <- bl_post_wide %>% 
  drop_na(`peg_4 month follow-up`)

###----------------------------------------------------------------------------------------------
#Baseline and post treatment values with change scores 

#Function to print values
summary_func <- function(a, b, c, d, e){
  print("Baseline:")
  print(c(summary(a), "SD" = sd(a, na.rm=TRUE)))
  
  print("Post Treatment:")
  print(c(summary(b), "SD" = sd(b, na.rm=TRUE)))
  
  print("2-Month Follow-up:")
  print(c(summary(c), "SD" = sd(c, na.rm=TRUE)))
  
  print("Change to Post-treatment:")
  print(c(summary(d), "SD" = sd(d, na.rm=TRUE)))
  
  print("Change to 2-Month Follow-up:")
  print(c(summary(e), "SD" = sd(e, na.rm=TRUE)))
}


#PEG
summary_func(bl_post_wide$peg_Baseline, bl_post_wide$`peg_2 month follow-up`, 
             bl_post_wide$`peg_4 month follow-up`, bl_post_wide$peg_change_2month, 
             bl_post_wide$peg_change_4month)

#Pain intensity
summary_func(bl_post_wide$intensity_Baseline, bl_post_wide$`intensity_2 month follow-up`, 
             bl_post_wide$`intensity_4 month follow-up`, bl_post_wide$intensity_change_2month, 
             bl_post_wide$intensity_change_4month)


#PROMIS Global Health post-treatment
summary_func(bl_post_wide$promis_global01_num_Baseline, bl_post_wide$`promis_global01_num_2 month follow-up`, 
             bl_post_wide$`promis_global01_num_4 month follow-up`, bl_post_wide$promis_general_change_2month,
             bl_post_wide$promis_general_change_4month)

#PROMIS Quality of life post treatment 
summary_func(bl_post_wide$promis_global02_num_Baseline, bl_post_wide$`promis_global02_num_2 month follow-up`,
             bl_post_wide$`promis_global02_num_4 month follow-up`, bl_post_wide$promis_quality_change_2month,
             bl_post_wide$promis_quality_change_4month)

#PROMIS Physical health post treatment 
summary_func(bl_post_wide$promis_physical_Baseline, bl_post_wide$`promis_physical_2 month follow-up`,
             bl_post_wide$`promis_physical_4 month follow-up`, bl_post_wide$promis_physical_change_2month,
             bl_post_wide$promis_physical_change_4month)



#PROMIS mental health post-treatment
summary_func(bl_post_wide$promis_mental_Baseline, bl_post_wide$`promis_mental_2 month follow-up`, 
        bl_post_wide$`promis_mental_4 month follow-up`, bl_post_wide$promis_mental_change_2month,
        bl_post_wide$promis_mental_change_4month)

#Depression post treatment
summary_func(bl_post_wide$depression_Baseline, bl_post_wide$`depression_2 month follow-up`, 
             bl_post_wide$`depression_4 month follow-up`, bl_post_wide$depression_change_2month, 
             bl_post_wide$depression_change_4month)

#Anxiety post treatment
summary_func(bl_post_wide$anxiety_Baseline, bl_post_wide$`anxiety_2 month follow-up`, 
             bl_post_wide$`anxiety_4 month follow-up`, bl_post_wide$anxiety_change_2month, 
             bl_post_wide$anxiety_change_4month)

#Kinesophobia post treatment
summary_func(bl_post_wide$kinesophobia_Baseline, bl_post_wide$`kinesophobia_2 month follow-up`, 
             bl_post_wide$`kinesophobia_4 month follow-up`, bl_post_wide$kinesophobia_change_2month,
             bl_post_wide$kinesophobia_change_4month)

#Pain concern post treatment
summary_func(bl_post_wide$painconcern_Baseline, bl_post_wide$`painconcern_2 month follow-up`, 
             bl_post_wide$`painconcern_4 month follow-up`, bl_post_wide$painconcern_change_2month,
             bl_post_wide$painconcern_change_4month)

#Life engagement post treatment 
summary_func(bl_post_wide$engagement_Baseline, bl_post_wide$`engagement_2 month follow-up`, 
             bl_post_wide$`engagement_4 month follow-up`, bl_post_wide$engagement_change_2month, 
             bl_post_wide$engagement_change_4month)

#Pain sites post treatment
summary_func(bl_post_wide$painsite_Baseline, bl_post_wide$painsite_2_month_followup, 
             bl_post_wide$painsite_4_month_followup, bl_post_wide$painsite_change_2_month, 
             bl_post_wide$painsite_change_4_month)

#Symptom severity post treatment
summary_func(bl_post_wide$WPI_Baseline, bl_post_wide$`WPI_2 month follow-up`, 
             bl_post_wide$`WPI_4 month follow-up`, bl_post_wide$WPI_change_2month, 
             bl_post_wide$WPI_change_4month)

#Opioid post treatment
summary_func(bl_post_wide$opioid_Baseline, bl_post_wide$`opioid_2 month follow-up`, 
             bl_post_wide$`opioid_4 month follow-up`, bl_post_wide$opioid_change_2month, 
             bl_post_wide$opioid_change_4month)

#Structural attributions baseline
summary_func(bl_post_wide$CPA_str_Baseline, bl_post_wide$`CPA_str_2 month follow-up`, 
             bl_post_wide$`CPA_str_4 month follow-up`, bl_post_wide$CPA_str_change_2month, 
             bl_post_wide$CPA_str_change_4month)

#Brain attributions baseline
summary_func(bl_post_wide$CPA_brain_Baseline, bl_post_wide$`CPA_brain_2 month follow-up`,
             bl_post_wide$`CPA_brain_4 month follow-up`, bl_post_wide$CPA_brain_change_2month,
             bl_post_wide$CPA_brain_change_4month)

###----------------------------------------------------------------------------------------------
#Looking at half reduction, thrid reduction, and pain free
bl_post_wide <- bl_post_wide %>% 
  mutate(pegofone = ifelse(`peg_2 month follow-up` <= 1, "Yes", "No")) %>%
  mutate(halfreduction = ifelse(`peg_2 month follow-up` <= (0.5 * peg_Baseline), "Yes", "No")) %>% 
  mutate(thirdreduction = ifelse(`peg_2 month follow-up` <= (0.70 * peg_Baseline), "Yes", "No")) %>% 
  mutate(halfreduction_intensity = ifelse(`intensity_2 month follow-up` <= (0.5*intensity_Baseline), "Yes", "No")) %>% 
  mutate(thirdreduction_intensity = ifelse(`intensity_2 month follow-up` <= (0.7*intensity_Baseline),"Yes","No")) %>% 
  mutate(painofone = ifelse(`intensity_2 month follow-up` <= 1, "Yes", "No"))


table(bl_post_wide$thirdreduction_intensity)
prop.table(table(bl_post_wide$thirdreduction_intensity))

table(bl_post_wide$painofone)
prop.table(table(bl_post_wide$painofone))


table(bl_post_wide$halfreduction_intensity)
prop.table(table(bl_post_wide$halfreduction_intensity))

table(bl_post_wide$thirdreduction)
prop.table(table(bl_post_wide$thirdreduction))


table(bl_post_wide$pegofone)
prop.table(table(bl_post_wide$pegofone))

###----------------------------------------------------------------------------------------------

#Modeling with mixed effect models

#Setting up the data set by restricting to time points of interest and coding them as factor variables
#and also standardizing the outcome score 

ESPRIT_mixed <- ESPRIT_full %>% 
  filter(`Event Name` %in% c("Baseline", "2 month follow-up", "4 month follow-up")) %>% 
  mutate(timepoint = case_when(
    `Event Name` == "Baseline" ~ 0,
    `Event Name` == "2 month follow-up" ~ 1, 
    `Event Name` == "4 month follow-up" ~ 2
  )) %>% 
  mutate(timepoint = factor(timepoint, levels=c(0, 1, 2))) %>% 
  mutate(across(where(is.numeric), ~scale(.)[,1], .names = "{.col}_z"))

#PRIMARY OUTCOMES: PEG and Pain Intensity Item. Looking at both standardized and non-standardized outcomes
peg_effect <- lmer(peg ~ timepoint + fg_change +  timepoint*fg_change + 
                (1|cohort:ids), data = ESPRIT_mixed)
summary(peg_effect)

confint(peg_effect)

peg_z_effect <- lmer(peg_z ~ timepoint + fg_change +  timepoint*fg_change + 
                       (1|cohort:ids), data = ESPRIT_mixed)

summary(peg_z_effect)

confint(peg_z_effect)


intensity_effect <- lmer(intensity  ~ timepoint + fg_change +  timepoint*fg_change + 
                           (1|cohort:ids), data = ESPRIT_mixed)
summary(intensity_effect)

confint(intensity_effect)

intensity_effect_z <- lmer(intensity_z  ~ timepoint + fg_change +  timepoint*fg_change + 
                             (1|cohort:ids), data = ESPRIT_mixed)

summary(intensity_effect_z)

confint(intensity_effect_z)


###Only doing standardized outcomes from here on out
#PROMIS Global Health
promis_GH <- lmer(promis_global01_num_z ~ timepoint + fg_change +  timepoint*fg_change + 
                    (1|cohort:ids), data = ESPRIT_mixed)

summary(promis_GH)

#PROMIS Quality 
promis_quality <- lmer(promis_global02_num_z ~ timepoint + fg_change +  timepoint*fg_change + 
                          (1|cohort:ids), data = ESPRIT_mixed)
summary(promis_quality)

#PROMIS Phsyical
ESPRIT_mixed$promis_physical_z

promis_physical <- lmer(promis_physical_z ~ timepoint + fg_change +  timepoint*fg_change + 
                          (1|cohort:ids), data = ESPRIT_mixed)
summary(promis_physical)

confint(promis_physical)

#PROMIS Mental 
ESPRIT_mixed$promis_mental_z

promis_mental <- lmer(promis_mental_z ~ timepoint + fg_change +  timepoint*fg_change + 
                        (1|cohort:ids), data = ESPRIT_mixed)

summary(promis_mental)

#Depression 
ESPRIT_mixed$depression_z

depression <- lmer(depression_z ~ timepoint + fg_change +  timepoint*fg_change + 
                     (1|cohort:ids), data = ESPRIT_mixed)

summary(depression)

confint(depression)

#Anxiety 
ESPRIT_mixed$anxiety_z

anxiety <- lmer(anxiety_z ~ timepoint + fg_change +  timepoint*fg_change + 
                  (1|cohort:ids), data = ESPRIT_mixed)

summary(anxiety)
confint(anxiety)

#Kinesophobia

kinesophobia <- lmer(kinesophobia_z ~ timepoint + fg_change +  timepoint*fg_change + 
                       (1|cohort:ids), data = ESPRIT_mixed)

summary(kinesophobia)

confint(kinesophobia)

#Concern about pain 
ESPRIT_mixed$painconcern_z

painconcern <- lmer(painconcern_z ~ timepoint + fg_change +  timepoint*fg_change + 
                      (1|cohort:ids), data = ESPRIT_mixed)

summary(painconcern)

confint(painconcern)

#Life engagement
ESPRIT_mixed$engagement_z

engagement <- lmer(engagement_z ~ timepoint + fg_change +  timepoint*fg_change + 
                     (1|cohort:ids), data = ESPRIT_mixed)

summary(engagement)

#WPI
ESPRIT_mixed$WPI_z

WPI <- lmer(WPI_z ~ timepoint + fg_change +  timepoint*fg_change + 
              (1|cohort:ids), data = ESPRIT_mixed)

summary(WPI)

total_pain_sites <- lmer(total_pain_sites_z ~ timepoint + fg_change +  timepoint*fg_change + 
                           (1|cohort:ids), data = ESPRIT_mixed)

summary(total_pain_sites)



#Opioid difficulty 
ESPRIT_mixed$opioid_z

opioid <- lmer(opioid_z ~ timepoint + fg_change +  timepoint*fg_change + 
                 (1|cohort:ids), data = ESPRIT_mixed)

summary(opioid)

#Structural attributions 
ESPRIT_mixed$CPA_str_z

str <- lmer(CPA_str_z ~ timepoint + fg_change +  timepoint*fg_change + 
              (1|cohort:ids), data = ESPRIT_mixed)

summary(str)

confint(str)

#Brain attributions 
brain <- lmer(CPA_brain_z ~ timepoint + fg_change +  timepoint*fg_change + 
                (1|cohort:ids), data = ESPRIT_mixed)

summary(brain)
confint(brain)


###-----------------------------------------------------------------------------
#Looking at potential mechanisms (kinesophobia and attributions)

#Structural attributions
strattr <- lmer(peg ~ timepoint + CPA_str + timepoint*CPA_str + fg_change*CPA_str +
                     (1|cohort:ids), data=ESPRIT_mixed)
summary(strattr)

confint(strattr)

#Brain attributions
brainattr <- lmer(peg ~ timepoint + CPA_brain + timepoint*CPA_brain + fg_change*CPA_brain +
                       (1|cohort:ids), data=ESPRIT_mixed)
summary(brainattr)

confint(brainattr)


#Kinesophobia
kin <- lmer(peg ~ timepoint + kinesophobia + timepoint*kinesophobia + fg_change*kinesophobia +
                 (1|cohort:ids), data=ESPRIT_mixed)
summary(kin)

confint(kin)

###--------------------------------------------------------------------------------------
#Attendance data (looking at avg number of attended sessions)

ESPRIT_attendance <- read_excel("/Users/zanwynia/Desktop/Datasets/ESPRIT_attendance.xlsx")

summary(ESPRIT_attendance$`Total session attended`)
sd(ESPRIT_attendance$`Total session attended`, na.rm=TRUE)


