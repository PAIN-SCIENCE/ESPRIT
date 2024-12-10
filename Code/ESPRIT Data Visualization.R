library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)



ESPRIT_full <- read_excel("/Users/zanwynia/Desktop/Datasets/ESPRIT_labels.xlsx")



###----------------------------------------------------------------------------
#Adding labels to participants in each cohort

table(ESPRIT_full$`Which group will participant be joining?`)

ESPRIT_full <- ESPRIT_full %>% 
  rename(cohort = `Which group will participant be joining?`) %>%
  mutate(cohort = case_when(
    cohort == "May 14th - July 2nd (Tuesdays at 10am)" ~ "Cohort 1", 
    cohort == "May 30th - July 25nd (Thursdays at 5pm)" ~ "Cohort 2", 
    cohort == "September 24th - November 12th (Tuesdays, at 10am)" ~ "Cohort 3", 
    cohort == "September 26th - November 14th (Thursdays at 5pm)" ~ "Cohort 4"
  ))


  

table(ESPRIT_full$cohort)

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
  ungroup()

print(ESPRIT_full$cohort)

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

ESPRIT_full$intensity[ESPRIT_full$intensity == "10 - pain as bad as you can imagine"] <- 10
ESPRIT_full$enjoyment[ESPRIT_full$enjoyment == "10 - pain as bad as you can imagine"] <- 10
ESPRIT_full$activity[ESPRIT_full$activity == "10 - pain as bad as you can imagine"] <- 10

ESPRIT_full$intensity <- as.numeric(ESPRIT_full$intensity)
ESPRIT_full$enjoyment <- as.numeric(ESPRIT_full$enjoyment)
ESPRIT_full$activity <- as.numeric(ESPRIT_full$activity)



summary(ESPRIT_full$intensity)

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


ESPRIT_full$promis_global01_num <- ESPRIT_full$promis_global01
ESPRIT_full$promis_global02_num <- ESPRIT_full$promis_global02
ESPRIT_full$promis_global03_num <- ESPRIT_full$promis_global03
ESPRIT_full$promis_global04_num <- ESPRIT_full$promis_global04
ESPRIT_full$promis_global05_num <- ESPRIT_full$promis_global05
ESPRIT_full$promis_global06_num <- ESPRIT_full$promis_global06
ESPRIT_full$promis_global08_num <- ESPRIT_full$promis_global08
ESPRIT_full$promis_global09_num <- ESPRIT_full$promis_global09
ESPRIT_full$promis_global10_num <- ESPRIT_full$promis_global10


ESPRIT_full <- ESPRIT_full %>% 
  mutate(promis_global01_num = case_when(
    promis_global01_num == "Poor" ~ 1, 
    promis_global01_num == "Fair" ~ 2,
    promis_global01_num == "Good" ~ 3, 
    promis_global01_num == "Very good" ~ 4, 
    promis_global01_num == "Excellent" ~ 5
  )
  ) %>% 
  mutate(promis_global02_num = case_when(
    promis_global02_num == "Poor" ~ 1,
    promis_global02_num == "Fair" ~ 2, 
    promis_global02_num == "Good" ~ 3, 
    promis_global02_num == "Very good" ~ 4, 
    promis_global02_num == "Excellent" ~ 5
  )) %>% 
  mutate(promis_global03_num = case_when(
    promis_global03_num == "Poor" ~ 1, 
    promis_global03_num == "Fair" ~ 2,
    promis_global03_num == "Good" ~ 3, 
    promis_global03_num == "Very good" ~ 4, 
    promis_global03_num == "Excellent" ~ 5, 
  )) %>% 
  mutate(promis_global04_num = case_when(
    promis_global04_num == "Poor" ~ 1, 
    promis_global04_num  == "Fair" ~ 2, 
    promis_global04_num == "Good" ~ 3, 
    promis_global04_num == "Very good" ~ 4, 
    promis_global04_num == "Excellent" ~ 5, 
    
  )) %>% 
  mutate(promis_global05_num = case_when(
    promis_global05_num == "Poor" ~ 1, 
    promis_global05_num == "Fair" ~ 2, 
    promis_global05_num == "Good" ~ 3, 
    promis_global05_num == "Very good" ~ 4, 
    promis_global05_num == "Excellent" ~ 5, 
  )) %>% 
  mutate(promis_global06_num = case_when(
    promis_global06_num == "Not at all" ~ 1, 
    promis_global06_num == "A little" ~ 2, 
    promis_global06_num == "Moderately" ~ 3, 
    promis_global06_num == "Mostly" ~ 4, 
    promis_global06_num == "Completely" ~ 5
  )) %>% 
  mutate(promis_global08_num = case_when(
    promis_global08_num == "Very Severe" ~ 1, 
    promis_global08_num == "Severe" ~ 2, 
    promis_global08_num == "Moderate" ~ 3, 
    promis_global08_num == "Mild" ~ 4, 
    promis_global08_num == "None" ~ 5, 
  )) %>% 
  mutate(promis_global09_num = case_when(
    promis_global09_num == "Poor" ~ 1, 
    promis_global09_num == "Fair" ~ 2, 
    promis_global09_num == "Good" ~ 3, 
    promis_global09_num == "Very good" ~ 4, 
    promis_global09_num == "Excellent" ~ 5,
  )) %>% 
  mutate(promis_global10_num = case_when(
    promis_global10_num == "Always" ~ 1, 
    promis_global10_num == "Often" ~ 2,
    promis_global10_num == "Sometimes" ~ 3, 
    promis_global10_num == "Rarely" ~ 4, 
    promis_global10_num == "Never" ~ 5
  ))


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


ESPRIT_full$phq8_1_num <- ESPRIT_full$phq8_1
ESPRIT_full$phq8_2_num <- ESPRIT_full$phq8_2
ESPRIT_full$phq8_3_num <- ESPRIT_full$phq8_3
ESPRIT_full$phq8_4_num <- ESPRIT_full$phq8_4
ESPRIT_full$phq8_5_num <- ESPRIT_full$phq8_5
ESPRIT_full$phq8_6_num <- ESPRIT_full$phq8_6
ESPRIT_full$phq8_7_num <- ESPRIT_full$phq8_7
ESPRIT_full$phq8_8_num <- ESPRIT_full$phq8_8


table(ESPRIT_full$phq8_1)

ESPRIT_full <- ESPRIT_full %>% 
  mutate(phq8_1_num = case_when(
    phq8_1_num == "0- Not at all" ~ 0, 
    phq8_1_num == "1- Several days" ~ 1,
    phq8_1_num == "2- More than half the days" ~ 2, 
    phq8_1_num == "3- Nearly every day" ~ 3
  )) %>% 
  mutate(phq8_2_num = case_when(
    phq8_2_num == "0- Not at all" ~ 0, 
    phq8_2_num == "1- Several days" ~ 1,
    phq8_2_num == "2- More than half the days" ~ 2, 
    phq8_2_num == "3- Nearly every day" ~ 3)) %>% 
  mutate(phq8_3_num = case_when(
    phq8_3_num == "0- Not at all" ~ 0, 
    phq8_3_num == "1- Several days" ~ 1,
    phq8_3_num == "2- More than half the days" ~ 2, 
    phq8_3_num == "3- Nearly every day" ~ 3)) %>% 
  mutate(phq8_4_num = case_when(
    phq8_4_num == "0- Not at all" ~ 0, 
    phq8_4_num == "1- Several days" ~ 1,
    phq8_4_num == "2- More than half the days" ~ 2, 
    phq8_4_num == "3- Nearly every day" ~ 3)) %>% 
  mutate(phq8_5_num = case_when(
    phq8_5_num == "0- Not at all" ~ 0, 
    phq8_5_num == "1- Several days" ~ 1,
    phq8_5_num == "2- More than half the days" ~ 2, 
    phq8_5_num == "3- Nearly every day" ~ 3)) %>% 
  mutate(phq8_6_num = case_when(
    phq8_6_num == "0- Not at all" ~ 0, 
    phq8_6_num == "1- Several days" ~ 1,
    phq8_6_num == "2- More than half the days" ~ 2, 
    phq8_6_num == "3- Nearly every day" ~ 3)) %>% 
  mutate(phq8_7_num = case_when(
    phq8_7_num == "0- Not at all" ~ 0, 
    phq8_7_num == "1- Several days" ~ 1,
    phq8_7_num == "2- More than half the days" ~ 2, 
    phq8_7_num == "3- Nearly every day" ~ 3)) %>% 
  mutate(phq8_8_num = case_when(
    phq8_8_num == "0- Not at all" ~ 0, 
    phq8_8_num == "1- Several days" ~ 1,
    phq8_8_num == "2- More than half the days" ~ 2, 
    phq8_8_num == "3- Nearly every day" ~ 3))
  

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


ESPRIT_full$gad1_num <- ESPRIT_full$gad1
ESPRIT_full$gad2_num <- ESPRIT_full$gad2
ESPRIT_full$gad3_num <- ESPRIT_full$gad3
ESPRIT_full$gad4_num <- ESPRIT_full$gad4
ESPRIT_full$gad5_num <- ESPRIT_full$gad5
ESPRIT_full$gad6_num <- ESPRIT_full$gad6
ESPRIT_full$gad7_num <- ESPRIT_full$gad7

table(ESPRIT_full$gad1_num)

ESPRIT_full <- ESPRIT_full %>% 
  mutate(gad1_num = case_when(
    gad1_num == "Not at all" ~ 0, 
    gad1_num == "Several days" ~ 1, 
    gad1_num == "More than half the days" ~ 2, 
    gad1_num == "Nearly every day" ~ 3
  )) %>% 
  mutate(gad2_num = case_when(
    gad2_num == "Not at all" ~ 0, 
    gad2_num == "Several days" ~ 1, 
    gad2_num == "More than half the days" ~ 2, 
    gad2_num == "Nearly every day" ~ 3
  )) %>% 
  mutate(gad3_num = case_when(
    gad3_num == "Not at all" ~ 0, 
    gad3_num == "Several days" ~ 1, 
    gad3_num == "More than half the days" ~ 2, 
    gad3_num == "Nearly every day" ~ 3
  )) %>% 
  mutate(gad4_num = case_when(
    gad4_num == "Not at all" ~ 0, 
    gad4_num == "Several days" ~ 1, 
    gad4_num == "More than half the days" ~ 2, 
    gad4_num == "Nearly every day" ~ 3
  )) %>% 
  mutate(gad5_num = case_when(
    gad5_num == "Not at all" ~ 0, 
    gad5_num == "Several days" ~ 1, 
    gad5_num == "More than half the days" ~ 2, 
    gad5_num == "Nearly every day" ~ 3
  )) %>% 
  mutate(gad6_num = case_when(
    gad6_num == "Not at all" ~ 0, 
    gad6_num == "Several days" ~ 1, 
    gad6_num == "More than half the days" ~ 2, 
    gad6_num == "Nearly every day" ~ 3
  )) %>% 
  mutate(gad7_num = case_when(
    gad7_num == "Not at all" ~ 0, 
    gad7_num == "Several days" ~ 1, 
    gad7_num == "More than half the days" ~ 2, 
    gad7_num == "Nearly every day" ~ 3
  ))
  
ESPRIT_full$anxiety <- ESPRIT_full$gad1_num + ESPRIT_full$gad2_num + ESPRIT_full$gad3_num + ESPRIT_full$gad4_num + ESPRIT_full$gad5_num + ESPRIT_full$gad6_num + ESPRIT_full$gad7_num


summary(ESPRIT_full$anxiety)


#Chronic pain attribution 



ESPRIT_full <- ESPRIT_full %>% 
  rename(
    str_atr = `To what extent do you believe your pain is or was due to structural issues in your body?...150`,
    brain_atr = `To what extent do you believe your pain is or was due to mind or brain processes`
  )


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


ESPRIT_full$tsk11_1_num <- ESPRIT_full$tsk11_1
ESPRIT_full$tsk11_2_num <- ESPRIT_full$tsk11_2
ESPRIT_full$tsk11_3_num <- ESPRIT_full$tsk11_3
ESPRIT_full$tsk11_4_num <- ESPRIT_full$tsk11_4
ESPRIT_full$tsk11_5_num <- ESPRIT_full$tsk11_5
ESPRIT_full$tsk11_6_num <- ESPRIT_full$tsk11_6
ESPRIT_full$tsk11_7_num <- ESPRIT_full$tsk11_7
ESPRIT_full$tsk11_8_num <- ESPRIT_full$tsk11_8
ESPRIT_full$tsk11_9_num <- ESPRIT_full$tsk11_9
ESPRIT_full$tsk11_10_num <- ESPRIT_full$tsk11_10
ESPRIT_full$tsk11_11_num <- ESPRIT_full$tsk11_11


ESPRIT_full <- ESPRIT_full %>% 
  mutate(tsk11_1_num = case_when(
    tsk11_1_num == "Strongly disagree" ~ 1, 
    tsk11_1_num == "Disagree" ~ 2, 
    tsk11_1_num == "Agree" ~ 3, 
    tsk11_1_num == "Strongly agree" ~ 4
  )) %>% 
  mutate(tsk11_2_num = case_when(
    tsk11_2_num == "Strongly disagree" ~ 1, 
    tsk11_2_num == "Disagree" ~ 2, 
    tsk11_2_num == "Agree" ~ 3, 
    tsk11_2_num == "Strongly agree" ~ 4
  )) %>% 
  mutate(tsk11_3_num = case_when(
    tsk11_3_num == "Strongly disagree" ~ 1, 
    tsk11_3_num == "Disagree" ~ 2, 
    tsk11_3_num == "Agree" ~ 3, 
    tsk11_3_num == "Strongly agree" ~ 4
  )) %>% 
  mutate(tsk11_4_num = case_when(
    tsk11_4_num == "Strongly disagree" ~ 1, 
    tsk11_4_num == "Disagree" ~ 2, 
    tsk11_4_num == "Agree" ~ 3, 
    tsk11_4_num == "Strongly agree" ~ 4
  )) %>% 
  mutate(tsk11_5_num = case_when(
    tsk11_5_num == "Strongly disagree" ~ 1, 
    tsk11_5_num == "Disagree" ~ 2, 
    tsk11_5_num == "Agree" ~ 3, 
    tsk11_5_num == "Strongly agree" ~ 4
  )) %>% 
  mutate(tsk11_6_num = case_when(
    tsk11_6_num == "Strongly disagree" ~ 1, 
    tsk11_6_num == "Disagree" ~ 2, 
    tsk11_6_num == "Agree" ~ 3, 
    tsk11_6_num == "Strongly agree" ~ 4
  )) %>% 
  mutate(tsk11_7_num = case_when(
    tsk11_7_num == "Strongly disagree" ~ 1, 
    tsk11_7_num == "Disagree" ~ 2, 
    tsk11_7_num == "Agree" ~ 3, 
    tsk11_7_num == "Strongly agree" ~ 4
  )) %>% 
  mutate(tsk11_8_num = case_when(
    tsk11_8_num == "Strongly disagree" ~ 1, 
    tsk11_8_num == "Disagree" ~ 2, 
    tsk11_8_num == "Agree" ~ 3, 
    tsk11_8_num == "Strongly agree" ~ 4
  )) %>% 
  mutate(tsk11_9_num = case_when(
    tsk11_9_num == "Strongly disagree" ~ 1, 
    tsk11_9_num == "Disagree" ~ 2, 
    tsk11_9_num == "Agree" ~ 3, 
    tsk11_9_num == "Strongly agree" ~ 4
  )) %>% 
  mutate(tsk11_10_num = case_when(
    tsk11_10_num == "Strongly disagree" ~ 1, 
    tsk11_10_num == "Disagree" ~ 2, 
    tsk11_10_num == "Agree" ~ 3, 
    tsk11_10_num == "Strongly agree" ~ 4
  )) %>% 
  mutate(tsk11_11_num = case_when(
    tsk11_11_num == "Strongly disagree" ~ 1, 
    tsk11_11_num == "Disagree" ~ 2, 
    tsk11_11_num == "Agree" ~ 3, 
    tsk11_11_num == "Strongly agree" ~ 4
  ))
  

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


ESPRIT_full$cap1_num <- ESPRIT_full$cap1
ESPRIT_full$cap2_num <- ESPRIT_full$cap2
ESPRIT_full$cap3_num <- ESPRIT_full$cap3
ESPRIT_full$cap4_num <- ESPRIT_full$cap4
ESPRIT_full$cap5_num <- ESPRIT_full$cap5
ESPRIT_full$cap6_num <- ESPRIT_full$cap6


ESPRIT_full <- ESPRIT_full %>% 
  mutate(cap1_num = case_when(
    cap1_num == "Never" ~ 1, 
    cap1_num == "Rarely" ~ 2, 
    cap1_num == "Sometimes" ~ 3, 
    cap1_num == "Often" ~ 4, 
    cap1_num == "Always" ~ 5
  )) %>% 
  mutate(cap2_num = case_when(
    cap2_num == "Never" ~ 1, 
    cap2_num == "Rarely" ~ 2, 
    cap2_num == "Sometimes" ~ 3, 
    cap2_num == "Often" ~ 4, 
    cap2_num == "Always" ~ 5
  )) %>% 
  mutate(cap3_num = case_when(
    cap3_num == "Never" ~ 1, 
    cap3_num == "Rarely" ~ 2, 
    cap3_num == "Sometimes" ~ 3, 
    cap3_num == "Often" ~ 4, 
    cap3_num == "Always" ~ 5
  )) %>% 
  mutate(cap4_num = case_when(
    cap4_num == "Never" ~ 1, 
    cap4_num == "Rarely" ~ 2, 
    cap4_num == "Sometimes" ~ 3, 
    cap4_num == "Often" ~ 4, 
    cap4_num == "Always" ~ 5
  )) %>% 
  mutate(cap5_num = case_when(
    cap5_num == "Never" ~ 1, 
    cap5_num == "Rarely" ~ 2, 
    cap5_num == "Sometimes" ~ 3, 
    cap5_num == "Often" ~ 4, 
    cap5_num == "Always" ~ 5
  )) %>% 
  mutate(cap6_num = case_when(
    cap6_num == "Never" ~ 1, 
    cap6_num == "Rarely" ~ 2, 
    cap6_num == "Sometimes" ~ 3, 
    cap6_num == "Often" ~ 4, 
    cap6_num == "Always" ~ 5
  ))


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

ESPRIT_full$let1_num <- ESPRIT_full$let1
ESPRIT_full$let2_num <- ESPRIT_full$let2
ESPRIT_full$let3_num <- ESPRIT_full$let3
ESPRIT_full$let4_num <- ESPRIT_full$let4
ESPRIT_full$let5_num <- ESPRIT_full$let5
ESPRIT_full$let6_num <- ESPRIT_full$let6


ESPRIT_full <- ESPRIT_full %>% 
  mutate(let1_num = case_when(
    let1_num == "Strongly disagree" ~ 5, 
    let1_num == "Disagree" ~ 4, 
    let1_num == "Neutral" ~ 3, 
    let1_num == "Agree" ~ 2, 
    let1_num == "Strongly agree" ~ 1
  )) %>% 
  mutate(let2_num = case_when(
    let2_num == "Strongly disagree" ~ 1, 
    let2_num == "Disagree" ~ 2, 
    let2_num == "Neutral" ~ 3, 
    let2_num == "Agree" ~ 4, 
    let2_num == "Strongly agree" ~ 5 
  )) %>% 
  mutate(let3_num = case_when(
    let3_num == "Strongly disagree" ~ 5, 
    let3_num == "Disagree" ~ 4, 
    let3_num == "Neutral" ~ 3, 
    let3_num == "Agree" ~ 2, 
    let3_num == "Strongly agree" ~ 1
  )) %>% 
  mutate(let4_num = case_when(
    let4_num == "Strongly disagree" ~ 1, 
    let4_num == "Disagree" ~ 2, 
    let4_num == "Neutral" ~ 3, 
    let4_num == "Agree" ~ 4, 
    let4_num == "Strongly agree" ~ 5
  )) %>% 
  mutate(let5_num = case_when(
    let5_num == "Strongly disagree" ~ 5, 
    let5_num == "Disagree" ~ 4, 
    let5_num == "Neutral" ~ 3, 
    let5_num == "Agree" ~ 2, 
    let5_num == "Strongly agree" ~ 1
  )) %>% 
  mutate(let6_num = case_when(
    let6_num == "Strongly disagree" ~ 1, 
    let6_num == "Disagree" ~ 2, 
    let6_num == "Neutral" ~ 3, 
    let6_num == "Agree" ~ 4, 
    let6_num == "Strongly agree" ~ 5
  ))

ESPRIT_full$engagement <- ESPRIT_full$let1_num + ESPRIT_full$let2_num + ESPRIT_full$let3_num + ESPRIT_full$let4_num + ESPRIT_full$let5_num + ESPRIT_full$let6_num



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
    lower_leg_right = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=j. Upper leg, right)`,
    jaw_left = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=m. Jaw, left)`,
    jaw_right = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=n. Jaw, right)`,
    chest = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=o, Chest)`, 
    abdomen = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=p. Abdomen)`, 
    upper_back = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=q. Upper back)`,
    lower_back = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=r. Lower back)`, 
    neck = `Using the image above for reference, please select the areas of you body where you have had PAIN or TENDERNESS over the PAST WEEK. (choice=s. Neck)`
  )


ESPRIT_full$wpi_severity1_num <- ESPRIT_full$wpi_severity1
ESPRIT_full$wpi_severity2_num <- ESPRIT_full$wpi_severity2
ESPRIT_full$wpi_severity3_num <- ESPRIT_full$wpi_severity3

ESPRIT_full <- ESPRIT_full %>% 
  mutate(wpi_severity1_num = case_when(
    wpi_severity1_num == "No problem" ~ 0, 
    wpi_severity1_num == "Slight or mild problems" ~ 1, 
    wpi_severity1_num == "Moderate problems" ~ 2, 
    wpi_severity1_num == "Severe problems" ~ 3
  )) %>% 
  mutate(wpi_severity2_num = case_when(
    wpi_severity2_num == "No problem" ~ 0, 
    wpi_severity2_num == "Slight or mild problems" ~ 1, 
    wpi_severity2_num == "Moderate problems" ~ 2, 
    wpi_severity2_num == "Severe problems" ~ 3
  )) %>% 
  mutate(wpi_severity3_num = case_when(
    wpi_severity3_num == "No problem" ~ 0, 
    wpi_severity3_num == "Slight or mild problems" ~ 1, 
    wpi_severity3_num == "Moderate problems" ~ 2, 
    wpi_severity3_num == "Severe problems" ~ 3
  ))

 
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

ESPRIT_full$pods1_num <- ESPRIT_full$pods1
ESPRIT_full$pods2_num <- ESPRIT_full$pods2
ESPRIT_full$pods3_num <- ESPRIT_full$pods3
ESPRIT_full$pods4_num <- ESPRIT_full$pods4
ESPRIT_full$pods5_num <- ESPRIT_full$pods5
ESPRIT_full$pods6_num <- ESPRIT_full$pods6
ESPRIT_full$pods7_num <- ESPRIT_full$pods7
ESPRIT_full$pods8_num <- ESPRIT_full$pods8


ESPRIT_full <- ESPRIT_full %>% 
  mutate(pods1_num = case_when(
    pods1_num == "Strongly disagree" ~ 1, 
    pods1_num == "Disagree" ~ 2, 
    pods1_num == "Neutral" ~ 3, 
    pods1_num == "Agree" ~ 4, 
    pods1_num == "Strongly agree" ~ 5
  )) %>% 
  mutate(pods2_num = case_when(
    pods2_num == "Strongly disagree" ~ 1, 
    pods2_num == "Disagree" ~ 2, 
    pods2_num == "Neutral" ~ 3, 
    pods2_num == "Agree" ~ 4, 
    pods2_num == "Strongly agree" ~ 5
  )) %>% 
  mutate(pods3_num = case_when(
    pods3_num == "Strongly disagree" ~ 1, 
    pods3_num == "Disagree" ~ 2, 
    pods3_num == "Neutral" ~ 3, 
    pods3_num == "Agree" ~ 4, 
    pods3_num == "Strongly agree" ~ 5
  )) %>% 
  mutate(pods4_num = case_when(
    pods4_num == "Strongly disagree" ~ 1, 
    pods4_num == "Disagree" ~ 2, 
    pods4_num == "Neutral" ~ 3, 
    pods4_num == "Agree" ~ 4, 
    pods4_num == "Strongly agree" ~ 5
  )) %>% 
  mutate(pods5_num = case_when(
    pods5_num == "Never" ~ 1, 
    pods5_num == "Rarely" ~ 2, 
    pods5_num == "Sometimes" ~ 3, 
    pods5_num == "Often" ~ 4, 
    pods5_num == "Always or almost every day" ~ 5
  )) %>% 
  mutate(pods6_num = case_when(
    pods6_num == "Never" ~ 1, 
    pods6_num == "Rarely" ~ 2, 
    pods6_num == "Sometimes" ~ 3, 
    pods6_num == "Often" ~ 4, 
    pods6_num == "Always or almost every day" ~ 5
  )) %>% 
  mutate(pods7_num = case_when(
    pods7_num == "Never" ~ 1, 
    pods7_num == "Once or twice" ~ 2, 
    pods7_num == "Three or more times" ~ 3
  )) %>% 
  mutate(pods8_num = case_when(
    pods8_num == "Not at all bothersome" ~ 1, 
    pods8_num == "A little bothersome" ~ 2, 
    pods8_num == "Moderately bothersome" ~ 3, 
    pods8_num == "Very bothersome" ~ 4, 
    pods8_num == "Extremely bothersome" ~ 5
  ))


ESPRIT_full$opioid <- ESPRIT_full$pods1_num + ESPRIT_full$pods2_num + ESPRIT_full$pods3_num + ESPRIT_full$pods4_num + ESPRIT_full$pods5_num + ESPRIT_full$pods6_num + ESPRIT_full$pods7_num + ESPRIT_full$pods8_num

#Global impressions of change 

ESPRIT_full <- ESPRIT_full %>% 
  rename(
    gic2a = `Physical health in general now?`, 
    gic2b = `Emotional problems (such as feeling anxious, depressed, or irritable) now?`,
    gic2 = `Pain severity now?`
  )

ESPRIT_full$gic2a_num <- ESPRIT_full$gic2a
ESPRIT_full$gic2b_num <- ESPRIT_full$gic2b
ESPRIT_full$gic2_num <- ESPRIT_full$gic2

ESPRIT_full <- ESPRIT_full %>% 
  mutate(gic2a_num = case_when(
    gic2a_num == "Much better" ~ 1, 
    gic2a_num == "Slightly better" ~ 2, 
    gic2a_num == "About the same" ~ 3, 
    gic2a_num == "Slightly worse" ~ 4, 
    gic2a_num == "Much worse" ~ 5
  )) %>% 
  mutate(gic2b_num = case_when(
    gic2b_num == "Much better" ~ 1, 
    gic2b_num == "Slightly better" ~ 2, 
    gic2b_num == "About the same" ~ 3, 
    gic2b_num == "Slightly worse" ~ 4, 
    gic2b_num == "Much worse" ~ 5
  )) %>% 
  mutate(gic2_num = case_when(
    gic2_num == "Much better" ~ 1, 
    gic2_num == "Slightly better" ~ 2, 
    gic2_num == "About the same" ~ 3, 
    gic2_num == "Slightly worse" ~ 4, 
    gic2_num == "Much worse" ~ 5
  ))


###----------------------------------------------------------------------------
#Making dataset that is just baseline values and post-treatment values 

bl_post <- ESPRIT_full %>% 
  filter(`Event Name` %in% c("2 month follow-up", "Baseline"))
#Making a wide dataset for bar graphs
bl_post_wide <- bl_post %>% 
  pivot_wider(
    id_cols = c(ids, cohort),
    names_from = `Event Name`,
    values_from = c(peg, promis_physical, promis_mental, 
                    promis_global01_num, promis_global02_num, kinesophobia, 
                    opioid, depression, anxiety, engagement, painconcern)
  )

bl_post_wide<-bl_post_wide %>% 
  mutate(
    peg_change = peg_Baseline - `peg_2 month follow-up`, 
    promis_physical_change = `promis_physical_2 month follow-up` - promis_physical_Baseline,
    promis_mental_change = `promis_mental_2 month follow-up` - promis_mental_Baseline, 
    promis_general_change = `promis_global01_num_2 month follow-up` - promis_global01_num_Baseline,
    promis_quality_change = `promis_global02_num_2 month follow-up` - promis_global02_num_Baseline,
    kinesophobia_change = kinesophobia_Baseline - `kinesophobia_2 month follow-up`,
    opioid_change = opioid_Baseline - `opioid_2 month follow-up`,
    depression_change = depression_Baseline - `depression_2 month follow-up`, 
    anxiety_change = anxiety_Baseline - `anxiety_2 month follow-up`, 
    engagement_change = `engagement_2 month follow-up` - engagement_Baseline, 
    painconcern_change = painconcern_Baseline - `painconcern_2 month follow-up`
  )


print(bl_post$cohort)
###----------------------------------------------------------------------------
#Making graphs starting with PEG

#PEG

#df to make line graph
peg_df <- bl_post %>% 
  group_by(`Event Name`, cohort) %>% 
  summarize(mean_peg = mean(peg, na.rm=TRUE), 
            se_peg = sd(peg, na.rm = TRUE)/sqrt(n())) %>%
  mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "2 month follow-up")))

#df to make bar graph
peg_df_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(peg_change, na.rm = TRUE),
            se_change = sd(peg_change, na.rm = TRUE)/sqrt(n()))

# Line Plot
peg_line <- peg_df %>%  
  ggplot(aes(x = `Event Name`, y = mean_peg, color = cohort, group = cohort)) +  
  geom_line(linewidth = 0.5) + 
  geom_point(size = 4) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_peg - se_peg, 
                    ymax = mean_peg + se_peg),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +  # Apply the same color palette as the bar plot
  labs(title = "Change in PEG",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "PEG Score") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    # Remove the legend from the line plot
    legend.position = "none"
  ) +  
  scale_y_continuous(
    breaks = seq(4, 7, 1),
    expand = expansion(mult = c(0, 0.1))
  )

# Bar Plot
peg_bar <- peg_df_wide %>%  
  ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
  geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_change - se_change, 
                    ymax = mean_change + se_change),
                position = position_dodge(width = 0.9),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
  scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
  labs(title = "PEG Mean Change",
       subtitle = "Baseline - Post",
       x = "",
       y = "Mean Change",
       fill = "") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "left",  
    legend.justification = "center", 
    legend.direction = "vertical",
    legend.title = element_blank(),   
    legend.text = element_text(size = 11)  
  )

#Combined plot of PEG bar and PEG line
peg_line + peg_bar + 
  plot_annotation(tag_levels = 'A')

###----------------------------------------------------------------------------
#PROMIS General 


PROMIS_generaldf <- bl_post %>% 
  group_by(`Event Name`, cohort) %>% 
  summarize(mean_PROMIS_general = mean(promis_global01_num, na.rm = TRUE),
            se_PROMIS_general = sd(promis_global01_num, na.rm = TRUE)/sqrt(n())) %>% 
  mutate(`Event Name` = factor(`Event Name`, levels = c("Baseline", "2 month follow-up")))



PROMIS_generaldf_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(promis_general_change, na.rm = TRUE),
            se_change = sd(promis_general_change, na.rm=TRUE)/sqrt(n()))


PROMIS_general_line <- PROMIS_generaldf %>% 
  ggplot(aes(x=`Event Name`, y=mean_PROMIS_general, color=cohort, group=cohort)) + 
  geom_line(linewidth = 0.5) + geom_point(size = 4) + theme_minimal() +
  geom_errorbar(aes(ymin = mean_PROMIS_general - se_PROMIS_general, 
                    ymax = mean_PROMIS_general + se_PROMIS_general),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  # Enhanced labels
  labs(title = "Change in PROMIS General Health",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "PROMIS General Health") +
  # Refined theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Add some padding around the plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none"
  ) +
  # Ensure y-axis starts at 0 for better context
  scale_y_continuous(
    breaks = seq(1, 4, 0.5),
    expand = expansion(mult = c(0, 0.1)))

PROMIS_general_bar <- PROMIS_generaldf_wide %>%  
  ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
  geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_change - se_change, 
                    ymax = mean_change + se_change),
                position = position_dodge(width = 0.9),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
  scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
  labs(title = "PROMIS General Health Difference",
       subtitle = "Post - Baseline",
       x = "",
       y = "Mean Change",
       fill = "") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "left",  
    legend.justification = "center", 
    legend.direction = "vertical",
    legend.title = element_blank(),   
    legend.text = element_text(size = 11)  
  )

PROMIS_general_line + PROMIS_general_bar + plot_annotation(tag_levels = 'A')




###----------------------------------------------------------------------------
#PROMIS Quality of Life


PROMIS_qualitydf <- bl_post %>% 
  group_by(`Event Name`, cohort) %>% 
  summarize(mean_PROMIS_quality = mean(promis_global02_num, na.rm = TRUE), 
            se_PROMIS_quality = sd(promis_global02_num, na.rm = TRUE)/sqrt(n())) %>% 
  mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "2 month follow-up")))


#Wide format for bar graph
PROMIS_qualitydf_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(promis_quality_change, na.rm=TRUE),
            se_change = sd(promis_quality_change, na.rm=TRUE)/sqrt(n()))




PROMIS_quality_line <- PROMIS_qualitydf %>% 
  ggplot(aes(x=`Event Name`, y=mean_PROMIS_quality, color=cohort, group=cohort)) + 
  geom_line(linewidth = 0.5) + geom_point(size = 4) + theme_minimal() +
  geom_errorbar(aes(ymin = mean_PROMIS_quality - se_PROMIS_quality, 
                    ymax = mean_PROMIS_quality + se_PROMIS_quality),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Change in PROMIS Quality of Life",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "PROMIS Quality of Life") +
  # Refined theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Add some padding around the plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none"
  ) +
  # Ensure y-axis starts at 0 for better context
  scale_y_continuous(
    breaks = seq(1, 4, 0.5),
    expand = expansion(mult = c(0, 0.1)))

PROMIS_quality_bar <- PROMIS_qualitydf_wide %>%  
  ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
  geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_change - se_change, 
                    ymax = mean_change + se_change),
                position = position_dodge(width = 0.9),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
  scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
  labs(title = "PROMIS Quality of Life Mean Change",
       subtitle = "Post - Baseline",
       x = "",
       y = "Mean Change",
       fill = "") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "left",  
    legend.justification = "center", 
    legend.direction = "vertical",
    legend.title = element_blank(),   
    legend.text = element_text(size = 11)  
  )

PROMIS_quality_line + PROMIS_quality_bar + plot_annotation(tag_levels = 'A')

###----------------------------------------------------------------------------
#PROMIS Physical Health


PROMIS_physicaldf <- bl_post %>% 
  group_by(`Event Name`, cohort) %>% 
  summarize(mean_PROMIS_physical = mean(promis_physical, na.rm = TRUE), 
            sd_PROMIS_physical = sd(promis_physical, na.rm=TRUE)/sqrt(n())) %>% 
  mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "2 month follow-up")))


PROMIS_physicaldf_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(promis_physical_change, na.rm=TRUE),
            se_change = sd(promis_physical_change, na.rm=TRUE)/sqrt(n()))




PROMIS_physical_line <- PROMIS_physicaldf %>% 
  ggplot(aes(x=`Event Name`, y=mean_PROMIS_physical, color=cohort, group=cohort)) + 
  geom_line(linewidth = 0.5) + geom_point(size = 4) + theme_minimal() +
  geom_errorbar(aes(ymin = mean_PROMIS_physical - sd_PROMIS_physical, 
                    ymax = mean_PROMIS_physical + sd_PROMIS_physical),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Change in PROMIS Physical Health",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "PROMIS Physical Health") +
  # Refined theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Add some padding around the plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none"
  ) +
  # Ensure y-axis starts at 0 for better context
  scale_y_continuous(
    breaks = seq(7, 10, 0.5),
    expand = expansion(mult = c(0, 0.1)))
  
PROMIS_physical_bar <- PROMIS_physicaldf_wide %>%  
    ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
    geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
    theme_minimal() +
    geom_errorbar(aes(ymin = mean_change - se_change, 
                      ymax = mean_change + se_change),
                  position = position_dodge(width = 0.9),
                  width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
    scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
    labs(title = "PROMIS Physical Health Mean Change",
         subtitle = "Post - Baseline",
         x = "",
         y = "Mean Change",
         fill = "") +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 11),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.position = "left",  
      legend.justification = "center", 
      legend.direction = "vertical",
      legend.title = element_blank(),   
      legend.text = element_text(size = 11)  
    )  

PROMIS_physical_line + PROMIS_physical_bar + plot_annotation(tag_levels = 'A')

###----------------------------------------------------------------------------
#PROMIS Mental Health

PROMIS_mentaldf <- bl_post %>% 
  group_by(`Event Name`, cohort) %>% 
  summarize(mean_PROMIS_mental = mean(promis_mental, na.rm = TRUE),
            se_PROMIS_mental = sd(promis_mental, na.rm=TRUE)/sqrt(n())) %>%
  mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "2 month follow-up")))

PROMIS_mentaldf_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(promis_mental_change, na.rm=TRUE),
            se_change = sd(promis_mental_change, na.rm=TRUE)/sqrt(n()))

  
PROMIS_mental_line <- PROMIS_mentaldf %>% 
  ggplot(aes(x=`Event Name`, y=mean_PROMIS_mental, color=cohort, group=cohort)) + 
  geom_line(linewidth = 0.5) + geom_point(size = 4) + theme_minimal() +
  geom_errorbar(aes(ymin = mean_PROMIS_mental - se_PROMIS_mental, 
                    ymax = mean_PROMIS_mental + se_PROMIS_mental),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Change in PROMIS Mental Health",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "PROMIS Mental Health") +
  # Refined theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Add some padding around the plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    legend.position = "none"
  ) +
  # Ensure y-axis starts at 0 for better context
  scale_y_continuous(
    breaks = seq(9, 12, 1),
    expand = expansion(mult = c(0, 0.1)))

PROMIS_mental_bar <- PROMIS_mentaldf_wide %>%  
  ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
  geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_change - se_change, 
                    ymax = mean_change + se_change),
                position = position_dodge(width = 0.9),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
  scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
  labs(title = "PROMIS Mental Health Mean Change",
       subtitle = "Post - Baseline",
       x = "",
       y = "Mean Change",
       fill = "") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "left",  
    legend.justification = "center", 
    legend.direction = "vertical",
    legend.title = element_blank(),   
    legend.text = element_text(size = 11)  
  )  


PROMIS_mental_line + PROMIS_mental_bar + plot_annotation(tag_levels = 'A')


###----------------------------------------------------------------------------
#PHQ
depression_df <- bl_post %>% 
  group_by(`Event Name`, cohort) %>%
  summarize(mean_depression = mean(depression, na.rm = TRUE),
            se_depression = sd(depression, na.rm = TRUE)/sqrt(n())) %>% 
  mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "2 month follow-up")))

depression_df_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(depression_change, na.rm=TRUE),
            se_change = sd(depression_change, na.rm=TRUE)/sqrt(n()))


depression_line <- depression_df %>% 
  ggplot(aes(x=`Event Name`, y=mean_depression, color=cohort, group=cohort)) + 
  geom_line(linewidth = 0.5) + geom_point(size = 4) + theme_minimal() +
  geom_errorbar(aes(ymin = mean_depression - se_depression, 
                    ymax = mean_depression + se_depression),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Change in Depression",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "PHQ-8 Score") +
  # Refined theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Add some padding around the plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none"
  ) +
  # Ensure y-axis starts at 0 for better context
  scale_y_continuous(
    breaks = seq(6, 11, 1),
    expand = expansion(mult = c(0, 0.1)))

depression_bar <- depression_df_wide %>%  
  ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
  geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_change - se_change, 
                    ymax = mean_change + se_change),
                position = position_dodge(width = 0.9),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
  scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
  labs(title = "Depression Mean Change",
       subtitle = "Baseline - Post",
       x = "",
       y = "Mean Change",
       fill = "") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "left",  
    legend.justification = "center", 
    legend.direction = "vertical",
    legend.title = element_blank(),   
    legend.text = element_text(size = 11)  
  )  

depression_line + depression_bar + plot_annotation(tag_levels = 'A')


###----------------------------------------------------------------------------
#GAD
anxiety_df <- bl_post %>%
  group_by(`Event Name`, cohort) %>%
  summarize(mean_anxiety = mean(anxiety, na.rm = TRUE),
            se_anxiety = sd(anxiety, na.rm=TRUE)/sqrt(n())) %>%
  mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "2 month follow-up")))

anxiety_df_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(anxiety_change, na.rm=TRUE),
            se_change = sd(anxiety_change, na.rm=TRUE)/sqrt(n()))

  
anxiety_line <- anxiety_df %>% 
  ggplot(aes(x=`Event Name`, y=mean_anxiety, color=cohort, group=cohort)) + 
  geom_line(linewidth = 0.5) + geom_point(size = 4) + theme_minimal() +
  geom_errorbar(aes(ymin = mean_anxiety - se_anxiety, 
                    ymax = mean_anxiety + se_anxiety),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Change in Anxiety",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "GAD-7 Score") +
  # Refined theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Add some padding around the plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none"
  ) +
  # Ensure y-axis starts at 0 for better context
  scale_y_continuous(
    breaks = seq(4, 12, 1),
    expand = expansion(mult = c(0, 0.1)))


anxiety_bar <- anxiety_df_wide %>%  
  ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
  geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_change - se_change, 
                    ymax = mean_change + se_change),
                position = position_dodge(width = 0.9),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
  scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
  labs(title = "Anxiety Mean Change",
       subtitle = "Baseline - Post",
       x = "",
       y = "Mean Change",
       fill = "") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "left",  
    legend.justification = "center", 
    legend.direction = "vertical",
    legend.title = element_blank(),   
    legend.text = element_text(size = 11)  
  )

anxiety_line + anxiety_bar + plot_annotation(tag_levels = 'A')

###----------------------------------------------------------------------------
#Concerns about pain

CAP_df <- bl_post %>%
  group_by(`Event Name`, cohort) %>%
  summarize(mean_CAP = mean(painconcern, na.rm = TRUE),
            se_CAP = sd(painconcern, na.rm=TRUE)/sqrt(n())) %>% 
  mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "2 month follow-up")))


CAP_df_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(painconcern_change, na.rm=TRUE),
            se_change = sd(painconcern_change, na.rm=TRUE)/sqrt(n()))


CAP_line <- CAP_df %>% 
  ggplot(aes(x=`Event Name`, y=mean_CAP, color=cohort, group=cohort)) + 
  geom_line(linewidth = 0.5) + geom_point(size = 4) + theme_minimal() +
  geom_errorbar(aes(ymin = mean_CAP - se_CAP, 
                    ymax = mean_CAP + se_CAP),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Change in Concern About Pain",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "CAP Score") +
  # Refined theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Add some padding around the plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none"
  ) +
  # Ensure y-axis starts at 0 for better context
  scale_y_continuous(
    breaks = seq(12, 18, 1),
    expand = expansion(mult = c(0, 0.1)))

CAP_bar <- CAP_df_wide %>%  
  ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
  geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_change - se_change, 
                    ymax = mean_change + se_change),
                position = position_dodge(width = 0.9),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
  scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
  labs(title = "Concern About Pain Mean Change",
       subtitle = "Baseline - Post",
       x = "",
       y = "Mean Change",
       fill = "") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "left",  
    legend.justification = "center", 
    legend.direction = "vertical",
    legend.title = element_blank(),   
    legend.text = element_text(size = 11)  
  )

CAP_line + CAP_bar + plot_annotation(tag_levels = 'A')

###----------------------------------------------------------------------------  
#Life Engagement 

engagement_df <- bl_post %>% 
  group_by(`Event Name`, cohort) %>%
  summarize(mean_engagement = mean(engagement, na.rm=TRUE), 
            se_engagement = sd(engagement, na.rm=TRUE)/sqrt(n())) %>%
  mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "2 month follow-up")))

engagement_df_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(engagement_change, na.rm=TRUE),
            se_change = sd(engagement_change, na.rm=TRUE)/sqrt(n()))

  
engagement_line <- engagement_df %>% 
  ggplot(aes(x=`Event Name`, y=mean_engagement, color=cohort, group=cohort)) + 
  geom_line(linewidth = 0.5) + geom_point(size = 4) + theme_minimal() +
  geom_errorbar(aes(ymin = mean_engagement - se_engagement, 
                    ymax = mean_engagement + se_engagement),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Change Life Engagement",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "LET Score") +
  # Refined theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Add some padding around the plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none"
  ) +
  # Ensure y-axis starts at 0 for better context
  scale_y_continuous(
    breaks = seq(22, 29, 1),
    expand = expansion(mult = c(0, 0.1)))  

engagement_bar <- engagement_df_wide %>%  
  ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
  geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_change - se_change, 
                    ymax = mean_change + se_change),
                position = position_dodge(width = 0.9),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
  scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
  labs(title = "Life Engagement Mean Change",
       subtitle = "Post - Baseline",
       x = "",
       y = "Mean Change",
       fill = "") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "left",  
    legend.justification = "center", 
    legend.direction = "vertical",
    legend.title = element_blank(),   
    legend.text = element_text(size = 11)  
  )

engagement_line+engagement_bar+plot_annotation(tag_levels = 'A')

###----------------------------------------------------------------------------
#TSK

kinseophobia_df <- bl_post %>%
  group_by(`Event Name`, cohort) %>%
  summarize(mean_kinesophobia = mean(kinesophobia, na.rm=TRUE),
            se_kinesophobia = sd(kinesophobia, na.rm=TRUE)/sqrt(n())) %>%
  mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "2 month follow-up")))


kinseophobia_df_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(kinesophobia_change, na.rm = TRUE),
            se_change = sd(kinesophobia_change, na.rm = TRUE)/sqrt(n()))


kinesophobia_line <- kinseophobia_df %>% 
  ggplot(aes(x=`Event Name`, y=mean_kinesophobia, color=cohort, group=cohort)) + 
  geom_line(linewidth = 0.5) + geom_point(size = 4) + theme_minimal() +
  geom_errorbar(aes(ymin = mean_kinesophobia - se_kinesophobia, 
                    ymax = mean_kinesophobia + se_kinesophobia),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Change in Kinesophobia",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "TSK-11 Score") +
  # Refined theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Add some padding around the plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none"
  ) +
  # Ensure y-axis starts at 0 for better context
  scale_y_continuous(
    breaks = seq(19, 29, 1),
    expand = expansion(mult = c(0, 0.1)))

kinesophobia_bar <- kinseophobia_df_wide %>%  
  ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
  geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_change - se_change, 
                    ymax = mean_change + se_change),
                position = position_dodge(width = 0.9),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
  scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
  labs(title = "Kinesophobia Mean Change",
       subtitle = "Baseline - Post",
       x = "",
       y = "Mean Change",
       fill = "") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "left",  
    legend.justification = "center", 
    legend.direction = "vertical",
    legend.title = element_blank(),   
    legend.text = element_text(size = 11)  
  )


kinesophobia_line+kinesophobia_bar+plot_annotation(tag_levels = 'A')
  
###----------------------------------------------------------------------------
#Opioid 

opioid_df<-bl_post %>%
  group_by(`Event Name`, cohort) %>%
  summarize(mean_opioid = mean(opioid, na.rm=TRUE),
            se_opioid = sd(opioid, na.rm=TRUE)/sqrt(n())) %>%
  mutate(`Event Name` = factor(`Event Name`, levels=c("Baseline", "2 month follow-up")))

opioid_df_wide <- bl_post_wide %>% 
  group_by(cohort) %>% 
  summarize(mean_change = mean(opioid_change, na.rm=TRUE),
            se_change = sd(opioid_change, na.rm=TRUE)/sqrt(n()))

opioid_line <- opioid_df %>% 
  ggplot(aes(x=`Event Name`, y=mean_opioid, color=cohort, group=cohort)) + 
  geom_line(linewidth = 0.5) + geom_point(size = 4) + theme_minimal() +
  geom_errorbar(aes(ymin = mean_opioid - se_opioid, 
                    ymax = mean_opioid + se_opioid),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Change in Opioid Use",
       subtitle = "From Baseline to 2-Month Follow-up",
       x = "",
       y = "POD Score") +
  # Refined theme elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Add some padding around the plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none"
  ) +
  # Ensure y-axis starts at 0 for better context
  scale_y_continuous(
    breaks = seq(8, 22, 1),
    expand = expansion(mult = c(0, 0.1))) 


opioid_bar <- opioid_df_wide %>%  
  ggplot(aes(x = cohort, y = mean_change, fill = cohort, group = cohort)) +  
  geom_col(position = position_dodge(width = 0.9), color = "black", size = 0.5) + 
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_change - se_change, 
                    ymax = mean_change + se_change),
                position = position_dodge(width = 0.9),
                width = 0.25, color = "black", alpha = 0.9, size = 0.8) + 
  scale_fill_brewer(palette = "Set2") +  # Same palette as the line plot
  labs(title = "Opioid Mean Change",
       subtitle = "Baseline - Post",
       x = "",
       y = "Mean Change",
       fill = "") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "left",  
    legend.justification = "center", 
    legend.direction = "vertical",
    legend.title = element_blank(),   
    legend.text = element_text(size = 11)  
  )
  
opioid_line + opioid_bar + plot_annotation(tag_levels = 'A')
