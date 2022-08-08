# Personal_Ind_Heart
Predicting Diabetes Based on Several Variables From a Kaggle Dataset
---
title: "Exploratory Data Analysis"
author: "STOR 320.01 Group 2"
date: "`r format(Sys.time(), '%July %12, %2022')`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)

heart <- read.csv("/Users/Staff/Desktop/heart_2020_cleaned.csv")
heart[sapply(heart, is.character)] <- lapply(heart[sapply(heart, is.character)], 
                                       as.factor)

library(kableExtra)
library(knitr)
library(sparkline)
library(formattable)
library(magick)

heart_GH<- heart%>%
count(GenHealth)
heart_GH$n<- color_bar("lightgreen")(heart_GH$n)
kbl(heart_GH,escape = F) %>%
kable_paper("hover", full_width = T)%>%
column_spec(2,width="5cm")

heart_ST<- heart%>%
count(Stroke)
heart_ST$n<- color_bar("aqua")(heart_ST$n)
kbl(heart_ST,escape = F) %>%
kable_paper("hover", full_width = T)%>%
column_spec(2,width="5cm")

heart_AS<- heart%>%
count(Asthma)
heart_AS$n<- color_bar("orange")(heart_AS$n)
kbl(heart_AS,escape = F) %>%
kable_paper("hover", full_width = T)%>%
column_spec(2,width="5cm")

heart_HD<- heart%>%
count(HeartDisease)
heart_HD$n<- color_bar("pink")(heart_HD$n)
kbl(heart_HD,escape = F) %>%
kable_paper("hover", full_width = T)%>%
column_spec(2,width="5cm")

heart_RA<- heart%>%
count(Race)
heart_RA$n<- color_bar("lavender")(heart_RA$n)
kbl(heart_RA,escape = F) %>%
kable_paper("hover", full_width = T)%>%
column_spec(2,width="5cm")

kbl(heart[1:20,])%>%
kable_styling("hover",fixed_thead = T)
```

```{r}
library(gganimate)
```


```{r}
heart_hda=heart%>%
  filter(HeartDisease=="Yes")
heart_wohd=heart%>%
  filter(HeartDisease=="No")

hda=nrow(heart_hda)
wohd=nrow(heart_wohd)

smoke_and_heart_disease1 = subset(heart_hda,Smoking=='Yes')
num_sh=nrow(smoke_and_heart_disease1)
hd_smoke=num_sh/hda

smoke_and_heart_disease11 = subset(heart_wohd,Smoking=='Yes')
num_sh1=nrow(smoke_and_heart_disease11)
wohd_smoke=num_sh1/wohd
wohd_smoke

stroke_and_heart_disease1 = subset(heart_hda,Stroke=='Yes')
num_st=nrow(stroke_and_heart_disease1)
hd_stroke=num_st/hda

stroke_and_heart_disease11 = subset(heart_wohd,Stroke=='Yes')
num_st1=nrow(stroke_and_heart_disease11)
wohd_stroke=num_st1/wohd

asthma_and_heart_disease1 = subset(heart_hda,Asthma=='Yes')
num_as=nrow(asthma_and_heart_disease1)
hd_asthma=num_as/hda

asthma_and_heart_disease11 = subset(heart_wohd,Asthma=='Yes')
num_as1=nrow(asthma_and_heart_disease11)
wohd_asthma=num_as1/wohd

AD_and_heart_disease1 = subset(heart_hda,AlcoholDrinking=='Yes')
num_ad=nrow(AD_and_heart_disease1)
hd_ad=num_ad/hda

AD_and_heart_disease11 = subset(heart_wohd,AlcoholDrinking=='Yes')
num_ad1=nrow(AD_and_heart_disease11)
wohd_ad1=num_ad1/wohd

HDorNot<-tribble(
  ~Factors, ~Without_Heart_Disease, ~With_Heart_Disease,
  "Smoking",        wohd_smoke,     hd_smoke,
  "Stroke",           wohd_stroke,   hd_stroke,
  "Asthma",      wohd_asthma,       hd_asthma,
  "AlcoholDrinking",      wohd_ad1,  hd_ad
)
HDorNot

HDorNot<-tribble(
  ~Factors, ~Proportion,
  "Smoking_withoutHD",    wohd_smoke,
  "Smoking_withHD",    hd_smoke,
  "Stroke_withoutHD",      wohd_stroke,
  "Stroke_withHD",      hd_stroke,
  "Asthma_withoutHD",      wohd_asthma,
   "Asthma_withHD",      hd_asthma,
  "AlcoholDrinking_withoutHD", wohd_ad1,
  "AlcoholDrinking_withHD", hd_ad,
  
)
HDorNot
visual<- ggplot(
  HDorNot,
 
)+
  geom_col( aes(Factors,Proportion,fill=Factors))
  
  
visual+transition_states(Factors,wrap=FALSE)+
  shadow_mark()
```


```{r}
# Creator: Abrah Furbee

### Q1: Do men or women seem to sleep more ? Is there a significant difference between the sexes ?

heart %>%
  group_by(Sex) %>%
  summarise(
          count = n(),
          min_sleep = min(SleepTime),
          max_sleep = max(SleepTime),
          mean_sleep = mean(SleepTime),
          q1_sleep = quantile(SleepTime, .25),
          q2_sleep = quantile(SleepTime,.5),
          q3_sleep = quantile(SleepTime, .75))
```

```{r}
heart %>% group_by(Sex) %>% ggplot(mapping=aes(x = SleepTime, color = Sex)) + geom_boxplot() + ggtitle("Sleep Time vs. Sex")
```

### Q2: Is a certain race more susceptible to heart disease ?

```{r}
heart %>% group_by(Race, HeartDisease) %>% summarize(count = n()) %>%
  ggplot() + geom_bar(aes(Race, count, fill = as.factor(HeartDisease)), stat = "Identity", position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Race") + labs(fill = "Heart \nDisease") + ggtitle("General Distribution of Heart Disease across Race")

heart_am_ind = round(.0016948358 / (.0016948358 + .0145718351), 3)
heart_asian = round(0.0008317829 / (0.0008317829 + 0.0243968793), 3)
heart_black = round(1729 / (1729 + 21210), 3)
heart_hispanic = round(1443 / (1443 + 26003), 3)
heart_other = round(886 / (886 + 10042), 3)
heart_white = round(22507 / (22507 + 222705), 3)

heart %>%
  group_by(Race, HeartDisease) %>%
  summarise(n = n()) %>% ungroup() %>% filter(HeartDisease == "Yes") %>% 
  mutate(prop = ifelse(Race == "Asian",heart_asian,
                       ifelse(Race == "Hispanic", heart_hispanic,
                              ifelse(Race == "Black", heart_black,
                                     ifelse(Race == "Other", heart_other,
                                            ifelse(Race =="White", heart_white,
                                                   ifelse(Race == "American Indian", heart_am_ind, "NA"))))))) %>% 
  ggplot() + geom_bar(aes(Race, prop, fill = as.factor(HeartDisease)), stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Proportion") + xlab("Race") + ggtitle("Mean Proportion of Heart Disease by Race")
  
```

# Interpreter: Chen Hua

### Q1: Are individuals who smoke more likel to experience heart disease?

```{r}
heart %>% group_by(Smoking, HeartDisease) %>% summarize(count = n()) %>%
  ggplot() + geom_bar(aes(Smoking, count, fill = as.factor(HeartDisease)), stat = "Identity",position='dodge') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size = 10)) + 
  ylab("Count") + xlab("Smoking") + labs(fill = "Heart \nDisease") + ggtitle("Effect of Smoking on Heart Disease")

smoke_and_heart_disease = subset(heart, HeartDisease=='Yes' & Smoking==
'Yes')

nrow(smoke_and_heart_disease)
num_smoke=subset(heart,Smoking=='Yes')
nrow(num_smoke)

no_smoke_and_heart_disease =subset(heart, HeartDisease=='Yes'& Smoking=='No')
num_no_smoke =subset (heart,Smoking=="No")
nrow(smoke_and_heart_disease)/nrow(num_smoke) # prob of heart disease for smokers
nrow(no_smoke_and_heart_disease)/nrow(num_no_smoke) # prob of heart disease for non-smokers

Smoking_status <- c("Smoker", "Non-smoker")
Prob_HD <- c(round(nrow(smoke_and_heart_disease)/nrow(num_smoke), 3), round(nrow(no_smoke_and_heart_disease)/nrow(num_no_smoke), 3))
smoke_df = data.frame(Smoking_status, Prob_HD)
smoke_df
```

### Q2: Are people with higher BMI's at an increased risk of experiencing heart disease?

```{r}
UNDERWEIGHT_BMI_HD=nrow(subset(heart, HeartDisease=='Yes' & BMI< 18.5))
UNDERWEIGHT_BMI=nrow(subset(heart,BMI<18.5))
prop_UNDER=UNDERWEIGHT_BMI_HD/UNDERWEIGHT_BMI

NORMAL_BMI_HD=nrow(subset(heart, HeartDisease=='Yes' & BMI>= 18.5 & BMI<=24.9))
NORMAL_BMI=nrow(subset(heart,BMI>=18.5 & BMI<=24.9))
prop_NORMAL=NORMAL_BMI_HD/NORMAL_BMI

OVERWEIGHT_BMI_HD=nrow(subset(heart, HeartDisease=='Yes' & BMI>24.9 & BMI<=29.9))
OVERWEIGHT_BMI=nrow(subset(heart,BMI>24.9 & BMI<=29.9))
prop_OVERWEIGHT=OVERWEIGHT_BMI_HD/OVERWEIGHT_BMI

OBESE_BMI_HD=nrow(subset(heart, HeartDisease=='Yes' & BMI> 29.9 & BMI<=34.9))
OBESE_BMI=nrow(subset(heart,BMI>29.9 & BMI<=34.9))
prop_OBESE=OBESE_BMI_HD/OBESE_BMI

EXTREM_BMI_HD=nrow(subset(heart, HeartDisease=='Yes' & BMI>34.9))
EXTREM_BMI=nrow(subset(heart,BMI>34.9))
prop_EXTREM=EXTREM_BMI_HD/EXTREM_BMI



DIFF_BMI<-tribble(
  ~Weight_classification, ~proportion,
  "UNDERWEIGHT",        prop_UNDER,
  "NORMAL",           prop_NORMAL,
  "OVERWEIGHT",      prop_OVERWEIGHT,
  "OBESE",           prop_OBESE,
  "EXTREMELY OBESE",  prop_EXTREM
)
DIFF_BMI$Weight_classification<-factor(DIFF_BMI$Weight_classification,levels = c("UNDERWEIGHT","NORMAL","OVERWEIGHT","OBESE","EXTREMELY OBESE"))
ggplot(DIFF_BMI, aes(Weight_classification, proportion, group = 1)) +
  geom_point()+
  geom_line() + theme_bw() +
         labs(title = "Proportion of Heart Disease by BMI")+ylab("Proportion")+xlab("BMI")

```

# Orator: Yesh Munagala

### Q1: Is there a difference in drinking/smoking among the sexes?

```{r}
# Alcohol Drinking by sex --> Question # 9
heart %>% group_by(AlcoholDrinking, Sex) %>% summarize(count = n()) %>%
  ggplot() + geom_bar(aes(AlcoholDrinking, count, fill = as.factor(Sex)), stat = "Identity", position="dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size = 10)) + 
  ylab("Count") + xlab("Alcohol Drinking") + labs(fill = "Sex")

males_drinkers = subset(heart, AlcoholDrinking =='Yes' & Sex=='Male')
no_male = subset(heart, Sex == "Male")
prob_male_drinkers = nrow(males_drinkers) / nrow(no_male)

females_drinkers = subset(heart, AlcoholDrinking=='Yes' & Sex=='Female')
no_female = subset(heart, Sex == "Female")
prob_female_drinkers = nrow(females_drinkers) / nrow(no_female)

prob_male_drinkers
prob_female_drinkers


# Smoking by sex
heart %>% group_by(Smoking, Sex) %>% summarize(count = n()) %>%
  ggplot() + geom_bar(aes(Smoking, count, fill = as.factor(Sex)), stat = "Identity", position="dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, size = 10)) + 
  ylab("Count") + xlab("Smoking") + labs(fill = "Sex")

males_smokers = subset(heart, Smoking =='Yes' & Sex=='Male')
no_male = subset(heart, Sex == "Male")
prob_male_smokers = nrow(males_smokers) / nrow(no_male)

females_smokers = subset(heart, Smoking=='Yes' & Sex=='Female')
no_female = subset(heart, Sex == "Female")
prob_female_smokers = nrow(females_smokers) / nrow(no_female)

prob_male_smokers
prob_female_smokers


Sex <- c("Male", "Female")
AlcoholDrinking <- c(round(prob_male_drinkers, 3), round(prob_female_drinkers,3))
Smoking <- c(round(prob_male_smokers, 3), round(prob_female_smokers,3))
sex_drink_smoke_df <- data.frame(Sex, AlcoholDrinking, Smoking)
sex_drink_smoke_df
```

### Q2: Is there a difference in the probability of heart disease by general health? (ie, do those who report overall better general health at lower risk of heart disease than those who report worse overall general health)?

```{r}
# mean heart disease by general health

heart_poor = 3850 / (3850 + 7439)
heart_fair = 7084 / (7084 + 27593)
heart_good = 9558 / (9558 + 83571)
heart_verygood = 5381/ (5381 + 108477)
heart_excellent = 1500 / (1500 + 63542)

heart$GenHealth <- factor(heart$GenHealth, levels=c('Poor', 'Fair', 'Good', 'Very good', 'Excellent'))

heart %>%
  group_by(GenHealth, HeartDisease, Sex) %>%
  summarise(n = n()) %>% ungroup()  %>% filter(HeartDisease == "Yes") %>% 
  mutate(prop = ifelse(GenHealth == "Poor",heart_poor,
                       ifelse(GenHealth == "Fair", heart_fair,
                              ifelse(GenHealth == "Good", heart_good,
                                     ifelse(GenHealth == "Very good", heart_verygood,
                                            ifelse(GenHealth =="Excellent", heart_excellent, "NA")))))) %>% 
  ggplot() + geom_bar(aes(GenHealth, prop, fill = Sex),stat = "Identity", position = 'dodge') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Proportion") + xlab("General Health")



Poor <- c(round(heart_poor, 3))
Fair <- c(round(heart_fair, 3))
Good <- c(round(heart_good, 3))
Very_good <- c(round(heart_verygood, 3))
Excellent <- c(round(heart_excellent, 3))
gen_health_lvls <- data.frame(Poor, Fair, Good, Very_good, Excellent)
gen_health_lvls
```

# Orator: Sanjana Chaudhary

### Q1: While observing the data provided for drinking and mental health, can we find a significant correlation?

```{r}
ggplot(heart, 
       aes(x = MentalHealth, 
           fill = AlcoholDrinking)) +
  geom_density(alpha = 0.4) +
  labs(title = "Mental Health by Drinking Status") + theme_bw()
```

### Q2: As men and women grow older, does the average BMI increase or decrease? Is there a significant difference in the change of average BMI over time between the genders?

```{r}
ggplot(data = heart, aes(x = AgeCategory, y = BMI, color = Sex)) + geom_col(aes(fill = Sex), position = "dodge") + theme(axis.text.x = element_text(angle = 40, hjust = 0.9)) + 
  xlab("Age")+ylab("BMI")+ggtitle("BMI vs. Age") + theme_bw()
```

# Deliverer: Kaden Graham

### Q1: In which age category do individuals have the highest probability of experiencing heart disease?

```{r}
ggplot(data=filter(heart, HeartDisease =="Yes"))+
  geom_bar(aes(x=AgeCategory, color = Sex), position = "dodge")+ylab("Number of Individuals with Heart Disease")+xlab("Age Category") +
  ggtitle("Heart Disease vs. Age") + theme_bw()
```

### Q2: Is there a relationship between sex and general health?

```{r}
ggplot(data=heart)+
  geom_bar(mapping=aes(x=GenHealth, fill=Sex, colour=Sex),position='dodge')+xlab("General Health")+ylab("Number of Individuals with Heart Disease") + ggtitle("General Health vs. Sex") + theme_bw()
```

# Follow-up Questions

### New Questions Based Off Initial Investigation

- Q1: Can we build logistic regression model to accurately predict whether an individual has heart disease?

- Q2: Which factors are most associated with heart disease? (ie, which conditions indicate the highest probability of developing heart disease?) 

- Q3: From the above, we see that presence of a stroke translates to the highest probability of developing heart disease. In the first initial question, we saw that American Indians have the highest probability of heart disease. Does this race also have the highest probability of stroke?

- Q4: We see that extrema in BMI correspond to a high probability of heart disease. Which other diseases do these indicate that an individual will develop? Since we saw that an individual who has had a stroke has the highest probability of heart disease, does the same pattern follow for BMI and stroke?

### Investigation of Follow-up Questions

GIVE WHAT 2 QUESTIONS YOU ATTEMPTED TO INVESTIGATE FURTHER IN COMPLETE SENTENCES (*Example:* Our group decided to investigate Q2 and Q4 in further detail.)

Our group decided to investigate Q2 and Q3 further.

SHOW AT LEAST 2 TABLES OR FIGURES BELOW THAT EXPLORE ANSWERS FOR THE QUESTIONS YOU ARE INVESTIGATING FURTHER.

```{r}
# FURTHER ANALYSIS QUESTION 1

# Heart disease by smoking 

smoke_and_heart_disease = subset(heart, HeartDisease=='Yes' & Smoking=='Yes')
nrow(smoke_and_heart_disease)
num_smoke = subset(heart, Smoking == "Yes")
nrow(num_smoke)
no_smoke_and_heart_disease = subset(heart, HeartDisease=='Yes' & Smoking=='No')
num_no_smoke = subset(heart, Smoking == "No")
smoke_HD <- nrow(smoke_and_heart_disease)/nrow(num_smoke) # prob of heart disease for smokers
non_smoke_HD <- nrow(no_smoke_and_heart_disease)/nrow(num_no_smoke) # prob of heart disease for non-smokers

# Heart disease by drinking

drink_and_heart_disease = subset(heart, HeartDisease=='Yes' & AlcoholDrinking=='Yes')
nrow(drink_and_heart_disease)
num_drink = subset(heart, AlcoholDrinking == "Yes")
nrow(num_drink)
no_drink_and_heart_disease = subset(heart, HeartDisease=='Yes' & AlcoholDrinking=='No')
num_no_drink = subset(heart, AlcoholDrinking == "No")
drink_HD <- nrow(drink_and_heart_disease)/nrow(num_drink) # prob of heart disease for drinkers
non_drink_HD <- nrow(no_drink_and_heart_disease)/nrow(num_no_drink) # prob of heart disease for non-drinkers

#Heart disease by stroke

stroke_and_heart_disease = subset(heart, HeartDisease=='Yes' & Stroke=='Yes')
nrow(stroke_and_heart_disease)
num_stroke = subset(heart, Stroke == "Yes")
nrow(num_stroke)
no_stroke_and_heart_disease = subset(heart, HeartDisease=='Yes' & Stroke=='No')
num_no_stroke = subset(heart, Stroke == "No")
stroke_HD <- nrow(stroke_and_heart_disease)/nrow(num_stroke) # prob of heart disease for stoke
non_stroke_HD <- nrow(no_stroke_and_heart_disease)/nrow(num_no_stroke) # prob of heart disease for non-stroke

# Heart disease by Kidney Disease

kidney_and_heart_disease = subset(heart, HeartDisease=='Yes' & KidneyDisease=='Yes')
nrow(kidney_and_heart_disease)
num_kidney = subset(heart, KidneyDisease == "Yes")
nrow(num_kidney)
no_kidney_and_heart_disease = subset(heart, HeartDisease=='Yes' & KidneyDisease=='No')
num_no_kidney = subset(heart, KidneyDisease == "No")
kid_HD <- nrow(kidney_and_heart_disease)/nrow(num_kidney) # prob of heart disease for kidney
non_kid_HD <- nrow(no_kidney_and_heart_disease)/nrow(num_no_kidney) # prob of heart disease for non-kidney

# Heart disease by Skin Cancer
skin_and_heart_disease = subset(heart, HeartDisease=='Yes' & SkinCancer =='Yes')
nrow(skin_and_heart_disease)
num_skin = subset(heart, SkinCancer == "Yes")
nrow(num_skin)
no_skin_and_heart_disease = subset(heart, HeartDisease=='Yes' & SkinCancer=='No')
num_no_skin = subset(heart, SkinCancer == "No")
can_HD <- nrow(skin_and_heart_disease)/nrow(num_skin) # prob of heart disease for skin cancer
non_can_HD <- nrow(no_skin_and_heart_disease)/nrow(num_no_skin) # prob of heart disease for non-skin-cancer


# Heart diseasey by Asthma

asthma_and_heart_disease = subset(heart, HeartDisease=='Yes' & Asthma =='Yes')
nrow(asthma_and_heart_disease)
num_asthma = subset(heart, Asthma == "Yes")
nrow(num_asthma)
no_asthma_and_heart_disease = subset(heart, HeartDisease=='Yes' & Asthma=='No')
num_no_asthma = subset(heart, Asthma == "No")
ast_HD <- nrow(asthma_and_heart_disease)/nrow(num_asthma) # prob of heart disease for asthma
non_ast_HD <- nrow(no_asthma_and_heart_disease)/nrow(num_no_asthma) # prob of heart disease for non-asthma


# Heart disease by Physical Activity 

physical_and_heart_disease = subset(heart, HeartDisease=='Yes' & PhysicalActivity =='Yes')
nrow(physical_and_heart_disease)
num_physical = subset(heart, PhysicalActivity == "Yes")
nrow(num_physical)
no_physical_and_heart_disease = subset(heart, HeartDisease=='Yes' & PhysicalActivity=='No')
num_no_physical = subset(heart, PhysicalActivity == "No")
phys_HD <- nrow(physical_and_heart_disease)/nrow(num_physical) # prob of heart disease for physically active
non_phys_HD <- nrow(no_physical_and_heart_disease)/nrow(num_no_physical) # prob of heart disease for not physically active

disease_factors <- c("Smoking", 
                     "Drinking", 
                     "Stroke",
                     "Asthma",
                     "Physical Activity",
                     "Kidney Disease",
                     "Skin Cancer")

hd_given_factor <- c(smoke_HD,drink_HD,stroke_HD, ast_HD,phys_HD,kid_HD, can_HD)
hd_without_factor <- c(non_smoke_HD, non_drink_HD, non_stroke_HD, non_ast_HD, non_phys_HD, non_kid_HD,non_can_HD )

df <- data.frame(disease_factors, hd_given_factor, hd_without_factor)
df$hd_given_factor = round(100*df$hd_given_factor,3)
df$hd_without_factor = round(100*df$hd_without_factor, 3)
df$Change =  round(df$hd_without_factor - df$hd_given_factor, 3)
df

```

```{r}
# Race vs. stroke

stroke_am_ind = round(307 / (307 + 4895),3)
stroke_asian = round(151 / (151 + 7917), 3)
stroke_black = round(1256 / (1256 + 21683), 3)
stroke_hispanic = round(595 / (595 + 26851), 3)
stroke_other = round(476 / (476 + 10452), 3)
stroke_white = round(9284 / (9284 + 235928), 3)

heart %>%
  group_by(Race, Stroke) %>%
  summarise(n = n()) %>% ungroup() %>% filter(Stroke == "Yes") %>% 
  mutate(prop = ifelse(Race == "American Indian",stroke_am_ind,
                       ifelse(Race == "Asian", stroke_asian,
                              ifelse(Race == "Black", stroke_black,
                                     ifelse(Race == "Hispanic", stroke_hispanic,
                                            ifelse(Race =="Other", stroke_other,
                                                   ifelse(Race == "White", stroke_white, "NA"))))))) %>% 
  ggplot() + geom_bar(aes(Race, prop, fill = as.factor(Stroke)), stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Proportion") + xlab("Race") + ggtitle("Proportion of Stroke by Race")

```

# Summary

GIVE A 2 PARAGRAPH SUMMARY. 

Part 1

Looking at the general distribution of heart disease among the races, we notice that a vast majority (around 77%) of the sample are while people. Thus, we attempted to determine the effect of race on heart disease by looking at the mean proportion of heart disease. We observed that Native Americans are at highest risk of heart disease when compared to risks of the other races. Results showed that around 10.4% of Native Americans in the sample have heart disease, followed by White people at 9%. The race with the lowest risk of heart disease was Asians, at around 3%. Next, analyzing the effect of smoking on heart disease, we clearly see that smokers are almost twice as likely to get heart disease; 6% of non-smokers get heart disease, while 12% of smokers get heart disease. 

There does seem to be a noticeable difference between the mental health of alcohol drinkers and non-drinkers. Non-drinkers have overall better mental health in comparison to those who do drink, as is illustrated by the pink peaks (non-drinkers) above the blue color (drinkers). We also attempted to see the difference in alcohol drinking and smoking between the sexes. Results showed that there does not seem to be a difference in alcohol drinking,  as around 6.92% of males drink alcohol and around 6.71% of females drink alcohol. There is a larger difference between the sexes in regard to smoking, as around 45.6% of males smoke and around 37.3% of females smoke. 

Analyzing the effect of BMI on heart disease, we observed that when an individual is in the normal BMI range (~25), they are at the lowest risk of  heart disease, while both underweight and overweight individuals have a higher probability of heart disease. We noticed that as BMI continues to rise, the probability of heart disease also increases. Then, we proceeded to test if there is a relationship between age and BMI, but concluded that there is no significant relationship between the two predictors. We noticed that for men, BMI increases slightly with age and for women, BMI decreases slightly with age. 

Next, we decided to examine the effect of age on heart disease; there is a clear positive relationship between age and probability of heart disease. The age category with the highest proportion of individuals who have heart disease is the “80 or older” age category, as expected. Though this trend continues throughout all age categories, we observed that the age category of “75-79 years” is not at as high risk of heart disease as “70-74 years” and “80 or older” age categories. Perhaps this interesting result prompts further analysis. We also decided to investigate the effect of general health on heart disease.  Results showed that those who reported their general health as “Excellent” have the lowest probability of heart disease while those who reported their general health as “Poor” have the highest chance of heart disease. Indeed, it is interesting to observe that an individual’s general health level corresponds very well to their probability of heart disease. There also does not seem to be any significant difference in sleep time between the sexes, with a mere 0.045 hour (2.7 minute) difference; mean sleep times for both sexes were around 7 hours. 

Part 2

After analyzing the initial question regarding the relationship between smoking and heart disease, where we found that smokers are twice as likely as non-smokers to develop heart disease, we were interested in seeing which other factors contribute most to developing heart disease. The factors we analyzed were: `Smoking`, `AlcoholDrinking`, `Stroke`, `Asthma`, `PhysicallyActive`, `KidneyDisease`, and `SkinCancer`. Results showed that the most important factor is `Stroke`, where those who have had a stroke are about 4.87 times more likely to have heart disease than those who have not had a stroke. The next most important factor was `KidneyDisease`, where those who have kidney disease are about 3.78 times more likely to have heart disease than those who do not have kidney disease. 

After analyzing the mean proportion of heart disease by race, we found that American Indians/Alaskan Natives are at the highest risk of heart disease, with a probability of about 10.4%. As stated before, we saw that stroke was the best predictor for heart disease; as such, we wanted to see if indeed that race  has the highest probability of stroke. Results confirmed this prediction, as American Indian/Alaskan Native people had around a 5.9% chance of stroke, then followed by Black people, who had around a 5.4% chance of stroke. These results make sense, as Native American reservations are usually located in food deserts, making it extremely hard to access healthy and fresh food. Government funding for these reservations is low and access to proper health care is sparse, making for lower qualities of life and contributing to higher rates of chronic diseases.


```{r}

levels(heart$Diabetic)[match("No, borderline diabetes", levels(heart$Diabetic))] = 0
levels(heart$Diabetic)[match("Yes (during pregnancy)", levels(heart$Diabetic))] = 1
levels(heart$Diabetic)[match("Yes", levels(heart$Diabetic))] = 1
levels(heart$Diabetic)[match("No", levels(heart$Diabetic))] = 0


# Random Forest with Heart Disease

set.seed(100)
yes_heart <- filter(heart, HeartDisease =="Yes")
head(heart)
trainingRows <- sample(1:nrow(yes_heart), 0.8 * nrow(yes_heart))
train_yes <- yes_heart[trainingRows, ]
test_yes <- yes_heart[-trainingRows, ]
library(randomForest)
yes_heart_rf<-randomForest(Diabetic ~ KidneyDisease+ Asthma + Stroke + SkinCancer,data=train_yes)
yes_heart_rf

plot(yes_heart_rf$err.rate[,1],type="l",main="Random Forest Error Rate",xlab="Number of Trees", ylab = "Error Rate")
varImpPlot(yes_heart_rf,main="Variable Importance Plot for Random Forest")
yes_heart_rfpred<-predict(yes_heart_rf,test_yes,type="response")
yes_heart_rft<-table(test_yes$Diabetic,yes_heart_rfpred)
yes_heart_rft

yes_heart_rf.accuracy = (yes_heart_rft[1,1] + yes_heart_rft[2,2]) / (sum(yes_heart_rft))
yes_heart_rf.sensitivity <- yes_heart_rft[2,2] / ( yes_heart_rft[2,2]+ yes_heart_rft[2,1])
yes_heart_rf.specificity <- yes_heart_rft[1,1] / (yes_heart_rft[1,1] + yes_heart_rft[1,2])

yes_heart_rf.accuracy
yes_heart_rf.sensitivity
yes_heart_rf.specificity




# Random Forest without Heart Disease

set.seed(100)
no_heart <- filter(heart, HeartDisease =="No")
trainingRows <- sample(1:nrow(no_heart), 0.8 * nrow(no_heart))
train_no <- no_heart[trainingRows, ]
test_no <- no_heart[-trainingRows, ]
library(randomForest)
no_heart_rf<-randomForest(Diabetic ~ KidneyDisease+ Asthma + Stroke + SkinCancer,data=train_no)
no_heart_rf

plot(no_heart_rf$err.rate[,1],type="l",main="Random Forest Error Rate",xlab="Number of Trees", ylab = "Error Rate")
varImpPlot(no_heart_rf,main="Variable Importance Plot for Random Forest")
no_heart_rfpred<-predict(no_heart_rf,test_no,type="response")
no_heart_rft<-table(test_no$Diabetic,no_heart_rfpred)
no_heart_rft

no_heart_rf.accuracy = (48271 + 4253) / (48271 + 4253 + 2697 + 3264)
no_heart_rf.sensitivity <- 4253 / ( 2697 + 4253)
no_heart_rf.specificity <- 48271 / (48271 + 3264)

no_heart_rf.accuracy
no_heart_rf.sensitivity
no_heart_rf.specificity



# Random Forest with Full data
set.seed(100)
trainingRows <- sample(1:nrow(heart), 0.8 * nrow(heart))
train <- heart[trainingRows, ]
test <- heart[-trainingRows, ]
library(randomForest)
heart_rf<-randomForest(Diabetic ~ KidneyDisease+ Asthma + Stroke + SkinCancer,data=train)
heart_rf

plot(heart_rf$err.rate[,1],type="l",main="Random Forest Error Rate",xlab="Number of Trees", ylab = "Error Rate")
varImpPlot(heart_rf,main="Variable Importance Plot for Random Forest")
heart_rfpred<-predict(heart_rf,test,type="response")
heart_rft<-table(test$Diabetic,heart_rfpred)
heart_rft

heart_rf.accuracy = (55192 + 33) / (55192 + 33 + 37 + 8697)
heart_rf.sensitivity <- 33 / ( 33 + 8697)
heart_rf.specificity <- 55192 / (55192 + 37)

heart_rf.accuracy
heart_rf.sensitivity
heart_rf.specificity
```


```{r}
# Question 1 Logistic Regression w/ FULL DATA 
library(caret)

set.seed(100)
trainingRows <- sample(1:nrow(heart), 0.8 * nrow(heart))
train <- heart[trainingRows, ]
test <- heart[-trainingRows, ]

model = glm(Diabetic ~ Stroke + KidneyDisease  + Asthma + SkinCancer, data = train, family = binomial) 

probabilities <- model %>% predict(test, type = "response")

probabilities2 <- ifelse(probabilities > 0.5, "1", "0")
table<-table(test$HeartDisease,probabilities2)

table

log.accuracy = (table[1,1] + table[2,2]) / (sum(table))
log.sensitivity <- 222 / ( 222+ 5239)
log.specificity <- 58270 / (58270 + 228)

log.accuracy
log.sensitivity
log.specificity


# Question 1 Logistic Regression WITHOUT HEART DISEASE

no_heart <- filter(heart, HeartDisease =="No")
set.seed(100)
trainingRows <- sample(1:nrow(no_heart), 0.8 * nrow(no_heart))
train_no <- no_heart[trainingRows, ]
test_no <- no_heart[-trainingRows, ]

no_heart_model = glm(Diabetic ~  KidneyDisease+ Asthma + Stroke + SkinCancer, data = train_no, family = binomial) 

no_heart_probabilities <- no_heart_model %>% predict(test_no, type = "response")

no_heart_probabilities2 <- ifelse(no_heart_probabilities > 0.5, 1, 0)

no_heart_table<-table(test_no$Diabetic,no_heart_probabilities2)

no_heart_table

no_heart_log.accuracy = (no_heart_table[1,1] + no_heart_table[2,2]) / (sum(no_heart_table))
no_heart_log.sensitivity <- 6893 / ( 57+ 6893)
no_heart_log.specificity <- 51461 / (51461 + 74)

no_heart_log.accuracy
no_heart_log.sensitivity
no_heart_log.specificity


# WITH HEART DISEASE

yes_heart <- filter(heart, HeartDisease =="Yes")
set.seed(100)
trainingRows <- sample(1:nrow(yes_heart), 0.8 * nrow(yes_heart))
train_yes <- yes_heart[trainingRows, ]
test_yes <- yes_heart[-trainingRows, ]

model = glm(Diabetic ~  KidneyDisease+ Asthma + Stroke + SkinCancer, data = train_yes, family = binomial) 

yes_heart_probabilities <- model %>% predict(test_yes, type = "response")

yes_heart_probabilities2 <- ifelse(yes_heart_probabilities > 0.5, 1, 0)

yes_heart_table<-table(test_yes$Diabetic,yes_heart_probabilities2)

yes_heart_table


yes_heart_log.accuracy = (yes_heart_table[1,1] + yes_heart_table[2,2]) / (sum(yes_heart_table))
yes_heart_log.sensitivity <- 317 / ( 1512+ 317)
yes_heart_log.specificity <- 3374 / (3374 + 272)

yes_heart_log.accuracy
yes_heart_log.sensitivity
yes_heart_log.specificity
```


```{r}
# ROC curves for logistic regression
library(ROCR)

pred <- prediction(probabilities, test$Diabetic)
yes_heart_pred <- prediction(yes_heart_probabilities, test_yes$Diabetic)
no_heart_pred <- prediction(no_heart_probabilities, test_no$Diabetic)
perf <- performance(pred,"tpr","fpr", auc = TRUE)
yes_heart_perf <- performance(yes_heart_pred,"tpr","fpr", auc = TRUE)
no_heart_perf <- performance(no_heart_pred,"tpr","fpr", auc = TRUE)


plot(perf,colorize=FALSE, main = "Diabetes ROC")
plot(yes_heart_perf,colorize=FALSE, add = TRUE, lty='longdash')
plot(no_heart_perf,colorize=FALSE, add = TRUE, lty = 'twodash')
abline(coef = c(0,1), lty = 3)
legend(.6,.2, legend=c("Full Data", "With Heart Disease", "Without Heart Disease"),
        lty=c("solid", "longdash", "twodash"), cex=0.8)
```


```{r}
# VISUALS OF ACCURACY, SENSITIVITY, AND SPECIFICITY FOR Q1 AND Q2

library(kableExtra)
q1.error.results = tibble (
  Model = c("Random Forest Full Data","Random Forest Heart Disease","Random Forest Without Heart Disease",
            "Logistic Regression Full Data","Logistic Regression Heart Disease", "Logisic Regression Without Heart Disease"),
  Accuracy = c(heart_rf.accuracy, yes_heart_rf.accuracy, no_heart_rf.accuracy, log.accuracy, yes_heart_log.accuracy,
               no_heart_log.accuracy),
  Sensitivity = c(heart_rf.sensitivity,yes_heart_rf.sensitivity,no_heart_rf.sensitivity,
                  log.sensitivity, yes_heart_log.sensitivity, yes_heart_log.sensitivity),
  Specificity = c(heart_rf.specificity, yes_heart_rf.specificity, no_heart_rf.specificity,
                  log.specificity, yes_heart_log.specificity, no_heart_log.specificity)
)
q1.error.results %>%
  kbl() %>%
  kable_styling()
```


```{r}
q1.error.results_2 = tibble (
  Model = c("Naive Bayes Full Data","Naive Bayes Heart Disease","Naive Bayes Without Heart Disease",
            "Logistic Regression Full Data","Logistic Regression Heart Disease", "Logisic Regression Without Heart Disease"),
  Accuracy = c(confusionMatrix(test$GenHealth, y_pred_bayes)$overall[1],
               confusionMatrix(test_yes$GenHealth, yes_heart_y_pred_bayes)$overall[1],
               confusionMatrix(test_no$GenHealth, no_heart_y_pred_bayes)$overall[1],
               q2_full_heart_log.accuracy, q2_yes_heart_log.accuracy, q2_no_heart_log.accuracy)),
  Sensitivity = c(confusionMatrix(test$GenHealth, y_pred_bayes)$byClass[1],
                  confusionMatrix(test_yes$GenHealth, yes_heart_y_pred_bayes)$byClass[1],
                  confusionMatrix(test_no$GenHealth, no_heart_y_pred_bayes)$byClass[1],
                  q2_full_heart_log.sensitivity, q2_yes_heart_log.sensitivity, q2_no_heart_log.sensitivity),
  Specificity = c(confusionMatrix(test$GenHealth, y_pred_bayes)$byClass[2],
                  confusionMatrix(test_yes$GenHealth, yes_heart_y_pred_bayes)$byClass[2],
                  confusionMatrix(test_no$GenHealth, no_heart_y_pred_bayes)$byClass[2],
                  q2_full_heart_log.specificity, q2_yes_heart_log.specificity, q2_no_heart_log.specificity)
)

q2.error.results %>%
  kbl() %>%
  kable_styling()

```


```{r}
# Question 2 Naive Bayes w/ full data 

levels(heart$GenHealth)[match("Poor", levels(heart$GenHealth))] = "1"
levels(heart$GenHealth)[match("Fair", levels(heart$GenHealth))] = "1"
levels(heart$GenHealth)[match("Good", levels(heart$GenHealth))] = "1"
levels(heart$GenHealth)[match("Very good", levels(heart$GenHealth))] = "2"
levels(heart$GenHealth)[match("Excellent", levels(heart$GenHealth))] = "2"

set.seed(100)
trainingRows <- sample(1:nrow(heart), 0.8 * nrow(heart))
train <- heart[trainingRows, ]
test <- heart[-trainingRows, ]

library(naivebayes)
library(caret)
# Predicting the Test set results
model <- naive_bayes(GenHealth ~ Smoking + AlcoholDrinking + BMI, data = train, usekernel = T) 
y_pred_bayes = predict(model, newdata = test)

# Making the Confusion Matrix
confusionMatrix(test$GenHealth, y_pred_bayes)[2]  # confusion matrix 
confusionMatrix(test$GenHealth, y_pred_bayes)$overall[1] # accuracy
paste("Sensitivity",confusionMatrix(test$GenHealth, y_pred_bayes)$byClass[1]) # sensitivity
paste("Specificity",confusionMatrix(test$GenHealth, y_pred_bayes)$byClass[2]) # specificity


# Naive Bayes w/ Heart Disease
yes_heart <- filter(heart, HeartDisease =="Yes")
set.seed(100)
trainingRows <- sample(1:nrow(yes_heart), 0.8 * nrow(yes_heart))
train_yes <- yes_heart[trainingRows, ]
test_yes <- yes_heart[-trainingRows, ]

yes_heart_nb_model <- naive_bayes(GenHealth ~ Smoking + AlcoholDrinking + BMI, data = train_yes, usekernel = T) 
yes_heart_y_pred_bayes = predict(yes_heart_nb_model, newdata = test_yes)

# Making the Confusion Matrix
confusionMatrix(test_yes$GenHealth, yes_heart_y_pred_bayes)[2]  # confusion matrix 
confusionMatrix(test_yes$GenHealth, yes_heart_y_pred_bayes)$overall[1] # accuracy
paste("Sensitivity",confusionMatrix(test_yes$GenHealth, yes_heart_y_pred_bayes)$byClass[1]) # sensitivity
paste("Specificity",confusionMatrix(test_yes$GenHealth, yes_heart_y_pred_bayes)$byClass[2]) # specificity


# Naive Bayes w/o Heart Disease
no_heart <- filter(heart, HeartDisease =="No")
set.seed(100)
trainingRows <- sample(1:nrow(no_heart), 0.8 * nrow(no_heart))
train_no <- no_heart[trainingRows, ]
test_no <- no_heart[-trainingRows, ]

no_heart_nb_model <- naive_bayes(GenHealth ~ Smoking + AlcoholDrinking + BMI, data = train_no, usekernel = T) 
no_heart_y_pred_bayes = predict(no_heart_nb_model, newdata = test_no)

# Making the Confusion Matrix
confusionMatrix(test_no$GenHealth, no_heart_y_pred_bayes)[2]  # confusion matrix 
confusionMatrix(test_no$GenHealth, no_heart_y_pred_bayes)$overall[1] # accuracy
paste("Sensitivity",confusionMatrix(test_no$GenHealth, no_heart_y_pred_bayes)$byClass[1]) # sensitivity
paste("Specificity",confusionMatrix(test_no$GenHealth, no_heart_y_pred_bayes)$byClass[2]) # specificity
```



```{r}
# Q2 Logistic Regression w/ Full Data

levels(heart$GenHealth)[match("Poor", levels(heart$GenHealth))] = "1"
levels(heart$GenHealth)[match("Fair", levels(heart$GenHealth))] = "1"
levels(heart$GenHealth)[match("Good", levels(heart$GenHealth))] = "1"
levels(heart$GenHealth)[match("Very good", levels(heart$GenHealth))] = "2"
levels(heart$GenHealth)[match("Excellent", levels(heart$GenHealth))] = "2"


# Question 1 Logistic Regression WITHOUT HEART DISEASE

no_heart <- filter(heart, HeartDisease =="No")
set.seed(100)
trainingRows <- sample(1:nrow(no_heart), 0.8 * nrow(no_heart))
train_no <- no_heart[trainingRows, ]
test_no <- no_heart[-trainingRows, ]

q2_no_heart_model = glm(GenHealth ~ Smoking + AlcoholDrinking + BMI, data = train_no, family = binomial) 

q2_no_heart_probabilities <- q2_no_heart_model %>% predict(test_no, type = "response")

q2_no_heart_probabilities2 <- ifelse(q2_no_heart_probabilities > 0.5, 1, 0)

q2_no_heart_table<-table(test_no$Diabetic,q2_no_heart_probabilities2)

q2_no_heart_table

q2_no_heart_log.accuracy = (q2_no_heart_table[1,1] + q2_no_heart_table[2,2]) / (sum(q2_no_heart_table))
q2_no_heart_log.sensitivity <- 2638 / ( 2638+ 4312)
q2_no_heart_log.specificity <- 42137 / (42137 + 9398)

q2_no_heart_log.accuracy
q2_no_heart_log.sensitivity
q2_no_heart_log.specificity


# Question 1 Logistic Regression WITH HEART DISEASE

yes_heart <- filter(heart, HeartDisease =="Yes")
set.seed(100)
trainingRows <- sample(1:nrow(yes_heart), 0.8 * nrow(yes_heart))
train_yes <- yes_heart[trainingRows, ]
test_yes <- yes_heart[-trainingRows, ]

q2_yes_heart_model = glm(GenHealth ~ Smoking + AlcoholDrinking + BMI, data = train_yes, family = binomial) 

q2_yes_heart_probabilities <- q2_yes_heart_model %>% predict(test_yes, type = "response")

q2_yes_heart_probabilities2 <- ifelse(q2_yes_heart_probabilities > 0.5, 1, 0)

q2_yes_heart_table<-table(test_yes$Diabetic, q2_yes_heart_probabilities2)

q2_yes_heart_table

q2_yes_heart_log.accuracy = (q2_yes_heart_table[1,1] + q2_yes_heart_table[2,2]) / (sum(q2_yes_heart_table))
q2_yes_heart_log.sensitivity <- 1 / ( 1+ 3645)
q2_yes_heart_log.specificity <- 1827 / (1827 + 2)

q2_yes_heart_log.accuracy
q2_yes_heart_log.sensitivity
q2_yes_heart_log.specificity


# Question 1 Logistic Regression WITH FULL DATA

trainingRows <- sample(1:nrow(yes_heart), 0.8 * nrow(yes_heart))
train <- heart[trainingRows, ]
test <- heart[-trainingRows, ]

q2_full_heart_model = glm(GenHealth ~ Smoking + AlcoholDrinking + BMI, data = train, family = binomial) 

q2_full_heart_probabilities <- q2_full_heart_model %>% predict(test, type = "response")

q2_full_heart_probabilities2 <- ifelse(q2_full_heart_probabilities > 0.5, 1, 0)

q2_full_heart_table<-table(test$Diabetic, q2_full_heart_probabilities2)

q2_full_heart_table

q2_full_heart_log.accuracy = (q2_full_heart_table[1,1] + q2_full_heart_table[2,2]) / (sum(q2_full_heart_table))
q2_full_heart_log.sensitivity <- 22635 / ( 22635+ 17722)
q2_full_heart_log.specificity <- 174993 / (174993 + 82547)

q2_full_heart_log.accuracy
q2_full_heart_log.sensitivity
q2_full_heart_log.specificity
```


