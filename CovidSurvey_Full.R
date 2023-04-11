############################################
###  International Mudslinging in Vain   ###
###       Jia Li and Rosemary Pang       ###
###          Last Modified Date:         ###
###             April 7, 2023            ###
###   Replication code for All Tables    ###
###               and Graphs             ###
############################################

## ================================= Set Working Environment =================================

# set working directory
setwd("/Users/mpang/Dropbox/Research/COVID-19 Student Survey/Replication Package")

# load packages
library(stargazer)
library(ggplot2)
library(psych)
library(dplyr)
library("ggpubr")
library(corrplot)
library(FactoMineR)
library(factoextra)
library(scales)

## ================================= Reading Wave 1 Data =================================
# read in data
Survey_Wave1 <- read.csv('Wave1.csv', header = TRUE, sep = ",", quote = "\"",
                         dec = ".", fill = TRUE, comment.char = "")
# To skip data cleaning process:
#Survey_Wave1_full <- read.csv('Survey_Wave1_full.csv', header = TRUE, sep = ",", quote = "\"",
#                        dec = ".", fill = TRUE, comment.char = "")

# reorder treatment groups
Survey_Wave1$group <- factor(Survey_Wave1$group, levels = c("control", "positive", "negative", "additive"))


## ================================= Reading Wave 2 Data =================================
# read in data
Survey_Wave2 <- read.csv('Wave2.csv', header = TRUE, sep = ",", quote = "\"",
                         dec = ".", fill = TRUE, comment.char = "")

# To skip data cleaning process:
#Survey_Wave2_full <- read.csv('Survey_Wave2_full.csv', header = TRUE, sep = ",", quote = "\"",
#                        dec = ".", fill = TRUE, comment.char = "")


# reorder treatment groups (baseline is control group)
Survey_Wave2$group <- factor(Survey_Wave2$group, levels = c("control", "positive", "negative", "additive"))


## ================================= Recode Wave 1 Variables =================================

#Outcome variables: China overall and Chinese government
Survey_Wave1$china_overall_num <- NA
Survey_Wave1[which(Survey_Wave1$china_overall=="不满意"),"china_overall_num"] <- 1
Survey_Wave1[which(Survey_Wave1$china_overall=="比较不满意"),"china_overall_num"] <- 2
Survey_Wave1[which(Survey_Wave1$china_overall=="一般"),"china_overall_num"] <- 3
Survey_Wave1[which(Survey_Wave1$china_overall=="比较满意"),"china_overall_num"] <- 4
Survey_Wave1[which(Survey_Wave1$china_overall=="满意"),"china_overall_num"] <- 5
table(Survey_Wave1$china_overall_num)


Survey_Wave1$china_gov_num <- NA
Survey_Wave1[which(Survey_Wave1$china_gov=="强烈反对"),"china_gov_num"] <- 1
Survey_Wave1[which(Survey_Wave1$china_gov=="反对"),"china_gov_num"] <- 2
Survey_Wave1[which(Survey_Wave1$china_gov=="既不同意也不反对"),"china_gov_num"] <- 3
Survey_Wave1[which(Survey_Wave1$china_gov=="同意"),"china_gov_num"] <- 4
Survey_Wave1[which(Survey_Wave1$china_gov=="强烈同意"),"china_gov_num"] <- 5
table(Survey_Wave1$china_gov_num)

# Covid severity assessment
# Nationwide
Survey_Wave1$covid_assess_1_num <- NA
Survey_Wave1[which(Survey_Wave1$covid_assess_1=="并不严重"),"covid_assess_1_num"] <- 1
Survey_Wave1[which(Survey_Wave1$covid_assess_1=="不怎么严重"),"covid_assess_1_num"] <- 2
Survey_Wave1[which(Survey_Wave1$covid_assess_1=="一般"),"covid_assess_1_num"] <- 3
Survey_Wave1[which(Survey_Wave1$covid_assess_1=="比较严重"),"covid_assess_1_num"] <- 4
Survey_Wave1[which(Survey_Wave1$covid_assess_1=="非常严重"),"covid_assess_1_num"] <- 5
table(Survey_Wave1$covid_assess_1_num)

# State
Survey_Wave1$covid_assess_2_num <- NA
Survey_Wave1[which(Survey_Wave1$covid_assess_2=="并不严重"),"covid_assess_2_num"] <- 1
Survey_Wave1[which(Survey_Wave1$covid_assess_2=="不怎么严重"),"covid_assess_2_num"] <- 2
Survey_Wave1[which(Survey_Wave1$covid_assess_2=="一般"),"covid_assess_2_num"] <- 3
Survey_Wave1[which(Survey_Wave1$covid_assess_2=="比较严重"),"covid_assess_2_num"] <- 4
Survey_Wave1[which(Survey_Wave1$covid_assess_2=="非常严重"),"covid_assess_2_num"] <- 5
table(Survey_Wave1$covid_assess_2_num)

# County
Survey_Wave1$covid_assess_3_num <- NA
Survey_Wave1[which(Survey_Wave1$covid_assess_3=="并不严重"),"covid_assess_3_num"] <- 1
Survey_Wave1[which(Survey_Wave1$covid_assess_3=="不怎么严重"),"covid_assess_3_num"] <- 2
Survey_Wave1[which(Survey_Wave1$covid_assess_3=="一般"),"covid_assess_3_num"] <- 3
Survey_Wave1[which(Survey_Wave1$covid_assess_3=="比较严重"),"covid_assess_3_num"] <- 4
Survey_Wave1[which(Survey_Wave1$covid_assess_3=="非常严重"),"covid_assess_3_num"] <- 5
table(Survey_Wave1$covid_assess_3_num)

## PCA to combine Covid severity assessment 
pca_COVIDassess.df <- 
  Survey_Wave1[,c("covid_assess_1_num", "covid_assess_2_num", "covid_assess_3_num")]
names(pca_COVIDassess.df) <- c("Nationwide", "State", "County")
colnames(pca_COVIDassess.df)
pca_COVIDassess <- PCA(pca_COVIDassess.df, scale.unit = TRUE, ncp = 12, graph = FALSE)

# put pc1 from pca_assess into data
pca_COVIDassess.df <- cbind(pca_COVIDassess.df, pca_COVIDassess$ind$coord[,1])
names(pca_COVIDassess.df) <- c("Nationwide", "State", "County", "U.S. Severity PCA")
Survey_Wave1$COVIDassess_pca <- pca_COVIDassess$ind$coord[,1]
Survey_Wave1$COVIDassess_pca <- rescale(Survey_Wave1$COVIDassess_pca, to = c(1, 5))
# higher number means the subject thinks COVID is more serious

# sex
Survey_Wave1$female <- NA
Survey_Wave1[which(Survey_Wave1$sex=="女"),"female"] <- 1
Survey_Wave1[which(Survey_Wave1$sex=="男"),"female"] <- 0
table(Survey_Wave1$female)
# 163 females, 204 males

# education
Survey_Wave1$education_gra <- NA
Survey_Wave1[which(Survey_Wave1$education=="博士后"),"education_gra"] <- 1
Survey_Wave1[which(Survey_Wave1$education=="博士生在读"),"education_gra"] <- 1
Survey_Wave1[which(Survey_Wave1$education=="研究生在读"),"education_gra"] <- 1
Survey_Wave1[which(Survey_Wave1$education=="本科在读"),"education_gra"] <- 0
table(Survey_Wave1$education_gra)
# 128 subjects currently in graduate school or higher

# education disciplines
Survey_Wave1$STEM <- 1
Survey_Wave1[which(Survey_Wave1$edu_subject=="人文学科及艺术"),"STEM"] <- 0
Survey_Wave1[which(Survey_Wave1$edu_subject=="社会科学及法学"),"STEM"] <- 0
Survey_Wave1[which(Survey_Wave1$edu_subject=="商科"),"STEM"] <- 0
Survey_Wave1[grep("商科", Survey_Wave1$edu_subject_6_TEXT), "STEM"] <- 0
Survey_Wave1[grep("教育", Survey_Wave1$edu_subject_6_TEXT), "STEM"] <- 0
Survey_Wave1[grep("Film", Survey_Wave1$edu_subject_6_TEXT), "STEM"] <- 0
Survey_Wave1[grep("传媒", Survey_Wave1$edu_subject_6_TEXT), "STEM"] <- 0
Survey_Wave1[grep("Hospitality Management", Survey_Wave1$edu_subject_6_TEXT), "STEM"] <- 0
table(Survey_Wave1$STEM)
# 199 subjects in STEM disciplines 


# family ties to the regime
Survey_Wave1$family_regime_tie <- 0
Survey_Wave1[grep("党和国家干部", Survey_Wave1$family), "family_regime_tie"] <- 1
Survey_Wave1[grep("国有企业经理", Survey_Wave1$family), "family_regime_tie"] <- 1
Survey_Wave1[grep("基层公务员", Survey_Wave1$family), "family_regime_tie"] <- 1
table(Survey_Wave1$family_regime_tie)
# 131 subjects at least one parent work as Chinese government employee


# Ideology
Survey_Wave1$ideology_1_num <- NA
Survey_Wave1[which(Survey_Wave1$ideology_1=="强烈反对"),"ideology_1_num"] <- 1
Survey_Wave1[which(Survey_Wave1$ideology_1=="反对"),"ideology_1_num"] <- 2
Survey_Wave1[which(Survey_Wave1$ideology_1=="同意"),"ideology_1_num"] <- 3
Survey_Wave1[which(Survey_Wave1$ideology_1=="强烈同意"),"ideology_1_num"] <- 4
table(Survey_Wave1$ideology_1_num)

Survey_Wave1$ideology_2_num <- NA
Survey_Wave1[which(Survey_Wave1$ideology_2=="强烈反对"),"ideology_2_num"] <- 1
Survey_Wave1[which(Survey_Wave1$ideology_2=="反对"),"ideology_2_num"] <- 2
Survey_Wave1[which(Survey_Wave1$ideology_2=="同意"),"ideology_2_num"] <- 3
Survey_Wave1[which(Survey_Wave1$ideology_2=="强烈同意"),"ideology_2_num"] <- 4
table(Survey_Wave1$ideology_2_num)

Survey_Wave1$ideology_3_num <- NA
Survey_Wave1[which(Survey_Wave1$ideology_3=="强烈反对"),"ideology_3_num"] <- 1
Survey_Wave1[which(Survey_Wave1$ideology_3=="反对"),"ideology_3_num"] <- 2
Survey_Wave1[which(Survey_Wave1$ideology_3=="同意"),"ideology_3_num"] <- 3
Survey_Wave1[which(Survey_Wave1$ideology_3=="强烈同意"),"ideology_3_num"] <- 4
table(Survey_Wave1$ideology_3_num)

Survey_Wave1$ideology_4_num <- NA
Survey_Wave1[which(Survey_Wave1$ideology_4=="强烈反对"),"ideology_4_num"] <- 1
Survey_Wave1[which(Survey_Wave1$ideology_4=="反对"),"ideology_4_num"] <- 2
Survey_Wave1[which(Survey_Wave1$ideology_4=="同意"),"ideology_4_num"] <- 3
Survey_Wave1[which(Survey_Wave1$ideology_4=="强烈同意"),"ideology_4_num"] <- 4
table(Survey_Wave1$ideology_4_num)

Survey_Wave1$ideology_5_num <- NA
Survey_Wave1[which(Survey_Wave1$ideology_5=="强烈反对"),"ideology_5_num"] <- 1
Survey_Wave1[which(Survey_Wave1$ideology_5=="反对"),"ideology_5_num"] <- 2
Survey_Wave1[which(Survey_Wave1$ideology_5=="同意"),"ideology_5_num"] <- 3
Survey_Wave1[which(Survey_Wave1$ideology_5=="强烈同意"),"ideology_5_num"] <- 4
table(Survey_Wave1$ideology_5_num)

# generate PCA of ideology
pca_ideology.df <- 
  Survey_Wave1[,c("ideology_1_num", "ideology_2_num", "ideology_3_num", "ideology_4_num", "ideology_5_num")]
names(pca_ideology.df) <- c("Open Info.", "Human Rights", "Uni. Suffrage", "Free Speech", "Multiparty")
pca_ideology <- PCA(pca_ideology.df, scale.unit = TRUE, ncp = 12, graph = FALSE)

# put pc1 from pca_ideology into data
pca_ideology.df <- cbind(pca_ideology.df1, pca_ideology$ind$coord[,1])
names(pca_ideology.df) <- c("Open Info.", "Human Rights", 
                            "Uni. Suffrage", "Free Speech", 
                            "Multiparty", "Ideology PCA")
Survey_Wave1$ideology_pca <- pca_ideology$ind$coord[,1]
Survey_Wave1$ideology_pca <- rescale(Survey_Wave1$ideology_pca, to = c(1, 4))


# US socialization
Survey_Wave1$socialized_US <- NA
Survey_Wave1[which(Survey_Wave1$education=="博士后" & Survey_Wave1$us_edu=="初中或更早"),"socialized_US"] <- 1
Survey_Wave1[which(Survey_Wave1$education=="博士生在读"),"socialized_US"] <- 1
Survey_Wave1[which(Survey_Wave1$education=="博士生在读" & Survey_Wave1$us_edu=="硕士/博士研究生或以上"),"socialized_US"] <- 0
Survey_Wave1[which(Survey_Wave1$education=="本科在读"),"socialized_US"] <- 1
Survey_Wave1[which(Survey_Wave1$education=="本科在读" & Survey_Wave1$us_edu=="本科（含预科）"),"socialized_US"] <- 0
Survey_Wave1[which(Survey_Wave1$education=="研究生在读"),"socialized_US"] <- 1
Survey_Wave1[which(Survey_Wave1$education=="研究生在读" & Survey_Wave1$us_edu=="硕士/博士研究生或以上"),"socialized_US"] <- 0
table(Survey_Wave1$socialized_US)
# 161 subjects "socialized" in US


# income in US (from inside or outside the campus)
Survey_Wave1$income_us <- 0
Survey_Wave1[grep("其他形式收入", Survey_Wave1$income), "income_us"] <- 1
Survey_Wave1[grep("校外工作", Survey_Wave1$income), "income_us"] <- 1
Survey_Wave1[grep("校内工作", Survey_Wave1$income), "income_us"] <- 1
table(Survey_Wave1$income_us)
# 180 subjects with some sort of income from the US

# racial discrimination
Survey_Wave1$race_experience <- 0
Survey_Wave1[which(Survey_Wave1$race_disc=="我遇到过"),"race_experience"] <- 1
table(Survey_Wave1$race_experience)
# 155 subjects have personal experience of racial discrimination

# knows someone infected with COVID-19 in the USA
Survey_Wave1$know_pos <- 0
Survey_Wave1[which(Survey_Wave1$Q48=="有，在我认识的人当中，有人在美国被感染"),"know_pos"] <- 1
Survey_Wave1[which(Survey_Wave1$Q48=="有，在中国和美国都有我认识的人被感染"),"know_pos"] <- 1
table(Survey_Wave1$know_pos)
# 62 subjects know someone infected in the USA

# health kit
Survey_Wave1$kit_got_2 <- 0
Survey_Wave1[which(Survey_Wave1$kit_got=="是"),"kit_got_2"] <- 1
table(Survey_Wave1$kit_got_2)
# 224 subjects already got health kits
Survey_Wave1$kit_timely_2 <- 0
Survey_Wave1[which(Survey_Wave1$kit_timely=="及时"),"kit_timely_2"] <- 1
table(Survey_Wave1$kit_timely_2)
# 188 subjects think health kits arrive timely

# return to China
Survey_Wave1$return_china <- NA
Survey_Wave1[which(Survey_Wave1$return=="已经或曾经回国"),"return_china"] <- 1
Survey_Wave1[which(Survey_Wave1$return=="未曾回国"),"return_china"] <- 0
table(Survey_Wave1$return_china)
# 89 have been back to China, 278 did not



## ================================= Recode Wave 2 Variables =================================
# return to China
Survey_Wave2$return_china <- NA
Survey_Wave2[which(Survey_Wave2$return=="已经或曾经回国"),"return_china"] <- 1
Survey_Wave2[which(Survey_Wave2$return=="未曾回国"),"return_china"] <- 0
table(Survey_Wave2$return_china)
# 104 have been back to China, 677 did not, 21 has never been to the U.S.
# The 21 respondents who has never been to the U.S. are dropped from the sample
Survey_Wave2 <- Survey_Wave2 %>% 
  filter(!is.na(return_china))

#Outcome variables: China overall and Chinese government
Survey_Wave2$china_overall_num <- NA
Survey_Wave2[which(Survey_Wave2$china_overall=="不满意"),"china_overall_num"] <- 1
Survey_Wave2[which(Survey_Wave2$china_overall=="比较不满意"),"china_overall_num"] <- 2
Survey_Wave2[which(Survey_Wave2$china_overall=="一般"),"china_overall_num"] <- 3
Survey_Wave2[which(Survey_Wave2$china_overall=="比较满意"),"china_overall_num"] <- 4
Survey_Wave2[which(Survey_Wave2$china_overall=="满意"),"china_overall_num"] <- 5
table(Survey_Wave2$china_overall_num)

Survey_Wave2$china_gov_num <- NA
Survey_Wave2[which(Survey_Wave2$china_gov=="强烈反对"),"china_gov_num"] <- 1
Survey_Wave2[which(Survey_Wave2$china_gov=="反对"),"china_gov_num"] <- 2
Survey_Wave2[which(Survey_Wave2$china_gov=="既不同意也不反对"),"china_gov_num"] <- 3
Survey_Wave2[which(Survey_Wave2$china_gov=="同意"),"china_gov_num"] <- 4
Survey_Wave2[which(Survey_Wave2$china_gov=="强烈同意"),"china_gov_num"] <- 5
table(Survey_Wave2$china_gov_num)

# Covid severity assessment
# Nationwide
Survey_Wave2$covid_assess_1_num <- NA
Survey_Wave2[which(Survey_Wave2$covid_assess_1=="并不严重"),"covid_assess_1_num"] <- 1
Survey_Wave2[which(Survey_Wave2$covid_assess_1=="不怎么严重"),"covid_assess_1_num"] <- 2
Survey_Wave2[which(Survey_Wave2$covid_assess_1=="一般"),"covid_assess_1_num"] <- 3
Survey_Wave2[which(Survey_Wave2$covid_assess_1=="比较严重"),"covid_assess_1_num"] <- 4
Survey_Wave2[which(Survey_Wave2$covid_assess_1=="非常严重"),"covid_assess_1_num"] <- 5
table(Survey_Wave2$covid_assess_1_num)

# State
Survey_Wave2$covid_assess_2_num <- NA
Survey_Wave2[which(Survey_Wave2$covid_assess_2=="并不严重"),"covid_assess_2_num"] <- 1
Survey_Wave2[which(Survey_Wave2$covid_assess_2=="不怎么严重"),"covid_assess_2_num"] <- 2
Survey_Wave2[which(Survey_Wave2$covid_assess_2=="一般"),"covid_assess_2_num"] <- 3
Survey_Wave2[which(Survey_Wave2$covid_assess_2=="比较严重"),"covid_assess_2_num"] <- 4
Survey_Wave2[which(Survey_Wave2$covid_assess_2=="非常严重"),"covid_assess_2_num"] <- 5
table(Survey_Wave2$covid_assess_2_num)

# County
Survey_Wave2$covid_assess_3_num <- NA
Survey_Wave2[which(Survey_Wave2$covid_assess_3=="并不严重"),"covid_assess_3_num"] <- 1
Survey_Wave2[which(Survey_Wave2$covid_assess_3=="不怎么严重"),"covid_assess_3_num"] <- 2
Survey_Wave2[which(Survey_Wave2$covid_assess_3=="一般"),"covid_assess_3_num"] <- 3
Survey_Wave2[which(Survey_Wave2$covid_assess_3=="比较严重"),"covid_assess_3_num"] <- 4
Survey_Wave2[which(Survey_Wave2$covid_assess_3=="非常严重"),"covid_assess_3_num"] <- 5
table(Survey_Wave2$covid_assess_3_num)

## PCA to combine Covid severity assessment 
pca_COVIDassess.df <- 
  Survey_Wave2[,c("covid_assess_1_num", "covid_assess_2_num", "covid_assess_3_num")]
names(pca_COVIDassess.df) <- c("Nationwide", "State", "County")
colnames(pca_COVIDassess.df)
pca_COVIDassess <- PCA(pca_COVIDassess.df, scale.unit = TRUE, ncp = 12, graph = FALSE)

# put pc1 from pca_assess into data
pca_COVIDassess.df <- cbind(pca_COVIDassess.df, pca_COVIDassess$ind$coord[,1])
names(pca_COVIDassess.df) <- c("Nationwide", "State", "County", "U.S. Severity PCA")
Survey_Wave2$COVIDassess_pca <- pca_COVIDassess$ind$coord[,1]
Survey_Wave2$COVIDassess_pca <- rescale(Survey_Wave2$COVIDassess_pca, to = c(1, 5))
# higher number means the subject thinks COVID is more serious

# sex
Survey_Wave2$female <- NA
Survey_Wave2[which(Survey_Wave2$sex=="女"),"female"] <- 1
Survey_Wave2[which(Survey_Wave2$sex=="其他"),"female"] <- 0
Survey_Wave2[which(Survey_Wave2$sex=="男"),"female"] <- 0
table(Survey_Wave2$female)
# 427 females, 354 males and other

# education
Survey_Wave2$education_gra <- NA
Survey_Wave2[which(Survey_Wave2$education=="博士后"),"education_gra"] <- 1
Survey_Wave2[which(Survey_Wave2$education=="博士生在读"),"education_gra"] <- 1
Survey_Wave2[which(Survey_Wave2$education=="研究生在读"),"education_gra"] <- 1
Survey_Wave2[which(Survey_Wave2$education=="本科在读"),"education_gra"] <- 0
table(Survey_Wave2$education_gra)
# 648 subjects currently in graduate school or higher

# education disciplines
Survey_Wave2$STEM <- 1
Survey_Wave2[which(Survey_Wave2$edu_subject=="人文学科及艺术"),"STEM"] <- 0
Survey_Wave2[which(Survey_Wave2$edu_subject=="社会科学及法学"),"STEM"] <- 0
Survey_Wave2[which(Survey_Wave2$edu_subject=="商科"),"STEM"] <- 0
Survey_Wave2[which(Survey_Wave2$edu_subject=="其他"),"STEM"] <- 0
table(Survey_Wave2$STEM)
# 466 subjects in STEM disciplines 

# family ties to the regime
Survey_Wave2$family_regime_tie <- 0
Survey_Wave2[grep("党和国家干部", Survey_Wave2$family), "family_regime_tie"] <- 1
Survey_Wave2[grep("国有企业经理", Survey_Wave2$family), "family_regime_tie"] <- 1
Survey_Wave2[grep("基层公务员", Survey_Wave2$family), "family_regime_tie"] <- 1
table(Survey_Wave2$family_regime_tie)
# 285 subjects at least one parent work as Chinese government employee

# Ideology
Survey_Wave2$ideology_1_num <- NA
Survey_Wave2[which(Survey_Wave2$ideology_1=="强烈反对"),"ideology_1_num"] <- 1
Survey_Wave2[which(Survey_Wave2$ideology_1=="反对"),"ideology_1_num"] <- 2
Survey_Wave2[which(Survey_Wave2$ideology_1=="同意"),"ideology_1_num"] <- 3
Survey_Wave2[which(Survey_Wave2$ideology_1=="强烈同意"),"ideology_1_num"] <- 4
table(Survey_Wave2$ideology_1_num)

Survey_Wave2$ideology_2_num <- NA
Survey_Wave2[which(Survey_Wave2$ideology_2=="强烈反对"),"ideology_2_num"] <- 1
Survey_Wave2[which(Survey_Wave2$ideology_2=="反对"),"ideology_2_num"] <- 2
Survey_Wave2[which(Survey_Wave2$ideology_2=="同意"),"ideology_2_num"] <- 3
Survey_Wave2[which(Survey_Wave2$ideology_2=="强烈同意"),"ideology_2_num"] <- 4
table(Survey_Wave2$ideology_2_num)

Survey_Wave2$ideology_3_num <- NA
Survey_Wave2[which(Survey_Wave2$ideology_3=="强烈反对"),"ideology_3_num"] <- 1
Survey_Wave2[which(Survey_Wave2$ideology_3=="反对"),"ideology_3_num"] <- 2
Survey_Wave2[which(Survey_Wave2$ideology_3=="同意"),"ideology_3_num"] <- 3
Survey_Wave2[which(Survey_Wave2$ideology_3=="强烈同意"),"ideology_3_num"] <- 4
table(Survey_Wave2$ideology_3_num)

Survey_Wave2$ideology_4_num <- NA
Survey_Wave2[which(Survey_Wave2$ideology_4=="强烈反对"),"ideology_4_num"] <- 1
Survey_Wave2[which(Survey_Wave2$ideology_4=="反对"),"ideology_4_num"] <- 2
Survey_Wave2[which(Survey_Wave2$ideology_4=="同意"),"ideology_4_num"] <- 3
Survey_Wave2[which(Survey_Wave2$ideology_4=="强烈同意"),"ideology_4_num"] <- 4
table(Survey_Wave2$ideology_4_num)

Survey_Wave2$ideology_5_num <- NA
Survey_Wave2[which(Survey_Wave2$ideology_5=="强烈反对"),"ideology_5_num"] <- 1
Survey_Wave2[which(Survey_Wave2$ideology_5=="反对"),"ideology_5_num"] <- 2
Survey_Wave2[which(Survey_Wave2$ideology_5=="同意"),"ideology_5_num"] <- 3
Survey_Wave2[which(Survey_Wave2$ideology_5=="强烈同意"),"ideology_5_num"] <- 4
table(Survey_Wave2$ideology_5_num)

# generate PCA of ideology
pca_ideology.df <- 
  Survey_Wave2[,c("ideology_1_num", "ideology_2_num", "ideology_3_num", "ideology_4_num", "ideology_5_num")]
names(pca_ideology.df) <- c("Open Info.", "Human Rights", "Uni. Suffrage", "Free Speech", "Multiparty")
pca_ideology <- PCA(pca_ideology.df, scale.unit = TRUE, ncp = 12, graph = FALSE)

# put pc1 from pca_ideology into data
pca_ideology.df <- cbind(pca_ideology.df, pca_ideology$ind$coord[,1])
names(pca_ideology.df) <- c("Open Info.", "Human Rights", 
                            "Uni. Suffrage", "Free Speech", 
                            "Multiparty", "Ideology PCA")
Survey_Wave2$ideology_pca <- pca_ideology$ind$coord[,1]
Survey_Wave2$ideology_pca <- rescale(Survey_Wave2$ideology_pca, to = c(1, 4))


# US socialization
Survey_Wave2$socialized_US <- NA
Survey_Wave2[which(Survey_Wave2$education=="博士后"),"socialized_US"] <- 1
Survey_Wave2[which(Survey_Wave2$education=="博士后" & Survey_Wave2$us_edu=="硕士/博士研究生或以上"),"socialized_US"] <- 0
Survey_Wave2[which(Survey_Wave2$education=="博士生在读"),"socialized_US"] <- 1
Survey_Wave2[which(Survey_Wave2$education=="博士生在读" & Survey_Wave2$us_edu=="硕士/博士研究生或以上"),"socialized_US"] <- 0
Survey_Wave2[which(Survey_Wave2$education=="本科在读"),"socialized_US"] <- 1
Survey_Wave2[which(Survey_Wave2$education=="本科在读" & Survey_Wave2$us_edu=="本科（含预科）"),"socialized_US"] <- 0
Survey_Wave2[which(Survey_Wave2$education=="本科在读" & Survey_Wave2$us_edu=="硕士/博士研究生或以上"),"socialized_US"] <- 0
Survey_Wave2[which(Survey_Wave2$education=="研究生在读"),"socialized_US"] <- 1
Survey_Wave2[which(Survey_Wave2$education=="研究生在读" & Survey_Wave2$us_edu=="硕士/博士研究生或以上"),"socialized_US"] <- 0
table(Survey_Wave2$socialized_US)
# 239 subjects "socialized" in US


# income in US (from inside or outside the campus)
Survey_Wave2$income_us <- 0
Survey_Wave2[grep("其他形式收入", Survey_Wave2$income), "income_us"] <- 1
Survey_Wave2[grep("校外工作", Survey_Wave2$income), "income_us"] <- 1
Survey_Wave2[grep("校内工作", Survey_Wave2$income), "income_us"] <- 1
table(Survey_Wave2$income_us)
# 631 subjects with some sort of income from the US


# racial discrimination
Survey_Wave2$race_experience <- 0
Survey_Wave2[which(Survey_Wave2$race_disc=="我遇到过"),"race_experience"] <- 1
table(Survey_Wave2$race_experience)
# 273 subjects have personal experience of racial discrimination


# knows someone infected with COVID-19 in the USA
Survey_Wave2$know_pos <- 0
Survey_Wave2[which(Survey_Wave2$Q48=="有，在我认识的人当中，有人在美国被感染"),"know_pos"] <- 1
Survey_Wave2[which(Survey_Wave2$Q48=="有，在中国和美国都有我认识的人被感染"),"know_pos"] <- 1
table(Survey_Wave2$know_pos)
# 441 subjects know someone infected in the USA


# health kit
Survey_Wave2$kit_got_2 <- 0
Survey_Wave2[which(Survey_Wave2$kit_got=="是"),"kit_got_2"] <- 1
table(Survey_Wave2$kit_got_2)
# 402 subjects already got health kits
Survey_Wave2$kit_timely_2 <- 0
Survey_Wave2[which(Survey_Wave2$kit_timely=="及时"),"kit_timely_2"] <- 1
table(Survey_Wave2$kit_timely_2)
# 379 subjects think health kits arrive timely


# return to China
Survey_Wave2$return_china <- NA
Survey_Wave2[which(Survey_Wave2$return=="已经或曾经回国"),"return_china"] <- 1
Survey_Wave2[which(Survey_Wave2$return=="未曾回国"),"return_china"] <- 0
table(Survey_Wave2$return_china)
# 104 have been back to China, 677 did not


# ================================= Appendix D: Summary Statistics =================================
### Wave 1
Survey_Wave1_full <- 
  Survey_Wave1 %>% 
  select(china_overall_num,china_gov_num,gov_assess_1,gov_assess_3,gov_assess_2,gov_assess_4,
         group,female,education_gra,STEM,family_regime_tie,ideology_1_num,ideology_2_num,
         ideology_3_num,ideology_4_num,ideology_5_num,ideology_pca,socialized_US,race_experience,
         know_pos,income_us,covid_assess_1_num,covid_assess_2_num,covid_assess_3_num,
         COVIDassess_pca,kit_got_2,kit_timely_2,return_china)

#Save as cvs
#write.csv(Survey_Wave1_full,"Survey_Wave1_full.csv")

stargazer(Survey_Wave1_full, align=TRUE, type="text", omit.summary.stat = c("p25", "p75"), digits = 1)

#to paste in Latex
#stargazer(Survey_Wave1_full, align=TRUE, omit.summary.stat = c("p25", "p75"), digits = 1)
table(Survey_Wave1_full$group)


### Wave 2
Survey_Wave2_full <- 
  Survey_Wave2 %>% 
  select(china_overall_num,china_gov_num,gov_assess_1,gov_assess_3,gov_assess_2,gov_assess_4,
         group,female,education_gra,STEM,family_regime_tie,ideology_1_num,ideology_2_num,
         ideology_3_num,ideology_4_num,ideology_5_num,ideology_pca,socialized_US,race_experience,
         know_pos,income_us,covid_assess_1_num,covid_assess_2_num,covid_assess_3_num,
         COVIDassess_pca,kit_got_2,kit_timely_2,return_china)

#Save as cvs
#write.csv(Survey_Wave2_full,"Survey_Wave2_full.csv")

stargazer(Survey_Wave2_full, align=TRUE, type="text", omit.summary.stat = c("p25", "p75"), digits = 1)

#to paste in Latex
#stargazer(Survey_Wave2_full, align=TRUE, omit.summary.stat = c("p25", "p75"), digits = 1)
table(Survey_Wave2_full$group)


##### correlation matrix of DVs (Figure 1) ####
### Wave 1
corDVs.df_1 <- Survey_Wave1_full[,c("china_overall_num", "china_gov_num", "gov_assess_1", "gov_assess_3", 
                             "gov_assess_2", "gov_assess_4")]
names(corDVs.df_1) <- 
  c("CHN Overall", "CHN Gov.", "CHN Gov. Covid", "CHN Gov. Covid-Studs.", 
    "US Gov. Covid", "US Gov. Covid-Studs.")
names(corDVs.df_1)

pdf(file = "corDVs_1.pdf")
corrplot(cor(corDVs.df_1, 
             use = "pairwise.complete.obs", method ="spearman"), type="full", order="original", 
         title = "Wave 1",
         tl.col="black", tl.srt=45, tl.cex=0.65, 
         method ="color", addCoef.col="black", 
         number.cex=0.75, number.font=2, 
         col = colorRampPalette(c("white", "white", "gray55"))(200), is.corr = TRUE,
         mar=c(0,0,1,0))
dev.off()


### Wave 2
corDVs.df_2 <- Survey_Wave2_full[,c("china_overall_num", "china_gov_num", "gov_assess_1", "gov_assess_3", 
                                    "gov_assess_2", "gov_assess_4")]
names(corDVs.df_2) <- 
  c("CHN Overall", "CHN Gov.", "CHN Gov. Covid", "CHN Gov. Covid-Studs.", 
    "US Gov. Covid", "US Gov. Covid-Studs.")
names(corDVs.df_2)

pdf(file = "corDVs_2.pdf")
corrplot(cor(corDVs.df_2, 
             use = "pairwise.complete.obs", method ="spearman"), type="full", order="original", 
         title = "Wave 2",
         tl.col="black", tl.srt=45, tl.cex=0.65, 
         method ="color", addCoef.col="black", 
         number.cex=0.75, number.font=2, 
         col = colorRampPalette(c("white", "white", "gray55"))(200), is.corr = TRUE,
         mar=c(0,0,1,0))
dev.off()


# ================================= Main model: positive as base group =================================
#### Wave 1 ####
# reorder group
levels(Survey_Wave1_full$group)
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("positive", "negative", "additive", "control"))

# Regression Analysis
# Chinese government on COVID
m3_1 <- lm(gov_assess_1 ~ 
           female + education_gra + STEM + family_regime_tie + 
           ideology_pca + socialized_US + race_experience + know_pos + income_us + 
           COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
         data=Survey_Wave1_full)
summary(m3_1)

# US government on COVID
m5_1 <- lm(gov_assess_2 ~ 
           female + education_gra + STEM + family_regime_tie + 
           ideology_pca + socialized_US + race_experience + know_pos + income_us + 
           COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
         data=Survey_Wave1_full)
summary(m5_1)

# Chinese government student
m4_1 <- lm(gov_assess_3 ~ 
           female + education_gra + STEM + family_regime_tie + 
           ideology_pca + socialized_US + race_experience + know_pos + income_us + 
           COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
         data=Survey_Wave1_full)
summary(m4_1)

# US government student 
m6_1 <- lm(gov_assess_4 ~ 
           female + education_gra + STEM + family_regime_tie + 
           ideology_pca + socialized_US + race_experience + know_pos + income_us + 
           COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
         data=Survey_Wave1_full)
summary(m6_1)

# China overall
m1_1 <- lm(china_overall_num ~ 
           female + education_gra + STEM + family_regime_tie + 
           ideology_pca + socialized_US + race_experience + know_pos + income_us + 
           COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
         data=Survey_Wave1_full)
summary(m1_1)

# Chinese government
m2_1 <- lm(china_gov_num ~ 
           female + education_gra + STEM + family_regime_tie + 
           ideology_pca + socialized_US + race_experience + know_pos + income_us + 
           COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
         data=Survey_Wave1_full)
summary(m2_1)


# generate tables
stargazer(m1_1,m2_1,m3_1,m4_1,m5_1,m6_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
# stargazer(m1_1,m2_1,m3_1,m4_1,m5_1,m6_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# without controls
m3nc_1 <- lm(gov_assess_1 ~ 
             group, data=Survey_Wave1_full)
summary(m3nc_1)

m5nc_1 <- lm(gov_assess_2 ~ 
             group, data=Survey_Wave1_full)
summary(m5nc_1)

m4nc_1 <- lm(gov_assess_3 ~ 
             group, data=Survey_Wave1_full)
summary(m4nc_1)

m6nc_1 <- lm(gov_assess_4 ~ 
             group, data=Survey_Wave1_full)
summary(m6nc_1)

m1nc_1 <- lm(china_overall_num ~ 
             group, data=Survey_Wave1_full)
summary(m1nc_1)

m2nc_1 <- lm(china_gov_num ~ 
             group, data=Survey_Wave1_full)
summary(m2nc_1)

# generate tables
stargazer(m1nc_1, m2nc_1, m3nc_1, m4nc_1, m5nc_1, m6nc_1, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1nc_1, m2nc_1, m3nc_1, m4nc_1, m5nc_1, m6nc_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))



#### Wave 2 ####
# reorder group
levels(Survey_Wave2_full$group)
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("positive", "negative", "additive", "control"))

# Regression Analysis
# Chinese government on COVID
m3_2 <- lm(gov_assess_1 ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave2_full)
summary(m3_2)

# US government on COVID
m5_2 <- lm(gov_assess_2 ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave2_full)
summary(m5_2)

# Chinese government student
m4_2 <- lm(gov_assess_3 ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave2_full)
summary(m4_2)

# US government student 
m6_2 <- lm(gov_assess_4 ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave2_full)
summary(m6_2)

# China overall
m1_2 <- lm(china_overall_num ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave2_full)
summary(m1_2)

# Chinese government
m2_2 <- lm(china_gov_num ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave2_full)
summary(m2_2)


# generate tables
stargazer(m1_2,m2_2,m3_2,m4_2,m5_2,m6_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
# stargazer(m1_2,m2_2,m3_2,m4_2,m5_2,m6_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# without controls
m3nc_2 <- lm(gov_assess_1 ~ 
               group, data=Survey_Wave2_full)
summary(m3nc_2)

m5nc_2 <- lm(gov_assess_2 ~ 
               group, data=Survey_Wave2_full)
summary(m5nc_2)

m4nc_2 <- lm(gov_assess_3 ~ 
               group, data=Survey_Wave2_full)
summary(m4nc_2)

m6nc_2 <- lm(gov_assess_4 ~ 
               group, data=Survey_Wave2_full)
summary(m6nc_2)

m1nc_2 <- lm(china_overall_num ~ 
               group, data=Survey_Wave2_full)
summary(m1nc_2)

m2nc_2 <- lm(china_gov_num ~ 
               group, data=Survey_Wave2_full)
summary(m2nc_2)

# generate tables
stargazer(m1nc_2, m2nc_2, m3nc_2, m4nc_2, m5nc_2, m6nc_2, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1nc_2, m2nc_2, m3nc_2, m4nc_2, m5nc_2, m6nc_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# ================================= plotting ATEs (Figure 2)=================================
# x axis
x_esti <-c(-0.1,0.9,1.9,0.1,1.1,2.1)
x_esti


#### China Overall ####
# estimate and SE of control: Wave 1
esti_china_overall_1 <-c(summary(m1nc_1)$coefficients[4,1],
                         summary(m1nc_1)$coefficients[2,1],
                         summary(m1nc_1)$coefficients[3,1],
                         summary(m1_1)$coefficients[17,1],
                         summary(m1_1)$coefficients[15,1],
                         summary(m1_1)$coefficients[16,1])

SE_china_overall_1 <- c(summary(m1nc_1)$coefficients[4,2],
                        summary(m1nc_1)$coefficients[2,2],
                        summary(m1nc_1)$coefficients[3,2],
                        summary(m1_1)$coefficients[17,2],
                        summary(m1_1)$coefficients[15,2],
                        summary(m1_1)$coefficients[16,2])

# estimate and SE of control: Wave 2
esti_china_overall_2 <-c(summary(m1nc_2)$coefficients[4,1],
                         summary(m1nc_2)$coefficients[2,1],
                         summary(m1nc_2)$coefficients[3,1],
                         summary(m1_2)$coefficients[17,1],
                         summary(m1_2)$coefficients[15,1],
                         summary(m1_2)$coefficients[16,1])

SE_china_overall_2 <- c(summary(m1nc_2)$coefficients[4,2],
                        summary(m1nc_2)$coefficients[2,2],
                        summary(m1nc_2)$coefficients[3,2],
                        summary(m1_2)$coefficients[17,2],
                        summary(m1_2)$coefficients[15,2],
                        summary(m1_2)$coefficients[16,2])

# plot China Overall
pdf("China_Overall.pdf")
par(mar=c(8,5,5,2), xpd=NA)
par(mfrow=c(1,2))
# China Overall Wave 1
plot(x_esti[1:3],esti_china_overall_1[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_china_overall_1[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("A: Wave 1", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_china_overall_1[1:6]+(1.96*SE_china_overall_1[1:6]), 
         x1=x_esti[1:6], y1= esti_china_overall_1[1:6]-(1.96*SE_china_overall_1[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_china_overall_1[1:6]+(1.645*SE_china_overall_1[1:6]), 
         x1=x_esti[1:6], y1= esti_china_overall_1[1:6]-(1.645*SE_china_overall_1[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# China Overall Wave 2
plot(x_esti[1:3],esti_china_overall_2[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_china_overall_2[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("B: Wave 2", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_china_overall_2[1:6]+(1.96*SE_china_overall_2[1:6]), 
         x1=x_esti[1:6], y1= esti_china_overall_2[1:6]-(1.96*SE_china_overall_2[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_china_overall_2[1:6]+(1.645*SE_china_overall_2[1:6]), 
         x1=x_esti[1:6], y1= esti_china_overall_2[1:6]-(1.645*SE_china_overall_2[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# legend
par(mfrow=c(1,1))
title("China Overall", cex.main=1.5, font.main=1, line = 3)
legend("bottom", inset = c(0, -0.35), legend = c("No Controls", "With Controls"), pch = c(21,16), 
       bty = "n", pt.cex = 1.5, cex = 1.2, text.col = "black", horiz = TRUE)
dev.off()


#### Chinese Government ####
# estimate and SE of control: Wave 1
esti_china_gov_1 <-c(summary(m2nc_1)$coefficients[4,1],
                     summary(m2nc_1)$coefficients[2,1],
                     summary(m2nc_1)$coefficients[3,1],
                     summary(m2_1)$coefficients[17,1],
                     summary(m2_1)$coefficients[15,1],
                     summary(m2_1)$coefficients[16,1])

SE_china_gov_1 <- c(summary(m2nc_1)$coefficients[4,2],
                    summary(m2nc_1)$coefficients[2,2],
                    summary(m2nc_1)$coefficients[3,2],
                    summary(m2_1)$coefficients[17,2],
                    summary(m2_1)$coefficients[15,2],
                    summary(m2_1)$coefficients[16,2])

# estimate and SE of control: Wave 2
esti_china_gov_2 <-c(summary(m2nc_2)$coefficients[4,1],
                     summary(m2nc_2)$coefficients[2,1],
                     summary(m2nc_2)$coefficients[3,1],
                     summary(m2_2)$coefficients[17,1],
                     summary(m2_2)$coefficients[15,1],
                     summary(m2_2)$coefficients[16,1])

SE_china_gov_2 <- c(summary(m2nc_2)$coefficients[4,2],
                    summary(m2nc_2)$coefficients[2,2],
                    summary(m2nc_2)$coefficients[3,2],
                    summary(m2_2)$coefficients[17,2],
                    summary(m2_2)$coefficients[15,2],
                    summary(m2_2)$coefficients[16,2])

# plot Chinese Government
pdf("Chinese_Government.pdf")
par(mar=c(8,5,5,2), xpd=NA)
par(mfrow=c(1,2))
# Chinese Government Wave 1
plot(x_esti[1:3],esti_china_gov_1[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_china_gov_1[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("C: Wave 1", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_china_gov_1[1:6]+(1.96*SE_china_gov_1[1:6]), 
         x1=x_esti[1:6], y1= esti_china_gov_1[1:6]-(1.96*SE_china_gov_1[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_china_gov_1[1:6]+(1.645*SE_china_gov_1[1:6]), 
         x1=x_esti[1:6], y1= esti_china_gov_1[1:6]-(1.645*SE_china_gov_1[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# Chinese Government Wave 2
plot(x_esti[1:3],esti_china_gov_2[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_china_gov_2[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("D: Wave 2", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_china_gov_2[1:6]+(1.96*SE_china_gov_2[1:6]), 
         x1=x_esti[1:6], y1= esti_china_gov_2[1:6]-(1.96*SE_china_gov_2[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_china_gov_2[1:6]+(1.645*SE_china_gov_2[1:6]), 
         x1=x_esti[1:6], y1= esti_china_gov_2[1:6]-(1.645*SE_china_gov_2[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# lengend
par(mfrow=c(1,1))
title("Chinese Goverment", cex.main=1.5, font.main=1, line = 3)
legend("bottom", inset = c(0, -0.35), legend = c("No Controls", "With Controls"), pch = c(21,16), 
       bty = "n", pt.cex = 1.5, cex = 1.2, text.col = "black", horiz = TRUE)
dev.off()


#### China COVID Response ####
# estimate and SE of control: Wave 1
esti_gov_assess_1_1 <-c(summary(m3nc_1)$coefficients[4,1],
                        summary(m3nc_1)$coefficients[2,1],
                        summary(m3nc_1)$coefficients[3,1],
                        summary(m3_1)$coefficients[17,1],
                        summary(m3_1)$coefficients[15,1],
                        summary(m3_1)$coefficients[16,1])

SE_gov_assess_1_1 <- c(summary(m3nc_1)$coefficients[4,2],
                       summary(m3nc_1)$coefficients[2,2],
                       summary(m3nc_1)$coefficients[3,2],
                       summary(m3_1)$coefficients[17,2],
                       summary(m3_1)$coefficients[15,2],
                       summary(m3_1)$coefficients[16,2])

# estimate and SE of control: Wave 2
esti_gov_assess_1_2 <-c(summary(m3nc_2)$coefficients[4,1],
                        summary(m3nc_2)$coefficients[2,1],
                        summary(m3nc_2)$coefficients[3,1],
                        summary(m3_2)$coefficients[17,1],
                        summary(m3_2)$coefficients[15,1],
                        summary(m3_2)$coefficients[16,1])

SE_gov_assess_1_2 <- c(summary(m3nc_2)$coefficients[4,2],
                       summary(m3nc_2)$coefficients[2,2],
                       summary(m3nc_2)$coefficients[3,2],
                       summary(m3_2)$coefficients[17,2],
                       summary(m3_2)$coefficients[15,2],
                       summary(m3_2)$coefficients[16,2])

# plot China COVID Response
pdf("China_COVID_Response.pdf")
par(mar=c(8,5,5,2), xpd=NA)
par(mfrow=c(1,2))
# China COVID Response Wave 1
plot(x_esti[1:3], esti_gov_assess_1_1[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_gov_assess_1_1[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("E: Wave 1", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_gov_assess_1_1[1:6]+(1.96*SE_gov_assess_1_1[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_1_1[1:6]-(1.96*SE_gov_assess_1_1[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_gov_assess_1_1[1:6]+(1.645*SE_gov_assess_1_1[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_1_1[1:6]-(1.645*SE_gov_assess_1_1[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# China COVID Response Wave 2
plot(x_esti[1:3], esti_gov_assess_1_2[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_gov_assess_1_2[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("F: Wave 2", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_gov_assess_1_2[1:6]+(1.96*SE_gov_assess_1_2[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_1_2[1:6]-(1.96*SE_gov_assess_1_2[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_gov_assess_1_2[1:6]+(1.645*SE_gov_assess_1_2[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_1_2[1:6]-(1.645*SE_gov_assess_1_2[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# legend
par(mfrow=c(1,1))
title("China COVID Response", cex.main=1.5, font.main=1, line = 3)
legend("bottom", inset = c(0, -0.35), legend = c("No Controls", "With Controls"), pch = c(21,16), 
       bty = "n", pt.cex = 1.5, cex = 1.2, text.col = "black", horiz = TRUE)
dev.off()


#### China COVID response: Students ####
# estimate and SE of control: Wave 1
esti_gov_assess_3_1 <-c(summary(m4nc_1)$coefficients[4,1],
                        summary(m4nc_1)$coefficients[2,1],
                        summary(m4nc_1)$coefficients[3,1],
                        summary(m4_1)$coefficients[17,1],
                        summary(m4_1)$coefficients[15,1],
                        summary(m4_1)$coefficients[16,1])

SE_gov_assess_3_1 <- c(summary(m4nc_1)$coefficients[4,2],
                       summary(m4nc_1)$coefficients[2,2],
                       summary(m4nc_1)$coefficients[3,2],
                       summary(m4_1)$coefficients[17,2],
                       summary(m4_1)$coefficients[15,2],
                       summary(m4_1)$coefficients[16,2])

# estimate and SE of control: Wave 2
esti_gov_assess_3_2 <-c(summary(m4nc_2)$coefficients[4,1],
                        summary(m4nc_2)$coefficients[2,1],
                        summary(m4nc_2)$coefficients[3,1],
                        summary(m4_2)$coefficients[17,1],
                        summary(m4_2)$coefficients[15,1],
                        summary(m4_2)$coefficients[16,1])

SE_gov_assess_3_2 <- c(summary(m4nc_2)$coefficients[4,2],
                       summary(m4nc_2)$coefficients[2,2],
                       summary(m4nc_2)$coefficients[3,2],
                       summary(m4_2)$coefficients[17,2],
                       summary(m4_2)$coefficients[15,2],
                       summary(m4_2)$coefficients[16,2])

# plot China COVID Response: Students
pdf("China_COVID_Response_Students.pdf")
par(mar=c(8,5,5,2), xpd=NA)
par(mfrow=c(1,2))
# China COVID Response Students Wave 1
plot(x_esti[1:3], esti_gov_assess_3_1[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_gov_assess_3_1[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("G: Wave 1", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_gov_assess_3_1[1:6]+(1.96*SE_gov_assess_3_1[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_3_1[1:6]-(1.96*SE_gov_assess_3_1[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_gov_assess_3_1[1:6]+(1.645*SE_gov_assess_3_1[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_3_1[1:6]-(1.645*SE_gov_assess_3_1[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# China COVID Response Students Wave 2
plot(x_esti[1:3], esti_gov_assess_3_2[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_gov_assess_3_2[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("H: Wave 2", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_gov_assess_3_2[1:6]+(1.96*SE_gov_assess_3_2[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_3_2[1:6]-(1.96*SE_gov_assess_3_2[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_gov_assess_3_2[1:6]+(1.645*SE_gov_assess_3_2[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_3_2[1:6]-(1.645*SE_gov_assess_3_2[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# legend
par(mfrow=c(1,1))
title("China COVID Response: Students", cex.main=1.5, font.main=1, line = 3)
legend("bottom", inset = c(0, -0.35), legend = c("No Controls", "With Controls"), pch = c(21,16), 
       bty = "n", pt.cex = 1.5, cex = 1.2, text.col = "black", horiz = TRUE)
dev.off()


#### US COVID response ####
# estimate and SE of control: Wave 1
esti_gov_assess_2_1 <-c(summary(m5nc_1)$coefficients[4,1],
                        summary(m5nc_1)$coefficients[2,1],
                        summary(m5nc_1)$coefficients[3,1],
                        summary(m5_1)$coefficients[17,1],
                        summary(m5_1)$coefficients[15,1],
                        summary(m5_1)$coefficients[16,1])

SE_gov_assess_2_1 <- c(summary(m5nc_1)$coefficients[4,2],
                       summary(m5nc_1)$coefficients[2,2],
                       summary(m5nc_1)$coefficients[3,2],
                       summary(m5_1)$coefficients[17,2],
                       summary(m5_1)$coefficients[15,2],
                       summary(m5_1)$coefficients[16,2])

# estimate and SE of control: Wave 2
esti_gov_assess_2_2 <-c(summary(m5nc_2)$coefficients[4,1],
                        summary(m5nc_2)$coefficients[2,1],
                        summary(m5nc_2)$coefficients[3,1],
                        summary(m5_2)$coefficients[17,1],
                        summary(m5_2)$coefficients[15,1],
                        summary(m5_2)$coefficients[16,1])

SE_gov_assess_2_2 <- c(summary(m5nc_2)$coefficients[4,2],
                       summary(m5nc_2)$coefficients[2,2],
                       summary(m5nc_2)$coefficients[3,2],
                       summary(m5_2)$coefficients[17,2],
                       summary(m5_2)$coefficients[15,2],
                       summary(m5_2)$coefficients[16,2])

# plot US COVID Response
pdf("US_COVID_Response.pdf")
par(mar=c(8,5,5,2), xpd=NA)
par(mfrow=c(1,2))
# US COVID Response Wave 1
plot(x_esti[1:3], esti_gov_assess_2_1[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_gov_assess_2_1[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("I: Wave 1", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_gov_assess_2_1[1:6]+(1.96*SE_gov_assess_2_1[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_2_1[1:6]-(1.96*SE_gov_assess_2_1[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_gov_assess_2_1[1:6]+(1.645*SE_gov_assess_2_1[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_2_1[1:6]-(1.645*SE_gov_assess_2_1[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# US COVID Response Wave 2
plot(x_esti[1:3], esti_gov_assess_2_2[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_gov_assess_2_2[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("J: Wave 2", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_gov_assess_2_2[1:6]+(1.96*SE_gov_assess_2_2[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_2_2[1:6]-(1.96*SE_gov_assess_2_2[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_gov_assess_2_2[1:6]+(1.645*SE_gov_assess_2_2[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_2_2[1:6]-(1.645*SE_gov_assess_2_2[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# legend
par(mfrow=c(1,1))
title("US COVID Response", cex.main=1.5, font.main=1, line = 3)
legend("bottom", inset = c(0, -0.35), legend = c("No Controls", "With Controls"), pch = c(21,16), 
       bty = "n", pt.cex = 1.5, cex = 1.2, text.col = "black", horiz = TRUE)
dev.off()


#### US COVID response: Students ####
# estimate and SE of control: Wave 1
esti_gov_assess_4_1 <-c(summary(m6nc_1)$coefficients[4,1],
                        summary(m6nc_1)$coefficients[2,1],
                        summary(m6nc_1)$coefficients[3,1],
                        summary(m6_1)$coefficients[17,1],
                        summary(m6_1)$coefficients[15,1],
                        summary(m6_1)$coefficients[16,1])

SE_gov_assess_4_1 <- c(summary(m6nc_1)$coefficients[4,2],
                       summary(m6nc_1)$coefficients[2,2],
                       summary(m6nc_1)$coefficients[3,2],
                       summary(m6_1)$coefficients[17,2],
                       summary(m6_1)$coefficients[15,2],
                       summary(m6_1)$coefficients[16,2])

# estimate and SE of control: Wave 2
esti_gov_assess_4_2 <-c(summary(m6nc_2)$coefficients[4,1],
                        summary(m6nc_2)$coefficients[2,1],
                        summary(m6nc_2)$coefficients[3,1],
                        summary(m6_2)$coefficients[17,1],
                        summary(m6_2)$coefficients[15,1],
                        summary(m6_2)$coefficients[16,1])

SE_gov_assess_4_2 <- c(summary(m6nc_2)$coefficients[4,2],
                       summary(m6nc_2)$coefficients[2,2],
                       summary(m6nc_2)$coefficients[3,2],
                       summary(m6_2)$coefficients[17,2],
                       summary(m6_2)$coefficients[15,2],
                       summary(m6_2)$coefficients[16,2])

# plot US COVID Response: Students
pdf("US_COVID_Response_Students.pdf")
par(mar=c(8,5,5,2), xpd=NA)
par(mfrow=c(1,2))
# US COVID Response Students Wave 1
plot(x_esti[1:3], esti_gov_assess_4_1[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_gov_assess_4_1[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("K: Wave 1", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_gov_assess_4_1[1:6]+(1.96*SE_gov_assess_4_1[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_4_1[1:6]-(1.96*SE_gov_assess_4_1[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_gov_assess_4_1[1:6]+(1.645*SE_gov_assess_4_1[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_4_1[1:6]-(1.645*SE_gov_assess_4_1[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# US COVID Response Students Wave 2
plot(x_esti[1:3], esti_gov_assess_4_2[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-0.6,0.5), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], esti_gov_assess_4_2[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("L: Wave 2", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0= esti_gov_assess_4_2[1:6]+(1.96*SE_gov_assess_4_2[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_4_2[1:6]-(1.96*SE_gov_assess_4_2[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0= esti_gov_assess_4_2[1:6]+(1.645*SE_gov_assess_4_2[1:6]), 
         x1=x_esti[1:6], y1= esti_gov_assess_4_2[1:6]-(1.645*SE_gov_assess_4_2[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-0.6,-0.4,-0.2,0,0.2,0.4))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# legend
par(mfrow=c(1,1))
title("US COVID Response: Students", cex.main=1.5, font.main=1, line = 3)
legend("bottom", inset = c(0, -0.35), legend = c("No Controls", "With Controls"), pch = c(21,16), 
       bty = "n", pt.cex = 1.5, cex = 1.2, text.col = "black", horiz = TRUE)
dev.off()




# ============================== plotting Graphs for Appendix C: Outcomes by Group ==============================

#### compare DVs mean by group: Wave 1 ####
gov_assess_1_bygroup_1 <- 
  as.data.frame(group_by(Survey_Wave1_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(gov_assess_1, na.rm = TRUE),
                    sd = sd(gov_assess_1, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
gov_assess_1_bygroup_1

gov_assess_2_bygroup_1 <- 
  as.data.frame(group_by(Survey_Wave1_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(gov_assess_2, na.rm = TRUE),
                    sd = sd(gov_assess_2, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
gov_assess_2_bygroup_1

gov_assess_3_bygroup_1 <- 
  as.data.frame(group_by(Survey_Wave1_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(gov_assess_3, na.rm = TRUE),
                    sd = sd(gov_assess_3, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
gov_assess_3_bygroup_1

gov_assess_4_bygroup_1 <- 
  as.data.frame(group_by(Survey_Wave1_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(gov_assess_4, na.rm = TRUE),
                    sd = sd(gov_assess_4, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
gov_assess_4_bygroup_1

china_overall_num_bygroup_1 <-
  as.data.frame(group_by(Survey_Wave1_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(china_overall_num, na.rm = TRUE),
                    sd = sd(china_overall_num, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
china_overall_num_bygroup_1

china_gov_num_bygroup_1 <- 
  as.data.frame(group_by(Survey_Wave1_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(china_gov_num, na.rm = TRUE),
                    sd = sd(china_gov_num, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
china_gov_num_bygroup_1


#### compare DVs mean by group: Wave 2 ####
gov_assess_1_bygroup_2 <- 
  as.data.frame(group_by(Survey_Wave2_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(gov_assess_1, na.rm = TRUE),
                    sd = sd(gov_assess_1, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
gov_assess_1_bygroup_2

gov_assess_2_bygroup_2 <- 
  as.data.frame(group_by(Survey_Wave2_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(gov_assess_2, na.rm = TRUE),
                    sd = sd(gov_assess_2, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
gov_assess_2_bygroup_2

gov_assess_3_bygroup_2 <- 
  as.data.frame(group_by(Survey_Wave2_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(gov_assess_3, na.rm = TRUE),
                    sd = sd(gov_assess_3, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
gov_assess_3_bygroup_2

gov_assess_4_bygroup_2 <- 
  as.data.frame(group_by(Survey_Wave2_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(gov_assess_4, na.rm = TRUE),
                    sd = sd(gov_assess_4, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
gov_assess_4_bygroup_2

china_overall_num_bygroup_2 <-
  as.data.frame(group_by(Survey_Wave2_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(china_overall_num, na.rm = TRUE),
                    sd = sd(china_overall_num, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
china_overall_num_bygroup_2

china_gov_num_bygroup_2 <- 
  as.data.frame(group_by(Survey_Wave2_full, group) %>%
                  summarise(
                    count = n(),
                    mean = mean(china_gov_num, na.rm = TRUE),
                    sd = sd(china_gov_num, na.rm = TRUE), 
                    se = sd/sqrt(count), 
                    me = qt(.975, df=count-1)*se))
china_gov_num_bygroup_2


### barplot of compared outcomes with error bars, overall by two governments
gov_assess_12_bygroup_1 <- 
  cbind(rbind(gov_assess_1_bygroup_1, gov_assess_2_bygroup_1), 
        rep(c("Chinese government", "U.S. government"), each = 4))
names(gov_assess_12_bygroup_1)[1] <- "Group"
names(gov_assess_12_bygroup_1)[7] <- "Outcome"
gov_assess_12_bygroup_1$Outcome <- 
  factor(gov_assess_12_bygroup_1$Outcome, levels = c("Chinese government", "U.S. government"))
gov_assess_12_bygroup_1


gov_assess_12_bygroup_2 <- 
  cbind(rbind(gov_assess_1_bygroup_2, gov_assess_2_bygroup_2), 
        rep(c("Chinese government", "U.S. government"), each = 4))
names(gov_assess_12_bygroup_2)[1] <- "Group"
names(gov_assess_12_bygroup_2)[7] <- "Outcome"
gov_assess_12_bygroup_2$Outcome <- 
  factor(gov_assess_12_bygroup_2$Outcome, levels = c("Chinese government", "U.S. government"))
gov_assess_12_bygroup_2


## Generate Figure C-1
p1 <- ggplot(gov_assess_12_bygroup_1, aes(x=Outcome, y=mean, fill=Group)) +
      geom_bar(stat="identity", position = position_dodge(width = 0.8), color="black", size=0.4, alpha=0.4)+
      geom_errorbar(aes(x=Outcome, ymin=mean-me, ymax=mean+me), position = position_dodge(width = 0.8), 
                width=0.4, colour="black", alpha=0.9, size=1.3) + 
      scale_fill_manual(values=c("white", "gray87", "gray28", "black")) + 
      theme_bw() + 
      theme(legend.position="none") + 
      ggtitle("Wave 1") + 
      theme(plot.title = element_text(face="bold")) + 
      theme(plot.title = element_text(hjust = 0.5, size=20)) + 
      labs(x = "") + 
      labs(y = "Mean Evaluation") + 
      scale_y_continuous(limits = c(0, 4.5), breaks=seq(0,4.5,1)) + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size=15)) + 
      theme(axis.text.x= element_text(colour="black", size=13), 
            axis.text.y= element_text(colour="black", size=15))

p2 <- ggplot(gov_assess_12_bygroup_2, aes(x=Outcome, y=mean, fill=Group)) +
      geom_bar(stat="identity", position = position_dodge(width = 0.8), color="black", size=0.4, alpha=0.4)+
      geom_errorbar(aes(x=Outcome, ymin=mean-me, ymax=mean+me), position = position_dodge(width = 0.8), 
                width=0.4, colour="black", alpha=0.9, size=1.3) + 
      scale_fill_manual(values=c("white", "gray87", "gray28", "black")) + 
      theme_bw() + 
      theme(legend.position="none") + 
      ggtitle("Wave 2") + 
      theme(plot.title = element_text(face="bold")) + 
      theme(plot.title = element_text(hjust = 0.5, size=20)) + 
      labs(x = "") + 
      labs(y = "Mean Evaluation") + 
      scale_y_continuous(limits = c(0, 4.5), breaks=seq(0,4.5,1)) + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size=15)) + 
      theme(axis.text.x= element_text(colour="black", size=13), 
            axis.text.y= element_text(colour="black", size=15))

pdf("COVID_Response.pdf",width = 8,height= 7, onefile=F)
ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off()



### barplot of compared outcomes with error bars, on Chinese students in US by two governments
gov_assess_34_bygroup_1 <- 
  cbind(rbind(gov_assess_3_bygroup_1, gov_assess_4_bygroup_1), 
        rep(c("Chinese government", "U.S. government"), each = 4))
names(gov_assess_34_bygroup_1)[1] <- "Group"
names(gov_assess_34_bygroup_1)[7] <- "Outcome"
gov_assess_34_bygroup_1$Outcome <- 
  factor(gov_assess_34_bygroup_1$Outcome, levels = c("Chinese government", "U.S. government"))
gov_assess_34_bygroup_1


gov_assess_34_bygroup_2 <- 
  cbind(rbind(gov_assess_3_bygroup_2, gov_assess_4_bygroup_2), 
        rep(c("Chinese government", "U.S. government"), each = 4))
names(gov_assess_34_bygroup_2)[1] <- "Group"
names(gov_assess_34_bygroup_2)[7] <- "Outcome"
gov_assess_34_bygroup_2$Outcome <- 
  factor(gov_assess_34_bygroup_2$Outcome, levels = c("Chinese government", "U.S. government"))
gov_assess_34_bygroup_2


# Generate Figure C-2
p1 <- ggplot(gov_assess_34_bygroup_1, aes(x=Outcome, y=mean, fill=Group)) +
      geom_bar(stat="identity", position = position_dodge(width = 0.8), color="black", size=0.4, alpha=0.4)+
      geom_errorbar(aes(x=Outcome, ymin=mean-me, ymax=mean+me), position = position_dodge(width = 0.8), 
                    width=0.4, colour="black", alpha=0.9, size=1.3) + 
      scale_fill_manual(values=c("white", "gray87", "gray28", "black")) + 
      theme_bw() + 
      theme(legend.position="none") + 
      ggtitle("Wave 1") + 
      theme(plot.title = element_text(face="bold")) + 
      theme(plot.title = element_text(hjust = 0.5, size=20)) + 
      labs(x = "") + 
      labs(y = "Mean Evaluation") + 
      scale_y_continuous(limits = c(0, 4.5), breaks=seq(0,4.5,1)) + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size=15)) + 
      theme(axis.text.x= element_text(colour="black", size=13), 
            axis.text.y= element_text(colour="black", size=15))


p2 <- ggplot(gov_assess_34_bygroup_2, aes(x=Outcome, y=mean, fill=Group)) +
      geom_bar(stat="identity", position = position_dodge(width = 0.8), color="black", size=0.4, alpha=0.4)+
      geom_errorbar(aes(x=Outcome, ymin=mean-me, ymax=mean+me), position = position_dodge(width = 0.8), 
                    width=0.4, colour="black", alpha=0.9, size=1.3) + 
      scale_fill_manual(values=c("white", "gray87", "gray28", "black")) + 
      theme_bw() + 
      theme(legend.position="none") + 
      ggtitle("Wave 2") + 
      theme(plot.title = element_text(face="bold")) + 
      theme(plot.title = element_text(hjust = 0.5, size=20)) + 
      labs(x = "") + 
      labs(y = "Mean Evaluation") + 
      scale_y_continuous(limits = c(0, 4.5), breaks=seq(0,4.5,1)) + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size=15)) + 
      theme(axis.text.x= element_text(colour="black", size=13), 
            axis.text.y= element_text(colour="black", size=15))

pdf("COVID_Students.pdf",width = 8,height= 7, onefile=F)
ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off()




### barplot of compared outcomes with error bars, on satisfaction on China overall and Chinese governments
China_assess_bygroup_1 <- 
  cbind(rbind(china_overall_num_bygroup_1, china_gov_num_bygroup_1), 
        rep(c("China overall", "Chinese government"), each = 4))
names(China_assess_bygroup_1)[1] <- "Group"
names(China_assess_bygroup_1)[7] <- "Outcome"
China_assess_bygroup_1$Outcome <- 
  factor(China_assess_bygroup_1$Outcome, levels = c("China overall", "Chinese government"))
China_assess_bygroup_1


China_assess_bygroup_2 <- 
  cbind(rbind(china_overall_num_bygroup_2, china_gov_num_bygroup_2), 
        rep(c("China overall", "Chinese government"), each = 4))
names(China_assess_bygroup_2)[1] <- "Group"
names(China_assess_bygroup_2)[7] <- "Outcome"
China_assess_bygroup_2$Outcome <- 
  factor(China_assess_bygroup_2$Outcome, levels = c("China overall", "Chinese government"))
China_assess_bygroup_2


# Generate Figure C-3
p1 <- ggplot(China_assess_bygroup_1, aes(x=Outcome, y=mean, fill=Group)) +
      geom_bar(stat="identity", position = position_dodge(width = 0.8), color="black", size=0.4, alpha=0.4)+
      geom_errorbar(aes(x=Outcome, ymin=mean-me, ymax=mean+me), position = position_dodge(width = 0.8), 
                    width=0.4, colour="black", alpha=0.9, size=1.3) + 
      scale_fill_manual(values=c("white", "gray87", "gray28", "black")) + 
      theme_bw() + 
      theme(legend.position="none") + 
      ggtitle("Wave 1") + 
      theme(plot.title = element_text(face="bold")) + 
      theme(plot.title = element_text(hjust = 0.5, size=20)) + 
      labs(x = "Group") + 
      labs(y = "Mean Evaluation") + 
      scale_y_continuous(limits = c(0, 4.5), breaks=seq(0,4.5,1)) + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size=15)) + 
      theme(axis.text.x= element_text(colour="black", size=13), 
            axis.text.y= element_text(colour="black", size=15))


p2 <- ggplot(China_assess_bygroup_2, aes(x=Outcome, y=mean, fill=Group)) +
      geom_bar(stat="identity", position = position_dodge(width = 0.8), color="black", size=0.4, alpha=0.4)+
      geom_errorbar(aes(x=Outcome, ymin=mean-me, ymax=mean+me), position = position_dodge(width = 0.8), 
                    width=0.4, colour="black", alpha=0.9, size=1.3) + 
      scale_fill_manual(values=c("white", "gray87", "gray28", "black")) + 
      theme_bw() + 
      theme(legend.position="none") + 
      ggtitle("Wave 2") + 
      theme(plot.title = element_text(face="bold")) + 
      theme(plot.title = element_text(hjust = 0.5, size=20)) + 
      labs(x = "Group") + 
      labs(y = "Mean Evaluation") + 
      scale_y_continuous(limits = c(0, 4.5), breaks=seq(0,4.5,1)) + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size=15)) + 
      theme(axis.text.x= element_text(colour="black", size=13), 
            axis.text.y= element_text(colour="black", size=15))

pdf("China_both.pdf",width = 8,height= 7, onefile=F)
ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off()



# ============================== Appendix E: Heterogenous Effects =============================
######### interactive models: ideology #######
#### Wave 1 ####
# reorder group
levels(Survey_Wave1_full$group)
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("positive", "negative", "additive", "control"))

m3ide_1 <- lm(gov_assess_1 ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:ideology_pca, 
            data=Survey_Wave1_full)
summary(m3ide_1)

m5ide_1 <- lm(gov_assess_2 ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:ideology_pca, 
            data=Survey_Wave1_full)
summary(m5ide_1)

m4ide_1 <- lm(gov_assess_3 ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:ideology_pca, 
            data=Survey_Wave1_full)
summary(m4ide_1)

m6ide_1 <- lm(gov_assess_4 ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:ideology_pca, 
            data=Survey_Wave1_full)
summary(m6ide_1)

m1ide_1 <- lm(china_overall_num ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:ideology_pca, 
            data=Survey_Wave1_full)
summary(m1ide_1)

m2ide_1 <- lm(china_gov_num ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:ideology_pca, 
            data=Survey_Wave1_full)
summary(m2ide_1)

# generate table
stargazer(m1ide_1, m2ide_1, m3ide_1, m4ide_1, m5ide_1, m6ide_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_1, m2ide_1, m3ide_1, m4ide_1, m5ide_1, m6ide_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))

m3ide_nc_1 <- lm(gov_assess_1 ~ 
                 ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m3ide_nc_1)

m5ide_nc_1 <- lm(gov_assess_2 ~ 
                 ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m5ide_nc_1)

m4ide_nc_1 <- lm(gov_assess_3 ~ 
                 ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m4ide_nc_1)

m6ide_nc_1 <- lm(gov_assess_4 ~ 
                 ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m6ide_nc_1)

m1ide_nc_1 <- lm(china_overall_num ~ 
                 ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m1ide_nc_1)

m2ide_nc_1 <- lm(china_gov_num ~ 
                 ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m2ide_nc_1)

# generate table
stargazer(m1ide_nc_1, m2ide_nc_1, m3ide_nc_1, m4ide_nc_1, m5ide_nc_1, m6ide_nc_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_nc_1, m2ide_nc_1, m3ide_nc_1, m4ide_nc_1, m5ide_nc_1, m6ide_nc_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# ============= Wave 1: plotting heterogeneous effect: ideology ===============
ideology_sim <- seq(min(Survey_Wave1_full$ideology_pca), max(Survey_Wave1_full$ideology_pca), 0.001)
ideology_sim

# ============= Wave 1: plotting ideology effect: China Overall (m1ide_1) ===============
# Control
# coefficients of control
coef(m1ide_1)[17]
# coefficients of control*ideology
coef(m1ide_1)[20]
# covariance of control & control
vcov(m1ide_1)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m1ide_1)[20,20]
# covariance of control & control*ideolgoy
vcov(m1ide_1)[17,20]
# values
y_esti_china_overall_ideology_control <- coef(m1ide_1)[17] + 
  ideology_sim*coef(m1ide_1)[20]
y_esti_china_overall_ideology_control
y_SE_china_overall_ideology_control <- 
  sqrt(vcov(m1ide_1)[17,17] + 
         (ideology_sim)^2*vcov(m1ide_1)[20,20] + 
         2*ideology_sim*vcov(m1ide_1)[17,20])
y_SE_china_overall_ideology_control
max(y_esti_china_overall_ideology_control+(1.96*y_SE_china_overall_ideology_control))
min(y_esti_china_overall_ideology_control-(1.96*y_SE_china_overall_ideology_control))

# plot
pdf("margin_china_overall_ideology_control1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_overall_ideology_control, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_overall_ideology_control + (1.96*y_SE_china_overall_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_control - (1.96*y_SE_china_overall_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_control + (1.645*y_SE_china_overall_ideology_control), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_control - (1.645*y_SE_china_overall_ideology_control), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_control + (1.96*y_SE_china_overall_ideology_control)), rev(y_esti_china_overall_ideology_control - (1.96*y_SE_china_overall_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_control + (1.645*y_SE_china_overall_ideology_control)), rev(y_esti_china_overall_ideology_control - (1.645*y_SE_china_overall_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-a: No Propaganda: China Overall", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m1ide_1)[15]
# coefficients of negative*ideology
coef(m1ide_1)[18]
# covariance of negative & negative
vcov(m1ide_1)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m1ide_1)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m1ide_1)[15,18]
# values
y_esti_china_overall_ideology_negative <- coef(m1ide_1)[15] + 
  ideology_sim*coef(m1ide_1)[18]
y_esti_china_overall_ideology_negative
y_SE_china_overall_ideology_negative <- 
  sqrt(vcov(m1ide_1)[15,15] + 
         (ideology_sim)^2*vcov(m1ide_1)[18,18] + 
         2*ideology_sim*vcov(m1ide_1)[15,18])
y_SE_china_overall_ideology_negative
max(y_esti_china_overall_ideology_negative+(1.96*y_SE_china_overall_ideology_negative))
min(y_esti_china_overall_ideology_negative-(1.96*y_SE_china_overall_ideology_negative))

# plot
pdf("margin_china_overall_ideology_negative1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_overall_ideology_negative, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_overall_ideology_negative + (1.96*y_SE_china_overall_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_negative - (1.96*y_SE_china_overall_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_negative + (1.645*y_SE_china_overall_ideology_negative), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_negative - (1.645*y_SE_china_overall_ideology_negative), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_negative + (1.96*y_SE_china_overall_ideology_negative)), rev(y_esti_china_overall_ideology_negative - (1.96*y_SE_china_overall_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_negative + (1.645*y_SE_china_overall_ideology_negative)), rev(y_esti_china_overall_ideology_negative - (1.645*y_SE_china_overall_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-a: Negative Propaganda: China Overall", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m1ide_1)[16]
# coefficients of additive*ideology
coef(m1ide_1)[19]
# covariance of additive & additive
vcov(m1ide_1)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m1ide_1)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m1ide_1)[16,19]
# values
y_esti_china_overall_ideology_additive <- coef(m1ide_1)[16] + 
  ideology_sim*coef(m1ide_1)[19]
y_esti_china_overall_ideology_additive
y_SE_china_overall_ideology_additive <- 
  sqrt(vcov(m1ide_1)[16,16] + 
         (ideology_sim)^2*vcov(m1ide_1)[19,19] + 
         2*ideology_sim*vcov(m1ide_1)[16,19])
y_SE_china_overall_ideology_additive
max(y_esti_china_overall_ideology_additive+(1.96*y_SE_china_overall_ideology_additive))
min(y_esti_china_overall_ideology_additive-(1.96*y_SE_china_overall_ideology_additive))

# plot
pdf("margin_china_overall_ideology_additive1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_overall_ideology_additive, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_overall_ideology_additive + (1.96*y_SE_china_overall_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_additive - (1.96*y_SE_china_overall_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_additive + (1.645*y_SE_china_overall_ideology_additive), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_additive - (1.645*y_SE_china_overall_ideology_additive), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_additive + (1.96*y_SE_china_overall_ideology_additive)), rev(y_esti_china_overall_ideology_negative - (1.96*y_SE_china_overall_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_additive + (1.645*y_SE_china_overall_ideology_additive)), rev(y_esti_china_overall_ideology_negative - (1.645*y_SE_china_overall_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-a: Additive Propaganda: China Overall", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()

# ============== Wave 1: plotting ideology effect: Chinese Government (m2ide_1) ==========

# Control
# coefficients of control
coef(m2ide_1)[17]
# coefficients of control*ideology
coef(m2ide_1)[20]
# covariance of control & control
vcov(m2ide_1)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m2ide_1)[20,20]
# covariance of control & control*ideolgoy
vcov(m2ide_1)[17,20]
# values
y_esti_china_gov_ideology_control <- coef(m2ide_1)[17] + 
  ideology_sim*coef(m2ide_1)[20]
y_esti_china_gov_ideology_control
y_SE_china_gov_ideology_control <- 
  sqrt(vcov(m2ide_1)[17,17] + 
         (ideology_sim)^2*vcov(m2ide_1)[20,20] + 
         2*ideology_sim*vcov(m2ide_1)[17,20])
y_SE_china_gov_ideology_control
max(y_esti_china_gov_ideology_control+(1.96*y_SE_china_gov_ideology_control))
min(y_esti_china_gov_ideology_control-(1.96*y_SE_china_gov_ideology_control))

# plot
pdf("margin_china_gov_ideology_control1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_gov_ideology_control, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_gov_ideology_control + (1.96*y_SE_china_gov_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_control - (1.96*y_SE_china_gov_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_control + (1.645*y_SE_china_gov_ideology_control), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_control - (1.645*y_SE_china_gov_ideology_control), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_control + (1.96*y_SE_china_gov_ideology_control)), rev(y_esti_china_gov_ideology_control - (1.96*y_SE_china_gov_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_control + (1.645*y_SE_china_gov_ideology_control)), rev(y_esti_china_gov_ideology_control - (1.645*y_SE_china_gov_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-b: No Propaganda: Chinese Government", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m2ide_1)[15]
# coefficients of negative*ideology
coef(m2ide_1)[18]
# covariance of negative & negative
vcov(m2ide_1)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m2ide_1)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m2ide_1)[15,18]
# values
y_esti_china_gov_ideology_negative <- coef(m2ide_1)[15] + 
  ideology_sim*coef(m2ide_1)[18]
y_esti_china_gov_ideology_negative
y_SE_china_gov_ideology_negative <- 
  sqrt(vcov(m2ide_1)[15,15] + 
         (ideology_sim)^2*vcov(m2ide_1)[18,18] + 
         2*ideology_sim*vcov(m2ide_1)[15,18])
y_SE_china_gov_ideology_negative
max(y_esti_china_gov_ideology_negative+(1.96*y_SE_china_gov_ideology_negative))
# max 0.5569428
min(y_esti_china_gov_ideology_negative-(1.96*y_SE_china_gov_ideology_negative))
# min -1.228653

# plot
pdf("margin_china_gov_ideology_negative1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_gov_ideology_negative, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_gov_ideology_negative + (1.96*y_SE_china_gov_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_negative - (1.96*y_SE_china_gov_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_negative + (1.645*y_SE_china_gov_ideology_negative), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_negative - (1.645*y_SE_china_gov_ideology_negative), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_negative + (1.96*y_SE_china_gov_ideology_negative)), rev(y_esti_china_gov_ideology_negative - (1.96*y_SE_china_gov_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_negative + (1.645*y_SE_china_gov_ideology_negative)), rev(y_esti_china_gov_ideology_negative - (1.645*y_SE_china_gov_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-b: Negative Propaganda: Chinese Government", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m2ide_1)[16]
# coefficients of additive*ideology
coef(m2ide_1)[19]
# covariance of additive & additive
vcov(m2ide_1)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m2ide_1)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m2ide_1)[16,19]
# values
y_esti_china_gov_ideology_additive <- coef(m2ide_1)[16] + 
  ideology_sim*coef(m2ide_1)[19]
y_esti_china_gov_ideology_additive
y_SE_china_gov_ideology_additive <- 
  sqrt(vcov(m2ide_1)[16,16] + 
         (ideology_sim)^2*vcov(m2ide_1)[19,19] + 
         2*ideology_sim*vcov(m2ide_1)[16,19])
y_SE_china_gov_ideology_additive
max(y_esti_china_gov_ideology_additive+(1.96*y_SE_china_gov_ideology_additive))
min(y_esti_china_gov_ideology_additive-(1.96*y_SE_china_gov_ideology_additive))

# plot
pdf("margin_china_gov_ideology_additive1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_gov_ideology_additive, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_gov_ideology_additive + (1.96*y_SE_china_gov_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_additive - (1.96*y_SE_china_gov_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_additive + (1.645*y_SE_china_gov_ideology_additive), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_additive - (1.645*y_SE_china_gov_ideology_additive), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_additive + (1.96*y_SE_china_gov_ideology_additive)), rev(y_esti_china_gov_ideology_additive - (1.96*y_SE_china_gov_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_additive + (1.645*y_SE_china_gov_ideology_additive)), rev(y_esti_china_gov_ideology_additive - (1.645*y_SE_china_gov_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-b: Additive Propaganda: Chinese Government", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()

# ============= Wave 1: plotting ideology effect: China COVID (m3ide_1) ==============

# Control
# coefficients of control
coef(m3ide_1)[17]
# coefficients of control*ideology
coef(m3ide_1)[20]
# covariance of control & control
vcov(m3ide_1)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m3ide_1)[20,20]
# covariance of control & control*ideolgoy
vcov(m3ide_1)[17,20]
# values
y_esti_gov_assess_1_ideology_control <- coef(m3ide_1)[17] + 
  ideology_sim*coef(m3ide_1)[20]
y_esti_gov_assess_1_ideology_control
y_SE_gov_assess_1_ideology_control <- 
  sqrt(vcov(m3ide_1)[17,17] + 
         (ideology_sim)^2*vcov(m3ide_1)[20,20] + 
         2*ideology_sim*vcov(m3ide_1)[17,20])
y_SE_gov_assess_1_ideology_control
max(y_esti_gov_assess_1_ideology_control+(1.96*y_SE_gov_assess_1_ideology_control))
min(y_esti_gov_assess_1_ideology_control-(1.96*y_SE_gov_assess_1_ideology_control))

# plot
pdf("margin_gov_assess_1_ideology_control1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_1_ideology_control, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_1_ideology_control + (1.96*y_SE_gov_assess_1_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_control - (1.96*y_SE_gov_assess_1_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_control + (1.645*y_SE_gov_assess_1_ideology_control), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_control - (1.645*y_SE_gov_assess_1_ideology_control), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_control + (1.96*y_SE_gov_assess_1_ideology_control)), rev(y_esti_gov_assess_1_ideology_control - (1.96*y_SE_gov_assess_1_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_control + (1.645*y_SE_gov_assess_1_ideology_control)), rev(y_esti_gov_assess_1_ideology_control - (1.645*y_SE_gov_assess_1_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-c: No Propaganda: China COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m3ide_1)[15]
# coefficients of negative*ideology
coef(m3ide_1)[18]
# covariance of negative & negative
vcov(m3ide_1)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m3ide_1)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m3ide_1)[15,18]
# values
y_esti_gov_assess_1_ideology_negative <- coef(m3ide_1)[15] + 
  ideology_sim*coef(m3ide_1)[18]
y_esti_gov_assess_1_ideology_negative
y_SE_gov_assess_1_ideology_negative <- 
  sqrt(vcov(m3ide_1)[15,15] + 
         (ideology_sim)^2*vcov(m3ide_1)[18,18] + 
         2*ideology_sim*vcov(m3ide_1)[15,18])
y_SE_gov_assess_1_ideology_negative
max(y_esti_gov_assess_1_ideology_negative+(1.96*y_SE_gov_assess_1_ideology_negative))
# max 0.593288
min(y_esti_gov_assess_1_ideology_negative-(1.96*y_SE_gov_assess_1_ideology_negative))
# min -1.129502

# plot
pdf("margin_gov_assess_1_ideology_negative1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_1_ideology_negative, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_1_ideology_negative + (1.96*y_SE_gov_assess_1_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_negative - (1.96*y_SE_gov_assess_1_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_negative + (1.645*y_SE_gov_assess_1_ideology_negative), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_negative - (1.645*y_SE_gov_assess_1_ideology_negative), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_negative + (1.96*y_SE_gov_assess_1_ideology_negative)), rev(y_esti_gov_assess_1_ideology_negative - (1.96*y_SE_gov_assess_1_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_negative + (1.645*y_SE_gov_assess_1_ideology_negative)), rev(y_esti_gov_assess_1_ideology_negative - (1.645*y_SE_gov_assess_1_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-c: Negative Propaganda: China COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m3ide_1)[16]
# coefficients of additive*ideology
coef(m3ide_1)[19]
# covariance of additive & additive
vcov(m3ide_1)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m3ide_1)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m3ide_1)[16,19]
# values
y_esti_gov_assess_1_ideology_additive <- coef(m3ide_1)[16] + 
  ideology_sim*coef(m3ide_1)[19]
y_esti_gov_assess_1_ideology_additive
y_SE_gov_assess_1_ideology_additive <- 
  sqrt(vcov(m3ide_1)[16,16] + 
         (ideology_sim)^2*vcov(m3ide_1)[19,19] + 
         2*ideology_sim*vcov(m3ide_1)[16,19])
y_SE_gov_assess_1_ideology_additive
max(y_esti_gov_assess_1_ideology_additive+(1.96*y_SE_gov_assess_1_ideology_additive))
# max 1.333285
min(y_esti_gov_assess_1_ideology_additive-(1.96*y_SE_gov_assess_1_ideology_additive))
# min -0.8652915

# plot
pdf("margin_gov_assess_1_ideology_additive1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_1_ideology_additive, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_1_ideology_additive + (1.96*y_SE_gov_assess_1_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_additive - (1.96*y_SE_gov_assess_1_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_additive + (1.645*y_SE_gov_assess_1_ideology_additive), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_additive - (1.645*y_SE_gov_assess_1_ideology_additive), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_additive + (1.96*y_SE_gov_assess_1_ideology_additive)), rev(y_esti_gov_assess_1_ideology_additive - (1.96*y_SE_gov_assess_1_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_additive + (1.645*y_SE_gov_assess_1_ideology_additive)), rev(y_esti_gov_assess_1_ideology_additive - (1.645*y_SE_gov_assess_1_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-c: Additive Propaganda: China COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()



# ========== Wave 1: plotting ideology effect: China COVID-Students (m4ide_1) ===========
# Control
# coefficients of control
coef(m4ide_1)[17]
# coefficients of control*ideology
coef(m4ide_1)[20]
# covariance of control & control
vcov(m4ide_1)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m4ide_1)[20,20]
# covariance of control & control*ideolgoy
vcov(m4ide_1)[17,20]
# values
y_esti_gov_assess_3_ideology_control <- coef(m4ide_1)[17] + 
  ideology_sim*coef(m4ide_1)[20]
y_esti_gov_assess_3_ideology_control
y_SE_gov_assess_3_ideology_control <- 
  sqrt(vcov(m4ide_1)[17,17] + 
         (ideology_sim)^2*vcov(m4ide_1)[20,20] + 
         2*ideology_sim*vcov(m4ide_1)[17,20])
y_SE_gov_assess_3_ideology_control
max(y_esti_gov_assess_3_ideology_control+(1.96*y_SE_gov_assess_3_ideology_control))
min(y_esti_gov_assess_3_ideology_control-(1.96*y_SE_gov_assess_3_ideology_control))

# plot
pdf("margin_gov_assess_3_ideology_control1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_3_ideology_control, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_3_ideology_control + (1.96*y_SE_gov_assess_3_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_control - (1.96*y_SE_gov_assess_3_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_control + (1.645*y_SE_gov_assess_3_ideology_control), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_control - (1.645*y_SE_gov_assess_3_ideology_control), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_control + (1.96*y_SE_gov_assess_3_ideology_control)), rev(y_esti_gov_assess_3_ideology_control - (1.96*y_SE_gov_assess_3_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_control + (1.645*y_SE_gov_assess_3_ideology_control)), rev(y_esti_gov_assess_3_ideology_control - (1.645*y_SE_gov_assess_3_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-d: No Propaganda: China COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=3.5, y0=1.3, x1=4, y1=1.3, lty=2, lwd = 2)
text(x=2.8, y=1.3, labels = "95% CI")
segments(x0=3.5, y0=1.1, x1=4, y1=1.1, lty=3, lwd = 2)
text(x=2.8, y=1.1, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m4ide_1)[15]
# coefficients of negative*ideology
coef(m4ide_1)[18]
# covariance of negative & negative
vcov(m4ide_1)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m4ide_1)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m4ide_1)[15,18]
# values
y_esti_gov_assess_3_ideology_negative <- coef(m4ide_1)[15] + 
  ideology_sim*coef(m4ide_1)[18]
y_esti_gov_assess_3_ideology_negative
y_SE_gov_assess_3_ideology_negative <- 
  sqrt(vcov(m4ide_1)[15,15] + 
         (ideology_sim)^2*vcov(m4ide_1)[18,18] + 
         2*ideology_sim*vcov(m4ide_1)[15,18])
y_SE_gov_assess_3_ideology_negative
max(y_esti_gov_assess_3_ideology_negative+(1.96*y_SE_gov_assess_3_ideology_negative))
# max 1.270406
min(y_esti_gov_assess_3_ideology_negative-(1.96*y_SE_gov_assess_3_ideology_negative))
# min -1.538248

# plot
pdf("margin_gov_assess_3_ideology_negative1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_3_ideology_negative, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_3_ideology_negative + (1.96*y_SE_gov_assess_3_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_negative - (1.96*y_SE_gov_assess_3_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_negative + (1.645*y_SE_gov_assess_3_ideology_negative), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_negative - (1.645*y_SE_gov_assess_3_ideology_negative), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_negative + (1.96*y_SE_gov_assess_3_ideology_negative)), rev(y_esti_gov_assess_3_ideology_negative - (1.96*y_SE_gov_assess_3_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_negative + (1.645*y_SE_gov_assess_3_ideology_negative)), rev(y_esti_gov_assess_3_ideology_negative - (1.645*y_SE_gov_assess_3_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-d: Negative Propaganda: China COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=3.5, y0=1.3, x1=4, y1=1.3, lty=2, lwd = 2)
text(x=2.8, y=1.3, labels = "95% CI")
segments(x0=3.5, y0=1.1, x1=4, y1=1.1, lty=3, lwd = 2)
text(x=2.8, y=1.1, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m4ide_1)[16]
# coefficients of additive*ideology
coef(m4ide_1)[19]
# covariance of additive & additive
vcov(m4ide_1)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m4ide_1)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m4ide_1)[16,19]
# values
y_esti_gov_assess_3_ideology_additive <- coef(m4ide_1)[16] + 
  ideology_sim*coef(m4ide_1)[19]
y_esti_gov_assess_3_ideology_additive
y_SE_gov_assess_3_ideology_additive <- 
  sqrt(vcov(m4ide_1)[16,16] + 
         (ideology_sim)^2*vcov(m4ide_1)[19,19] + 
         2*ideology_sim*vcov(m4ide_1)[16,19])
y_SE_gov_assess_3_ideology_additive
max(y_esti_gov_assess_3_ideology_additive+(1.96*y_SE_gov_assess_3_ideology_additive))
min(y_esti_gov_assess_3_ideology_additive-(1.96*y_SE_gov_assess_3_ideology_additive))

# plot
pdf("margin_gov_assess_3_ideology_additive1.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_3_ideology_additive, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_3_ideology_additive + (1.96*y_SE_gov_assess_3_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_additive - (1.96*y_SE_gov_assess_3_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_additive + (1.645*y_SE_gov_assess_3_ideology_additive), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_additive - (1.645*y_SE_gov_assess_3_ideology_additive), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_additive + (1.96*y_SE_gov_assess_3_ideology_additive)), rev(y_esti_gov_assess_3_ideology_additive - (1.96*y_SE_gov_assess_3_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_additive + (1.645*y_SE_gov_assess_3_ideology_additive)), rev(y_esti_gov_assess_3_ideology_additive - (1.645*y_SE_gov_assess_3_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-d: Additive Propaganda: China COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=3.5, y0=1.3, x1=4, y1=1.3, lty=2, lwd = 2)
text(x=2.8, y=1.3, labels = "95% CI")
segments(x0=3.5, y0=1.1, x1=4, y1=1.1, lty=3, lwd = 2)
text(x=2.8, y=1.1, labels = "90% CI")
dev.off()


#### Wave 2 ####
# reorder group
levels(Survey_Wave2_full$group)
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("positive", "negative", "additive", "control"))

m3ide_2 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
              data=Survey_Wave2_full)
summary(m3ide_2)

m5ide_2 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
              data=Survey_Wave2_full)
summary(m5ide_2)

m4ide_2 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
              data=Survey_Wave2_full)
summary(m4ide_2)

m6ide_2 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
              data=Survey_Wave2_full)
summary(m6ide_2)

m1ide_2 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
              data=Survey_Wave2_full)
summary(m1ide_2)

m2ide_2 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
              data=Survey_Wave2_full)
summary(m2ide_2)

# generate table
stargazer(m1ide_2, m2ide_2, m3ide_2, m4ide_2, m5ide_2, m6ide_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_2, m2ide_2, m3ide_2, m4ide_2, m5ide_2, m6ide_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))

m3ide_nc_2 <- lm(gov_assess_1 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m3ide_nc_2)

m5ide_nc_2 <- lm(gov_assess_2 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m5ide_nc_2)

m4ide_nc_2 <- lm(gov_assess_3 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m4ide_nc_2)

m6ide_nc_2 <- lm(gov_assess_4 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m6ide_nc_2)

m1ide_nc_2 <- lm(china_overall_num ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m1ide_nc_2)

m2ide_nc_2 <- lm(china_gov_num ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m2ide_nc_2)

# generate table
stargazer(m1ide_nc_2, m2ide_nc_2, m3ide_nc_2, m4ide_nc_2, m5ide_nc_2, m6ide_nc_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_nc_2, m2ide_nc_2, m3ide_nc_2, m4ide_nc_2, m5ide_nc_2, m6ide_nc_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# ============= Wave 2: plotting heterogeneous effect: ideology ===============
ideology_sim <- seq(min(Survey_Wave2_full$ideology_pca), max(Survey_Wave2_full$ideology_pca), 0.001)
ideology_sim

# ============= Wave 2: plotting ideology effect: China Overall (m1ide_2) ===============
# Control
# coefficients of control
coef(m1ide_2)[17]
# coefficients of control*ideology
coef(m1ide_2)[20]
# covariance of control & control
vcov(m1ide_2)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m1ide_2)[20,20]
# covariance of control & control*ideolgoy
vcov(m1ide_2)[17,20]
# values
y_esti_china_overall_ideology_control <- coef(m1ide_2)[17] + 
  ideology_sim*coef(m1ide_2)[20]
y_esti_china_overall_ideology_control
y_SE_china_overall_ideology_control <- 
  sqrt(vcov(m1ide_2)[17,17] + 
         (ideology_sim)^2*vcov(m1ide_2)[20,20] + 
         2*ideology_sim*vcov(m1ide_2)[17,20])
y_SE_china_overall_ideology_control
max(y_esti_china_overall_ideology_control+(1.96*y_SE_china_overall_ideology_control))
min(y_esti_china_overall_ideology_control-(1.96*y_SE_china_overall_ideology_control))

# plot
pdf("margin_china_overall_ideology_control2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_overall_ideology_control, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_overall_ideology_control + (1.96*y_SE_china_overall_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_control - (1.96*y_SE_china_overall_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_control + (1.645*y_SE_china_overall_ideology_control), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_control - (1.645*y_SE_china_overall_ideology_control), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_control + (1.96*y_SE_china_overall_ideology_control)), rev(y_esti_china_overall_ideology_control - (1.96*y_SE_china_overall_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_control + (1.645*y_SE_china_overall_ideology_control)), rev(y_esti_china_overall_ideology_control - (1.645*y_SE_china_overall_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-a: No Propaganda: China Overall", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m1ide_2)[15]
# coefficients of negative*ideology
coef(m1ide_2)[18]
# covariance of negative & negative
vcov(m1ide_2)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m1ide_2)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m1ide_2)[15,18]
# values
y_esti_china_overall_ideology_negative <- coef(m1ide_2)[15] + 
  ideology_sim*coef(m1ide_2)[18]
y_esti_china_overall_ideology_negative
y_SE_china_overall_ideology_negative <- 
  sqrt(vcov(m1ide_2)[15,15] + 
         (ideology_sim)^2*vcov(m1ide_2)[18,18] + 
         2*ideology_sim*vcov(m1ide_2)[15,18])
y_SE_china_overall_ideology_negative
max(y_esti_china_overall_ideology_negative+(1.96*y_SE_china_overall_ideology_negative))
min(y_esti_china_overall_ideology_negative-(1.96*y_SE_china_overall_ideology_negative))

# plot
pdf("margin_china_overall_ideology_negative2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_overall_ideology_negative, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_overall_ideology_negative + (1.96*y_SE_china_overall_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_negative - (1.96*y_SE_china_overall_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_negative + (1.645*y_SE_china_overall_ideology_negative), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_negative - (1.645*y_SE_china_overall_ideology_negative), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_negative + (1.96*y_SE_china_overall_ideology_negative)), rev(y_esti_china_overall_ideology_negative - (1.96*y_SE_china_overall_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_negative + (1.645*y_SE_china_overall_ideology_negative)), rev(y_esti_china_overall_ideology_negative - (1.645*y_SE_china_overall_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-a: Negative Propaganda: China Overall", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m1ide_2)[16]
# coefficients of additive*ideology
coef(m1ide_2)[19]
# covariance of additive & additive
vcov(m1ide_2)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m1ide_2)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m1ide_2)[16,19]
# values
y_esti_china_overall_ideology_additive <- coef(m1ide_2)[16] + 
  ideology_sim*coef(m1ide_2)[19]
y_esti_china_overall_ideology_additive
y_SE_china_overall_ideology_additive <- 
  sqrt(vcov(m1ide_2)[16,16] + 
         (ideology_sim)^2*vcov(m1ide_2)[19,19] + 
         2*ideology_sim*vcov(m1ide_2)[16,19])
y_SE_china_overall_ideology_additive
max(y_esti_china_overall_ideology_additive+(1.96*y_SE_china_overall_ideology_additive))
min(y_esti_china_overall_ideology_additive-(1.96*y_SE_china_overall_ideology_additive))

# plot
pdf("margin_china_overall_ideology_additive2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_overall_ideology_additive, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_overall_ideology_additive + (1.96*y_SE_china_overall_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_additive - (1.96*y_SE_china_overall_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_additive + (1.645*y_SE_china_overall_ideology_additive), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_overall_ideology_additive - (1.645*y_SE_china_overall_ideology_additive), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_additive + (1.96*y_SE_china_overall_ideology_additive)), rev(y_esti_china_overall_ideology_negative - (1.96*y_SE_china_overall_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_overall_ideology_additive + (1.645*y_SE_china_overall_ideology_additive)), rev(y_esti_china_overall_ideology_negative - (1.645*y_SE_china_overall_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-a: Additive Propaganda: China Overall", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()

# ============== Wave 2: plotting ideology effect: Chinese Government (m2ide_2) ==========
# Control
# coefficients of control
coef(m2ide_2)[17]
# coefficients of control*ideology
coef(m2ide_2)[20]
# covariance of control & control
vcov(m2ide_2)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m2ide_2)[20,20]
# covariance of control & control*ideolgoy
vcov(m2ide_2)[17,20]
# values
y_esti_china_gov_ideology_control <- coef(m2ide_2)[17] + 
  ideology_sim*coef(m2ide_2)[20]
y_esti_china_gov_ideology_control
y_SE_china_gov_ideology_control <- 
  sqrt(vcov(m2ide_2)[17,17] + 
         (ideology_sim)^2*vcov(m2ide_2)[20,20] + 
         2*ideology_sim*vcov(m2ide_2)[17,20])
y_SE_china_gov_ideology_control
max(y_esti_china_gov_ideology_control+(1.96*y_SE_china_gov_ideology_control))
min(y_esti_china_gov_ideology_control-(1.96*y_SE_china_gov_ideology_control))

# plot
pdf("margin_china_gov_ideology_control2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_gov_ideology_control, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_gov_ideology_control + (1.96*y_SE_china_gov_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_control - (1.96*y_SE_china_gov_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_control + (1.645*y_SE_china_gov_ideology_control), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_control - (1.645*y_SE_china_gov_ideology_control), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_control + (1.96*y_SE_china_gov_ideology_control)), rev(y_esti_china_gov_ideology_control - (1.96*y_SE_china_gov_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_control + (1.645*y_SE_china_gov_ideology_control)), rev(y_esti_china_gov_ideology_control - (1.645*y_SE_china_gov_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-b: No Propaganda: Chinese Government", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m2ide_2)[15]
# coefficients of negative*ideology
coef(m2ide_2)[18]
# covariance of negative & negative
vcov(m2ide_2)[15,15]
# covariance of negative*ideology & negative*ideology
vcov(m2ide_2)[18,18]
# covariance of negative & negative*ideology
vcov(m2ide_2)[15,18]
# values
y_esti_china_gov_ideology_negative <- coef(m2ide_2)[15] + 
  ideology_sim*coef(m2ide_2)[18]
y_esti_china_gov_ideology_negative
y_SE_china_gov_ideology_negative <- 
  sqrt(vcov(m2ide_2)[15,15] + 
         (ideology_sim)^2*vcov(m2ide_2)[18,18] + 
         2*ideology_sim*vcov(m2ide_2)[15,18])
y_SE_china_gov_ideology_negative
max(y_esti_china_gov_ideology_negative+(1.96*y_SE_china_gov_ideology_negative))
min(y_esti_china_gov_ideology_negative-(1.96*y_SE_china_gov_ideology_negative))

# plot
pdf("margin_china_gov_ideology_negative2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_gov_ideology_negative, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_gov_ideology_negative + (1.96*y_SE_china_gov_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_negative - (1.96*y_SE_china_gov_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_negative + (1.645*y_SE_china_gov_ideology_negative), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_negative - (1.645*y_SE_china_gov_ideology_negative), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_negative + (1.96*y_SE_china_gov_ideology_negative)), rev(y_esti_china_gov_ideology_negative - (1.96*y_SE_china_gov_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_negative + (1.645*y_SE_china_gov_ideology_negative)), rev(y_esti_china_gov_ideology_negative - (1.645*y_SE_china_gov_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-b: Negative Propaganda: Chinese Government", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m2ide_2)[16]
# coefficients of additive*ideology
coef(m2ide_2)[19]
# covariance of additive & additive
vcov(m2ide_2)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m2ide_2)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m2ide_2)[16,19]
# values
y_esti_china_gov_ideology_additive <- coef(m2ide_2)[16] + 
  ideology_sim*coef(m2ide_2)[19]
y_esti_china_gov_ideology_additive
y_SE_china_gov_ideology_additive <- 
  sqrt(vcov(m2ide_2)[16,16] + 
         (ideology_sim)^2*vcov(m2ide_2)[19,19] + 
         2*ideology_sim*vcov(m2ide_2)[16,19])
y_SE_china_gov_ideology_additive
max(y_esti_china_gov_ideology_additive+(1.96*y_SE_china_gov_ideology_additive))
min(y_esti_china_gov_ideology_additive-(1.96*y_SE_china_gov_ideology_additive))

# plot
pdf("margin_china_gov_ideology_additive2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_china_gov_ideology_additive, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_china_gov_ideology_additive + (1.96*y_SE_china_gov_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_additive - (1.96*y_SE_china_gov_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_additive + (1.645*y_SE_china_gov_ideology_additive), lty=3, lwd=2)
lines(ideology_sim, y_esti_china_gov_ideology_additive - (1.645*y_SE_china_gov_ideology_additive), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_additive + (1.96*y_SE_china_gov_ideology_additive)), rev(y_esti_china_gov_ideology_additive - (1.96*y_SE_china_gov_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_china_gov_ideology_additive + (1.645*y_SE_china_gov_ideology_additive)), rev(y_esti_china_gov_ideology_additive - (1.645*y_SE_china_gov_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-b: Additive Propaganda: Chinese Government", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()

# ============= Wave 2: plotting ideology effect: China COVID (m3ide_2) ==============
# Control
# coefficients of control
coef(m3ide_2)[17]
# coefficients of control*ideology
coef(m3ide_2)[20]
# covariance of control & control
vcov(m3ide_2)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m3ide_2)[20,20]
# covariance of control & control*ideolgoy
vcov(m3ide_2)[17,20]
# values
y_esti_gov_assess_1_ideology_control <- coef(m3ide_2)[17] + 
  ideology_sim*coef(m3ide_2)[20]
y_esti_gov_assess_1_ideology_control
y_SE_gov_assess_1_ideology_control <- 
  sqrt(vcov(m3ide_2)[17,17] + 
         (ideology_sim)^2*vcov(m3ide_2)[20,20] + 
         2*ideology_sim*vcov(m3ide_2)[17,20])
y_SE_gov_assess_1_ideology_control
max(y_esti_gov_assess_1_ideology_control+(1.96*y_SE_gov_assess_1_ideology_control))
min(y_esti_gov_assess_1_ideology_control-(1.96*y_SE_gov_assess_1_ideology_control))

# plot
pdf("margin_gov_assess_1_ideology_control2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_1_ideology_control, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_1_ideology_control + (1.96*y_SE_gov_assess_1_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_control - (1.96*y_SE_gov_assess_1_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_control + (1.645*y_SE_gov_assess_1_ideology_control), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_control - (1.645*y_SE_gov_assess_1_ideology_control), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_control + (1.96*y_SE_gov_assess_1_ideology_control)), rev(y_esti_gov_assess_1_ideology_control - (1.96*y_SE_gov_assess_1_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_control + (1.645*y_SE_gov_assess_1_ideology_control)), rev(y_esti_gov_assess_1_ideology_control - (1.645*y_SE_gov_assess_1_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-c: No Propaganda: China COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m3ide_2)[15]
# coefficients of negative*ideology
coef(m3ide_2)[18]
# covariance of negative & negative
vcov(m3ide_2)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m3ide_2)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m3ide_2)[15,18]
# values
y_esti_gov_assess_1_ideology_negative <- coef(m3ide_2)[15] + 
  ideology_sim*coef(m3ide_2)[18]
y_esti_gov_assess_1_ideology_negative
y_SE_gov_assess_1_ideology_negative <- 
  sqrt(vcov(m3ide_2)[15,15] + 
         (ideology_sim)^2*vcov(m3ide_2)[18,18] + 
         2*ideology_sim*vcov(m3ide_2)[15,18])
y_SE_gov_assess_1_ideology_negative
max(y_esti_gov_assess_1_ideology_negative+(1.96*y_SE_gov_assess_1_ideology_negative))
min(y_esti_gov_assess_1_ideology_negative-(1.96*y_SE_gov_assess_1_ideology_negative))

# plot
pdf("margin_gov_assess_1_ideology_negative2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_1_ideology_negative, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_1_ideology_negative + (1.96*y_SE_gov_assess_1_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_negative - (1.96*y_SE_gov_assess_1_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_negative + (1.645*y_SE_gov_assess_1_ideology_negative), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_negative - (1.645*y_SE_gov_assess_1_ideology_negative), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_negative + (1.96*y_SE_gov_assess_1_ideology_negative)), rev(y_esti_gov_assess_1_ideology_negative - (1.96*y_SE_gov_assess_1_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_negative + (1.645*y_SE_gov_assess_1_ideology_negative)), rev(y_esti_gov_assess_1_ideology_negative - (1.645*y_SE_gov_assess_1_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-c: Negative Propaganda: China COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m3ide_2)[16]
# coefficients of additive*ideology
coef(m3ide_2)[19]
# covariance of additive & additive
vcov(m3ide_2)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m3ide_2)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m3ide_2)[16,19]
# values
y_esti_gov_assess_1_ideology_additive <- coef(m3ide_2)[16] + 
  ideology_sim*coef(m3ide_2)[19]
y_esti_gov_assess_1_ideology_additive
y_SE_gov_assess_1_ideology_additive <- 
  sqrt(vcov(m3ide_2)[16,16] + 
         (ideology_sim)^2*vcov(m3ide_2)[19,19] + 
         2*ideology_sim*vcov(m3ide_2)[16,19])
y_SE_gov_assess_1_ideology_additive
max(y_esti_gov_assess_1_ideology_additive+(1.96*y_SE_gov_assess_1_ideology_additive))
min(y_esti_gov_assess_1_ideology_additive-(1.96*y_SE_gov_assess_1_ideology_additive))

# plot
pdf("margin_gov_assess_1_ideology_additive2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_1_ideology_additive, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_1_ideology_additive + (1.96*y_SE_gov_assess_1_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_additive - (1.96*y_SE_gov_assess_1_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_additive + (1.645*y_SE_gov_assess_1_ideology_additive), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_1_ideology_additive - (1.645*y_SE_gov_assess_1_ideology_additive), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_additive + (1.96*y_SE_gov_assess_1_ideology_additive)), rev(y_esti_gov_assess_1_ideology_additive - (1.96*y_SE_gov_assess_1_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_1_ideology_additive + (1.645*y_SE_gov_assess_1_ideology_additive)), rev(y_esti_gov_assess_1_ideology_additive - (1.645*y_SE_gov_assess_1_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-c: Additive Propaganda: China COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=-4.2, y0=1.3, x1=-3.7, y1=1.3, lty=2, lwd = 2)
text(x=-3, y=1.3, labels = "95% CI")
segments(x0=-4.2, y0=1.1, x1=-3.7, y1=1.1, lty=3, lwd = 2)
text(x=-3, y=1.1, labels = "90% CI")
dev.off()



# ========== Wave 2: plotting ideology effect: China COVID-Students (m4ide_2) ===========
# Control
# coefficients of control
coef(m4ide_2)[17]
# coefficients of control*ideology
coef(m4ide_2)[20]
# covariance of control & control
vcov(m4ide_2)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m4ide_2)[20,20]
# covariance of control & control*ideolgoy
vcov(m4ide_2)[17,20]
# values
y_esti_gov_assess_3_ideology_control <- coef(m4ide_2)[17] + 
  ideology_sim*coef(m4ide_2)[20]
y_esti_gov_assess_3_ideology_control
y_SE_gov_assess_3_ideology_control <- 
  sqrt(vcov(m4ide_2)[17,17] + 
         (ideology_sim)^2*vcov(m4ide_2)[20,20] + 
         2*ideology_sim*vcov(m4ide_2)[17,20])
y_SE_gov_assess_3_ideology_control
max(y_esti_gov_assess_3_ideology_control+(1.96*y_SE_gov_assess_3_ideology_control))
min(y_esti_gov_assess_3_ideology_control-(1.96*y_SE_gov_assess_3_ideology_control))

# plot
pdf("margin_gov_assess_3_ideology_control2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_3_ideology_control, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_3_ideology_control + (1.96*y_SE_gov_assess_3_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_control - (1.96*y_SE_gov_assess_3_ideology_control), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_control + (1.645*y_SE_gov_assess_3_ideology_control), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_control - (1.645*y_SE_gov_assess_3_ideology_control), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_control + (1.96*y_SE_gov_assess_3_ideology_control)), rev(y_esti_gov_assess_3_ideology_control - (1.96*y_SE_gov_assess_3_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_control + (1.645*y_SE_gov_assess_3_ideology_control)), rev(y_esti_gov_assess_3_ideology_control - (1.645*y_SE_gov_assess_3_ideology_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-d: No Propaganda: China COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=3.5, y0=1.3, x1=4, y1=1.3, lty=2, lwd = 2)
text(x=2.8, y=1.3, labels = "95% CI")
segments(x0=3.5, y0=1.1, x1=4, y1=1.1, lty=3, lwd = 2)
text(x=2.8, y=1.1, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m4ide_2)[15]
# coefficients of negative*ideology
coef(m4ide_2)[18]
# covariance of negative & negative
vcov(m4ide_2)[15,15]
# covariance of negative*ideology & negative*ideology
vcov(m4ide_2)[18,18]
# covariance of negative & negative*ideology
vcov(m4ide_2)[15,18]
# values
y_esti_gov_assess_3_ideology_negative <- coef(m4ide_2)[15] + 
  ideology_sim*coef(m4ide_2)[18]
y_esti_gov_assess_3_ideology_negative
y_SE_gov_assess_3_ideology_negative <- 
  sqrt(vcov(m4ide_2)[15,15] + 
         (ideology_sim)^2*vcov(m4ide_2)[18,18] + 
         2*ideology_sim*vcov(m4ide_2)[15,18])
y_SE_gov_assess_3_ideology_negative
max(y_esti_gov_assess_3_ideology_negative+(1.96*y_SE_gov_assess_3_ideology_negative))
min(y_esti_gov_assess_3_ideology_negative-(1.96*y_SE_gov_assess_3_ideology_negative))

# plot
pdf("margin_gov_assess_3_ideology_negative2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_3_ideology_negative, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_3_ideology_negative + (1.96*y_SE_gov_assess_3_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_negative - (1.96*y_SE_gov_assess_3_ideology_negative), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_negative + (1.645*y_SE_gov_assess_3_ideology_negative), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_negative - (1.645*y_SE_gov_assess_3_ideology_negative), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_negative + (1.96*y_SE_gov_assess_3_ideology_negative)), rev(y_esti_gov_assess_3_ideology_negative - (1.96*y_SE_gov_assess_3_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_negative + (1.645*y_SE_gov_assess_3_ideology_negative)), rev(y_esti_gov_assess_3_ideology_negative - (1.645*y_SE_gov_assess_3_ideology_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-d: Negative Propaganda: China COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=3.5, y0=1.3, x1=4, y1=1.3, lty=2, lwd = 2)
text(x=2.8, y=1.3, labels = "95% CI")
segments(x0=3.5, y0=1.1, x1=4, y1=1.1, lty=3, lwd = 2)
text(x=2.8, y=1.1, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m4ide_2)[16]
# coefficients of additive*ideology
coef(m4ide_2)[19]
# covariance of additive & additive
vcov(m4ide_2)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m4ide_2)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m4ide_2)[16,19]
# values
y_esti_gov_assess_3_ideology_additive <- coef(m4ide_2)[16] + 
  ideology_sim*coef(m4ide_2)[19]
y_esti_gov_assess_3_ideology_additive
y_SE_gov_assess_3_ideology_additive <- 
  sqrt(vcov(m4ide_2)[16,16] + 
         (ideology_sim)^2*vcov(m4ide_2)[19,19] + 
         2*ideology_sim*vcov(m4ide_2)[16,19])
y_SE_gov_assess_3_ideology_additive
max(y_esti_gov_assess_3_ideology_additive+(1.96*y_SE_gov_assess_3_ideology_additive))
# max 0.8225778
min(y_esti_gov_assess_3_ideology_additive-(1.96*y_SE_gov_assess_3_ideology_additive))
# min -0.9747498

# plot
pdf("margin_gov_assess_3_ideology_additive2.pdf")
par(mar=c(5,5,5,2))
plot(ideology_sim, y_esti_gov_assess_3_ideology_additive, t="l",lwd=2,xlab="Ideology",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,4), ylim=c(-1.5,1.5))
lines(ideology_sim, y_esti_gov_assess_3_ideology_additive + (1.96*y_SE_gov_assess_3_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_additive - (1.96*y_SE_gov_assess_3_ideology_additive), lty=2, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_additive + (1.645*y_SE_gov_assess_3_ideology_additive), lty=3, lwd=2)
lines(ideology_sim, y_esti_gov_assess_3_ideology_additive - (1.645*y_SE_gov_assess_3_ideology_additive), lty=3, lwd=2)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_additive + (1.96*y_SE_gov_assess_3_ideology_additive)), rev(y_esti_gov_assess_3_ideology_additive - (1.96*y_SE_gov_assess_3_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(ideology_sim, rev(ideology_sim)), 
        c((y_esti_gov_assess_3_ideology_additive + (1.645*y_SE_gov_assess_3_ideology_additive)), rev(y_esti_gov_assess_3_ideology_additive - (1.645*y_SE_gov_assess_3_ideology_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-d: Additive Propaganda: China COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=3.5, y0=1.3, x1=4, y1=1.3, lty=2, lwd = 2)
text(x=2.8, y=1.3, labels = "95% CI")
segments(x0=3.5, y0=1.1, x1=4, y1=1.1, lty=3, lwd = 2)
text(x=2.8, y=1.1, labels = "90% CI")
dev.off()


######### interactive models: Health Kit #######
#### Wave 1 ####
# reorder group
levels(Survey_Wave1_full$group)
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("positive", "negative", "additive", "control"))

m3kit_1 <- lm(gov_assess_1 ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:kit_got_2, 
              data=Survey_Wave1_full)
summary(m3kit_1)

m5kit_1 <- lm(gov_assess_2 ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:kit_got_2, 
              data=Survey_Wave1_full)
summary(m5kit_1)

m4kit_1 <- lm(gov_assess_3 ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:kit_got_2, 
              data=Survey_Wave1_full)
summary(m4kit_1)

m6kit_1 <- lm(gov_assess_4 ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:kit_got_2, 
              data=Survey_Wave1_full)
summary(m6kit_1)

m1kit_1 <- lm(china_overall_num ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:kit_got_2, 
              data=Survey_Wave1_full)
summary(m1kit_1)

m2kit_1 <- lm(china_gov_num ~ 
              female + education_gra + STEM + family_regime_tie + 
              ideology_pca + socialized_US + race_experience + know_pos + income_us + 
              COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
              group:kit_got_2, 
              data=Survey_Wave1_full)
summary(m2kit_1)

# generate table
stargazer(m1kit_1, m2kit_1, m3kit_1, m4kit_1, m5kit_1, m6kit_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_1, m2kit_1, m3kit_1, m4kit_1, m5kit_1, m6kit_1, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))     


m3kit_nc_1 <- lm(gov_assess_1 ~ kit_got_2 + group + group:kit_got_2,
              data=Survey_Wave1_full)
summary(m3kit_nc_1)

m5kit_nc_1 <- lm(gov_assess_2 ~ kit_got_2 + group + group:kit_got_2, 
              data=Survey_Wave1_full)
summary(m5kit_nc_1)

m4kit_nc_1 <- lm(gov_assess_3 ~kit_got_2 + group + group:kit_got_2,
              data=Survey_Wave1_full)
summary(m4kit_nc_1)

m6kit_nc_1 <- lm(gov_assess_4 ~kit_got_2 + group + group:kit_got_2,
              data=Survey_Wave1_full)
summary(m6kit_nc_1)

m1kit_nc_1 <- lm(china_overall_num ~kit_got_2 + group + group:kit_got_2,
              data=Survey_Wave1_full)
summary(m1kit_nc_1)

m2kit_nc_1 <- lm(china_gov_num ~kit_got_2 + group + group:kit_got_2,
              data=Survey_Wave1_full)
summary(m2kit_nc_1)

# generate table
stargazer(m1kit_nc_1, m2kit_nc_1, m3kit_nc_1, m4kit_nc_1, m5kit_nc_1, m6kit_nc_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_nc_1, m2kit_nc_1, m3kit_nc_1, m4kit_nc_1, m5kit_nc_1, m6kit_nc_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))     


# ============ Wave 1: plotting heterogeneous effect: health kit  ==============
# x axis
x_esti <-c(-0.1,0.9,1.9,0.1,1.1,2.1)
x_esti

healthkit_sim <- c(0,1)
healthkit_sim

# ============= Wave 1: plotting health kit effect: China Overall & Government (m1kit_1, m2kit_1)===================
# China Overall (m1kit_1)
# Control
# coefficients of control
coef(m1kit_1)[17]
# coefficients of control*kit
coef(m1kit_1)[20]
# covariance of control & control
vcov(m1kit_1)[17,17]
# covariance of control*kit & control*kit
vcov(m1kit_1)[20,20]
# covariance of control & control*kit
vcov(m1kit_1)[17,20]
# values
y_esti_china_overall_kit_control <- coef(m1kit_1)[17] + 
  healthkit_sim*coef(m1kit_1)[20]
y_esti_china_overall_kit_control
y_SE_china_overall_kit_control <- 
  sqrt(vcov(m1kit_1)[17,17] + 
         (healthkit_sim)^2*vcov(m1kit_1)[20,20] + 
         2*healthkit_sim*vcov(m1kit_1)[17,20])
y_SE_china_overall_kit_control

# Negative
# coefficients of negative
coef(m1kit_1)[15]
# coefficients of negative*ideology
coef(m1kit_1)[18]
# covariance of negative & negative
vcov(m1kit_1)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m1kit_1)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m1kit_1)[15,18]
# values
y_esti_china_overall_kit_negative <- coef(m1kit_1)[15] + 
  healthkit_sim*coef(m1kit_1)[18]
y_esti_china_overall_kit_negative
y_SE_china_overall_kit_negative <- 
  sqrt(vcov(m1kit_1)[15,15] + 
         (healthkit_sim)^2*vcov(m1kit_1)[18,18] + 
         2*healthkit_sim*vcov(m1kit_1)[15,18])
y_SE_china_overall_kit_negative

# Additive
# coefficients of additive
coef(m1kit_1)[16]
# coefficients of additive*ideology
coef(m1kit_1)[19]
# covariance of additive & additive
vcov(m1kit_1)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m1kit_1)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m1kit_1)[16,19]
# values
y_esti_china_overall_kit_additive <- coef(m1kit_1)[16] + 
  healthkit_sim*coef(m1kit_1)[19]
y_esti_china_overall_kit_additive
y_SE_china_overall_kit_additive <- 
  sqrt(vcov(m1kit_1)[16,16] + 
         (healthkit_sim)^2*vcov(m1kit_1)[19,19] + 
         2*healthkit_sim*vcov(m1kit_1)[16,19])
y_SE_china_overall_kit_additive

# values
y_esti_china_overall_kit <-c(y_esti_china_overall_kit_control[1],
                             y_esti_china_overall_kit_negative[1],
                             y_esti_china_overall_kit_additive[1],
                             y_esti_china_overall_kit_control[2],
                             y_esti_china_overall_kit_negative[2],
                             y_esti_china_overall_kit_additive[2])
y_esti_china_overall_kit
y_SE_china_overall_kit <- c(y_SE_china_overall_kit_control[1],
                            y_SE_china_overall_kit_negative[1],
                            y_SE_china_overall_kit_additive[1],
                            y_SE_china_overall_kit_control[2],
                            y_SE_china_overall_kit_negative[2],
                            y_SE_china_overall_kit_additive[2])
y_SE_china_overall_kit

# Chinese Government (m2kit_1)
# Control
# coefficients of control
coef(m2kit_1)[17]
# coefficients of control*kit
coef(m2kit_1)[20]
# covariance of control & control
vcov(m2kit_1)[17,17]
# covariance of control*kit & control*kit
vcov(m2kit_1)[20,20]
# covariance of control & control*kit
vcov(m2kit_1)[17,20]
# values
y_esti_china_gov_kit_control <- coef(m2kit_1)[17] + 
  healthkit_sim*coef(m2kit_1)[20]
y_esti_china_gov_kit_control
y_SE_china_gov_kit_control <- 
  sqrt(vcov(m2kit_1)[17,17] + 
         (healthkit_sim)^2*vcov(m2kit_1)[20,20] + 
         2*healthkit_sim*vcov(m2kit_1)[17,20])
y_SE_china_gov_kit_control

# Negative
# coefficients of negative
coef(m2kit_1)[15]
# coefficients of negative*ideology
coef(m2kit_1)[18]
# covariance of negative & negative
vcov(m2kit_1)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m2kit_1)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m2kit_1)[15,18]
# values
y_esti_china_gov_kit_negative <- coef(m2kit_1)[15] + 
  healthkit_sim*coef(m2kit_1)[18]
y_esti_china_gov_kit_negative
y_SE_china_gov_kit_negative <- 
  sqrt(vcov(m2kit_1)[15,15] + 
         (healthkit_sim)^2*vcov(m2kit_1)[18,18] + 
         2*healthkit_sim*vcov(m2kit_1)[15,18])
y_SE_china_gov_kit_negative

# Additive
# coefficients of additive
coef(m2kit_1)[16]
# coefficients of additive*ideology
coef(m2kit_1)[19]
# covariance of additive & additive
vcov(m2kit_1)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m2kit_1)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m2kit_1)[16,19]
# values
y_esti_china_gov_kit_additive <- coef(m2kit_1)[16] + 
  healthkit_sim*coef(m2kit_1)[19]
y_esti_china_gov_kit_additive
y_SE_china_gov_kit_additive <- 
  sqrt(vcov(m2kit_1)[16,16] + 
         (healthkit_sim)^2*vcov(m2kit_1)[19,19] + 
         2*healthkit_sim*vcov(m2kit_1)[16,19])
y_SE_china_gov_kit_additive

# values
y_esti_china_gov_kit <-c(y_esti_china_gov_kit_control[1],
                         y_esti_china_gov_kit_negative[1],
                         y_esti_china_gov_kit_additive[1],
                         y_esti_china_gov_kit_control[2],
                         y_esti_china_gov_kit_negative[2],
                         y_esti_china_gov_kit_additive[2])
y_esti_china_gov_kit
y_SE_china_gov_kit <- c(y_SE_china_gov_kit_control[1],
                        y_SE_china_gov_kit_negative[1],
                        y_SE_china_gov_kit_additive[1],
                        y_SE_china_gov_kit_control[2],
                        y_SE_china_gov_kit_negative[2],
                        y_SE_china_gov_kit_additive[2])
y_SE_china_gov_kit

# plot
pdf("margin_china_overall_gov_kit1.pdf")
par(mar=c(8,5,5,2), xpd=NA)
par(mfrow=c(1,2))
# China Overall
plot(x_esti[1:3],y_esti_china_overall_kit[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-1,0.8), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], y_esti_china_overall_kit[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("A: China Overall", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0=y_esti_china_overall_kit[1:6]+(1.96*y_SE_china_overall_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_china_overall_kit[1:6]-(1.96*y_SE_china_overall_kit[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0=y_esti_china_overall_kit[1:6]+(1.645*y_SE_china_overall_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_china_overall_kit[1:6]-(1.645*y_SE_china_overall_kit[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# China Government
plot(x_esti[1:3],y_esti_china_gov_kit[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-1,0.8), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], y_esti_china_gov_kit[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("B: Chinese Government", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0=y_esti_china_gov_kit[1:6]+(1.96*y_SE_china_gov_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_china_gov_kit[1:6]-(1.96*y_SE_china_gov_kit[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0=y_esti_china_gov_kit[1:6]+(1.645*y_SE_china_gov_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_china_gov_kit[1:6]-(1.645*y_SE_china_gov_kit[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# legend
par(mfrow=c(1,1))
legend("bottom", inset = c(0, -0.35), legend = c("No Health Kit   ", "Health Kit Received"), pch = c(21,16), 
       bty = "n", pt.cex = 1.5, cex = 1.2, text.col = "black", horiz = TRUE)
dev.off()

# ============= Wave 1: plotting health kit effect: China COVID and to students (m3kit_1, m4kit_1)================================
# China COVID Response (m3kit_1)
# Control
# coefficients of control
coef(m3kit_1)[17]
# coefficients of control*kit
coef(m3kit_1)[20]
# covariance of control & control
vcov(m3kit_1)[17,17]
# covariance of control*kit & control*kit
vcov(m3kit_1)[20,20]
# covariance of control & control*kit
vcov(m3kit_1)[17,20]
# values
y_esti_gov_assess_1_kit_control <- coef(m3kit_1)[17] + 
  healthkit_sim*coef(m3kit_1)[20]
y_esti_gov_assess_1_kit_control
y_SE_gov_assess_1_kit_control <- 
  sqrt(vcov(m3kit_1)[17,17] + 
         (healthkit_sim)^2*vcov(m3kit_1)[20,20] + 
         2*healthkit_sim*vcov(m3kit_1)[17,20])
y_SE_gov_assess_1_kit_control

# Negative
# coefficients of negative
coef(m3kit_1)[15]
# coefficients of negative*ideology
coef(m3kit_1)[18]
# covariance of negative & negative
vcov(m3kit_1)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m3kit_1)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m3kit_1)[15,18]
# values
y_esti_gov_assess_1_kit_negative <- coef(m3kit_1)[15] + 
  healthkit_sim*coef(m3kit_1)[18]
y_esti_gov_assess_1_kit_negative
y_SE_gov_assess_1_kit_negative <- 
  sqrt(vcov(m3kit_1)[15,15] + 
         (healthkit_sim)^2*vcov(m3kit_1)[18,18] + 
         2*healthkit_sim*vcov(m3kit_1)[15,18])
y_SE_gov_assess_1_kit_negative

# Additive
# coefficients of additive
coef(m3kit_1)[16]
# coefficients of additive*ideology
coef(m3kit_1)[19]
# covariance of additive & additive
vcov(m3kit_1)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m3kit_1)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m3kit_1)[16,19]
# values
y_esti_gov_assess_1_kit_additive <- coef(m3kit_1)[16] + 
  healthkit_sim*coef(m3kit_1)[19]
y_esti_gov_assess_1_kit_additive
y_SE_gov_assess_1_kit_additive <- 
  sqrt(vcov(m3kit_1)[16,16] + 
         (healthkit_sim)^2*vcov(m3kit_1)[19,19] + 
         2*healthkit_sim*vcov(m3kit_1)[16,19])
y_SE_gov_assess_1_kit_additive

# values
y_esti_gov_assess_1_kit <-c(y_esti_gov_assess_1_kit_control[1],
                            y_esti_gov_assess_1_kit_negative[1],
                            y_esti_gov_assess_1_kit_additive[1],
                            y_esti_gov_assess_1_kit_control[2],
                            y_esti_gov_assess_1_kit_negative[2],
                            y_esti_gov_assess_1_kit_additive[2])
y_esti_gov_assess_1_kit
y_SE_gov_assess_1_kit <- c(y_SE_gov_assess_1_kit_control[1],
                           y_SE_gov_assess_1_kit_negative[1],
                           y_SE_gov_assess_1_kit_additive[1],
                           y_SE_gov_assess_1_kit_control[2],
                           y_SE_gov_assess_1_kit_negative[2],
                           y_SE_gov_assess_1_kit_additive[2])
y_SE_gov_assess_1_kit

# Chinese COVID Response: Students (m4kit_1)
# Control
# coefficients of control
coef(m4kit_1)[17]
# coefficients of control*kit
coef(m4kit_1)[20]
# covariance of control & control
vcov(m4kit_1)[17,17]
# covariance of control*kit & control*kit
vcov(m4kit_1)[20,20]
# covariance of control & control*kit
vcov(m4kit_1)[17,20]
# values
y_esti_gov_assess_3_kit_control <- coef(m4kit_1)[17] + 
  healthkit_sim*coef(m4kit_1)[20]
y_esti_gov_assess_3_kit_control
y_SE_gov_assess_3_kit_control <- 
  sqrt(vcov(m4kit_1)[17,17] + 
         (healthkit_sim)^2*vcov(m4kit_1)[20,20] + 
         2*healthkit_sim*vcov(m4kit_1)[17,20])
y_SE_gov_assess_3_kit_control

# Negative
# coefficients of negative
coef(m4kit_1)[15]
# coefficients of negative*ideology
coef(m4kit_1)[18]
# covariance of negative & negative
vcov(m4kit_1)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m4kit_1)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m4kit_1)[15,18]
# values
y_esti_gov_assess_3_kit_negative <- coef(m4kit_1)[15] + 
  healthkit_sim*coef(m4kit_1)[18]
y_esti_gov_assess_3_kit_negative
y_SE_gov_assess_3_kit_negative <- 
  sqrt(vcov(m4kit_1)[15,15] + 
         (healthkit_sim)^2*vcov(m4kit_1)[18,18] + 
         2*healthkit_sim*vcov(m4kit_1)[15,18])
y_SE_gov_assess_3_kit_negative

# Additive
# coefficients of additive
coef(m4kit_1)[16]
# coefficients of additive*ideology
coef(m4kit_1)[19]
# covariance of additive & additive
vcov(m4kit_1)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m4kit_1)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m4kit_1)[16,19]
# values
y_esti_gov_assess_3_kit_additive <- coef(m4kit_1)[16] + 
  healthkit_sim*coef(m4kit_1)[19]
y_esti_gov_assess_3_kit_additive
y_SE_gov_assess_3_kit_additive <- 
  sqrt(vcov(m4kit_1)[16,16] + 
         (healthkit_sim)^2*vcov(m4kit_1)[19,19] + 
         2*healthkit_sim*vcov(m4kit_1)[16,19])
y_SE_gov_assess_3_kit_additive

# values
y_esti_gov_assess_3_kit <-c(y_esti_gov_assess_3_kit_control[1],
                            y_esti_gov_assess_3_kit_negative[1],
                            y_esti_gov_assess_3_kit_additive[1],
                            y_esti_gov_assess_3_kit_control[2],
                            y_esti_gov_assess_3_kit_negative[2],
                            y_esti_gov_assess_3_kit_additive[2])
y_esti_gov_assess_3_kit
y_SE_gov_assess_3_kit <- c(y_SE_gov_assess_3_kit_control[1],
                           y_SE_gov_assess_3_kit_negative[1],
                           y_SE_gov_assess_3_kit_additive[1],
                           y_SE_gov_assess_3_kit_control[2],
                           y_SE_gov_assess_3_kit_negative[2],
                           y_SE_gov_assess_3_kit_additive[2])
y_SE_gov_assess_3_kit

# plot
pdf("margin_gov_assess_1_3_kit1.pdf")
par(mar=c(8,5,5,2), xpd=NA)
par(mfrow=c(1,2))
# China COVID 
plot(x_esti[1:3],y_esti_gov_assess_1_kit[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-1,0.8), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], y_esti_gov_assess_1_kit[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("C: China COVID Re.", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0=y_esti_gov_assess_1_kit[1:6]+(1.96*y_SE_gov_assess_1_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_gov_assess_1_kit[1:6]-(1.96*y_SE_gov_assess_1_kit[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0=y_esti_gov_assess_1_kit[1:6]+(1.645*y_SE_gov_assess_1_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_gov_assess_1_kit[1:6]-(1.645*y_SE_gov_assess_1_kit[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# China COVID Students
plot(x_esti[1:3],y_esti_gov_assess_3_kit[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-1,0.8), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], y_esti_gov_assess_3_kit[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("D: China COVID Re.-Stud.", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0=y_esti_gov_assess_3_kit[1:6]+(1.96*y_SE_gov_assess_3_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_gov_assess_3_kit[1:6]-(1.96*y_SE_gov_assess_3_kit[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0=y_esti_gov_assess_3_kit[1:6]+(1.645*y_SE_gov_assess_3_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_gov_assess_3_kit[1:6]-(1.645*y_SE_gov_assess_3_kit[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# legend
par(mfrow=c(1,1))
legend("bottom", inset = c(0, -0.35), legend = c("No Health Kit   ", "Health Kit Received"), pch = c(21,16), 
       bty = "n", pt.cex = 1.5, cex = 1.2, text.col = "black", horiz = TRUE)
dev.off()



#### Wave 2 ####
# reorder group
levels(Survey_Wave2_full$group)
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("positive", "negative", "additive", "control"))

m3kit_2 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
              data=Survey_Wave2_full)
summary(m3kit_2)

m5kit_2 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
              data=Survey_Wave2_full)
summary(m5kit_2)

m4kit_2 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
              data=Survey_Wave2_full)
summary(m4kit_2)

m6kit_2 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
              data=Survey_Wave2_full)
summary(m6kit_2)

m1kit_2 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
              data=Survey_Wave2_full)
summary(m1kit_2)

m2kit_2 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
              data=Survey_Wave2_full)
summary(m2kit_2)

# generate table
stargazer(m1kit_2, m2kit_2, m3kit_2, m4kit_2, m5kit_2, m6kit_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_2, m2kit_2, m3kit_2, m4kit_2, m5kit_2, m6kit_2, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))     


m3kit_nc_2 <- lm(gov_assess_1 ~ kit_got_2 + group + group:kit_got_2,
                 data=Survey_Wave2_full)
summary(m3kit_nc_2)

m5kit_nc_2 <- lm(gov_assess_2 ~ kit_got_2 + group + group:kit_got_2, 
                 data=Survey_Wave2_full)
summary(m5kit_nc_2)

m4kit_nc_2 <- lm(gov_assess_3 ~kit_got_2 + group + group:kit_got_2,
                 data=Survey_Wave2_full)
summary(m4kit_nc_2)

m6kit_nc_2 <- lm(gov_assess_4 ~kit_got_2 + group + group:kit_got_2,
                 data=Survey_Wave2_full)
summary(m6kit_nc_2)

m1kit_nc_2 <- lm(china_overall_num ~kit_got_2 + group + group:kit_got_2,
                 data=Survey_Wave2_full)
summary(m1kit_nc_2)

m2kit_nc_2 <- lm(china_gov_num ~kit_got_2 + group + group:kit_got_2,
                 data=Survey_Wave2_full)
summary(m2kit_nc_2)

# generate table
stargazer(m1kit_nc_2, m2kit_nc_2, m3kit_nc_2, m4kit_nc_2, m5kit_nc_2, m6kit_nc_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_nc_2, m2kit_nc_2, m3kit_nc_2, m4kit_nc_2, m5kit_nc_2, m6kit_nc_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))     


# ========= Wave 2: plotting heterogeneous effect: health kit  ===============
# x axis
x_esti <-c(-0.1,0.9,1.9,0.1,1.1,2.1)
x_esti

healthkit_sim <- c(0,1)
healthkit_sim

# ========== Wave 2: plotting health kit effect: China Overall & Government (m1kit_2, m2kit_2) =========================
# China Overall (m1kit_2)
# Control
# coefficients of control
coef(m1kit_2)[17]
# coefficients of control*kit
coef(m1kit_2)[20]
# covariance of control & control
vcov(m1kit_2)[17,17]
# covariance of control*kit & control*kit
vcov(m1kit_2)[20,20]
# covariance of control & control*kit
vcov(m1kit_2)[17,20]
# values
y_esti_china_overall_kit_control <- coef(m1kit_2)[17] + 
  healthkit_sim*coef(m1kit_2)[20]
y_esti_china_overall_kit_control
y_SE_china_overall_kit_control <- 
  sqrt(vcov(m1kit_2)[17,17] + 
         (healthkit_sim)^2*vcov(m1kit_2)[20,20] + 
         2*healthkit_sim*vcov(m1kit_2)[17,20])
y_SE_china_overall_kit_control

# Negative
# coefficients of negative
coef(m1kit_2)[15]
# coefficients of negative*ideology
coef(m1kit_2)[18]
# covariance of negative & negative
vcov(m1kit_2)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m1kit_2)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m1kit_2)[15,18]
# values
y_esti_china_overall_kit_negative <- coef(m1kit_2)[15] + 
  healthkit_sim*coef(m1kit_2)[18]
y_esti_china_overall_kit_negative
y_SE_china_overall_kit_negative <- 
  sqrt(vcov(m1kit_2)[15,15] + 
         (healthkit_sim)^2*vcov(m1kit_2)[18,18] + 
         2*healthkit_sim*vcov(m1kit_2)[15,18])
y_SE_china_overall_kit_negative

# Additive
# coefficients of additive
coef(m1kit_2)[16]
# coefficients of additive*ideology
coef(m1kit_2)[19]
# covariance of additive & additive
vcov(m1kit_2)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m1kit_2)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m1kit_2)[16,19]
# values
y_esti_china_overall_kit_additive <- coef(m1kit_2)[16] + 
  healthkit_sim*coef(m1kit_2)[19]
y_esti_china_overall_kit_additive
y_SE_china_overall_kit_additive <- 
  sqrt(vcov(m1kit_2)[16,16] + 
         (healthkit_sim)^2*vcov(m1kit_2)[19,19] + 
         2*healthkit_sim*vcov(m1kit_2)[16,19])
y_SE_china_overall_kit_additive

# values
y_esti_china_overall_kit <-c(y_esti_china_overall_kit_control[1],
                             y_esti_china_overall_kit_negative[1],
                             y_esti_china_overall_kit_additive[1],
                             y_esti_china_overall_kit_control[2],
                             y_esti_china_overall_kit_negative[2],
                             y_esti_china_overall_kit_additive[2])
y_esti_china_overall_kit
y_SE_china_overall_kit <- c(y_SE_china_overall_kit_control[1],
                            y_SE_china_overall_kit_negative[1],
                            y_SE_china_overall_kit_additive[1],
                            y_SE_china_overall_kit_control[2],
                            y_SE_china_overall_kit_negative[2],
                            y_SE_china_overall_kit_additive[2])
y_SE_china_overall_kit

# Chinese Government (m2kit_2)
# Control
# coefficients of control
coef(m2kit_2)[17]
# coefficients of control*kit
coef(m2kit_2)[20]
# covariance of control & control
vcov(m2kit_2)[17,17]
# covariance of control*kit & control*kit
vcov(m2kit_2)[20,20]
# covariance of control & control*kit
vcov(m2kit_2)[17,20]
# values
y_esti_china_gov_kit_control <- coef(m2kit_2)[17] + 
  healthkit_sim*coef(m2kit_2)[20]
y_esti_china_gov_kit_control
y_SE_china_gov_kit_control <- 
  sqrt(vcov(m2kit_2)[17,17] + 
         (healthkit_sim)^2*vcov(m2kit_2)[20,20] + 
         2*healthkit_sim*vcov(m2kit_2)[17,20])
y_SE_china_gov_kit_control

# Negative
# coefficients of negative
coef(m2kit_2)[15]
# coefficients of negative*ideology
coef(m2kit_2)[18]
# covariance of negative & negative
vcov(m2kit_2)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m2kit_2)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m2kit_2)[15,18]
# values
y_esti_china_gov_kit_negative <- coef(m2kit_2)[15] + 
  healthkit_sim*coef(m2kit_2)[18]
y_esti_china_gov_kit_negative
y_SE_china_gov_kit_negative <- 
  sqrt(vcov(m2kit_2)[15,15] + 
         (healthkit_sim)^2*vcov(m2kit_2)[18,18] + 
         2*healthkit_sim*vcov(m2kit_2)[15,18])
y_SE_china_gov_kit_negative

# Additive
# coefficients of additive
coef(m2kit_2)[16]
# coefficients of additive*ideology
coef(m2kit_2)[19]
# covariance of additive & additive
vcov(m2kit_2)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m2kit_2)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m2kit_2)[16,19]
# values
y_esti_china_gov_kit_additive <- coef(m2kit_2)[16] + 
  healthkit_sim*coef(m2kit_2)[19]
y_esti_china_gov_kit_additive
y_SE_china_gov_kit_additive <- 
  sqrt(vcov(m2kit_2)[16,16] + 
         (healthkit_sim)^2*vcov(m2kit_2)[19,19] + 
         2*healthkit_sim*vcov(m2kit_2)[16,19])
y_SE_china_gov_kit_additive

# values
y_esti_china_gov_kit <-c(y_esti_china_gov_kit_control[1],
                         y_esti_china_gov_kit_negative[1],
                         y_esti_china_gov_kit_additive[1],
                         y_esti_china_gov_kit_control[2],
                         y_esti_china_gov_kit_negative[2],
                         y_esti_china_gov_kit_additive[2])
y_esti_china_gov_kit
y_SE_china_gov_kit <- c(y_SE_china_gov_kit_control[1],
                        y_SE_china_gov_kit_negative[1],
                        y_SE_china_gov_kit_additive[1],
                        y_SE_china_gov_kit_control[2],
                        y_SE_china_gov_kit_negative[2],
                        y_SE_china_gov_kit_additive[2])
y_SE_china_gov_kit

# plot
pdf("margin_china_overall_gov_kit2.pdf")
par(mar=c(8,5,5,2), xpd=NA)
par(mfrow=c(1,2))
# China Overall
plot(x_esti[1:3],y_esti_china_overall_kit[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-1,0.8), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], y_esti_china_overall_kit[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("A: China Overall", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0=y_esti_china_overall_kit[1:6]+(1.96*y_SE_china_overall_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_china_overall_kit[1:6]-(1.96*y_SE_china_overall_kit[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0=y_esti_china_overall_kit[1:6]+(1.645*y_SE_china_overall_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_china_overall_kit[1:6]-(1.645*y_SE_china_overall_kit[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# China Government
plot(x_esti[1:3],y_esti_china_gov_kit[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-1,0.8), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], y_esti_china_gov_kit[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("B: Chinese Government", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0=y_esti_china_gov_kit[1:6]+(1.96*y_SE_china_gov_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_china_gov_kit[1:6]-(1.96*y_SE_china_gov_kit[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0=y_esti_china_gov_kit[1:6]+(1.645*y_SE_china_gov_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_china_gov_kit[1:6]-(1.645*y_SE_china_gov_kit[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# legend
par(mfrow=c(1,1))
legend("bottom", inset = c(0, -0.35), legend = c("No Health Kit   ", "Health Kit Received"), pch = c(21,16), 
       bty = "n", pt.cex = 1.5, cex = 1.2, text.col = "black", horiz = TRUE)
dev.off()

# ============ Wave 2: plotting health kit effect: China COVID and to students (m3kit_2, m4kit_2)=================================
# China COVID Response (m3kit_2)
# Control
# coefficients of control
coef(m3kit_2)[17]
# coefficients of control*kit
coef(m3kit_2)[20]
# covariance of control & control
vcov(m3kit_2)[17,17]
# covariance of control*kit & control*kit
vcov(m3kit_2)[20,20]
# covariance of control & control*kit
vcov(m3kit_2)[17,20]
# values
y_esti_gov_assess_1_kit_control <- coef(m3kit_2)[17] + 
  healthkit_sim*coef(m3kit_2)[20]
y_esti_gov_assess_1_kit_control
y_SE_gov_assess_1_kit_control <- 
  sqrt(vcov(m3kit_2)[17,17] + 
         (healthkit_sim)^2*vcov(m3kit_2)[20,20] + 
         2*healthkit_sim*vcov(m3kit_2)[17,20])
y_SE_gov_assess_1_kit_control

# Negative
# coefficients of negative
coef(m3kit_2)[15]
# coefficients of negative*ideology
coef(m3kit_2)[18]
# covariance of negative & negative
vcov(m3kit_2)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m3kit_2)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m3kit_2)[15,18]
# values
y_esti_gov_assess_1_kit_negative <- coef(m3kit_2)[15] + 
  healthkit_sim*coef(m3kit_2)[18]
y_esti_gov_assess_1_kit_negative
y_SE_gov_assess_1_kit_negative <- 
  sqrt(vcov(m3kit_2)[15,15] + 
         (healthkit_sim)^2*vcov(m3kit_2)[18,18] + 
         2*healthkit_sim*vcov(m3kit_2)[15,18])
y_SE_gov_assess_1_kit_negative

# Additive
# coefficients of additive
coef(m3kit_2)[16]
# coefficients of additive*ideology
coef(m3kit_2)[19]
# covariance of additive & additive
vcov(m3kit_2)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m3kit_2)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m3kit_2)[16,19]
# values
y_esti_gov_assess_1_kit_additive <- coef(m3kit_2)[16] + 
  healthkit_sim*coef(m3kit_2)[19]
y_esti_gov_assess_1_kit_additive
y_SE_gov_assess_1_kit_additive <- 
  sqrt(vcov(m3kit_2)[16,16] + 
         (healthkit_sim)^2*vcov(m3kit_2)[19,19] + 
         2*healthkit_sim*vcov(m3kit_2)[16,19])
y_SE_gov_assess_1_kit_additive

# values
y_esti_gov_assess_1_kit <-c(y_esti_gov_assess_1_kit_control[1],
                            y_esti_gov_assess_1_kit_negative[1],
                            y_esti_gov_assess_1_kit_additive[1],
                            y_esti_gov_assess_1_kit_control[2],
                            y_esti_gov_assess_1_kit_negative[2],
                            y_esti_gov_assess_1_kit_additive[2])
y_esti_gov_assess_1_kit
y_SE_gov_assess_1_kit <- c(y_SE_gov_assess_1_kit_control[1],
                           y_SE_gov_assess_1_kit_negative[1],
                           y_SE_gov_assess_1_kit_additive[1],
                           y_SE_gov_assess_1_kit_control[2],
                           y_SE_gov_assess_1_kit_negative[2],
                           y_SE_gov_assess_1_kit_additive[2])
y_SE_gov_assess_1_kit

# Chinese COVID Response: Students (m4kit_2)
# Control
# coefficients of control
coef(m4kit_2)[17]
# coefficients of control*kit
coef(m4kit_2)[20]
# covariance of control & control
vcov(m4kit_2)[17,17]
# covariance of control*kit & control*kit
vcov(m4kit_2)[20,20]
# covariance of control & control*kit
vcov(m4kit_2)[17,20]
# values
y_esti_gov_assess_3_kit_control <- coef(m4kit_2)[17] + 
  healthkit_sim*coef(m4kit_2)[20]
y_esti_gov_assess_3_kit_control
y_SE_gov_assess_3_kit_control <- 
  sqrt(vcov(m4kit_2)[17,17] + 
         (healthkit_sim)^2*vcov(m4kit_2)[20,20] + 
         2*healthkit_sim*vcov(m4kit_2)[17,20])
y_SE_gov_assess_3_kit_control

# Negative
# coefficients of negative
coef(m4kit_2)[15]
# coefficients of negative*ideology
coef(m4kit_2)[18]
# covariance of negative & negative
vcov(m4kit_2)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m4kit_2)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m4kit_2)[15,18]
# values
y_esti_gov_assess_3_kit_negative <- coef(m4kit_2)[15] + 
  healthkit_sim*coef(m4kit_2)[18]
y_esti_gov_assess_3_kit_negative
y_SE_gov_assess_3_kit_negative <- 
  sqrt(vcov(m4kit_2)[15,15] + 
         (healthkit_sim)^2*vcov(m4kit_2)[18,18] + 
         2*healthkit_sim*vcov(m4kit_2)[15,18])
y_SE_gov_assess_3_kit_negative

# Additive
# coefficients of additive
coef(m4kit_2)[16]
# coefficients of additive*ideology
coef(m4kit_2)[19]
# covariance of additive & additive
vcov(m4kit_2)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m4kit_2)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m4kit_2)[16,19]
# values
y_esti_gov_assess_3_kit_additive <- coef(m4kit_2)[16] + 
  healthkit_sim*coef(m4kit_2)[19]
y_esti_gov_assess_3_kit_additive
y_SE_gov_assess_3_kit_additive <- 
  sqrt(vcov(m4kit_2)[16,16] + 
         (healthkit_sim)^2*vcov(m4kit_2)[19,19] + 
         2*healthkit_sim*vcov(m4kit_2)[16,19])
y_SE_gov_assess_3_kit_additive

# values
y_esti_gov_assess_3_kit <-c(y_esti_gov_assess_3_kit_control[1],
                            y_esti_gov_assess_3_kit_negative[1],
                            y_esti_gov_assess_3_kit_additive[1],
                            y_esti_gov_assess_3_kit_control[2],
                            y_esti_gov_assess_3_kit_negative[2],
                            y_esti_gov_assess_3_kit_additive[2])
y_esti_gov_assess_3_kit
y_SE_gov_assess_3_kit <- c(y_SE_gov_assess_3_kit_control[1],
                           y_SE_gov_assess_3_kit_negative[1],
                           y_SE_gov_assess_3_kit_additive[1],
                           y_SE_gov_assess_3_kit_control[2],
                           y_SE_gov_assess_3_kit_negative[2],
                           y_SE_gov_assess_3_kit_additive[2])
y_SE_gov_assess_3_kit

# plot
pdf("margin_gov_assess_1_3_kit2.pdf")
par(mar=c(8,5,5,2), xpd=NA)
par(mfrow=c(1,2))
# China COVID 
plot(x_esti[1:3],y_esti_gov_assess_1_kit[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-1,0.8), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], y_esti_gov_assess_1_kit[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("C: China COVID Re.", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0=y_esti_gov_assess_1_kit[1:6]+(1.96*y_SE_gov_assess_1_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_gov_assess_1_kit[1:6]-(1.96*y_SE_gov_assess_1_kit[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0=y_esti_gov_assess_1_kit[1:6]+(1.645*y_SE_gov_assess_1_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_gov_assess_1_kit[1:6]-(1.645*y_SE_gov_assess_1_kit[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# China COVID Students
plot(x_esti[1:3],y_esti_gov_assess_3_kit[1:3], 
     axes=FALSE, xlab="Propaganda", ylab="Marginal Effect", xlim=c(-0.2, 2.2), 
     ylim=c(-1,0.8), cex.lab=1.2, cex=1.5, pch=21)
points(x_esti[4:6], y_esti_gov_assess_3_kit[4:6], cex.lab=1.2, cex=1.8, pch=16)
title("D: China COVID Re.-Stud.", cex.main=1.2, font.main=1, line = 1)
segments(x0=x_esti[1:6], y0=y_esti_gov_assess_3_kit[1:6]+(1.96*y_SE_gov_assess_3_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_gov_assess_3_kit[1:6]-(1.96*y_SE_gov_assess_3_kit[1:6]), 
         lty = 1, lwd = 1.5)
segments(x0=x_esti[1:6], y0=y_esti_gov_assess_3_kit[1:6]+(1.645*y_SE_gov_assess_3_kit[1:6]), 
         x1=x_esti[1:6], y1 = y_esti_gov_assess_3_kit[1:6]-(1.645*y_SE_gov_assess_3_kit[1:6]), 
         lty = 1, lwd = 5)
axis(side=1,at=c(0,1,2),labels=c("No","Neg.","Add."))
axis(side=2,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
segments(x0=-0.5, y0=0, x1=2.5, y1 = 0, lty = 1, lwd = 1)
# legend
par(mfrow=c(1,1))
legend("bottom", inset = c(0, -0.35), legend = c("No Health Kit   ", "Health Kit Received"), pch = c(21,16), 
       bty = "n", pt.cex = 1.5, cex = 1.2, text.col = "black", horiz = TRUE)
dev.off()



######### interactive models: COVID assess #######
#### Wave 1 ####
# reorder group
levels(Survey_Wave1_full$group)
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("positive", "negative", "additive", "control"))


m3covid_1 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:COVIDassess_pca, 
              data=Survey_Wave1_full)
summary(m3covid_1)

m5covid_1 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:COVIDassess_pca, 
              data=Survey_Wave1_full)
summary(m5covid_1)

m4covid_1 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:COVIDassess_pca, 
              data=Survey_Wave1_full)
summary(m4covid_1)

m6covid_1 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:COVIDassess_pca, 
              data=Survey_Wave1_full)
summary(m6covid_1)

m1covid_1 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:COVIDassess_pca, 
              data=Survey_Wave1_full)
summary(m1covid_1)

m2covid_1 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:COVIDassess_pca, 
              data=Survey_Wave1_full)
summary(m2covid_1)

# generate table
stargazer(m1covid_1, m2covid_1, m3covid_1, m4covid_1, m5covid_1, m6covid_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_1, m2covid_1, m3covid_1, m4covid_1, m5covid_1, m6covid_1, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))

m3covid_nc_1 <- lm(gov_assess_1 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                data=Survey_Wave1_full)
summary(m3covid_nc_1)

m5covid_nc_1 <- lm(gov_assess_2 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                data=Survey_Wave1_full)
summary(m5covid_nc_1)

m4covid_nc_1 <- lm(gov_assess_3 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                data=Survey_Wave1_full)
summary(m4covid_nc_1)

m6covid_nc_1 <- lm(gov_assess_4 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                data=Survey_Wave1_full)
summary(m6covid_nc_1)

m1covid_nc_1 <- lm(china_overall_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                data=Survey_Wave1_full)
summary(m1covid_nc_1)

m2covid_nc_1 <- lm(china_gov_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                data=Survey_Wave1_full)
summary(m2covid_nc_1)

# generate table
stargazer(m1covid_nc_1, m2covid_nc_1, m3covid_nc_1, m4covid_nc_1, m5covid_nc_1, m6covid_nc_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_nc_1, m2covid_nc_1, m3covid_nc_1, m4covid_nc_1, m5covid_nc_1, m6covid_nc_1, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))



# =========== Wave 1: plotting heterogeneous effect: covid severity assess ================
COVIDassess_sim <- seq(min(Survey_Wave1_full$COVIDassess_pca), max(Survey_Wave1_full$COVIDassess_pca), 0.001)
COVIDassess_sim

# =========== Wave 1: plotting covid assess effect: US COVID Response (m5covid_1)==============
# Control
# coefficients of control
coef(m5covid_1)[17]
# coefficients of control*ideology
coef(m5covid_1)[20]
# covariance of control & control
vcov(m5covid_1)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m5covid_1)[20,20]
# covariance of control & control*ideolgoy
vcov(m5covid_1)[17,20]
# values
y_esti_gov_assess_2_covidassess_control <- coef(m5covid_1)[17] + 
  COVIDassess_sim*coef(m5covid_1)[20]
y_esti_gov_assess_2_covidassess_control
y_SE_gov_assess_2_covidassess_control <- 
  sqrt(vcov(m5covid_1)[17,17] + 
         (COVIDassess_sim)^2*vcov(m5covid_1)[20,20] + 
         2*COVIDassess_sim*vcov(m5covid_1)[17,20])
y_SE_gov_assess_2_covidassess_control
max(y_esti_gov_assess_2_covidassess_control+(1.96*y_SE_gov_assess_2_covidassess_control))
# max 0.7491842
min(y_esti_gov_assess_2_covidassess_control-(1.96*y_SE_gov_assess_2_covidassess_control))
# min -1.624104

# plot
pdf("margin_gov_assess_2_covidassess_control1.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_2_covidassess_control, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_control + (1.96*y_SE_gov_assess_2_covidassess_control), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_control - (1.96*y_SE_gov_assess_2_covidassess_control), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_control + (1.645*y_SE_gov_assess_2_covidassess_control), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_control - (1.645*y_SE_gov_assess_2_covidassess_control), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_control + (1.96*y_SE_gov_assess_2_covidassess_control)), rev(y_esti_gov_assess_2_covidassess_control - (1.96*y_SE_gov_assess_2_covidassess_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_control + (1.645*y_SE_gov_assess_2_covidassess_control)), rev(y_esti_gov_assess_2_covidassess_control - (1.645*y_SE_gov_assess_2_covidassess_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-a: No Propaganda: U.S. COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m5covid_1)[15]
# coefficients of negative*ideology
coef(m5covid_1)[18]
# covariance of negative & negative
vcov(m5covid_1)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m5covid_1)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m5covid_1)[15,18]
# values
y_esti_gov_assess_2_covidassess_negative <- coef(m5covid_1)[15] + 
  COVIDassess_sim*coef(m5covid_1)[18]
y_esti_gov_assess_2_covidassess_negative
y_SE_gov_assess_2_covidassess_negative <- 
  sqrt(vcov(m5covid_1)[15,15] + 
         (COVIDassess_sim)^2*vcov(m5covid_1)[18,18] + 
         2*COVIDassess_sim*vcov(m5covid_1)[15,18])
y_SE_gov_assess_2_covidassess_negative
max(y_esti_gov_assess_2_covidassess_negative+(1.96*y_SE_gov_assess_2_covidassess_negative))
# max 1.652853
min(y_esti_gov_assess_2_covidassess_negative-(1.96*y_SE_gov_assess_2_covidassess_negative))
# min -0.6023097

# plot
pdf("margin_gov_assess_2_covidassess_negative1.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_2_covidassess_negative, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_negative + (1.96*y_SE_gov_assess_2_covidassess_negative), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_negative - (1.96*y_SE_gov_assess_2_covidassess_negative), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_negative + (1.645*y_SE_gov_assess_2_covidassess_negative), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_negative - (1.645*y_SE_gov_assess_2_covidassess_negative), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_negative + (1.96*y_SE_gov_assess_2_covidassess_negative)), rev(y_esti_gov_assess_2_covidassess_negative - (1.96*y_SE_gov_assess_2_covidassess_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_negative + (1.645*y_SE_gov_assess_2_covidassess_negative)), rev(y_esti_gov_assess_2_covidassess_negative - (1.645*y_SE_gov_assess_2_covidassess_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-a: Negative Propaganda: U.S. COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m5covid_1)[16]
# coefficients of additive*ideology
coef(m5covid_1)[19]
# covariance of additive & additive
vcov(m5covid_1)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m5covid_1)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m5covid_1)[16,19]
# values
y_esti_gov_assess_2_covidassess_additive <- coef(m5covid_1)[16] + 
  COVIDassess_sim*coef(m5covid_1)[19]
y_esti_gov_assess_2_covidassess_additive
y_SE_gov_assess_2_covidassess_additive <- 
  sqrt(vcov(m5covid_1)[16,16] + 
         (COVIDassess_sim)^2*vcov(m5covid_1)[19,19] + 
         2*COVIDassess_sim*vcov(m5covid_1)[16,19])
y_SE_gov_assess_2_covidassess_additive
max(y_esti_gov_assess_2_covidassess_additive+(1.96*y_SE_gov_assess_2_covidassess_additive))
# max 2.740596
min(y_esti_gov_assess_2_covidassess_additive-(1.96*y_SE_gov_assess_2_covidassess_additive))
# min -1.16672

# plot
pdf("margin_gov_assess_2_covidassess_additive1.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_2_covidassess_additive, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_additive + (1.96*y_SE_gov_assess_2_covidassess_additive), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_additive - (1.96*y_SE_gov_assess_2_covidassess_additive), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_additive + (1.645*y_SE_gov_assess_2_covidassess_additive), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_additive - (1.645*y_SE_gov_assess_2_covidassess_additive), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_additive + (1.96*y_SE_gov_assess_2_covidassess_additive)), rev(y_esti_gov_assess_2_covidassess_additive - (1.96*y_SE_gov_assess_2_covidassess_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_additive + (1.645*y_SE_gov_assess_2_covidassess_additive)), rev(y_esti_gov_assess_2_covidassess_additive - (1.645*y_SE_gov_assess_2_covidassess_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-a: Additive Propaganda: U.S. COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()

# =========== Wave 1: plotting covid assess effect: US COVID Response-Stud (m6covid_1)=================================
# Control
# coefficients of control
coef(m6covid_1)[17]
# coefficients of control*ideology
coef(m6covid_1)[20]
# covariance of control & control
vcov(m6covid_1)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m6covid_1)[20,20]
# covariance of control & control*ideolgoy
vcov(m6covid_1)[17,20]
# values
y_esti_gov_assess_4_covidassess_control <- coef(m6covid_1)[17] + 
  COVIDassess_sim*coef(m6covid_1)[20]
y_esti_gov_assess_4_covidassess_control
y_SE_gov_assess_4_covidassess_control <- 
  sqrt(vcov(m6covid_1)[17,17] + 
         (COVIDassess_sim)^2*vcov(m6covid_1)[20,20] + 
         2*COVIDassess_sim*vcov(m6covid_1)[17,20])
y_SE_gov_assess_4_covidassess_control
max(y_esti_gov_assess_4_covidassess_control+(1.96*y_SE_gov_assess_4_covidassess_control))
# max 0.8320992
min(y_esti_gov_assess_4_covidassess_control-(1.96*y_SE_gov_assess_4_covidassess_control))
# min -1.527539

# plot
pdf("margin_gov_assess_4_covidassess_control1.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_4_covidassess_control, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_control + (1.96*y_SE_gov_assess_4_covidassess_control), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_control - (1.96*y_SE_gov_assess_4_covidassess_control), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_control + (1.645*y_SE_gov_assess_4_covidassess_control), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_control - (1.645*y_SE_gov_assess_4_covidassess_control), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_control + (1.96*y_SE_gov_assess_4_covidassess_control)), rev(y_esti_gov_assess_4_covidassess_control - (1.96*y_SE_gov_assess_4_covidassess_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_control + (1.645*y_SE_gov_assess_4_covidassess_control)), rev(y_esti_gov_assess_4_covidassess_control - (1.645*y_SE_gov_assess_4_covidassess_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-b: No Propaganda: U.S. COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m6covid_1)[15]
# coefficients of negative*ideology
coef(m6covid_1)[18]
# covariance of negative & negative
vcov(m6covid_1)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m6covid_1)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m6covid_1)[15,18]
# values
y_esti_gov_assess_4_covidassess_negative <- coef(m6covid_1)[15] + 
  COVIDassess_sim*coef(m6covid_1)[18]
y_esti_gov_assess_4_covidassess_negative
y_SE_gov_assess_4_covidassess_negative <- 
  sqrt(vcov(m6covid_1)[15,15] + 
         (COVIDassess_sim)^2*vcov(m6covid_1)[18,18] + 
         2*COVIDassess_sim*vcov(m6covid_1)[15,18])
y_SE_gov_assess_4_covidassess_negative
max(y_esti_gov_assess_4_covidassess_negative+(1.96*y_SE_gov_assess_4_covidassess_negative))
# max 1.973121
min(y_esti_gov_assess_4_covidassess_negative-(1.96*y_SE_gov_assess_4_covidassess_negative))
# min -0.7669656

# plot
pdf("margin_gov_assess_4_covidassess_negative1.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_4_covidassess_negative, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_negative + (1.96*y_SE_gov_assess_4_covidassess_negative), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_negative - (1.96*y_SE_gov_assess_4_covidassess_negative), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_negative + (1.645*y_SE_gov_assess_4_covidassess_negative), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_negative - (1.645*y_SE_gov_assess_4_covidassess_negative), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_negative + (1.96*y_SE_gov_assess_4_covidassess_negative)), rev(y_esti_gov_assess_4_covidassess_negative - (1.96*y_SE_gov_assess_4_covidassess_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_negative + (1.645*y_SE_gov_assess_4_covidassess_negative)), rev(y_esti_gov_assess_4_covidassess_negative - (1.645*y_SE_gov_assess_4_covidassess_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-b: Negative Propaganda: U.S. COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m6covid_1)[16]
# coefficients of additive*ideology
coef(m6covid_1)[19]
# covariance of additive & additive
vcov(m6covid_1)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m6covid_1)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m6covid_1)[16,19]
# values
y_esti_gov_assess_4_covidassess_additive <- coef(m6covid_1)[16] + 
  COVIDassess_sim*coef(m6covid_1)[19]
y_esti_gov_assess_4_covidassess_additive
y_SE_gov_assess_4_covidassess_additive <- 
  sqrt(vcov(m6covid_1)[16,16] + 
         (COVIDassess_sim)^2*vcov(m6covid_1)[19,19] + 
         2*COVIDassess_sim*vcov(m6covid_1)[16,19])
y_SE_gov_assess_4_covidassess_additive
max(y_esti_gov_assess_4_covidassess_additive+(1.96*y_SE_gov_assess_4_covidassess_additive))
# max 2.860267
min(y_esti_gov_assess_4_covidassess_additive-(1.96*y_SE_gov_assess_4_covidassess_additive))
# min -0.9225022

# plot
pdf("margin_gov_assess_4_covidassess_additive1.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_4_covidassess_additive, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_additive + (1.96*y_SE_gov_assess_4_covidassess_additive), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_additive - (1.96*y_SE_gov_assess_4_covidassess_additive), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_additive + (1.645*y_SE_gov_assess_4_covidassess_additive), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_additive - (1.645*y_SE_gov_assess_4_covidassess_additive), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_additive + (1.96*y_SE_gov_assess_4_covidassess_additive)), rev(y_esti_gov_assess_4_covidassess_additive - (1.96*y_SE_gov_assess_4_covidassess_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_additive + (1.645*y_SE_gov_assess_4_covidassess_additive)), rev(y_esti_gov_assess_4_covidassess_additive - (1.645*y_SE_gov_assess_4_covidassess_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-b: Additive Propaganda: U.S. COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()



#### Wave 2 ####
# reorder group
levels(Survey_Wave2_full$group)
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("positive", "negative", "additive", "control"))


m3covid_2 <- lm(gov_assess_1 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                data=Survey_Wave2_full)
summary(m3covid_2)

m5covid_2 <- lm(gov_assess_2 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                data=Survey_Wave2_full)
summary(m5covid_2)

m4covid_2 <- lm(gov_assess_3 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                data=Survey_Wave2_full)
summary(m4covid_2)

m6covid_2 <- lm(gov_assess_4 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                data=Survey_Wave2_full)
summary(m6covid_2)

m1covid_2 <- lm(china_overall_num ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                data=Survey_Wave2_full)
summary(m1covid_2)

m2covid_2 <- lm(china_gov_num ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                data=Survey_Wave2_full)
summary(m2covid_2)

# generate table
stargazer(m1covid_2, m2covid_2, m3covid_2, m4covid_2, m5covid_2, m6covid_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_2, m2covid_2, m3covid_2, m4covid_2, m5covid_2, m6covid_2,  
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))

m3covid_nc_2 <- lm(gov_assess_1 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m3covid_nc_2)

m5covid_nc_2 <- lm(gov_assess_2 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m5covid_nc_2)

m4covid_nc_2 <- lm(gov_assess_3 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m4covid_nc_2)

m6covid_nc_2 <- lm(gov_assess_4 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m6covid_nc_2)

m1covid_nc_2 <- lm(china_overall_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m1covid_nc_2)

m2covid_nc_2 <- lm(china_gov_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m2covid_nc_2)

# generate table
stargazer(m1covid_nc_2, m2covid_nc_2, m3covid_nc_2, m4covid_nc_2, m5covid_nc_2, m6covid_nc_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_nc_2, m2covid_nc_2, m3covid_nc_2, m4covid_nc_2, m5covid_nc_2, m6covid_nc_2, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))



# =========== Wave 2: plotting heterogeneous effect: covid severity assess =================================
COVIDassess_sim <- seq(min(Survey_Wave2_full$COVIDassess_pca), max(Survey_Wave2_full$COVIDassess_pca), 0.001)
COVIDassess_sim

# =========== Wave 2: plotting covid assess effect: US COVID Response (m5covid_2) ======================
# Control
# coefficients of control
coef(m5covid_2)[17]
# coefficients of control*ideology
coef(m5covid_2)[20]
# covariance of control & control
vcov(m5covid_2)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m5covid_2)[20,20]
# covariance of control & control*ideolgoy
vcov(m5covid_2)[17,20]
# values
y_esti_gov_assess_2_covidassess_control <- coef(m5covid_2)[17] + 
  COVIDassess_sim*coef(m5covid_2)[20]
y_esti_gov_assess_2_covidassess_control
y_SE_gov_assess_2_covidassess_control <- 
  sqrt(vcov(m5covid_2)[17,17] + 
         (COVIDassess_sim)^2*vcov(m5covid_2)[20,20] + 
         2*COVIDassess_sim*vcov(m5covid_2)[17,20])
y_SE_gov_assess_2_covidassess_control
max(y_esti_gov_assess_2_covidassess_control+(1.96*y_SE_gov_assess_2_covidassess_control))
# max 0.7491842
min(y_esti_gov_assess_2_covidassess_control-(1.96*y_SE_gov_assess_2_covidassess_control))
# min -1.624104

# plot
pdf("margin_gov_assess_2_covidassess_control2.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_2_covidassess_control, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_control + (1.96*y_SE_gov_assess_2_covidassess_control), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_control - (1.96*y_SE_gov_assess_2_covidassess_control), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_control + (1.645*y_SE_gov_assess_2_covidassess_control), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_control - (1.645*y_SE_gov_assess_2_covidassess_control), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_control + (1.96*y_SE_gov_assess_2_covidassess_control)), rev(y_esti_gov_assess_2_covidassess_control - (1.96*y_SE_gov_assess_2_covidassess_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_control + (1.645*y_SE_gov_assess_2_covidassess_control)), rev(y_esti_gov_assess_2_covidassess_control - (1.645*y_SE_gov_assess_2_covidassess_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-a: No Propaganda: U.S. COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m5covid_2)[15]
# coefficients of negative*ideology
coef(m5covid_2)[18]
# covariance of negative & negative
vcov(m5covid_2)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m5covid_2)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m5covid_2)[15,18]
# values
y_esti_gov_assess_2_covidassess_negative <- coef(m5covid_2)[15] + 
  COVIDassess_sim*coef(m5covid_2)[18]
y_esti_gov_assess_2_covidassess_negative
y_SE_gov_assess_2_covidassess_negative <- 
  sqrt(vcov(m5covid_2)[15,15] + 
         (COVIDassess_sim)^2*vcov(m5covid_2)[18,18] + 
         2*COVIDassess_sim*vcov(m5covid_2)[15,18])
y_SE_gov_assess_2_covidassess_negative
max(y_esti_gov_assess_2_covidassess_negative+(1.96*y_SE_gov_assess_2_covidassess_negative))
# max 1.652853
min(y_esti_gov_assess_2_covidassess_negative-(1.96*y_SE_gov_assess_2_covidassess_negative))
# min -0.6023097

# plot
pdf("margin_gov_assess_2_covidassess_negative2.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_2_covidassess_negative, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_negative + (1.96*y_SE_gov_assess_2_covidassess_negative), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_negative - (1.96*y_SE_gov_assess_2_covidassess_negative), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_negative + (1.645*y_SE_gov_assess_2_covidassess_negative), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_negative - (1.645*y_SE_gov_assess_2_covidassess_negative), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_negative + (1.96*y_SE_gov_assess_2_covidassess_negative)), rev(y_esti_gov_assess_2_covidassess_negative - (1.96*y_SE_gov_assess_2_covidassess_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_negative + (1.645*y_SE_gov_assess_2_covidassess_negative)), rev(y_esti_gov_assess_2_covidassess_negative - (1.645*y_SE_gov_assess_2_covidassess_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-a: Negative Propaganda: U.S. COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m5covid_2)[16]
# coefficients of additive*ideology
coef(m5covid_2)[19]
# covariance of additive & additive
vcov(m5covid_2)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m5covid_2)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m5covid_2)[16,19]
# values
y_esti_gov_assess_2_covidassess_additive <- coef(m5covid_2)[16] + 
  COVIDassess_sim*coef(m5covid_2)[19]
y_esti_gov_assess_2_covidassess_additive
y_SE_gov_assess_2_covidassess_additive <- 
  sqrt(vcov(m5covid_2)[16,16] + 
         (COVIDassess_sim)^2*vcov(m5covid_2)[19,19] + 
         2*COVIDassess_sim*vcov(m5covid_2)[16,19])
y_SE_gov_assess_2_covidassess_additive
max(y_esti_gov_assess_2_covidassess_additive+(1.96*y_SE_gov_assess_2_covidassess_additive))
# max 2.740596
min(y_esti_gov_assess_2_covidassess_additive-(1.96*y_SE_gov_assess_2_covidassess_additive))
# min -1.16672

# plot
pdf("margin_gov_assess_2_covidassess_additive2.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_2_covidassess_additive, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_additive + (1.96*y_SE_gov_assess_2_covidassess_additive), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_additive - (1.96*y_SE_gov_assess_2_covidassess_additive), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_additive + (1.645*y_SE_gov_assess_2_covidassess_additive), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_2_covidassess_additive - (1.645*y_SE_gov_assess_2_covidassess_additive), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_additive + (1.96*y_SE_gov_assess_2_covidassess_additive)), rev(y_esti_gov_assess_2_covidassess_additive - (1.96*y_SE_gov_assess_2_covidassess_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_2_covidassess_additive + (1.645*y_SE_gov_assess_2_covidassess_additive)), rev(y_esti_gov_assess_2_covidassess_additive - (1.645*y_SE_gov_assess_2_covidassess_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-a: Additive Propaganda: U.S. COVID Re.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()

# ============ Wave 2: plotting covid assess effect: US COVID Response-Stud (m6covid_2)========================
# Control
# coefficients of control
coef(m6covid_2)[17]
# coefficients of control*ideology
coef(m6covid_2)[20]
# covariance of control & control
vcov(m6covid_2)[17,17]
# covariance of control*ideolgoy & control*ideolgoy
vcov(m6covid_2)[20,20]
# covariance of control & control*ideolgoy
vcov(m6covid_2)[17,20]
# values
y_esti_gov_assess_4_covidassess_control <- coef(m6covid_2)[17] + 
  COVIDassess_sim*coef(m6covid_2)[20]
y_esti_gov_assess_4_covidassess_control
y_SE_gov_assess_4_covidassess_control <- 
  sqrt(vcov(m6covid_2)[17,17] + 
         (COVIDassess_sim)^2*vcov(m6covid_2)[20,20] + 
         2*COVIDassess_sim*vcov(m6covid_2)[17,20])
y_SE_gov_assess_4_covidassess_control
max(y_esti_gov_assess_4_covidassess_control+(1.96*y_SE_gov_assess_4_covidassess_control))
# max 0.8320992
min(y_esti_gov_assess_4_covidassess_control-(1.96*y_SE_gov_assess_4_covidassess_control))
# min -1.527539

# plot
pdf("margin_gov_assess_4_covidassess_control2.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_4_covidassess_control, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_control + (1.96*y_SE_gov_assess_4_covidassess_control), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_control - (1.96*y_SE_gov_assess_4_covidassess_control), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_control + (1.645*y_SE_gov_assess_4_covidassess_control), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_control - (1.645*y_SE_gov_assess_4_covidassess_control), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_control + (1.96*y_SE_gov_assess_4_covidassess_control)), rev(y_esti_gov_assess_4_covidassess_control - (1.96*y_SE_gov_assess_4_covidassess_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_control + (1.645*y_SE_gov_assess_4_covidassess_control)), rev(y_esti_gov_assess_4_covidassess_control - (1.645*y_SE_gov_assess_4_covidassess_control))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "A-b: No Propaganda: U.S. COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()

# Negative
# coefficients of negative
coef(m6covid_2)[15]
# coefficients of negative*ideology
coef(m6covid_2)[18]
# covariance of negative & negative
vcov(m6covid_2)[15,15]
# covariance of negative*ideolgoy & negative*ideolgoy
vcov(m6covid_2)[18,18]
# covariance of negative & negative*ideolgoy
vcov(m6covid_2)[15,18]
# values
y_esti_gov_assess_4_covidassess_negative <- coef(m6covid_2)[15] + 
  COVIDassess_sim*coef(m6covid_2)[18]
y_esti_gov_assess_4_covidassess_negative
y_SE_gov_assess_4_covidassess_negative <- 
  sqrt(vcov(m6covid_2)[15,15] + 
         (COVIDassess_sim)^2*vcov(m6covid_2)[18,18] + 
         2*COVIDassess_sim*vcov(m6covid_2)[15,18])
y_SE_gov_assess_4_covidassess_negative
max(y_esti_gov_assess_4_covidassess_negative+(1.96*y_SE_gov_assess_4_covidassess_negative))
# max 1.973121
min(y_esti_gov_assess_4_covidassess_negative-(1.96*y_SE_gov_assess_4_covidassess_negative))
# min -0.7669656

# plot
pdf("margin_gov_assess_4_covidassess_negative2.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_4_covidassess_negative, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_negative + (1.96*y_SE_gov_assess_4_covidassess_negative), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_negative - (1.96*y_SE_gov_assess_4_covidassess_negative), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_negative + (1.645*y_SE_gov_assess_4_covidassess_negative), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_negative - (1.645*y_SE_gov_assess_4_covidassess_negative), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_negative + (1.96*y_SE_gov_assess_4_covidassess_negative)), rev(y_esti_gov_assess_4_covidassess_negative - (1.96*y_SE_gov_assess_4_covidassess_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_negative + (1.645*y_SE_gov_assess_4_covidassess_negative)), rev(y_esti_gov_assess_4_covidassess_negative - (1.645*y_SE_gov_assess_4_covidassess_negative))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "B-b: Negative Propaganda: U.S. COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()


# Additive
# coefficients of additive
coef(m6covid_2)[16]
# coefficients of additive*ideology
coef(m6covid_2)[19]
# covariance of additive & additive
vcov(m6covid_2)[16,16]
# covariance of additive*ideolgoy & additive*ideolgoy
vcov(m6covid_2)[19,19]
# covariance of additive & additive*ideolgoy
vcov(m6covid_2)[16,19]
# values
y_esti_gov_assess_4_covidassess_additive <- coef(m6covid_2)[16] + 
  COVIDassess_sim*coef(m6covid_2)[19]
y_esti_gov_assess_4_covidassess_additive
y_SE_gov_assess_4_covidassess_additive <- 
  sqrt(vcov(m6covid_2)[16,16] + 
         (COVIDassess_sim)^2*vcov(m6covid_2)[19,19] + 
         2*COVIDassess_sim*vcov(m6covid_2)[16,19])
y_SE_gov_assess_4_covidassess_additive
max(y_esti_gov_assess_4_covidassess_additive+(1.96*y_SE_gov_assess_4_covidassess_additive))
# max 2.860267
min(y_esti_gov_assess_4_covidassess_additive-(1.96*y_SE_gov_assess_4_covidassess_additive))
# min -0.9225022

# plot
pdf("margin_gov_assess_4_covidassess_additive2.pdf")
par(mar=c(5,5,5,2))
plot(COVIDassess_sim, y_esti_gov_assess_4_covidassess_additive, t="l",lwd=2,xlab="Assess. of COVID Severity",
     ylab="Marginal Effect of Propaganda", cex.lab=1.5, xlim=c(1,5), ylim=c(-1.6,3))
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_additive + (1.96*y_SE_gov_assess_4_covidassess_additive), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_additive - (1.96*y_SE_gov_assess_4_covidassess_additive), lty=2, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_additive + (1.645*y_SE_gov_assess_4_covidassess_additive), lty=3, lwd=2)
lines(COVIDassess_sim, y_esti_gov_assess_4_covidassess_additive - (1.645*y_SE_gov_assess_4_covidassess_additive), lty=3, lwd=2)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_additive + (1.96*y_SE_gov_assess_4_covidassess_additive)), rev(y_esti_gov_assess_4_covidassess_additive - (1.96*y_SE_gov_assess_4_covidassess_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.1), border = NA)
polygon(c(COVIDassess_sim, rev(COVIDassess_sim)), 
        c((y_esti_gov_assess_4_covidassess_additive + (1.645*y_SE_gov_assess_4_covidassess_additive)), rev(y_esti_gov_assess_4_covidassess_additive - (1.645*y_SE_gov_assess_4_covidassess_additive))),
        col = rgb(red = 0.22, green = 0.22, blue = 0.22, alpha = 0.2), border = NA)
title(main = "C-b: Additive Propaganda: U.S. COVID Re.-Stud.", cex.main=1.5)
abline(h=0,lwd=1,lty=1)
segments(x0=4.75, y0=2.5, x1=5, y1=2.5, lty=2, lwd = 2)
text(x=4.4, y=2.5, labels = "95% CI")
segments(x0=4.75, y0=2.2, x1=5, y1=2.2, lty=3, lwd = 2)
text(x=4.4, y=2.2, labels = "90% CI")
dev.off()


# ============================== Regressions for Appendix F: Complete Results ==============================
############ Main Model: Control Group as Baseline ###############

#### Wave 1 ####
# reorder group
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("control", "positive", "negative", "additive"))

# without controls (Appendix F-1)
m3nc_cb_1 <- lm(gov_assess_1 ~ 
                group, data=Survey_Wave1_full)
summary(m3nc_cb_1)

m5nc_cb_1 <- lm(gov_assess_2 ~ 
                group, data=Survey_Wave1_full)
summary(m5n_cbc_1)

m4nc_cb_1 <- lm(gov_assess_3 ~ 
                group, data=Survey_Wave1_full)
summary(m4nc_cb_1)

m6nc_cb_1 <- lm(gov_assess_4 ~ 
                group, data=Survey_Wave1_full)
summary(m6nc_cb_1)

m1nc_cb_1 <- lm(china_overall_num ~ 
                group, data=Survey_Wave1_full)
summary(m1nc_cb_1)

m2nc_cb_1 <- lm(china_gov_num ~ 
                group, data=Survey_Wave1_full)
summary(m2nc_cb_1)

# generate tables
stargazer(m1nc_cb_1,m2nc_cb_1,m3nc_cb_1,m4nc_cb_1,m5nc_cb_1,m6nc_cb_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
# stargazer(m1nc_cb_1,m2nc_cb_1,m3nc_cb_1,m4nc_cb_1,m5nc_cb_1,m6nc_cb_1,
#           align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With Control (Table F-2)
m3_cb_1 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave1_full)
summary(m3_cb_1)

m5_cb_1 <- lm(gov_assess_2 ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave1_full)
summary(m5_cb_1)

m4_cb_1 <- lm(gov_assess_3 ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave1_full)
summary(m4_cb_1)

m6_cb_1 <- lm(gov_assess_4 ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave1_full)
summary(m6_cb_1)

m1_cb_1 <- lm(china_overall_num ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave1_full)
summary(m1_cb_1)

m2_cb_1 <- lm(china_gov_num ~ 
             female + education_gra + STEM + family_regime_tie + 
             ideology_pca + socialized_US + race_experience + know_pos + income_us + 
             COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
           data=Survey_Wave1_full)
summary(m2_cb_1)


# generate tables
stargazer(m1_cb_1,m2_cb_1,m3_cb_1,m4_cb_1,m5_cb_1,m6_cb_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
# stargazer(m1_cb_1,m2_cb_1,m3_cb_1,m4_cb_1,m5_cb_1,m6_cb_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))



#### Wave 2 ####
# reorder group
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("control", "positive", "negative", "additive"))

# without controls (Appendix F-3)
m3nc_cb_2 <- lm(gov_assess_1 ~ 
               group, data=Survey_Wave2_full)
summary(m3nc_cb_2)

m5nc_cb_2 <- lm(gov_assess_2 ~ 
               group, data=Survey_Wave2_full)
summary(m5nc_cb_2)

m4nc_cb_2 <- lm(gov_assess_3 ~ 
               group, data=Survey_Wave2_full)
summary(m4nc_cb_2)

m6nc_cb_2 <- lm(gov_assess_4 ~ 
               group, data=Survey_Wave2_full)
summary(m6nc_cb_2)

m1nc_cb_2 <- lm(china_overall_num ~ 
               group, data=Survey_Wave2_full)
summary(m1nc_cb_2)

m2nc_cb_2 <- lm(china_gov_num ~ 
               group, data=Survey_Wave2_full)
summary(m2nc_cb_2)


# generate tables
stargazer(m1nc_cb_2,m2nc_cb_2,m3nc_cb_2,m4nc_cb_2,m5nc_cb_2,m6nc_cb_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
# stargazer(m1nc_cb_2,m2nc_cb_2,m3nc_cb_2,m4nc_cb_2,m5nc_cb_2,m6nc_cb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With Control (Table F-4)
m3_cb_2 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m3_cb_2)

m5_cb_2 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m5_cb_2)

m4_cb_2 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m4_cb_2)

m6_cb_2 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m6_cb_2)

m1_cb_2 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m1_cb_2)

m2_cb_2 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m2_cb_2)


# generate tables
stargazer(m1_cb_2,m2_cb_2,m3_cb_2,m4_cb_2,m5_cb_2,m6_cb_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
# stargazer(m1_cb_2,m2_cb_2,m3_cb_2,m4_cb_2,m5_cb_2,m6_cb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))



#### Main Model: Negative Group as Baseline ####

#### Wave 1 ####
# reorder group
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("negative", "positive", "additive", "control"))

# without controls (Appendix F-1)
m3nc_nb_1 <- lm(gov_assess_1 ~ 
                  group, data=Survey_Wave1_full)
summary(m3nc_nb_1)

m5nc_nb_1 <- lm(gov_assess_2 ~ 
                  group, data=Survey_Wave1_full)
summary(m5nc_nb_1)

m4nc_nb_1 <- lm(gov_assess_3 ~ 
                  group, data=Survey_Wave1_full)
summary(m4nc_nb_1)

m6nc_nb_1 <- lm(gov_assess_4 ~ 
                  group, data=Survey_Wave1_full)
summary(m6nc_nb_1)

m1nc_nb_1 <- lm(china_overall_num ~ 
                  group, data=Survey_Wave1_full)
summary(m1nc_nb_1)

m2nc_nb_1 <- lm(china_gov_num ~ 
                  group, data=Survey_Wave1_full)
summary(m2nc_nb_1)

# generate tables
stargazer(m1nc_nb_1,m2nc_nb_1,m3nc_nb_1,m4nc_nb_1,m5nc_nb_1,m6nc_nb_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
# stargazer(m1nc_nb_1,m2nc_nb_1,m3nc_nb_1,m4nc_nb_1,m5nc_nb_1,m6nc_nb_1,
#           align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))

# With Control (Appendix F-2)
m3_nb_1 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave1_full) 
summary(m3_nb_1)

m5_nb_1 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave1_full)
summary(m5_nb_1)

m4_nb_1 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave1_full)
summary(m4_nb_1)

m6_nb_1 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave1_full)
summary(m6_nb_1)

m1_nb_1 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave1_full)
summary(m1_nb_1)

m2_nb_1 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave1_full)
summary(m2_nb_1)

# generate tables
stargazer(m1_nb_1,m2_nb_1,m3_nb_1,m4_nb_1,m5_nb_1,m6_nb_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
# stargazer(m1_nb_1,m2_nb_1,m3_nb_1,m4_nb_1,m5_nb_1,m6_nb_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


#### Wave 2 ####
# reorder group
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("negative", "positive", "additive", "control"))
# without controls (Appendix F-3)
m3nc_nb_2 <- lm(gov_assess_1 ~ 
               group, data=Survey_Wave2_full)
summary(m3nc_nb_2)

m5nc_nb_2 <- lm(gov_assess_2 ~ 
               group, data=Survey_Wave2_full)
summary(m5nc_nb_2)

m4nc_nb_2 <- lm(gov_assess_3 ~ 
               group, data=Survey_Wave2_full)
summary(m4nc_nb_2)

m6nc_nb_2 <- lm(gov_assess_4 ~ 
               group, data=Survey_Wave2_full)
summary(m6nc_nb_2)

m1nc_nb_2 <- lm(china_overall_num ~ 
               group, data=Survey_Wave2_full)
summary(m1nc_nb_2)

m2nc_nb_2 <- lm(china_gov_num ~ 
               group, data=Survey_Wave2_full)
summary(m2nc_nb_2)

# generate tables
stargazer(m1nc_nb_2,m2nc_nb_2,m3nc_nb_2,m4nc_nb_2,m5nc_nb_2,m6nc_nb_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
# stargazer(m1nc_nb_2,m2nc_nb_2,m3nc_nb_2,m4nc_nb_2,m5nc_nb_2,m6nc_nb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With Control (Appendix F-4)
m3_nb_2 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full) 
summary(m3_nb_2)

m5_nb_2 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m5_nb_2)

m4_nb_2 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m4_nb_2)

m6_nb_2 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m6_nb_2)

m1_nb_2 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m1_nb_2)

m2_nb_2 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group, 
              data=Survey_Wave2_full)
summary(m2_nb_2)

# generate tables
stargazer(m1_nb_2,m2_nb_2,m3_nb_2,m4_nb_2,m5_nb_2,m6_nb_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
# stargazer(m1_nb_2,m2_nb_2,m3_nb_2,m4_nb_2,m5_nb_2,m6_nb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))



########### Heterogeneous effect: Ideology ################################
####### Control as baseline ########

#### Wave 1 ####
# reorder group
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("control", "positive", "negative", "additive"))

# Without control (Appendix F-5)
m3ide_nc_cb_1 <- lm(gov_assess_1 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m3ide_nc_cb_1)

m5ide_nc_cb_1 <- lm(gov_assess_2 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m5ide_nc_cb_1)

m4ide_nc_cb_1 <- lm(gov_assess_3 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m4ide_nc_cb_1)

m6ide_nc_cb_1 <- lm(gov_assess_4 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m6ide_nc_cb_1)

m1ide_nc_cb_1 <- lm(china_overall_num ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m1ide_nc_cb_1)

m2ide_nc_cb_1 <- lm(china_gov_num ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m2ide_nc_cb_1)

# generate table
stargazer(m1ide_nc_cb_1, m2ide_nc_cb_1, m3ide_nc_cb_1, m4ide_nc_cb_1, m5ide_nc_cb_1, m6ide_nc_cb_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_nc_cb_1, m2ide_nc_cb_1, m3ide_nc_cb_1, m4ide_nc_cb_1, m5ide_nc_cb_1, m6ide_nc_cb_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With control (Appendix F-6)
m3ide_cb_1 <- lm(gov_assess_1 ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m3ide_cb_1)

m5ide_cb_1 <- lm(gov_assess_2 ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m5ide_cb_1)

m4ide_cb_1 <- lm(gov_assess_3 ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m4ide_cb_1)

m6ide_cb_1 <- lm(gov_assess_4 ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m6ide_cb_1)

m1ide_cb_1 <- lm(china_overall_num ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m1ide_cb_1)

m2ide_cb_1 <- lm(china_gov_num ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m2ide_cb_1)

# generate table
stargazer(m1ide_cb_1, m2ide_cb_1, m3ide_cb_1, m4ide_cb_1, m5ide_cb_1, m6ide_cb_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_cb_1, m2ide_cb_1, m3ide_cb_1, m4ide_cb_1, m5ide_cb_1, m6ide_cb_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


#### Wave 2 ####
# reorder group
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("control", "positive", "negative", "additive"))

# Without control (Appendix F-7)
m3ide_nc_cb_2 <- lm(gov_assess_1 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m3ide_nc_cb_2)

m5ide_nc_cb_2 <- lm(gov_assess_2 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m5ide_nc_cb_2)

m4ide_nc_cb_2 <- lm(gov_assess_3 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m4ide_nc_cb_2)

m6ide_nc_cb_2 <- lm(gov_assess_4 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m6ide_nc_cb_2)

m1ide_nc_cb_2 <- lm(china_overall_num ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m1ide_nc_cb_2)

m2ide_nc_cb_2 <- lm(china_gov_num ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m2ide_nc_cb_2)

# generate table
stargazer(m1ide_nc_cb_2, m2ide_nc_cb_2, m3ide_nc_cb_2, m4ide_nc_cb_2, m5ide_nc_cb_2, m6ide_nc_cb_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_nc_cb_2, m2ide_nc_cb_2, m3ide_nc_cb_2, m4ide_nc_cb_2, m5ide_nc_cb_2, m6ide_nc_cb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With control (Appendix F-8)
m3ide_cb_2 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m3ide_cb_2)

m5ide_cb_2 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m5ide_cb_2)

m4ide_cb_2 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m4ide_cb_2)

m6ide_cb_2 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m6ide_cb_2)

m1ide_cb_2 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m1ide_cb_2)

m2ide_cb_2 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m2ide_cb_2)

# generate table
stargazer(m1ide_cb_2, m2ide_cb_2, m3ide_cb_2, m4ide_cb_2, m5ide_cb_2, m6ide_cb_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_cb_2, m2ide_cb_2, m3ide_cb_2, m4ide_cb_2, m5ide_cb_2, m6ide_cb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


####### Negative as baseline ########

#### Wave 1 ####
# reorder group
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("negative", "positive", "additive", "control"))

# Without control (Appendix F-5)
m3ide_nc_nb_1 <- lm(gov_assess_1 ~ 
                      ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m3ide_nc_nb_1)

m5ide_nc_nb_1 <- lm(gov_assess_2 ~ 
                      ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m5ide_nc_nb_1)

m4ide_nc_nb_1 <- lm(gov_assess_3 ~ 
                      ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m4ide_nc_nb_1)

m6ide_nc_nb_1 <- lm(gov_assess_4 ~ 
                      ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m6ide_nc_nb_1)

m1ide_nc_nb_1 <- lm(china_overall_num ~ 
                      ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m1ide_nc_nb_1)

m2ide_nc_nb_1 <- lm(china_gov_num ~ 
                      ideology_pca + group + group:ideology_pca, data=Survey_Wave1_full)
summary(m2ide_nc_nb_1)

# generate table
stargazer(m1ide_nc_nb_1, m2ide_nc_nb_1, m3ide_nc_nb_1, m4ide_nc_nb_1, m5ide_nc_nb_1, m6ide_nc_nb_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_nc_nb_1, m2ide_nc_nb_1, m3ide_nc_nb_1, m4ide_nc_nb_1, m5ide_nc_nb_1, m6ide_nc_nb_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With control (Appendix F-6)
m3ide_nb_1 <- lm(gov_assess_1 ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m3ide_nb_1)

m5ide_nb_1 <- lm(gov_assess_2 ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m5ide_nb_1)

m4ide_nb_1 <- lm(gov_assess_3 ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m4ide_nb_1)

m6ide_nb_1 <- lm(gov_assess_4 ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m6ide_nb_1)

m1ide_nb_1 <- lm(china_overall_num ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m1ide_nb_1)

m2ide_nb_1 <- lm(china_gov_num ~ 
                 female + education_gra + STEM + family_regime_tie + 
                 ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                 COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                 group:ideology_pca, 
                 data=Survey_Wave1_full)
summary(m2ide_nb_1)

# generate table
stargazer(m1ide_nb_1, m2ide_nb_1, m3ide_nb_1, m4ide_nb_1, m5ide_nb_1, m6ide_nb_1,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_nb_1, m2ide_nb_1, m3ide_nb_1, m4ide_nb_1, m5ide_nb_1, m6ide_nb_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))



#### Wave 2 ####
# reorder group
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("negative", "positive", "additive", "control"))

# Without control (Appendix F-7)
m3ide_nc_nb_2 <- lm(gov_assess_1 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m3ide_nc_nb_2)

m5ide_nc_nb_2 <- lm(gov_assess_2 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m5ide_nc_nb_2)

m4ide_nc_nb_2 <- lm(gov_assess_3 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m4ide_nc_nb_2)

m6ide_nc_nb_2 <- lm(gov_assess_4 ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m6ide_nc_nb_2)

m1ide_nc_nb_2 <- lm(china_overall_num ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m1ide_nc_nb_2)

m2ide_nc_nb_2 <- lm(china_gov_num ~ 
                   ideology_pca + group + group:ideology_pca, data=Survey_Wave2_full)
summary(m2ide_nc_nb_2)

# generate table
stargazer(m1ide_nc_nb_2, m2ide_nc_nb_2, m3ide_nc_nb_2, m4ide_nc_nb_2, m5ide_nc_nb_2, m6ide_nc_nb_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_nc_nb_2, m2ide_nc_nb_2, m3ide_nc_nb_2, m4ide_nc_nb_2, m5ide_nc_nb_2, m6ide_nc_nb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With control (Appendix F-8)
m3ide_nb_2 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m3ide_nb_2)

m5ide_nb_2 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m5ide_nb_2)

m4ide_nb_2 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m4ide_nb_2)

m6ide_nb_2 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m6ide_nb_2)

m1ide_nb_2 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m1ide_nb_2)

m2ide_nb_2 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:ideology_pca, 
                data=Survey_Wave2_full)
summary(m2ide_nb_2)

# generate table
stargazer(m1ide_nb_2, m2ide_nb_2, m3ide_nb_2, m4ide_nb_2, m5ide_nb_2, m6ide_nb_2,
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))

# to paste in Latex
#stargazer(m1ide_nb_2, m2ide_nb_2, m3ide_nb_2, m4ide_nb_2, m5ide_nb_2, m6ide_nb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))





####### Heterogeneous Effects: Receiving Health Kit ############

####### Control as baseline ########

#### Wave 1 ####
# reorder group
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("control", "positive", "negative", "additive"))

# Without control (Appendix F-9)
m3kit_nc_cb_1 <- lm(gov_assess_1 ~ kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave1_full)
summary(m3kit_nc_cb_1)

m5kit_nc_cb_1 <- lm(gov_assess_2 ~ kit_got_2 + group + group:kit_got_2, 
                    data=Survey_Wave1_full)
summary(m5kit_nc_cb_1)

m4kit_nc_cb_1 <- lm(gov_assess_3 ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave1_full)
summary(m4kit_nc_cb_1)

m6kit_nc_cb_1 <- lm(gov_assess_4 ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave1_full)
summary(m6kit_nc_cb_1)

m1kit_nc_cb_1 <- lm(china_overall_num ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave1_full)
summary(m1kit_nc_cb_1)

m2kit_nc_cb_1 <- lm(china_gov_num ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave1_full)
summary(m2kit_nc_cb_1)

# generate table
stargazer(m1kit_nc_cb_1, m2kit_nc_cb_1, m3kit_nc_cb_1, m4kit_nc_cb_1, m5kit_nc_cb_1, m6kit_nc_cb_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_nc_cb_1, m2kit_nc_cb_1, m3kit_nc_cb_1, m4kit_nc_cb_1, m5kit_nc_cb_1, m6kit_nc_cb_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))  

# With control (Appendix F-10)
m3kit_cb_1 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m3kit_cb_1)

m5kit_cb_1 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m5kit_cb_1)

m4kit_cb_1 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m4kit_cb_1)

m6kit_cb_1 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m6kit_cb_1)

m1kit_cb_1 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m1kit_cb_1)

m2kit_cb_1 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m2kit_cb_1)

# generate table
stargazer(m1kit_cb_1, m2kit_cb_1, m3kit_cb_1, m4kit_cb_1, m5kit_cb_1, m6kit_cb_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_cb_1, m2kit_cb_1, m3kit_cb_1, m4kit_cb_1, m5kit_cb_1, m6kit_cb_1, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))     



#### Wave 2 ####
# reorder group
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("control", "positive", "negative", "additive"))

# Without control (Appendix F-11)
m3kit_nc_cb_2 <- lm(gov_assess_1 ~ kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave2_full)
summary(m3kit_nc_cb_2)

m5kit_nc_cb_2 <- lm(gov_assess_2 ~ kit_got_2 + group + group:kit_got_2, 
                    data=Survey_Wave2_full)
summary(m5kit_nc_cb_2)

m4kit_nc_cb_2 <- lm(gov_assess_3 ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave2_full)
summary(m4kit_nc_cb_2)

m6kit_nc_cb_2 <- lm(gov_assess_4 ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave2_full)
summary(m6kit_nc_cb_2)

m1kit_nc_cb_2 <- lm(china_overall_num ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave2_full)
summary(m1kit_nc_cb_2)

m2kit_nc_cb_2 <- lm(china_gov_num ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave2_full)
summary(m2kit_nc_cb_2)

# generate table
stargazer(m1kit_nc_cb_2, m2kit_nc_cb_2, m3kit_nc_cb_2, m4kit_nc_cb_2, m5kit_nc_cb_2, m6kit_nc_cb_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_nc_cb_2, m2kit_nc_cb_2, m3kit_nc_cb_2, m4kit_nc_cb_2, m5kit_nc_cb_2, m6kit_nc_cb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))   


# With control (Appendix F-12)
m3kit_cb_2 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave2_full)
summary(m3kit_cb_2)

m5kit_cb_2 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave2_full)
summary(m5kit_cb_2)

m4kit_cb_2 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave2_full)
summary(m4kit_cb_2)

m6kit_cb_2 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave2_full)
summary(m6kit_cb_2)

m1kit_cb_2 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave2_full)
summary(m1kit_cb_2)

m2kit_cb_2 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave2_full)
summary(m2kit_cb_2)

# generate table
stargazer(m1kit_cb_2, m2kit_cb_2, m3kit_cb_2, m4kit_cb_2, m5kit_cb_2, m6kit_cb_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_cb_2, m2kit_cb_2, m3kit_cb_2, m4kit_cb_2, m5kit_cb_2, m6kit_cb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))     



####### Negative as baseline ########

#### Wave 1 ####
# reorder group
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("negative", "positive", "additive", "control"))

# Without control (Appendix F-9)
m3kit_nc_nb_1 <- lm(gov_assess_1 ~ kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave1_full)
summary(m3kit_nc_nb_1)

m5kit_nc_nb_1 <- lm(gov_assess_2 ~ kit_got_2 + group + group:kit_got_2, 
                    data=Survey_Wave1_full)
summary(m5kit_nc_nb_1)

m4kit_nc_nb_1 <- lm(gov_assess_3 ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave1_full)
summary(m4kit_nc_nb_1)

m6kit_nc_nb_1 <- lm(gov_assess_4 ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave1_full)
summary(m6kit_nc_nb_1)

m1kit_nc_nb_1 <- lm(china_overall_num ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave1_full)
summary(m1kit_nc_nb_1)

m2kit_nc_nb_1 <- lm(china_gov_num ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave1_full)
summary(m2kit_nc_nb_1)

# generate table
stargazer(m1kit_nc_nb_1, m2kit_nc_nb_1, m3kit_nc_nb_1, m4kit_nc_nb_1, m5kit_nc_nb_1, m6kit_nc_nb_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_nc_nb_1, m2kit_nc_nb_1, m3kit_nc_nb_1, m4kit_nc_nb_1, m5kit_nc_nb_1, m6kit_nc_nb_1,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))   


# With control (Appendix F-10)
m3kit_nb_1 <- lm(gov_assess_1 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m3kit_nb_1)

m5kit_nb_1 <- lm(gov_assess_2 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m5kit_nb_1)

m4kit_nb_1 <- lm(gov_assess_3 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m4kit_nb_1)

m6kit_nb_1 <- lm(gov_assess_4 ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m6kit_nb_1)

m1kit_nb_1 <- lm(china_overall_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m1kit_nb_1)

m2kit_nb_1 <- lm(china_gov_num ~ 
                female + education_gra + STEM + family_regime_tie + 
                ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                group:kit_got_2, 
                data=Survey_Wave1_full)
summary(m2kit_nb_1)

# generate table
stargazer(m1kit_nb_1, m2kit_nb_1, m3kit_nb_1, m4kit_nb_1, m5kit_nb_1, m6kit_nb_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_nb_1, m2kit_nb_1, m3kit_nb_1, m4kit_nb_1, m5kit_nb_1, m6kit_nb_1, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))     


#### Wave 2 ####
# reorder group
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("negative", "positive", "additive", "control"))

# Without control (Appendix F-11)
m3kit_nc_nb_2 <- lm(gov_assess_1 ~ kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave2_full)
summary(m3kit_nc_nb_2)

m5kit_nc_nb_2 <- lm(gov_assess_2 ~ kit_got_2 + group + group:kit_got_2, 
                    data=Survey_Wave2_full)
summary(m5kit_nc_nb_2)

m4kit_nc_nb_2 <- lm(gov_assess_3 ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave2_full)
summary(m4kit_nc_nb_2)

m6kit_nc_nb_2 <- lm(gov_assess_4 ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave2_full)
summary(m6kit_nc_nb_2)

m1kit_nc_nb_2 <- lm(china_overall_num ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave2_full)
summary(m1kit_nc_nb_2)

m2kit_nc_nb_2 <- lm(china_gov_num ~kit_got_2 + group + group:kit_got_2,
                    data=Survey_Wave2_full)
summary(m2kit_nc_nb_2)

# generate table
stargazer(m1kit_nc_nb_2, m2kit_nc_nb_2, m3kit_nc_nb_2, m4kit_nc_nb_2, m5kit_nc_nb_2, m6kit_nc_nb_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_nc_nb_2, m2kit_nc_nb_2, m3kit_nc_nb_2, m4kit_nc_nb_2, m5kit_nc_nb_2, m6kit_nc_nb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))   


# With control (Appendix F-12)
m3kit_nb_2 <- lm(gov_assess_1 ~ 
                   female + education_gra + STEM + family_regime_tie + 
                   ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                   COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                   group:kit_got_2, 
                   data=Survey_Wave2_full)
summary(m3kit_nb_2)

m5kit_nb_2 <- lm(gov_assess_2 ~ 
                   female + education_gra + STEM + family_regime_tie + 
                   ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                   COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                   group:kit_got_2, 
                 data=Survey_Wave2_full)
summary(m5kit_nb_2)

m4kit_nb_2 <- lm(gov_assess_3 ~ 
                   female + education_gra + STEM + family_regime_tie + 
                   ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                   COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                   group:kit_got_2, 
                 data=Survey_Wave2_full)
summary(m4kit_nb_2)

m6kit_nb_2 <- lm(gov_assess_4 ~ 
                   female + education_gra + STEM + family_regime_tie + 
                   ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                   COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                   group:kit_got_2, 
                 data=Survey_Wave2_full)
summary(m6kit_nb_2)

m1kit_nb_2 <- lm(china_overall_num ~ 
                   female + education_gra + STEM + family_regime_tie + 
                   ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                   COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                   group:kit_got_2, 
                 data=Survey_Wave2_full)
summary(m1kit_nb_2)

m2kit_nb_2 <- lm(china_gov_num ~ 
                   female + education_gra + STEM + family_regime_tie + 
                   ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                   COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                   group:kit_got_2, 
                 data=Survey_Wave2_full)
summary(m2kit_nb_2)

# generate table
stargazer(m1kit_nb_2, m2kit_nb_2, m3kit_nb_2, m4kit_nb_2, m5kit_nb_2, m6kit_nb_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1kit_nb_2, m2kit_nb_2, m3kit_nb_2, m4kit_nb_2, m5kit_nb_2, m6kit_nb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))     




######### Heterogeneous Effects: COVID-19 assess in U.S. ########################
######### Control as baseline

##### Wave 1 #####
# reorder group
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("control", "positive", "negative", "additive"))

# Without control (Appendix F-13)
m3covid_nc_cb_1 <- lm(gov_assess_1 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m3covid_nc_cb_1)

m5covid_nc_cb_1 <- lm(gov_assess_2 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m5covid_nc_cb_1)

m4covid_nc_cb_1 <- lm(gov_assess_3 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m4covid_nc_cb_1)

m6covid_nc_cb_1 <- lm(gov_assess_4 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m6covid_nc_cb_1)

m1covid_nc_cb_1 <- lm(china_overall_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave1_full)
summary(m1covid_nc_cb_1)

m2covid_nc_cb_1 <- lm(china_gov_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m2covid_nc_cb_1)

# generate table
stargazer(m1covid_nc_cb_1, m2covid_nc_cb_1, m3covid_nc_cb_1, m4covid_nc_cb_1, m5covid_nc_cb_1, m6covid_nc_cb_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_nc_cb_1, m2covid_nc_cb_1, m3covid_nc_cb_1, m4covid_nc_cb_1, m5covid_nc_cb_1, m6covid_nc_cb_1, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With control (Appendix F-14)
m3covid_cb_1 <- lm(gov_assess_1 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m3covid_cb_1)

m5covid_cb_1 <- lm(gov_assess_2 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m5covid_cb_1)

m4covid_cb_1 <- lm(gov_assess_3 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m4covid_cb_1)

m6covid_cb_1 <- lm(gov_assess_4 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m6covid_cb_1)

m1covid_cb_1 <- lm(china_overall_num ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m1covid_cb_1)

m2covid_cb_1 <- lm(china_gov_num ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m2covid_cb_1)

# generate table
stargazer(m1covid_cb_1, m2covid_cb_1, m3covid_cb_1, m4covid_cb_1, m5covid_cb_1, m6covid_cb_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_cb_1, m2covid_cb_1, m3covid_cb_1, m4covid_cb_1, m5covid_cb_1, m6covid_cb_1, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


##### Wave 2 #####
# reorder group
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("control", "positive", "negative", "additive"))

# Without control (Appendix F-15)
m3covid_nc_cb_2 <- lm(gov_assess_1 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m3covid_nc_cb_2)

m5covid_nc_cb_2 <- lm(gov_assess_2 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m5covid_nc_cb_2)

m4covid_nc_cb_2 <- lm(gov_assess_3 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m4covid_nc_cb_2)

m6covid_nc_cb_2 <- lm(gov_assess_4 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m6covid_nc_cb_2)

m1covid_nc_cb_2 <- lm(china_overall_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m1covid_nc_cb_2)

m2covid_nc_cb_2 <- lm(china_gov_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                   data=Survey_Wave2_full)
summary(m2covid_nc_cb_2)

# generate table
stargazer(m1covid_nc_cb_2, m2covid_nc_cb_2, m3covid_nc_cb_2, m4covid_nc_cb_2, m5covid_nc_cb_2, m6covid_nc_cb_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_nc_cb_2, m2covid_nc_cb_2, m3covid_nc_cb_2, m4covid_nc_cb_2, m5covid_nc_cb_2, m6covid_nc_cb_2,
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With control (Appendix F-16)
m3covid_cb_2 <- lm(gov_assess_1 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m3covid_cb_2)

m5covid_cb_2 <- lm(gov_assess_2 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m5covid_cb_2)

m4covid_cb_2 <- lm(gov_assess_3 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m4covid_cb_2)

m6covid_cb_2 <- lm(gov_assess_4 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m6covid_cb_2)

m1covid_cb_2 <- lm(china_overall_num ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m1covid_cb_2)

m2covid_cb_2 <- lm(china_gov_num ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m2covid_cb_2)

# generate table
stargazer(m1covid_cb_2, m2covid_cb_2, m3covid_cb_2, m4covid_cb_2, m5covid_cb_2, m6covid_cb_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_cb_2, m2covid_cb_2, m3covid_cb_2, m4covid_cb_2, m5covid_cb_2, m6covid_cb_2,  
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))



########## Negative as baseline

##### Wave 1 #####
# reorder group
Survey_Wave1_full$group <- factor(Survey_Wave1_full$group, levels = c("negative", "positive", "additive", "control"))

# Without control (Appendix F-13)
m3covid_nc_nb_1 <- lm(gov_assess_1 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m3covid_nc_nb_1)

m5covid_nc_nb_1 <- lm(gov_assess_2 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m5covid_nc_nb_1)

m4covid_nc_nb_1 <- lm(gov_assess_3 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m4covid_nc_nb_1)

m6covid_nc_nb_1 <- lm(gov_assess_4 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m6covid_nc_nb_1)

m1covid_nc_nb_1 <- lm(china_overall_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m1covid_nc_nb_1)

m2covid_nc_nb_1 <- lm(china_gov_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave1_full)
summary(m2covid_nc_nb_1)

# generate table
stargazer(m1covid_nc_nb_1, m2covid_nc_nb_1, m3covid_nc_nb_1, m4covid_nc_nb_1, m5covid_nc_nb_1, m6covid_nc_nb_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_nc_nb_1, m2covid_nc_nb_1, m3covid_nc_nb_1, m4covid_nc_nb_1, m5covid_nc_nb_1, m6covid_nc_nb_1,  
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With control (Appendix F-14)
m3covid_nb_1 <- lm(gov_assess_1 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m3covid_nb_1)

m5covid_nb_1 <- lm(gov_assess_2 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m5covid_nb_1)

m4covid_nb_1 <- lm(gov_assess_3 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m4covid_nb_1)

m6covid_nb_1 <- lm(gov_assess_4 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m6covid_nb_1)

m1covid_nb_1 <- lm(china_overall_num ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m1covid_nb_1)

m2covid_nb_1 <- lm(china_gov_num ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave1_full)
summary(m2covid_nb_1)

# generate table
stargazer(m1covid_nb_1, m2covid_nb_1, m3covid_nb_1, m4covid_nb_1, m5covid_nb_1, m6covid_nb_1, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_nb_1, m2covid_nb_1, m3covid_nb_1, m4covid_nb_1, m5covid_nb_1, m6covid_nb_1, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


##### Wave 2 #####
# reorder group
Survey_Wave2_full$group <- factor(Survey_Wave2_full$group, levels = c("negative", "positive", "additive", "control"))

# Without control (Appendix F-15)
m3covid_nc_nb_2 <- lm(gov_assess_1 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave2_full)
summary(m3covid_nc_nb_2)

m5covid_nc_nb_2 <- lm(gov_assess_2 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave2_full)
summary(m5covid_nc_nb_2)

m4covid_nc_nb_2 <- lm(gov_assess_3 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave2_full)
summary(m4covid_nc_nb_2)

m6covid_nc_nb_2 <- lm(gov_assess_4 ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave2_full)
summary(m6covid_nc_nb_2)

m1covid_nc_nb_2 <- lm(china_overall_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave2_full)
summary(m1covid_nc_nb_2)

m2covid_nc_nb_2 <- lm(china_gov_num ~ COVIDassess_pca + group + group:COVIDassess_pca, 
                      data=Survey_Wave2_full)
summary(m2covid_nc_nb_2)

# generate table
stargazer(m1covid_nc_nb_2, m2covid_nc_nb_2, m3covid_nc_nb_2, m4covid_nc_nb_2, m5covid_nc_nb_2, m6covid_nc_nb_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_nc_nb_2, m2covid_nc_nb_2, m3covid_nc_nb_2, m4covid_nc_nb_2, m5covid_nc_nb_2, m6covid_nc_nb_2, 
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))


# With control (Appendix F-16)
m3covid_nb_2 <- lm(gov_assess_1 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m3covid_nb_2)

m5covid_nb_2 <- lm(gov_assess_2 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m5covid_nb_2)

m4covid_nb_2 <- lm(gov_assess_3 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m4covid_nb_2)

m6covid_nb_2 <- lm(gov_assess_4 ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m6covid_nb_2)

m1covid_nb_2 <- lm(china_overall_num ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m1covid_nb_2)

m2covid_nb_2 <- lm(china_gov_num ~ 
                  female + education_gra + STEM + family_regime_tie + 
                  ideology_pca + socialized_US + race_experience + know_pos + income_us + 
                  COVIDassess_pca + kit_got_2 + kit_timely_2 + return_china + group + 
                  group:COVIDassess_pca, 
                  data=Survey_Wave2_full)
summary(m2covid_nb_2)

# generate table
stargazer(m1covid_nb_2, m2covid_nb_2, m3covid_nb_2, m4covid_nb_2, m5covid_nb_2, m6covid_nb_2, 
          align=TRUE, type="text", star.cutoffs = c(0.1, 0.05, 0.01))
# to paste in Latex
#stargazer(m1covid_nb_2, m2covid_nb_2, m3covid_nb_2, m4covid_nb_2, m5covid_nb_2, m6covid_nb_2,  
#          align=TRUE, star.cutoffs = c(0.1, 0.05, 0.01))



# ============================== Graphs for Appendix G: Political Ideology ==============================
#### Generate PCA of ideology ####
#### Wave 1 ####
pca_ideology.df1 <- 
  Survey_Wave1[,c("ideology_1_num", "ideology_2_num", "ideology_3_num", "ideology_4_num", "ideology_5_num")]
names(pca_ideology.df1) <- c("Open Info.", "Human Rights", "Uni. Suffrage", "Free Speech", "Multiparty")
pca_ideology1 <- PCA(pca_ideology.df1, scale.unit = TRUE, ncp = 12, graph = FALSE)

# put pc1 from pca_ideology into data
pca_ideology.df1 <- cbind(pca_ideology.df1, pca_ideology1$ind$coord[,1])
names(pca_ideology.df1) <- c("Open Info.", "Human Rights", 
                            "Uni. Suffrage", "Free Speech", 
                            "Multiparty", "Ideology PCA")
Survey_Wave1$ideology_pca <- pca_ideology1$ind$coord[,1]
Survey_Wave1$ideology_pca <- rescale(Survey_Wave1$ideology_pca, to = c(1, 4))


ideology.df1 <- data.frame("Ideology" = c(Survey_Wave1[,"ideology_1_num"], 
                                         Survey_Wave1[,"ideology_2_num"], 
                                         Survey_Wave1[,"ideology_3_num"], 
                                         Survey_Wave1[,"ideology_4_num"], 
                                         Survey_Wave1[,"ideology_5_num"]), 
                          "Question"=rep(c("Open Info.", "Human Rights", "Uni. Suffrage", 
                                           "Free Speech", "Multiparty"), 
                                         each = nrow(Survey_Wave1)))

ideology.df1[which(ideology.df1$Ideology==1),"Ideology"] <- "Strongly disagree"
ideology.df1[which(ideology.df1$Ideology==2),"Ideology"] <- "Disagree"
ideology.df1[which(ideology.df1$Ideology==3),"Ideology"] <- "Agree"
ideology.df1[which(ideology.df1$Ideology==4),"Ideology"] <- "Strongly agree"
ideology.df1$Ideology <- 
  factor(ideology.df1$Ideology, levels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
ideology.df1$Question <- 
  factor(ideology.df1$Question, levels = c("Open Info.", "Human Rights", 
                                          "Uni. Suffrage", "Free Speech", "Multiparty"))


#### Wave 2 ####
pca_ideology.df2 <- 
  Survey_Wave2[,c("ideology_1_num", "ideology_2_num", "ideology_3_num", "ideology_4_num", "ideology_5_num")]
names(pca_ideology.df2) <- c("Open Info.", "Human Rights", "Uni. Suffrage", "Free Speech", "Multiparty")
pca_ideology2 <- PCA(pca_ideology.df2, scale.unit = TRUE, ncp = 12, graph = FALSE)

# put pc1 from pca_ideology into data
pca_ideology.df2 <- cbind(pca_ideology.df2, pca_ideology2$ind$coord[,1])
names(pca_ideology.df2) <- c("Open Info.", "Human Rights", 
                             "Uni. Suffrage", "Free Speech", 
                             "Multiparty", "Ideology PCA")
Survey_Wave2$ideology_pca <- pca_ideology2$ind$coord[,1]
Survey_Wave2$ideology_pca <- rescale(Survey_Wave2$ideology_pca, to = c(1, 4))



ideology.df2 <- data.frame("Ideology" = c(Survey_Wave2[,"ideology_1_num"], 
                                          Survey_Wave2[,"ideology_2_num"], 
                                          Survey_Wave2[,"ideology_3_num"], 
                                          Survey_Wave2[,"ideology_4_num"], 
                                          Survey_Wave2[,"ideology_5_num"]), 
                           "Question"=rep(c("Open Info.", "Human Rights", "Uni. Suffrage", 
                                            "Free Speech", "Multiparty"), 
                                          each = nrow(Survey_Wave2)))

ideology.df2[which(ideology.df2$Ideology==1),"Ideology"] <- "Strongly disagree"
ideology.df2[which(ideology.df2$Ideology==2),"Ideology"] <- "Disagree"
ideology.df2[which(ideology.df2$Ideology==3),"Ideology"] <- "Agree"
ideology.df2[which(ideology.df2$Ideology==4),"Ideology"] <- "Strongly agree"
ideology.df2$Ideology <- 
  factor(ideology.df2$Ideology, levels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
ideology.df2$Question <- 
  factor(ideology.df2$Question, levels = c("Open Info.", "Human Rights", 
                                           "Uni. Suffrage", "Free Speech", "Multiparty"))


##### Generate Appendix Figure G-1: Responses to Questions about Ideology
p1 <- ggplot(ideology.df1, aes(x = Question)) +
     geom_histogram(stat="count", aes(fill = Ideology), color="black", size=0.4, 
                 position = position_dodge(width = 0.8), alpha = 0.4) +
     scale_fill_manual(values = c("black", "gray28", "gray60", "white")) + 
     theme_bw() + 
     ggtitle("Wave 1") + 
     theme(legend.position="none") + 
     theme(plot.title = element_text(face="bold")) + 
     theme(plot.title = element_text(hjust = 0.5, size=20)) + 
     labs(x = "") + 
     labs(y = "Count") + 
     theme(axis.title.x = element_blank(),
           axis.title.y = element_text(size=15)) + 
     theme(axis.text.x= element_text(colour="black", size=13), 
           axis.text.y= element_text(colour="black", size=15))

p2 <- ggplot(ideology.df2, aes(x = Question)) +
  geom_histogram(stat="count", aes(fill = Ideology), color="black", size=0.4, 
                 position = position_dodge(width = 0.8), alpha = 0.4) +
  scale_fill_manual(values = c("black", "gray28", "gray60", "white")) + 
  theme_bw() + 
  ggtitle("Wave 2") + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(face="bold")) + 
  theme(plot.title = element_text(hjust = 0.5, size=20)) + 
  labs(x = "") + 
  labs(y = "Count") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15)) + 
  theme(axis.text.x= element_text(colour="black", size=13), 
        axis.text.y= element_text(colour="black", size=15))

pdf("ideology.pdf",width = 8,height= 7, onefile=F)
ggarrange(p1, p2, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
dev.off()

##### Generate Appendix Figure G-2: Correlations of Ideology Responses 
 pdf("cor_ideology.pdf",width = 8,height= 5, onefile=F)
 par(mfrow=c(1,2))
 #### Wave 1 ####
 p1 <- corrplot(cor(pca_ideology.df1, 
                    use = "pairwise.complete.obs", method ="spearman"), type="full", order="original", 
                tl.col="black", tl.srt=45, tl.cex=0.65,
                method ="color", addCoef.col="black", 
                number.cex=0.75, number.font=2, 
                col = colorRampPalette(c("white", "white", "gray55"))(200), is.corr = TRUE)

mtext(expression(bold("Wave 1")), line=0.5, cex=1)
#### Wave 2 ####
p2 <- corrplot(cor(pca_ideology.df2, 
                  use = "pairwise.complete.obs", method ="spearman"), type="full", order="original", 
               tl.col="black", tl.srt=45, tl.cex=0.65, 
               method ="color", addCoef.col="black", 
               number.cex=0.75, number.font=2, 
               col = colorRampPalette(c("white", "white", "gray55"))(200), is.corr = TRUE)

mtext(expression(bold("Wave 2")), line=0.5, cex=1)

dev.off()


# Generate Appendix Figure G-3: Scree Plot of Ideology Variables
#### Wave 1 ####
fviz_screeplot(pca_ideology1)

screeplot_var1 <-
  fviz_eig(pca_ideology1, choice = "variance", geom = c("bar", "line"), addlabels = TRUE, 
           ylim = c(0, 50), barfill = "grey", 
           barcolor = "grey", linecolor = "black") + 
  ggtitle("Wave 1") + 
  labs(y = "Variance Explained") + 
  scale_y_continuous(limits = c(0, 50), breaks=seq(0,50,10)) + 
  theme(plot.title = element_text(hjust = 0.5, size=15,face ='bold'))  + 
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15)) +
  theme(axis.text.x= element_text(colour="black", size=10), 
        axis.text.y= element_text(colour="black", size=10))

#### Wave 2 ####
fviz_screeplot(pca_ideology2)

screeplot_var2 <-
  fviz_eig(pca_ideology2, choice = "variance", geom = c("bar", "line"), addlabels = TRUE, 
           ylim = c(0, 50), barfill = "grey", 
           barcolor = "grey", linecolor = "black") + 
  ggtitle("Wave 2") + 
  labs(y = "Variance Explained") + 
  scale_y_continuous(limits = c(0, 50), breaks=seq(0,50,10)) + 
  theme(plot.title = element_text(hjust = 0.5, size=15,face ='bold'))  + 
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15)) +
  theme(axis.text.x= element_text(colour="black", size=10), 
        axis.text.y= element_text(colour="black", size=10))

combined <- gridExtra::grid.arrange(screeplot_var1,screeplot_var2)
ggsave(filename="screeplot_var_ideology.pdf", plot=combined, width = 6, height = 10)


# Generate Appendix Figure G-4
#### Wave 1
pcagraph_var1 <-
  fviz_pca_var(pca_ideology1, axes = c(1, 2), geom = c("arrow", "text"),
               col.var = "black", repel = TRUE, alpha.var=1) + 
  ggtitle("Wave 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=20, face ='bold')) + 
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))
#### Wave 2
pcagraph_var2 <-
  fviz_pca_var(pca_ideology2, axes = c(1, 2), geom = c("arrow", "text"),
               col.var = "black", repel = TRUE, alpha.var=1) + 
  ggtitle("Wave 2") + 
  theme(plot.title = element_text(hjust = 0.5, size=20, face ='bold')) + 
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

combined <- gridExtra::grid.arrange(pcagraph_var1,pcagraph_var2)
ggsave(filename="pcagraph_var_ideology.pdf", plot=combined, width = 6, height = 10)


# ============================== Graphs for Appendix H: COVID assess ==============================
#### Generate PCA of COVID assess ####
#### Wave 1 ####
pca_COVIDassess.df1 <- 
  Survey_Wave1[,c("covid_assess_1_num", "covid_assess_2_num", "covid_assess_3_num")]
names(pca_COVIDassess.df1) <- c("Nationwide", "State", "County")
colnames(pca_COVIDassess.df1)
pca_COVIDassess1 <- PCA(pca_COVIDassess.df1, scale.unit = TRUE, ncp = 12, graph = FALSE)

# put pc1 from pca_assess into data
pca_COVIDassess.df1 <- cbind(pca_COVIDassess.df1, pca_COVIDassess1$ind$coord[,1])
names(pca_COVIDassess.df1) <- c("Nationwide", "State", "County", "U.S. Severity PCA")
Survey_Wave1$COVIDassess_pca <- pca_COVIDassess1$ind$coord[,1]
Survey_Wave1$COVIDassess_pca <- rescale(Survey_Wave1$COVIDassess_pca, to = c(1, 5))


covid_assess.df1 <- data.frame("covid_assess" = c(Survey_Wave1[,"covid_assess_1_num"], 
                                                 Survey_Wave1[,"covid_assess_2_num"], 
                                                 Survey_Wave1[,"covid_assess_3_num"]), 
                              "Level"=rep(c("Nationwide", "State", "County"), each = nrow(Survey_Wave1)))

covid_assess.df1$Level <- 
  factor(covid_assess.df1$Level, levels = c("Nationwide", "State", "County"))
covid_assess.df1[which(covid_assess.df1$covid_assess==1),"covid_assess"] <- "Not at all"
covid_assess.df1[which(covid_assess.df1$covid_assess==2),"covid_assess"] <- "Not quite"
covid_assess.df1[which(covid_assess.df1$covid_assess==3),"covid_assess"] <- "Neutral"
covid_assess.df1[which(covid_assess.df1$covid_assess==4),"covid_assess"] <- "Quite"
covid_assess.df1[which(covid_assess.df1$covid_assess==5),"covid_assess"] <- "Very"
covid_assess.df1$covid_assess <- 
  factor(covid_assess.df1$covid_assess, levels = c("Not at all", "Not quite", "Neutral", "Quite", "Very"))


#### Wave 2 ####
pca_COVIDassess.df2 <- 
  Survey_Wave2[,c("covid_assess_1_num", "covid_assess_2_num", "covid_assess_3_num")]
names(pca_COVIDassess.df2) <- c("Nationwide", "State", "County")
colnames(pca_COVIDassess.df2)
pca_COVIDassess2 <- PCA(pca_COVIDassess.df2, scale.unit = TRUE, ncp = 12, graph = FALSE)

# put pc1 from pca_assess into data
pca_COVIDassess.df2 <- cbind(pca_COVIDassess.df2, pca_COVIDassess2$ind$coord[,1])
names(pca_COVIDassess.df2) <- c("Nationwide", "State", "County", "U.S. Severity PCA")
Survey_Wave2$COVIDassess_pca <- pca_COVIDassess2$ind$coord[,1]
Survey_Wave2$COVIDassess_pca <- rescale(Survey_Wave2$COVIDassess_pca, to = c(1, 5))


covid_assess.df2 <- data.frame("covid_assess" = c(Survey_Wave2[,"covid_assess_1_num"], 
                                                 Survey_Wave2[,"covid_assess_2_num"], 
                                                 Survey_Wave2[,"covid_assess_3_num"]), 
                              "Level"=rep(c("Nationwide", "State", "County"), each = nrow(Survey_Wave2)))

covid_assess.df2$Level <- 
  factor(covid_assess.df2$Level, levels = c("Nationwide", "State", "County"))
covid_assess.df2[which(covid_assess.df2$covid_assess==1),"covid_assess"] <- "Not at all"
covid_assess.df2[which(covid_assess.df2$covid_assess==2),"covid_assess"] <- "Not quite"
covid_assess.df2[which(covid_assess.df2$covid_assess==3),"covid_assess"] <- "Neutral"
covid_assess.df2[which(covid_assess.df2$covid_assess==4),"covid_assess"] <- "Quite"
covid_assess.df2[which(covid_assess.df2$covid_assess==5),"covid_assess"] <- "Very"
covid_assess.df2$covid_assess <- 
  factor(covid_assess.df2$covid_assess, levels = c("Not at all", "Not quite", "Neutral", "Quite", "Very"))


# Generates Appendix Figure H-1: How severe is the COVID-19
p1 <- ggplot(covid_assess.df1, aes(x = covid_assess)) +
      geom_histogram(stat="count", aes(fill = Level), color="black", size=0.4, 
                 position = position_dodge(width = 0.8), alpha = 0.4) +
      scale_fill_manual(values = c("gray28", "gray87", "white")) + 
      theme_bw() + 
      ggtitle("Wave 1") + 
      theme(legend.position="bottom") + 
      theme(plot.title = element_text(face="bold")) + 
      theme(plot.title = element_text(hjust = 0.5, size=20)) + 
      labs(x = "") + 
      labs(y = "Count") + 
      scale_y_continuous(limits = c(0, 300), breaks=seq(0,300,50)) + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size=15)) + 
      theme(axis.text.x= element_text(colour="black", size=13), 
            axis.text.y= element_text(colour="black", size=15))


p2 <- ggplot(covid_assess.df2, aes(x = covid_assess)) +
  geom_histogram(stat="count", aes(fill = Level), color="black", size=0.4, 
                 position = position_dodge(width = 0.8), alpha = 0.4) +
  scale_fill_manual(values = c("gray28", "gray87", "white")) + 
  theme_bw() + 
  ggtitle("Wave 2") + 
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(face="bold")) + 
  theme(plot.title = element_text(hjust = 0.5, size=20)) + 
  labs(x = "") + 
  labs(y = "Count") + 
  scale_y_continuous(limits = c(0, 350), breaks=seq(0,300,50)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15)) + 
  theme(axis.text.x= element_text(colour="black", size=13), 
        axis.text.y= element_text(colour="black", size=15))


pdf("covid_assess.pdf",width = 8,height= 7, onefile=F)
ggarrange(p1, p2, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
dev.off()


# Generates Appendix Figure H-2: Correlations of COVID-19 assessments
pdf("cor_COVIDassess.pdf",width = 8,height= 5, onefile=F)
par(mfrow=c(1,2))
#### Wave 1 ####
p1 <- corrplot(cor(pca_COVIDassess.df1, 
                   use = "pairwise.complete.obs", method ="spearman"), type="full", order="original", 
               tl.col="black", tl.srt=45, tl.cex=0.65, 
               method ="color", addCoef.col="black", 
               number.cex=0.75, number.font=2, 
               col = colorRampPalette(c("white", "white", "gray55"))(200), is.corr = TRUE)

mtext(expression(bold("Wave 1")), line=0.5, cex=1)
#### Wave 2 ####
p2 <- corrplot(cor(pca_COVIDassess.df2, 
                   use = "pairwise.complete.obs", method ="spearman"), type="full", order="original", 
               tl.col="black", tl.srt=45, tl.cex=0.65, 
               method ="color", addCoef.col="black", 
               number.cex=0.75, number.font=2, 
               col = colorRampPalette(c("white", "white", "gray55"))(200), is.corr = TRUE)

mtext(expression(bold("Wave 2")), line=0.5, cex=1)

dev.off()


# Generates Appendix Figure H-3: Scree Plot using variance
#### Wave 1 ####
screeplot_var1 <-
  fviz_eig(pca_COVIDassess1, choice = "variance", geom = c("bar", "line"), addlabels = TRUE, 
           ylim = c(0, 70), barfill = "grey", 
           barcolor = "grey", linecolor = "black") + 
  ggtitle("Wave 1") + 
  labs(y = "Variance Explained") + 
  scale_y_continuous(limits = c(0, 70), breaks=seq(0,60,20)) + 
  theme(plot.title = element_text(hjust = 0.5, size=20))  + 
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20)) +
  theme(axis.text.x= element_text(colour="black", size=15), 
        axis.text.y= element_text(colour="black", size=15))

#### Wave 2 ####
screeplot_var2 <-
  fviz_eig(pca_COVIDassess2, choice = "variance", geom = c("bar", "line"), addlabels = TRUE, 
           ylim = c(0, 80), barfill = "grey", 
           barcolor = "grey", linecolor = "black") + 
  ggtitle("Wave 2") + 
  labs(y = "Variance Explained") + 
  scale_y_continuous(limits = c(0, 80), breaks=seq(0,60,20)) + 
  theme(plot.title = element_text(hjust = 0.5, size=20))  + 
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20)) +
  theme(axis.text.x= element_text(colour="black", size=15), 
        axis.text.y= element_text(colour="black", size=15))

combined <- gridExtra::grid.arrange(screeplot_var1,screeplot_var2)
ggsave(filename="screeplot_var_COVIDassess.pdf", plot=combined, width = 6, height = 10)


# Generates Figure H-4: Graph of variables
#### Wave 1
pcagraph_var1 <-
  fviz_pca_var(pca_COVIDassess1, axes = c(1, 2), geom = c("arrow", "text"),
               col.var = "black", repel = TRUE, alpha.var=1) + 
  ggtitle("Wave 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=20)) + 
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))
#### Wave 2
pcagraph_var2 <-
  fviz_pca_var(pca_COVIDassess2, axes = c(1, 2), geom = c("arrow", "text"),
               col.var = "black", repel = TRUE, alpha.var=1) + 
  ggtitle("Wave 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=20)) + 
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

combined <- gridExtra::grid.arrange(pcagraph_var1,pcagraph_var2)
ggsave(filename="pcagraph_var_COVIDassess.pdf", plot=combined, width = 6, height = 10)



