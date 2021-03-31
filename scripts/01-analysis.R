##############################################################################
# File-Name: 01-analysis.R
# Date: 2021
# Purpose: Main script that creates the indicators, tests the covariate balance 
#          (of the final sample and the attrition), specifies the models, 
#          and runs the intent-to-treat analysis.
# Machine: MacOS High Sierra
##############################################################################

# Set your own working directory
rootArg <-  if(Sys.info()["user"]=="nejlaasimovic"){
setwd("/Users/nejlaasimovic/Desktop/pnas_revisions/code_pnas")
}
sessionInfo()
# Source the analysis functions
source("/Users/nejlaasimovic/Desktop/pnas_revisions/code_pnas/functions.R") 

# Install ("install.packages()") or/and activate packages
library(ggplot2); library(ggpubr); 
library(stringr); library(estimatr); 
library(Rmisc); library(devtools);
library(plotly);  library(plyr);
library(sandwich);library(lmtest)

# Upload the main dataset
complete_data <- read.csv("replication_data.csv", stringsAsFactors = FALSE)

# Use only the data on final respondents (exclude attriters) for the first set of analysis
final_data <- complete_data[complete_data$attrition=="0",]

options(digits=3) # round the digits to three decimal points

# -----------------------------------------------------------------
#    Table S1: DESCRIPTIVE STATISTICS - baseline characteristics
# -----------------------------------------------------------------

des.variables <- c("age","gender","educ","employ1","trust_media.1","time_usage","freq_usage","freq_news","numb_forums","freq_fbnews","politics_int", "imp_ethn","imp_cntry","imp_relg","imp_family","imp_neigh")
stargazer::stargazer(final_data[des.variables], type="text")
# NB: Min and max are min and max possible values within the survey,
# hence, the discrepancy on imp_family (no respondent selected the min value)

# -------------------------------------------------------------------
#    Table S2: DESCRIPTIVE STATISTICS - ethnic composition
# --------------------------------------------------------------------

# Given that the ethnic and religious belonging almost completely overlap within the context,
# we combine the two, categorizing people by the ethnic/religious category they report 
final_data$ethn_t <- NA
final_data[which(final_data$ethn=="1" | final_data$ethn=="-1"),]$ethn_t <- 1 #Bosniak
final_data[which(final_data$ethn=="2"|final_data$ethn=="-4"),]$ethn_t <- 2 #Serb
final_data[which(final_data$ethn=="3"|final_data$ethn=="-5"),]$ethn_t <- 3 #Croat
final_data[which(final_data$ethn=="0"),]$ethn_t <- 4 #Bosnian
final_data[which(final_data$ethn=="-3"|final_data$ethn=="-2"),]$ethn_t <- 5 #Other/Do not want to say

tableS2<-table(final_data$ethn_t) 
names(tableS2) <- c("Bosniak","Serb","Croat","Bosnian","Other/Do not want to say") # add row names
tableS2<-cbind(tableS2, as.vector(tableS2/353*100)) # add % (share of population)
colnames(tableS2) <- c("No. of people","Share") # add column names
tableS2

# --------------------------------
#   Table S3: COVARIATE BALANCE
# ---------------------------------
# --- The appearance of Table S3 in the paper edited within Overleaf

bal.variables <- c("gender","age","educ","employ1","trust_media","freq_news","freq_fbnews","politics_int","numb_forums","time_usage", "freq_usage","imp_ethn","imp_family","imp_cntry","imp_relg","imp_career","imp_neigh","imp_hobby")
df <- final_data[,bal.variables]
# Calculate p-values and build the balance table
p.values <- cbind(sapply(bal.variables, function (x) t.test(final_data[final_data$treatment==1,][,x], final_data[final_data$treatment==0,][,x]))[3,])
balance_table <- do.call(data.frame, # creating a table combining the mean values and p-values
               list(TreatmentGroup = apply(df[final_data$treatment == 1,], 2, mean, na.rm=TRUE),
                    ControlGroup =  apply(df[final_data$treatment == 0,], 2, mean, na.rm=TRUE),
                    TreatmentSD =  apply(df[final_data$treatment == 1,], 2, sd, na.rm=TRUE),
                    ControlSD =  apply(df[final_data$treatment == 0,], 2, sd, na.rm=TRUE),
                    t.test=p.values))
balance_table

# -----------------------------------------------
#           ATTRITION 
# ------------------------------------------------

bal.variables<-c("gender","age","educ","employ1","trust_media.1","freq_news","politics_int","numb_forums","time_usage","freq_usage","imp_ethn","imp_family","imp_cntry","imp_relg","imp_career","imp_neigh")

# Comparing observable characteristics of the attrition group with the characteristics of the overall sample
p.values<-cbind(sapply(bal.variables, function (x) t.test(complete_data[complete_data$attrition=="1",][,x],complete_data[complete_data$attrition=="0",][,x]))[3,])
bal_attr_final <- do.call(data.frame, # creating a table combining the mean values and p-values
                 list(TreatmentGroup = apply(round(complete_data[,bal.variables][complete_data$attrition=="1",], digits=3), 2, mean, na.rm=TRUE),
                      ControlGroup =  apply(round(complete_data[,bal.variables][complete_data$attrition=="0",], digits=3), 2, mean, na.rm=TRUE),
                      TreatmentSD =  apply(round(complete_data[,bal.variables][complete_data$attrition=="1",], digits=3), 2, sd, na.rm=TRUE),
                      ControlSD =  apply(round(complete_data[,bal.variables][complete_data$attrition=="0",], digits=3), 2, sd, na.rm=TRUE),
                      t.test=p.values))
print(bal_attr_final , digits=4)

# Comparing observable characteristics of the attrition group with the characteristics of the overall sample, separating treatment and control
# 1. Control group: attrition & final sample
p.values0<-cbind(sapply(bal.variables, function (x) t.test(complete_data[complete_data$treatment==0 & complete_data$attrition=="0",][,x],complete_data[complete_data$treatment==0 & complete_data$attrition=="1",][,x]))[3,])
balance_attrition0 <- do.call(data.frame, # creating a table combining the mean values and p-values
                              list(CntrlParticipated = apply(complete_data[,bal.variables][complete_data$treatment==0 &complete_data$attrition=="0",], 2, mean, na.rm=TRUE),
                                   CntrlParticipated_SD =  apply(complete_data[,bal.variables][complete_data$treatment==0 &complete_data$attrition=="0",], 2, sd, na.rm=TRUE),
                                   CntrlAttrition =  apply(complete_data[,bal.variables][complete_data$treatment==0 & complete_data$attrition=="1",], 2, mean, na.rm=TRUE),
                                   CntrlAttrition_SD =  apply(complete_data[,bal.variables][complete_data$treatment==0 & complete_data$attrition=="1",], 2, sd, na.rm=TRUE),
                                   t.test=p.values0))
print(balance_attrition0, digits=4) # Control/Control Attrition

# 2. Treatment group: attrition & final sample
p.values<-cbind(sapply(bal.variables, function (x) t.test(complete_data[complete_data$treatment==1 & complete_data$attrition=="0",][,x], complete_data[complete_data$treatment==1 & complete_data$attrition=="1",][,x]))[3,])
balance_attrition1 <- do.call(data.frame, # creating a table combining the mean values and p-values
               list(TreatParticipated = apply(complete_data[,bal.variables][complete_data$treatment==1 & complete_data$attrition=="0",], 2, mean, na.rm=TRUE),
                    TreatParticipated_SD =  apply(complete_data[,bal.variables][complete_data$treatment==1 & complete_data$attrition=="0",], 2, sd, na.rm=TRUE),
                    TreatAttrition =  apply(complete_data[,bal.variables][complete_data$treatment==1 & complete_data$attrition=="1",], 2, mean, na.rm=TRUE),
                    TreatAttrition_SD =  apply(complete_data[,bal.variables][complete_data$treatment==1 & complete_data$attrition=="1",], 2, sd, na.rm=TRUE),
                    t.test=p.values))
print(balance_attrition1, digits=4) # Treatment/Treatment Attrition

# Comparing treatment and control attrition samples
p.values_comp <- cbind(sapply(bal.variables, function (x) t.test(complete_data[complete_data$treatment==0 & complete_data$attrition=="1",][,x], complete_data[complete_data$treatment==1 & complete_data$attrition=="1",][,x]))[3,])
balance_attr <- do.call(data.frame, # creating a table combining the mean values and p-values
                              list(CntrlAttrition = apply(complete_data[,bal.variables][complete_data$treatment==0 & complete_data$attrition=="1",], 2, mean, na.rm=TRUE),
                                   CntrlAttrition_SD =  apply(complete_data[,bal.variables][complete_data$treatment==0 & complete_data$attrition=="1",], 2, sd, na.rm=TRUE),
                                   TreatAttrition =  apply(complete_data[,bal.variables][complete_data$treatment==1 & complete_data$attrition=="1",], 2, mean, na.rm=TRUE),
                                   TreatAttrition_SD =  apply(complete_data[,bal.variables][complete_data$treatment==1 & complete_data$attrition=="1",], 2, sd, na.rm=TRUE),
                                   t.test=p.values_comp))
print(balance_attr, digits=4) # Comparing treatment and control attrition

# ---------------------------------------
#    Table S5: Intention-to-Treat Results
# ---------------------------------------
# Table includes ITT results on two indices:
# 1. Subjective well-being [SWB]
# 2. Knowledge of the news

## -------------- SWB 
# Reverse coding negative emotions, so that higher values indicate more positive subjective well-being
final_data$nerv <- (-1) * final_data$nerv 
final_data$depression <- (-1) * final_data$depression
final_data$loneliness <- (-1) * final_data$loneliness
final_data$boredom <- (-1) * final_data$boredom
final_data$isol<- (-1) * final_data$isol

# Checking the internal consistency of the scale
wellbeing_vars <- c("satisf", "joy","fulf","depression","boredom","isol","loneliness","nerv")
wellbeing_df <- final_data[,wellbeing_vars]
psych::alpha(wellbeing_df, 'check.keys=TRUE')$total$std.alpha #0.829 
prcomp<- prcomp(wellbeing_df)
print(prcomp, digits=3) # checking the loadings

# Observe relatively uniform loadings across variables, proceed by creating sum score index
for (i in 1:nrow(final_data)){
  final_data$swb[i] <- scale(final_data$satisf)[i]+scale(final_data$joy)[i]+scale(final_data$fulf)[i]+scale(final_data$depression)[i] +scale(final_data$loneliness)[i] + scale(final_data$nerv)[i] 
  +scale(final_data$boredom)[i]+scale(final_data$isol[i])
}

# Referenced in the paper [within the discussion of main results]:

# Excluding the outliers
outlier.values <- boxplot.stats(final_data$swb)$out  
no_swb_outliers <- final_data[!final_data$swb %in% outlier.values,]
# Test the treatment effect (Model 3, full covariate specification) on swb excluding the outliers
reg_res_mod3("swb",no_swb_outliers)
# Test the one directional hypothesis
t.test(no_swb_outliers[no_swb_outliers$treatment=="0",]$swb, no_swb_outliers[no_swb_outliers$treatment=="1",]$swb, alternative="less")

## -------------- NEWS KNOWLEDGE

# Creating the news index as sum of correct/false responses
# (within the dataset, each  response coded -1 if a false response was given; 1 if true and 0 if "unsure" was selected)
c_news <- NA
for (i in 1:nrow(final_data)){
  final_data$c_news[i] <- final_data$fbnews_st1[i]+final_data$fbnews_st2[i]+final_data$fbnews_st3[i]+final_data$fbnews_st4[i]+final_data$fbnews_st5[i]+final_data$fbnews_st6[i]+final_data$fbnews_st7[i]+final_data$fbnews_st8[i]
}

## -------------- PUTTING IT ALL TOGETHER, TABLE S5
TableS5.1 <- do.call(data.frame,rbind(
  reg_res_mod1("c_news",final_data),
  reg_res_mod1("swb",final_data),
  reg_res_mod1("satisf",final_data),
  reg_res_mod1("joy",final_data),
  reg_res_mod1("fulf",final_data),
  reg_res_mod1("nerv",final_data),
  reg_res_mod1("boredom",final_data),
  reg_res_mod1("loneliness",final_data),
  reg_res_mod1("depression",final_data),
  reg_res_mod1("isol",final_data)))


TableS5.2 <- do.call(data.frame,rbind(
  reg_res_mod2("c_news",final_data),
  reg_res_mod2("swb",final_data),
  reg_res_mod2("satisf",final_data),
  reg_res_mod2("joy",final_data),
  reg_res_mod2("fulf",final_data),
  reg_res_mod2("nerv",final_data),
  reg_res_mod2("boredom",final_data),
  reg_res_mod2("loneliness",final_data),
  reg_res_mod2("depression",final_data),
  reg_res_mod2("isol",final_data)))


TableS5.3 <- do.call(data.frame,rbind(
  reg_res_mod3("c_news",final_data),
  reg_res_mod3("swb",final_data),
  reg_res_mod3("satisf",final_data),
  reg_res_mod3("joy",final_data),
  reg_res_mod3("fulf",final_data),
  reg_res_mod3("nerv",final_data),
  reg_res_mod3("boredom",final_data),
  reg_res_mod3("loneliness",final_data),
  reg_res_mod3("depression",final_data),
  reg_res_mod3("isol",final_data)))

# -------- *Table S5* ------------
tableS5 <- join_all(list(TableS5.1, TableS5.2, TableS5.3), by="outcome") 
tableS5
# -------- *Table S5* ------------

# ---------------------------------------
#    Table S6: Intention-to-Treat Results
# ---------------------------------------

## ------- AFFECT/FEELING THERMOMETER
# Creating feeling thermometer variable: subtracting ft of the outgroup from ft of the ingroup
final_data$ft <- final_data$outfeel_therm-final_data$infeel_therm # higher number indicates more pos atribution

## ------- COOPERATION 
# Creating cooperation index as a sum of z-scores from the agreement with two statements:
# 1.Multi-ethnic parties cannot secure the interest of my people.
# 2.It is only possible to cooperate with members of my ethnic group.

for (i in 1:nrow(final_data)){
final_data$multi_stat[i] <- scale(final_data$agr_st5)[i] + scale(final_data$agr_st7)[i]}


##  -------- GROUP CHARACTERISTICS
# Perception of out-group evaluations

prcp_chrc <- c("in_broadv","in_intel","in_hyp","in_evil","in_patriot","in_gener","in_unreliab","in_honest","in_self")
# Higher values indicate higher level of disagreement, hence positive characteristics need to be recoded
# so that higher values indicate more positive attribution across (multiplied by -1 within the index creation)

for (i in 1:nrow(final_data)){
  final_data$inchrct_final_data[i] <- - final_data$in_broadv[i] - final_data$in_intel[i]+
  final_data$in_hyp [i] + final_data$in_evil[i] - final_data$in_patriot[i] - final_data$in_gener[i] +
  final_data$in_unreliab [i] - final_data$in_honest[i] + final_data$in_self[i]}

##  -------- GROUP CHARACTERISTICS
# Out-Group Traits

oth_chrc <- c("oth_broadv","oth_intel","oth_hyp","oth_evil","oth_patriot","oth_gener","oth_unreliab","oth_honest","oth_self")
# Higher values indicate higher level of disagreement, hence positive characteristics need to be recoded (multiplied by -1 within the index creation)

for (i in 1:nrow(final_data)){
  final_data$othchrct_final_data[i] <- - final_data$oth_broadv[i]  - final_data$oth_intel[i] +
  final_data$oth_hyp [i] + final_data$oth_evil[i] - final_data$oth_patriot[i] - final_data$oth_gener[i] +
  final_data$oth_unreliab [i] - final_data$oth_honest[i] + final_data$oth_self[i]}

## ------ OUTGROUP INDEX
# We use the PC score as our primary index outgroup regard, but also report results using the sum score:

# -- Index as sum of z-scores of the 5 components
final_data$outgroup_index <- scale(final_data$sd) + scale(final_data$multi_stat) + scale(final_data$ft) + scale(final_data$othchrct_final_data)+scale(final_data$inchrct_final_data)

# -- Index as a principal component score of the 5 components

# Checking internal validity and Chronbach alpha
outgroup_variables <- c("sd",
                        "multi_stat",
                        "othchrct_final_data",
                        "ft","inchrct_final_data")
outgroup_variables_pc <- final_data[,outgroup_variables]
psych::alpha(outgroup_variables_pc, 'check.keys=TRUE')$total$std.alpha #0.518
prcomp <- prcomp(outgroup_variables_pc)
print(prcomp, digits=3)

# Creating PC index of outgroup regard
group_attitudes_vars <- c("sd","multi_stat","ft","inchrct_final_data","othchrct_final_data")
final_data$outgroup_princomp <- pc_index(dataIn=final_data,
                                             varList=group_attitudes_vars)

# ---------  Table S6
TableS6.1 <- do.call(data.frame,rbind(
  reg_res_mod1("ft",final_data),
  reg_res_mod1("sd",final_data),
  reg_res_mod1("multi_stat",final_data),
  reg_res_mod1("inchrct_final_data",final_data),
  reg_res_mod1("othchrct_final_data",final_data),
  reg_res_mod1("outgroup_index",final_data),
  reg_res_mod1("outgroup_princomp",final_data)))

TableS6.2 <- do.call(data.frame,rbind(
  reg_res_mod2("ft",final_data),
  reg_res_mod2("sd",final_data),
  reg_res_mod2("multi_stat",final_data),
  reg_res_mod2("inchrct_final_data",final_data),
  reg_res_mod2("othchrct_final_data",final_data),
  reg_res_mod2("outgroup_index",final_data),
  reg_res_mod2("outgroup_princomp",final_data)))

TableS6.3 <- do.call(data.frame,rbind(
  reg_res_mod3("ft",final_data),
  reg_res_mod3("sd",final_data),
  reg_res_mod3("multi_stat",final_data),
  reg_res_mod3("inchrct_final_data",final_data),
  reg_res_mod3("othchrct_final_data",final_data),
  reg_res_mod3("outgroup_index",final_data),
  reg_res_mod3("outgroup_princomp",final_data)))


# -------- *Table S6* ------------
tableS6 <- join_all(list(TableS6.1, TableS6.2, TableS6.3), by="outcome")
print(tableS6, digits=3)
# -------- *Table S6* ------------

# --------------------------------------
#  Robustness to Outliers: Table S7
# --------------------------------------

# Examine the robustness of our findings to outliers by using Cook’s Distance
# 1. identify cases which are four times the mean value of Cook’s distance for all observations
# 2. exclude those cases, and re-estimate the model
                                                                          
var <- c("outgroup_index","outgroup_princomp","c_news","swb") # main outcome variables

sapply(var, function (i){
sec <- lm(as.formula(paste0(i,"~","treatment")), data=final_data)
cookoutgroup_index<-cooks.distance(sec) 
influential <- as.numeric(na.omit(names(cookoutgroup_index)[(cookoutgroup_index > 4*mean(cookoutgroup_index, na.rm=T))]))
final_data$rownumber <- as.numeric(rownames(final_data))
rep_rmalloutlier <- final_data[ ! final_data$rownumber %in% influential,]
print(reg_res_mod3(paste0(i), rep_rmalloutlier))
})


# --------------------------------------
#  FDR Adjusted Results: Table S8 & S9
# --------------------------------------

# Table S8, Subjective Well Being: Adjusting for a full set of covariates
p.values_swb <- as.table(c(
  reg_res_mod3("satisf",final_data)$p.value,
  reg_res_mod3("joy",final_data)$p.value,
  reg_res_mod3("fulf",final_data)$p.value,
  reg_res_mod3("nerv",final_data)$p.value,
  reg_res_mod3("boredom",final_data)$p.value,
  reg_res_mod3("loneliness",final_data)$p.value,
  reg_res_mod3("depression",final_data)$p.value,
  reg_res_mod3("isol",final_data)$p.value,
  reg_res_mod3("swb",final_data)$p.value))

names <- c("Satisfaction","Joy","Fulfillment","Anxiety (rev coded)","Boredom (rev coded)","Loneliness (rev coded)", "Depression (rev coded)","Isolation (rev coded)","Well-Being index")
names(p.values_swb) <- names
adjusted.p_swb <- as.table(p.adjust(sort(p.values_swb),"BH"))
FDR_swb <- cbind(p.values_swb[names], adjusted.p_swb[names])
colnames(FDR_swb) <- c("Non-adjusted p-value","BH-adjusted p-value")
print(FDR_swb, digits = 4)
# For Table S8, treatment effect estimates and standard errors extracted from Table 5

# --------------------------------------
# Table S9, Outgroup Regard: Adjusting for a full set of covariates
p.values_outgroup <- as.table(c(
  reg_res_mod3("ft",final_data)$p.value,
  reg_res_mod3("sd",final_data)$p.value,
  reg_res_mod3("multi_stat",final_data)$p.value,
  reg_res_mod3("inchrct_final_data",final_data)$p.value,
  reg_res_mod3("othchrct_final_data",final_data)$p.value,
  reg_res_mod3("outgroup_index",final_data)$p.value,
  reg_res_mod3("outgroup_princomp",final_data)$p.value))
names<-c("feeling thermometer", "sd","cooperation","perception of out-group evaluations",
         "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
names(p.values_outgroup) <- names

adjusted.p_outgroup <- as.table(p.adjust(sort(p.values_outgroup),"BH"))
FDR_outgroup <- cbind(p.values_outgroup[names], adjusted.p_outgroup[names])
colnames(FDR_outgroup) <- c("Non-adjusted p-value","BH-adjusted p-value")
print(FDR_outgroup, digits = 2) 
# For Table S9, treatment effect estimates and standard errors extracted from Table 6

