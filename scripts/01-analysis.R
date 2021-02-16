# Set your own working directory
rootArg <-  if(Sys.info()["user"]=="nejlaasimovic"){
setwd("/Users/nejlaasimovic/Desktop/code")
}

# Source the analysis functions
source("/Users/nejlaasimovic/Desktop/code/functions.R") 

# Install or/and activate packages
library(ggplot2); library(ggpubr); 
library(stringr); library(estimatr); 
library(Rmisc); library(devtools);
library(plotly); library(dplyr); 
library(sandwich); library(plyr);
library(Rmisc); library(lmtest);
library(tibble)

# Upload the main dataset
combined <- read.csv("bosnia_data_anonym.csv", stringsAsFactors = FALSE)
options(digits=3)

# -----------------------------------------------------------------
#    Table S1: DESCRIPTIVE STATISTICS - baseline characteristics
# -----------------------------------------------------------------

des.variables <- c("age","gender","educ","employ1","trust_media.1","time_usage","freq_usage","freq_news","numb_forums","freq_fbnews","politics_int", "imp_ethn","imp_cntry","imp_relg","imp_family","imp_neigh")
stargazer::stargazer(combined[des.variables], type="text")
# NB: Min and max are min and max possible values within the survey,
# hence, the discrepancy on imp_family (no respondent selected the min value)

# -------------------------------------------------------------------
#    Table S2: DESCRIPTIVE STATISTICS - ethnic composition
# --------------------------------------------------------------------

# Given that the ethnic and religious belonging almost completely overlap within the context,
# we combine the two, categorizing people by the ethnic/religious category they report 
combined$ethn_t <- NA
combined[which(combined$ethn=="1" | combined$ethn=="-1"),]$ethn_t <- 1 #Bosniak
combined[which(combined$ethn=="2"|combined$ethn=="-4"),]$ethn_t <- 2 #Serb
combined[which(combined$ethn=="3"|combined$ethn=="-5"),]$ethn_t <- 3 #Croat
combined[which(combined$ethn=="0"),]$ethn_t <- 4 #Bosnian
combined[which(combined$ethn=="-3"|combined$ethn=="-2"),]$ethn_t <- 5 #Other/Do not want to say

tableS2<-table(combined$ethn_t) 
names(tableS2) <- c("Bosniak","Serb","Croat","Bosnian","Other/Do not want to say")
tableS2<-cbind(tableS2, as.vector(tableS2/353*100))
colnames(tableS2) <- c("No. of people","Share")
tableS2

# --------------------------------
#   Table S3: COVARIATE BALANCE
# ---------------------------------
# --- The appearance of Table S3 in the paper edited within Overleaf

bal.variables <- c("gender","age","educ","employ1","trust_media","freq_news","freq_fbnews","politics_int","numb_forums","time_usage", "freq_usage","imp_ethn","imp_family","imp_cntry","imp_relg","imp_career","imp_neigh","imp_hobby")
df <- combined[,bal.variables]
# p-values
p.values <- cbind(sapply(bal.variables, function (x) t.test(combined[combined$treatment==1,][,x], combined[combined$treatment==0,][,x]))[3,])
balance_table <- do.call(data.frame, # creating a table combining the mean values and p-values
               list(TreatmentGroup = apply(df[combined$treatment == 1,], 2, mean, na.rm=TRUE),
                    ControlGroup =  apply(df[combined$treatment == 0,], 2, mean, na.rm=TRUE),
                    TreatmentSD =  apply(df[combined$treatment == 1,], 2, sd, na.rm=TRUE),
                    ControlSD =  apply(df[combined$treatment == 0,], 2, sd, na.rm=TRUE),
                    t.test=p.values))
balance_table

# -----------------------------------------------
#           ATTRITION 
# ------------------------------------------------

attrition<-read.csv("bosnia_data_anonym_wattrition.csv", stringsAsFactors = FALSE)
attrition$time_usage<-as.numeric(attrition$time_usage)
bal.variables<-c("gender","age","educ","employ1","trust_media.1","freq_news","politics_int","numb_forums","time_usage","freq_usage","imp_ethn","imp_family","imp_cntry","imp_relg","imp_career","imp_neigh")
attrition_df<- attrition[,bal.variables]

p.values0<-cbind(sapply(bal.variables, function (x) t.test(attrition[attrition$treatment==0 & attrition$attrition=="0",][,x], attrition[attrition$treatment==0 & attrition$attrition=="1",][,x]))[3,])
balance_attrition0 <- do.call(data.frame, # creating a table combining the mean values and p-values
                              list(CntrlParticipated = apply(attrition_df[attrition$treatment==0 & attrition$attrition=="0",], 2, mean, na.rm=TRUE),
                                   CntrlParticipated_SD =  apply(attrition_df[attrition$treatment==0 & attrition$attrition=="0",], 2, sd, na.rm=TRUE),
                                   CntrlAttrition =  apply(attrition_df[attrition$treatment==0 & attrition$attrition=="1",], 2, mean, na.rm=TRUE),
                                   CntrlAttrition_SD =  apply(attrition_df[attrition$treatment==0 & attrition$attrition=="1",], 2, sd, na.rm=TRUE),
                                   t.test=p.values0))
balance_attrition0 # Control/Control Attrition

p.values<-cbind(sapply(bal.variables, function (x) t.test(attrition[attrition$treatment==1 & attrition$attrition=="0",][,x], attrition[attrition$treatment==1 & attrition$attrition=="1",][,x]))[3,])
balance_attrition1 <- do.call(data.frame, # creating a table combining the mean values and p-values
               list(TreatParticipated = apply(attrition_df[attrition$treatment==1 & attrition$attrition=="0",], 2, mean, na.rm=TRUE),
                    TreatParticipated_SD =  apply(attrition_df[attrition$treatment==1 & attrition$attrition=="0",], 2, sd, na.rm=TRUE),
                    TreatAttrition =  apply(attrition_df[attrition$treatment==1 & attrition$attrition=="1",], 2, mean, na.rm=TRUE),
                    TreatAttrition_SD =  apply(attrition_df[attrition$treatment==1 & attrition$attrition=="1",], 2, sd, na.rm=TRUE),
                    t.test=p.values))
print(balance_attrition1, digits=4) # Treatment/Treatment Attrition

p.values_comp<-cbind(sapply(bal.variables, function (x) t.test(attrition[attrition$treatment==0 & attrition$attrition=="1",][,x], attrition[attrition$treatment==1 & attrition$attrition=="1",][,x]))[3,])
balance_attrition_comp <- do.call(data.frame, # creating a table combining the mean values and p-values
                              list(CntrlAttrition = apply(attrition_df[attrition$treatment==0 & attrition$attrition=="1",], 2, mean, na.rm=TRUE),
                                   CntrlAttrition_SD =  apply(attrition_df[attrition$treatment==0 & attrition$attrition=="1",], 2, sd, na.rm=TRUE),
                                   TreatAttrition =  apply(attrition_df[attrition$treatment==1 & attrition$attrition=="1",], 2, mean, na.rm=TRUE),
                                   TreatAttrition_SD =  apply(attrition_df[attrition$treatment==1 & attrition$attrition=="1",], 2, sd, na.rm=TRUE),
                                   t.test=p.values_comp))
print(balance_attrition_comp, digits=4) # Comparing treatment and control attrition

# ---------------------------------------
#    Table S5: Intention-to-Treat Results
# ---------------------------------------
# Table includes ITT results on two indices:
# 1. Subjective well-being [SWB]
# 2. Knowledge of the news

## -------------- SWB 
# Reverse coding negative emotions, so that higher values indicate more positive subjective well-being
combined$nerv <- (-1) * combined$nerv 
combined$depression <- (-1) * combined$depression
combined$loneliness <- (-1) * combined$loneliness
combined$boredom <- (-1) * combined$boredom
combined$isol<- (-1) * combined$isol

# Checking the internal consistency of the scale
wellbeing_vars <- c("satisf", "joy","fulf","depression","boredom","isol","loneliness","nerv")
wellbeing_df <- combined[,wellbeing_vars]
psych::alpha(wellbeing_df, 'check.keys=TRUE')$total$std.alpha #0.829 
prcomp<- prcomp(wellbeing_df)
print(prcomp, digits=3) # checking the loadings

# Observe relatively uniform loadings across variables, proceed by creating sum score index
for (i in 1:nrow(combined)){
  combined$swb[i] <- scale(combined$satisf)[i]+scale(combined$joy)[i]+scale(combined$fulf)[i]+scale(combined$depression)[i] +scale(combined$loneliness)[i] + scale(combined$nerv)[i] 
  +scale(combined$boredom)[i]+scale(combined$isol[i])
}

# Referenced in the paper [within the discussion of main results]:
# Excluding the outliers
outlier.values<-boxplot.stats(combined$swb)$out  
no_swb_outliers<-combined[!combined$swb %in% outlier.values,]
outlier.values
# Model 3 (full covariate specification), treatment effect on swb excluding the outliers
reg_res_mod3("swb",no_swb_outliers)
# One directional hypothesis
t.test(no_swb_outliers[no_swb_outliers$treatment=="0",]$swb, no_swb_outliers[no_swb_outliers$treatment=="1",]$swb, alternative="less")

## -------------- NEWS KNOWLEDGE

# Creating the news index as sum of correct/false responses
# (within the dataset, each  response coded -1 if a false response was given; 1 if true and 0 if "unsure" was selected)
c_news<-NA
for (i in 1:nrow(combined)){
  combined$c_news[i] <- combined$fbnews_st1[i]+combined$fbnews_st2[i]+combined$fbnews_st3[i]+combined$fbnews_st4[i]+combined$fbnews_st5[i]+combined$fbnews_st6[i]+combined$fbnews_st7[i]+combined$fbnews_st8[i]
}

## -------------- PUTTING IT ALL TOGETHER, TABLE S5
TableS5.1 <- do.call(data.frame,rbind(
  reg_res_mod1("c_news",combined),
  reg_res_mod1("swb",combined),
  reg_res_mod1("satisf",combined),
  reg_res_mod1("joy",combined),
  reg_res_mod1("fulf",combined),
  reg_res_mod1("nerv",combined),
  reg_res_mod1("boredom",combined),
  reg_res_mod1("loneliness",combined),
  reg_res_mod1("depression",combined),
  reg_res_mod1("isol",combined)))


TableS5.2 <- do.call(data.frame,rbind(
  reg_res_mod2("c_news",combined),
  reg_res_mod2("swb",combined),
  reg_res_mod2("satisf",combined),
  reg_res_mod2("joy",combined),
  reg_res_mod2("fulf",combined),
  reg_res_mod2("nerv",combined),
  reg_res_mod2("boredom",combined),
  reg_res_mod2("loneliness",combined),
  reg_res_mod2("depression",combined),
  reg_res_mod2("isol",combined)))


TableS5.3 <- do.call(data.frame,rbind(
  reg_res_mod3("c_news",combined),
  reg_res_mod3("swb",combined),
  reg_res_mod3("satisf",combined),
  reg_res_mod3("joy",combined),
  reg_res_mod3("fulf",combined),
  reg_res_mod3("nerv",combined),
  reg_res_mod3("boredom",combined),
  reg_res_mod3("loneliness",combined),
  reg_res_mod3("depression",combined),
  reg_res_mod3("isol",combined)))

# -------- *Table S5* ------------
tableS5<-join_all(list(TableS5.1, TableS5.2, TableS5.3), by="outcome") 
tableS5
# -------- *Table S5* ------------

# ---------------------------------------
#    Table S6: Intention-to-Treat Results
# ---------------------------------------

## ------- AFFECT/FEELING THERMOMETER
# Creating feeling thermometer variable: subtracting ft of the outgroup from ft of the ingroup
combined$ft <- combined$outfeel_therm-combined$infeel_therm # higher number indicates more pos atribution

## ------- COOPERATION 
# Creating cooperation index as a sum of z-scales from the agreement with two statements:
# 1.Multi-ethnic parties cannot secure the interest of my people.
# 2.It is only possible to cooperate with members of my ethnic group.

for (i in 1:nrow(combined)){
combined$multi_stat[i] <- scale(combined$agr_st5)[i] + scale(combined$agr_st7)[i]}


##  -------- GROUP CHARACTERISTICS
# Perception of out-group evaluations

prcp_chrc <- c("in_broadv","in_intel","in_hyp","in_evil","in_patriot","in_gener","in_unreliab","in_honest","in_self")
# Higher values indicate higher level of disagreement, hence positive characteristics need to be recoded
# so that higher values indicate more positive attribution across (multiplied by -1 within the index creation)

for (i in 1:nrow(combined)){
  combined$inchrct_combined[i] <- - combined$in_broadv[i] - combined$in_intel[i]+
  combined$in_hyp [i] + combined$in_evil[i] - combined$in_patriot[i] - combined$in_gener[i] +
  combined$in_unreliab [i] - combined$in_honest[i] + combined$in_self[i]}

##  -------- GROUP CHARACTERISTICS
# Out-Group Traits

oth_chrc <- c("oth_broadv","oth_intel","oth_hyp","oth_evil","oth_patriot","oth_gener","oth_unreliab","oth_honest","oth_self")
# Higher values indicate higher level of disagreement, hence positive characteristics need to be recoded (multiplied by -1 within the index creation)

for (i in 1:nrow(combined)){
  combined$othchrct_combined[i] <- - combined$oth_broadv[i]  - combined$oth_intel[i] +
  combined$oth_hyp [i] + combined$oth_evil[i] - combined$oth_patriot[i] - combined$oth_gener[i] +
  combined$oth_unreliab [i] - combined$oth_honest[i] + combined$oth_self[i]}

## ------ OUTGROUP INDEX
# We use the PC score as our primary index outgroup regard, but also report results using the sum score:

# -- Index as sum of z-scores of the 5 components
combined$outgroup_index <-  scale(combined$sd) + scale(combined$multi_stat) + scale(combined$ft) + scale(combined$othchrct_combined)+scale(combined$inchrct_combined)

# -- Index as a principal component score of the 5 components

# Checking internal validity and Chronbach alpha
outgroup_variables <- c("sd",
                        "multi_stat",
                        "othchrct_combined",
                        "ft","inchrct_combined")
outgroup_variables_pc <- combined[,outgroup_variables]
psych::alpha(outgroup_variables_pc, 'check.keys=TRUE')$total$std.alpha #0.518
prcomp <- prcomp(outgroup_variables_pc)
print(prcomp, digits=3)

# Creating PC index of outgroup regard
group_attitudes_vars <- c("sd","multi_stat","ft","inchrct_combined","othchrct_combined")
combined$outgroup_princomp <- pc_index(dataIn=combined,
                                             varList=group_attitudes_vars)

# ---------  Table S6
TableS6.1<-do.call(data.frame,rbind(
  reg_res_mod1("ft",combined),
  reg_res_mod1("sd",combined),
  reg_res_mod1("multi_stat",combined),
  reg_res_mod1("inchrct_combined",combined),
  reg_res_mod1("othchrct_combined",combined),
  reg_res_mod1("outgroup_index",combined),
  reg_res_mod1("outgroup_princomp",combined)))

TableS6.2<-do.call(data.frame,rbind(
  reg_res_mod2("ft",combined),
  reg_res_mod2("sd",combined),
  reg_res_mod2("multi_stat",combined),
  reg_res_mod2("inchrct_combined",combined),
  reg_res_mod2("othchrct_combined",combined),
  reg_res_mod2("outgroup_index",combined),
  reg_res_mod2("outgroup_princomp",combined)))

TableS6.3<-do.call(data.frame,rbind(
  reg_res_mod3("ft",combined),
  reg_res_mod3("sd",combined),
  reg_res_mod3("multi_stat",combined),
  reg_res_mod3("inchrct_combined",combined),
  reg_res_mod3("othchrct_combined",combined),
  reg_res_mod3("outgroup_index",combined),
  reg_res_mod3("outgroup_princomp",combined)))


# -------- *Table S6* ------------
tableS6<-join_all(list(TableS6.1, TableS6.2, TableS6.3), by="outcome")
print(tableS6, digits=3)
# -------- *Table S6* ------------

# --------------------------------------
#  FDR Adjusted Results: Table S7 & S8
# --------------------------------------
# Table 7, Subjective Well Being: Adjusting for a full set of covariates
p.values_swb<-as.table(c(
  reg_res_mod3("satisf",combined)$p.value,
  reg_res_mod3("joy",combined)$p.value,
  reg_res_mod3("fulf",combined)$p.value,
  reg_res_mod3("nerv",combined)$p.value,
  reg_res_mod3("boredom",combined)$p.value,
  reg_res_mod3("loneliness",combined)$p.value,
  reg_res_mod3("depression",combined)$p.value,
  reg_res_mod3("isol",combined)$p.value,
  reg_res_mod3("swb",combined)$p.value))
names <-c("Satisfaction","Joy","Fulfillment","Anxiety (rev coded)","Boredom (rev coded)","Loneliness (rev coded)", "Depression (rev coded)","Isolation (rev coded)","Well-Being index")
names(p.values_swb)<-names
adjusted.p_swb <-as.table(p.adjust(sort(p.values_swb),"BH"))
FDR_swb <- cbind(p.values_swb[names], adjusted.p_swb[names])
colnames(FDR_swb) <- c("Non-adjusted p-value","BH-adjusted p-value")
print(FDR_swb, digits = 4)
# For Table S7, treatment effect estimates and standard errors extracted from Table 5

# Table S8, Outgroup Regard: Adjusting for a full set of covariates
p.values_outgroup <-as.table(c(
  reg_res_mod3("ft",combined)$p.value,
  reg_res_mod3("sd",combined)$p.value,
  reg_res_mod3("multi_stat",combined)$p.value,
  reg_res_mod3("inchrct_combined",combined)$p.value,
  reg_res_mod3("othchrct_combined",combined)$p.value,
  reg_res_mod3("outgroup_index",combined)$p.value,
  reg_res_mod3("outgroup_princomp",combined)$p.value))
names<-c("feeling thermometer", "sd","cooperation","perception of out-group evaluations",
         "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
names(p.values_outgroup)<-names
adjusted.p_outgroup<-as.table(p.adjust(sort(p.values_outgroup),"BH"))
FDR_outgroup<- cbind(p.values_outgroup[names], adjusted.p_outgroup[names])
colnames(FDR_outgroup) <- c("Non-adjusted p-value","BH-adjusted p-value")
print(FDR_outgroup, digits = 2) 
# For Table S7, treatment effect estimates and standard errors extracted from Table 6
