# Set working directory
rootArg <-  if(Sys.info()["user"]=="nejlaasimovic"){
  setwd("/Users/nejlaasimovic/Desktop")
}

# Install or/and activate packages
# install.packages
library(ggplot2); library(ggpubr); library(readxl);
library(stringr); library(estimatr);library(plm)
library(skimr); library(Rmisc); library(devtools);
library(stargazer); library(lmtest); library(plyr); library(psych);

# Upload the main dataset
combined <- read.csv("bosnia_data_creview.csv", stringsAsFactors = FALSE)

# ------------------------------------
#    DESCRIPTIVE STATISTICS
# ------------------------------------

# --- Table S1
des.variables<-c("age","gender","educ","employ1","trust_media","freq_news","freq_fbnews","politics_int","numb_forums","time_usage", "imp_ethn","imp_cntry","imp_relg","imp_family","imp_hobby","imp_career","imp_neigh","freq_usage")
stargazer::stargazer(combined[des.variables], type="text")
# skim(combined[des.variables]) # looking at full distribution

# --- Table S2
# As their ethnic belonging, some participants indicated religion, some nationality, others
# chose not to respond. While we use these selections as different categories (when including the covariate in the main analysis)
# to allow for any nuances that may exist between them, below we combine the religion
# with the corresponding/most frequent ethnic category (as the two greatly overlap within the context)

combined$ethn_t <- NA
combined[which(combined$ethn=="1" | combined$ethn=="-1"),]$ethn_t <- 1 #Bosniak
combined[which(combined$ethn=="2"|combined$ethn=="-4"),]$ethn_t<-2 #Serb
combined[which(combined$ethn=="3"|combined$ethn=="-5"),]$ethn_t<-3 #Croat
combined[which(combined$ethn=="-3"|combined$ethn=="-2"),]$ethn_t<-4 #Bosnian
combined[which(combined$ethn=="0"),]$ethn_t<-5 #Other/Do not want to say

table2<-table(combined$ethn_t) 
names(table2) <- c("Bosniak","Serb","Croat","Bosnian","Other/Do not want to say")
table2<-cbind(table2, as.vector(table2/353*100))
colnames(table2)<-c("No. of people","Share")
table2

# -----------------------
#   COVARIATE BALANCE
# -----------------------
# --- Table S3; table in the paper edited using excel2latex

bal.variables<-c("gender","age","educ","employ1","trust_media","freq_news","freq_fbnews","politics_int","numb_forums","time_usage", "freq_usage","imp_ethn","imp_family","imp_cntry","imp_relg","imp_career","imp_neigh","imp_hobby")
df <- combined[,bal.variables]
# p-values
p.values<-cbind(sapply(bal.variables, function (x) t.test(combined[combined$treatment==1,][,x], combined[combined$treatment==0,][,x]))[3,])
balance_table <- do.call(data.frame, # creating a table combining the mean values and p-values
               list(TreatmentGroup = apply(df[combined$treatment == 1,], 2, mean, na.rm=TRUE),
                    ControlGroup =  apply(df[combined$treatment == 0,], 2, mean, na.rm=TRUE),
                    TreatmentSD =  apply(df[combined$treatment == 1,], 2, sd, na.rm=TRUE),
                    ControlSD =  apply(df[combined$treatment == 0,], 2, sd, na.rm=TRUE),
                    t.test=p.values))
balance_table

# -----------------------
#   ATTRITION
# -----------------------
# --- Table S4

attrition_ctrl<-read_excel("sept18_active_attrition_c.xlsx")
attrition_treat <-read_excel("sept18_attrition_treatedgroup_c_sept20.xlsx")

attrition_ctrl <- combined[,bal.variables]
attrition_treat  <- combined[,bal.variables]

p.values<-cbind(sapply(bal.variables, function (x) t.test(combined[combined$treatment==1,][,x], attrition_treat[,x]))[3,])

balance_attrition1 <- do.call(data.frame, # creating a table combining the mean values and p-values
               list(CntrlParticipated = apply(df[which(combined$treatment==1),], 2, mean, na.rm=TRUE),
                    CntrlAttrition =  apply(attrition_treat, 2, mean, na.rm=TRUE),
                    CntrlParticipated_SD =  apply(df[which(combined$treatment==1),], 2, sd, na.rm=TRUE),
                    CntrlAttrition_SD =  apply(attrition_treat, 2, sd, na.rm=TRUE),
                    t.test=p.values))
balance_attrition1


# ---------------------------------------
# MAIN RESULTS: SUBJECTIVE WELL-BEING
# ---------------------------------------
# Reverse coding negative emotions, so that higher values indicate more positive swb
combined$nerv <- (-1) * combined$nerv 
combined$depression <- (-1) * combined$depression
combined$loneliness <- (-1) * combined$loneliness
combined$boredom <- (-1) * combined$boredom
combined$isol<- (-1) * combined$isol

# Internal consistency of the scale
wellbeing_vars <- c("satisf", "joy","fulf","depression","boredom","isol","loneliness","nerv")
wellbeing_df <- combined[,wellbeing_vars]
psych::alpha(wellbeing_df, 'check.keys=TRUE')$total$std.alpha #0.829
prcomp<- prcomp(wellbeing_df)
print(prcomp, digits=3)

# Observe relatively uniform loadings across variables, proceed by creating sum score index
# Primary index - sum of z-scores
combined$happiness_rev<--combined$happiness
for (i in 1:nrow(combined)){
  combined$swb [i]<- scale(combined$satisf)[i]+scale(combined$joy)[i]+scale(combined$fulf)[i]+scale(combined$depression)[i] +scale(combined$loneliness)[i] + scale(combined$nerv)[i] 
  +scale(combined$boredom)[i]+scale(combined$isol)
}

#  model 1: baseline
swb0 <-lm((swb)~(treatment), data = combined)
swb_coef<-coeftest(swb0, vcov = vcovHC(swb0, type = "HC1"))[2,1]/sd(combined[combined$treatment=="0",]$swb)
swb_se<-coeftest(swb0, vcov = vcovHC(swb0, type = "HC1"))[2,2]/sd(combined[combined$treatment=="0",]$swb)

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
swb1<-lm_lin(swb~treatment, covariates=covIn, data=combined)
summary(swb1)
swb1$coefficients[2] /sd(combined[combined$treatment=="0",]$swb)
swb1$std.error[2]/ sd(combined[combined$treatment=="0",]$swb)

# model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
swb2<-lm_lin(swb~treatment, covariates=covIn, data=combined)
summary(swb2)
swb2$coefficients[2] /sd(combined[combined$treatment=="0",]$swb)
swb2$std.error[2]/ sd(combined[combined$treatment=="0",]$swb)

# --------------  Coefficients for all the separate outcomes 
# --- Satisfaction, Model 1
satisfaction <-lm((satisf)~(treatment), data = combined)
satisfaction_coef<-coeftest(satisfaction , vcov = vcovHC(satisfaction, type = "HC1"))[2,1]/sd(combined[combined$treatment=="0",]$satisf)
satisfaction_se<-coeftest(satisfaction, vcov = vcovHC(satisfaction, type = "HC1"))[2,2]/sd(combined[combined$treatment=="0",]$satisf)

# Satisfaction, Model 2
covIn <- as.formula("~freq_usage") 
satisfaction1<- lm_lin(satisf~treatment, covariates=covIn, data=combined)
satisfaction1$coefficients[2] /sd(combined[combined$treatment=="0",]$satisf)
satisfaction1$std.error[2]/ sd(combined[combined$treatment=="0",]$satisf)

# Satisfaction, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
satisfaction2 <- lm_lin(satisf~treatment, covariates=covIn, data=combined)
satisfaction2$coefficients[2] /sd(combined[combined$treatment=="0",]$satisf)
satisfaction2$std.error[2]/ sd(combined[combined$treatment=="0",]$satisf)

# --- Joy, Model 1
joy <-lm((joy)~(treatment), data = combined)
joy_coef<-coeftest(joy , vcov = vcovHC(joy, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$joy)))
joy_se<-coeftest(joy, vcov = vcovHC(joy, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$joy)))

# Joy, Model 2
covIn <- as.formula("~freq_usage")
joy1<-lm_lin(joy~treatment, covariates=covIn, data=combined)
joy1$coefficients[2]/sd(combined[combined$treatment=="0",]$joy)
joy1$std.error[2]/ sd(combined[combined$treatment=="0",]$joy)

# Joy, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
joy2<-lm_lin(joy~treatment, covariates=covIn, data=combined)
joy2$coefficients[2]/sd(combined[combined$treatment=="0",]$joy)
joy2$std.error[2]/ sd(combined[combined$treatment=="0",]$joy)

# --- Fulfillment, Model 1
fulf <-lm(fulf~treatment, data = combined)
fulf_coef<-coeftest(fulf , vcov = vcovHC(fulf, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$fulf)))
fulf_se<-coeftest(fulf, vcov = vcovHC(fulf, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$fulf)))

# Fulfillment, Model 2
covIn <- as.formula("~freq_usage")
fulfillment1<-lm_lin(fulf~treatment, covariates=covIn, data=combined)
fulfillment1$coefficients[2]/sd(combined[combined$treatment=="0",]$fulf)
fulfillment1$std.error[2]/ sd(combined[combined$treatment=="0",]$fulf)

# Fulfillment, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
fulfillment2<-lm_lin(fulf~treatment, covariates=covIn, data=combined)
fulfillment2$coefficients[2]/sd(combined[combined$treatment=="0",]$fulf)
fulfillment2$std.error[2]/ sd(combined[combined$treatment=="0",]$fulf)

# --- Anxiety, Model 1
nerv <-lm((nerv)~(treatment), data = combined)
nerv_coef<-coeftest(nerv , vcov = vcovHC(nerv, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$nerv)))
nerv_se<-coeftest(nerv, vcov = vcovHC(nerv, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$nerv)))

# Anxiety, Model 2
covIn <- as.formula("~freq_usage")
nerv1<-lm_lin(nerv~treatment, covariates=covIn, data=combined)
nerv1$coefficients[2] /sd(combined[combined$treatment=="0",]$nerv)
nerv1$std.error[2]/ sd(combined[combined$treatment=="0",]$nerv)

# Anxiety, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
nerv2<-lm_lin(nerv~treatment, covariates=covIn, data=combined)
nerv2$coefficients[2] /sd(combined[combined$treatment=="0",]$nerv)
nerv2$std.error[2]/ sd(combined[combined$treatment=="0",]$nerv)
summary(nerv2)

# --- Boredom, Model 1
boredom <-lm((boredom)~(treatment), data = combined)
boredom_coef<-coeftest(boredom, vcov = vcovHC(boredom, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$boredom)))
boredom_se<-coeftest(boredom, vcov = vcovHC(boredom, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$boredom)))

# Boredom, Model 2
covIn <- as.formula("~freq_usage")
boredom1<-lm_lin(boredom~treatment, covariates=covIn, data=combined)
boredom1$coefficients[2] /sd(combined[combined$treatment=="0",]$boredom)
boredom1$std.error[2]/ sd(combined[combined$treatment=="0",]$boredom)

# Boredom, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
boredom2<-lm_lin(boredom~treatment, covariates=covIn, data=combined)
boredom2$coefficients[2] /sd(combined[combined$treatment=="0",]$boredom)
boredom2$std.error[2]/ sd(combined[combined$treatment=="0",]$boredom)
summary(boredom2)

# --- Loneliness, Model 1
loneliness <-lm((loneliness)~(treatment), data = combined)
loneliness_coef<-coeftest(loneliness , vcov = vcovHC(loneliness, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$loneliness)))
loneliness_se<-coeftest(loneliness, vcov = vcovHC(loneliness, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$loneliness)))

#  Loneliness, Model 2
covIn <- as.formula("~freq_usage")
loneliness1<-lm_lin(loneliness~treatment, covariates=covIn, data=combined)
loneliness1$coefficients[2] /sd(combined[combined$treatment=="0",]$loneliness)
loneliness1$std.error[2]/ sd(combined[combined$treatment=="0",]$loneliness)

#  Loneliness, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
loneliness2<-lm_lin(loneliness~treatment, covariates=covIn, data=combined)
loneliness2$coefficients[2] /sd(combined[combined$treatment=="0",]$loneliness)
loneliness2$std.error[2]/ sd(combined[combined$treatment=="0",]$loneliness)


# --- Depression, Model 1
depression <-lm((depression)~(treatment), data = combined)
depression_coef<-coeftest(depression, vcov = vcovHC(depression, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$depression)))
depression_se<-coeftest(depression, vcov = vcovHC(depression, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$depression)))

# Depression, Model 2
covIn <- as.formula("~freq_usage")
depression1<-lm_lin(depression~treatment, covariates=covIn, data=combined)
depression1$coefficients[2] /sd(combined[combined$treatment=="0",]$depression)
depression1$std.error[2]/ sd(combined[combined$treatment=="0",]$depression)

# Depression, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
depression2<-lm_lin(depression~treatment, covariates=covIn, data=combined)
depression2$coefficients[2] /sd(combined[combined$treatment=="0",]$depression)
depression2$std.error[2]/ sd(combined[combined$treatment=="0",]$depression)


# --- Isolation, Model 1
isol <-lm((isol)~(treatment), data = combined)
isol_coef<-coeftest(isol , vcov = vcovHC(isol, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$isol)))
isol_se<-coeftest(isol, vcov = vcovHC(isol, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$isol)))

# Isolation, Model 2
covIn <- as.formula("~freq_usage")
isolation1<-lm_lin(isol~treatment, covariates=covIn, data=combined)
isolation1$coefficients[2]/sd(combined[combined$treatment=="0",]$isol)
isolation1$std.error[2]/ sd(combined[combined$treatment=="0",]$isol)

# Isolation, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
isolation2<-lm_lin(isol~treatment, covariates=covIn, data=combined)
isolation2$coefficients[2]/sd(combined[combined$treatment=="0",]$isol)
isolation2$std.error[2]/ sd(combined[combined$treatment=="0",]$isol)


# For Table S5
# ----------------------------
#    FACTUAL NEWS KNOWLEDGE
# -----------------------------
# Creating the news index as sum of correct/false responses
# (each  response coded -1 if a false response was given; 1 if true and 0 if "unsure" was selected)
c_news<-NA
for (i in 1:nrow(combined)){
  combined$c_news[i]<- combined$fbnews_st1[i]+combined$fbnews_st2[i]+combined$fbnews_st3[i]+combined$fbnews_st4[i]+combined$fbnews_st5[i]+combined$fbnews_st6[i]+combined$fbnews_st7[i]+combined$fbnews_st8[i]
}

# --- News - Model 1
news <-lm((c_news)~(treatment), data = combined)
news_coef<-coeftest(news , vcov = vcovHC(news, type = "HC1"))[2,1]/sd(na.omit(combined[combined$treatment=="0",]$c_news))
news_se<-coeftest(news, vcov = vcovHC(news, type = "HC1"))[2,2]/sd(na.omit(combined[combined$treatment=="0",]$c_news))

# News - Model 2
covIn <- as.formula("~freq_usage")
news1<-lm_lin(c_news~treatment, covariates=covIn, data=combined)
news1$coefficients[2]/sd(combined[combined$treatment=="0",]$c_news)
news1$std.error[2]/ sd(combined[combined$treatment=="0",]$c_news)

# News - Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
news2<-lm_lin(c_news~treatment, covariates=covIn, data=combined)
news2$coefficients[2]/sd(combined[combined$treatment=="0",]$c_news)
news2$std.error[2]/ sd(combined[combined$treatment=="0",]$c_news)

# Table S5 - Model 1
# extract results of interest [models as estimated within main-analyses.R/code_review_megan.R]
coef.vec <- c(news_coef,
               swb_coef,
               satisfaction_coef,
               joy_coef,   
               fulf_coef, 
               nerv_coef,
               boredom_coef,
               loneliness_coef,
               depression_coef, 
               isol_coef)

se.vec <-  c(news_se,
             swb_se,
             satisfaction_se,
             joy_se,   
             fulf_se, 
             nerv_se,
             boredom_se,
             loneliness_se,
             depression_se, 
             isol_se)
names <- c("News Knowledge","Well-Being index","Satisfaction","Joy","Fulfillment","Anxiety","Boredom","Loneliness", "Depression","Isolation")
merged <- data.frame(names, coef.vec, se.vec)

# Table S5 - Model 2
coef.vec1 <- c(news1$coefficients[2]/sd(combined[combined$treatment=="0",]$c_news),
              swb1$coefficients[2]/sd(combined[combined$treatment=="0",]$swb),
              satisfaction1$coefficients[2]/sd(combined[combined$treatment=="0",]$satisf),
              joy1$coefficients[2]/sd(combined[combined$treatment=="0",]$joy),
              fulfillment1$coefficients[2]/sd(combined[combined$treatment=="0",]$fulf),
              nerv1$coefficients[2]/sd(combined[combined$treatment=="0",]$nerv),
              boredom1$coefficients[2]/sd(combined[combined$treatment=="0",]$boredom),
              loneliness1$coefficients[2]/sd(combined[combined$treatment=="0",]$loneliness),
              depression1$coefficients[2]/sd(combined[combined$treatment=="0",]$depression),
              isolation1$coefficients[2]/sd(combined[combined$treatment=="0",]$isol))
            

se.vec1 <-  c(news1$std.error[2]/sd(combined[combined$treatment=="0",]$c_news),
             swb1$std.error[2]/sd(combined[combined$treatment=="0",]$swb),
             satisfaction1$std.error[2]/sd(combined[combined$treatment=="0",]$satisf),
             joy1$std.error[2]/sd(combined[combined$treatment=="0",]$joy),
             fulfillment1$std.error[2]/sd(combined[combined$treatment=="0",]$fulf),
             nerv1$std.error[2]/sd(combined[combined$treatment=="0",]$nerv),
             boredom1$std.error[2]/sd(combined[combined$treatment=="0",]$boredom),
             loneliness1$std.error[2]/sd(combined[combined$treatment=="0",]$loneliness),
             depression1$std.error[2]/sd(combined[combined$treatment=="0",]$depression),
             isolation1$std.error[2]/sd(combined[combined$treatment=="0",]$isol))
         
merged1 <- data.frame(names, coef.vec1, se.vec1)
merged1

# Table S5 - Model 2
# extract results of interest [models as estimated within main-analyses.R/code_review_megan.R]
coef.vec2 <- c(news2$coefficients[2]/sd(combined[combined$treatment=="0",]$c_news),
              swb2$coefficients[2]/sd(combined[combined$treatment=="0",]$swb),
              satisfaction2$coefficients[2]/sd(combined[combined$treatment=="0",]$satisf),
              joy2$coefficients[2]/sd(combined[combined$treatment=="0",]$joy),
              fulfillment2$coefficients[2]/sd(combined[combined$treatment=="0",]$fulf),
              nerv2$coefficients[2]/sd(combined[combined$treatment=="0",]$nerv),
              boredom2$coefficients[2]/sd(combined[combined$treatment=="0",]$boredom),
              loneliness2$coefficients[2]/sd(combined[combined$treatment=="0",]$loneliness),
              depression2$coefficients[2]/sd(combined[combined$treatment=="0",]$depression),
              isolation2$coefficients[2]/sd(combined[combined$treatment=="0",]$isol))
          

se.vec2 <-  c(news2$std.error[2]/sd(combined[combined$treatment=="0",]$c_news),
             swb2$std.error[2]/sd(combined[combined$treatment=="0",]$swb),
             satisfaction2$std.error[2]/sd(combined[combined$treatment=="0",]$satisf),
             joy2$std.error[2]/sd(combined[combined$treatment=="0",]$joy),
             fulfillment2$std.error[2]/sd(combined[combined$treatment=="0",]$fulf),
             nerv2$std.error[2]/sd(combined[combined$treatment=="0",]$nerv),
             boredom2$std.error[2]/sd(combined[combined$treatment=="0",]$boredom),
             loneliness2$std.error[2]/sd(combined[combined$treatment=="0",]$loneliness),
             depression2$std.error[2]/sd(combined[combined$treatment=="0",]$depression),
             isolation2$std.error[2]/sd(combined[combined$treatment=="0",]$isol))
           
merged2 <- data.frame(names, coef.vec2, se.vec2)
merged2

# -------- *Table S5* ------------
join_all(list(merged, merged1, merged2), by="names")
# -------- *Table S5* ------------
summary(news2)

# ---------------------------
#   INTER-ETHNIC ATTITUDES
# ---------------------------
# ------- AFFECT/FEELING THERMOMETER
# creating feeling thermometer variable
combined$ft <- -(combined$infeel_therm - combined$outfeel_therm) # recoded, so that higher number is more pos atribution

# Model 1: baseline
ft_model1 <- lm(combined$ft~combined$treatment) 
summary(ft_model1)
ft_coef<-coeftest(ft_model1, vcov = vcovHC(ft_model1, type = "HC1"))[2,1]/sd(na.omit(combined[combined$treatment == 0,]$ft))
ft_se<-coeftest(ft_model1, vcov = vcovHC(ft_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$treatment == 0,]$ft))

# Model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
m_ft1<-lm_lin(ft~treatment, covariates=covIn, data=combined)
ft_coef1<-m_ft1$coefficients[2]/sd(combined[combined$treatment == 0,]$ft)
ft_se1<-m_ft1$std.error[2]/sd( combined[combined$treatment == 0,]$ft)

# Model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_ft2<-lm_lin(ft~treatment, covariates=covIn, data=combined)
ft_coef2<-m_ft2$coefficients[2]/sd(combined[combined$treatment == 0,]$ft)
ft_se2<-m_ft2$std.error[2]/sd(combined[combined$treatment == 0,]$ft)

# ------- SOCIAL DISTANCE
# participants asked to indicate all which applies; recoded so that higher number suggests more closeness

# Model 1: baseline
sd_model1 <- lm(combined$sd~combined$treatment) 
sd_coef<-coeftest(sd_model1, vcov = vcovHC(sd_model1, type = "HC1"))[2,1]/sd(na.omit(combined[combined$treatment=="0",]$sd))
sd_se<-coeftest(sd_model1, vcov = vcovHC(sd_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$treatment=="0",]$sd))

# Model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
m_sd<-lm_lin(sd~treatment, covariates=covIn, data=combined)
summary(m_sd)
sd_coef1<-m_sd$coefficients[2]/sd(combined[combined$treatment=="0",]$sd)
sd_se1<-m_sd$std.error[2]/sd(combined[combined$treatment=="0",]$sd)

# Model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_sd2<-lm_lin(sd~treatment, covariates=covIn, data=combined)
summary(m_sd2)
sd_coef2<-m_sd2$coefficients[2] /sd(combined[combined$treatment=="0",]$sd)
sd_se2<-m_sd2$std.error[2]/ sd(combined[combined$treatment=="0",]$sd)

# ------- COOPERATION 
# creating cooperation index as a sum of z-scales from the agreement with two statements:
# 1.Multi-ethnic parties cannot secure the interest of my people.
# 2.It is only possible to cooperate with members of my ethnic group.
# statements <-c("agr_st5","agr_st7")
# combined[,"agr_st5"] <- sapply("agr_st5", function (x) combined[,x] <- imputeGroupMode (dataIn=combined, varIn = x, groupVar="age"))
# for (i in 1:nrow(combined)){
#  combined$multi_stat[i] <- scale(combined$agr_st5)[i] + scale(combined$agr_st7)[i] 
# }

# Model 1: baseline
multi_stat_model1 <- lm(multi_stat~treatment, data=combined) 
summary(multi_stat_model1)
cpr_coef<-coeftest(multi_stat_model1 , vcov = vcovHC(multi_stat_model1, type = "HC1"))[2,1]/sd(combined[combined$treatment=="0",]$multi_stat)
cpr_se<-coeftest(multi_stat_model1 , vcov = vcovHC(multi_stat_model1, type = "HC1"))[2,2]/sd(combined[combined$treatment=="0",]$multi_stat)

# Model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
m_cpr<-lm_lin(multi_stat~treatment, covariates=covIn, data=combined)
cpr_coef1<-m_cpr$coefficients[2] /sd(combined[combined$treatment=="0",]$multi_stat)
cpr_se1<-m_cpr$std.error[2]/ sd(combined[combined$treatment=="0",]$multi_stat)

# Model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_cpr2<-lm_lin(multi_stat~treatment, covariates=covIn, data=combined)
summary(m_cpr2)
cpr_coef2<-m_cpr2$coefficients[2] /sd(combined[combined$treatment=="0",]$multi_stat)
cpr_se2<-m_cpr2$std.error[2]/ sd(combined[combined$treatment=="0",]$multi_stat)


#  -------- GROUP CHARACTERISTICS
# ------ Perception of out-group evaluations
prcp_chrc <- c("in_broadv","in_intel","in_hyp","in_evil","in_patriot","in_gener","in_unreliab","in_honest","in_self")
# higher values indicate higher level of disagreement, hence positive characteristics need to be recoded (multiplied by -1 within the index creation)
for (i in 1:nrow(combined)){
  combined$inchrct_combined[i] <- - combined$in_broadv[i] - combined$in_intel[i]+
    combined$in_hyp [i] + combined$in_evil[i] - combined$in_patriot[i] - combined$in_gener[i] +
    combined$in_unreliab [i] - combined$in_honest[i] + combined$in_self[i]}

# Model 1: baseline
inchrct_model1<- lm(combined$inchrct_combined~combined$treatment) 
perc_coef<-coeftest(inchrct_model1 , vcov = vcovHC(inchrct_model1, type = "HC1"))[2,1]/sd(combined[combined$treatment=="0",]$inchrct_combined)
perc_se<-coeftest(inchrct_model1 , vcov = vcovHC(inchrct_model1, type = "HC1"))[2,2]/sd(combined[combined$treatment=="0",]$inchrct_combined)

# Model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
m_perc<-lm_lin(combined$inchrct_combined~treatment, covariates=covIn, data=combined)
perc_coef1<-m_perc$coefficients[2]/sd(combined[combined$treatment=="0",]$inchrct_combined)
perc_se1<-m_perc$std.error[2]/ sd(combined[combined$treatment=="0",]$inchrct_combined)

# Model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_perc2<-lm_lin(inchrct_combined~treatment, covariates=covIn, data=combined)
summary(m_perc2)
perc_coef2<-m_perc2$coefficients[2] /sd(combined[combined$treatment=="0",]$inchrct_combined)
perc_se2<-m_perc2$std.error[2]/ sd(combined[combined$treatment=="0",]$inchrct_combined)

#  -------- GROUP CHARACTERISTICS
# ------ Out-Group Traits

oth_chrc <- c("oth_broadv","oth_intel","oth_hyp","oth_evil","oth_patriot","oth_gener","oth_unreliab","oth_honest","oth_self")
# higher values indicate higher level of disagreement, hence positive characteristics need to be recoded (multiplied by -1 within the index creation)

for (i in 1:nrow(combined)){
  combined$othchrct_combined[i] <- - combined$oth_broadv[i]  - combined$oth_intel[i] +
    combined$oth_hyp [i] + combined$oth_evil[i] - combined$oth_patriot[i] - combined$oth_gener[i] +
    combined$oth_unreliab [i] - combined$oth_honest[i] + combined$oth_self[i]}

# Model 1: baseline
othchrct_model1<- lm(combined$othchrct_combined ~combined$treatment) 
otgrp_coef<-coeftest(othchrct_model1 , vcov = vcovHC(othchrct_model1, type = "HC1"))[2,1]/ sd(combined[combined$treatment=="0",]$othchrct_combined)
otgrp_se<-coeftest(othchrct_model1 , vcov = vcovHC(othchrct_model1, type = "HC1"))[2,2]/ sd(combined[combined$treatment=="0",]$othchrct_combined)

# Model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
m_otgrp<-lm_lin(othchrct_combined~treatment, covariates=covIn, data=combined)
otgrp_coef1<-m_otgrp$coefficients[2] /sd(combined[combined$treatment=="0",]$othchrct_combined)
otgrp_se1<-m_otgrp$std.error[2]/ sd(combined[combined$treatment=="0",]$othchrct_combined)

# Model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_otgrp2<-lm_lin(othchrct_combined~treatment, covariates=covIn, data=combined)
summary(m_otgrp2)
otgrp_coef2<-m_otgrp2$coefficients[2] /sd(combined[combined$treatment=="0",]$othchrct_combined)
otgrp_se2<-m_otgrp2$std.error[2]/ sd(combined[combined$treatment=="0",]$othchrct_combined)

# ------ OUTGROUP INDEX
# We use the PC score as our primary index outgroup regard, but also report results using the sum score
# Index as sum of z-scores of the 5 components
combined$outgroup_index <-  scale(combined$sd) + scale(combined$multi_stat) + scale(combined$ft) + scale(combined$othchrct_combined)+scale(combined$inchrct_combined)

# Index as a principal component score of the 5 components
outgroup_variables <- c("sd",
                        "multi_stat",
                        "othchrct_combined",
                        "ft","inchrct_combined")

# Checking internal validity and Chronbach alpha
outgroup_variables_pc <- combined[,outgroup_variables]
psych::alpha(outgroup_variables_pc, 'check.keys=TRUE')$total$std.alpha #0.518
prcomp <- prcomp(outgroup_variables_pc)
print(prcomp, digits=3)

group_attitudes_vars <- c("sd","multi_stat","ft","inchrct_combined","othchrct_combined")

# pca index of outgroup_index
first_pc_index <- function(dataIn=NULL,
                            varList=NULL){
  x <- dataIn[,varList]
  res   <- princomp(x)
  index <- as.vector(predict(res, newdata=x)[,1])
  return(index)
}
combined$outgroup_princomp <- first_pc_index(dataIn=combined,
                                             varList=group_attitudes_vars)


# Model 1: baseline [first, analysis on index as sum of z-scores; second, analysis on index as PCA score]
outgroup_index_model1<- lm(combined$outgroup_index~combined$treatment) 
summary(outgroup_index_model1)
outgroup_coef<-coeftest(outgroup_index_model1 , vcov = vcovHC(outgroup_index_model1, type = "HC1"))[2,1]/sd(combined[combined$treatment=="0",]$outgroup_index)
outgroup_se<-coeftest(outgroup_index_model1, vcov = vcovHC(outgroup_index_model1, type = "HC1"))[2,2]/sd(combined[combined$treatment=="0",]$outgroup_index)
outgroup_index_model2<- lm(combined$outgroup_princomp~combined$treatment) 
summary(outgroup_index_model2)
outgroup_pc_coef<-coeftest(outgroup_index_model2 , vcov = vcovHC(outgroup_index_model2, type = "HC1"))[2,1]/sd(combined[combined$treatment=="0",]$outgroup_princomp)
outgroup_pc_se<-coeftest(outgroup_index_model2, vcov = vcovHC(outgroup_index_model2, type = "HC1"))[2,2]/sd(combined[combined$treatment=="0",]$outgroup_princomp)

# Model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
m_otgrp_index<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined)
outgroup_coef1<-m_otgrp_index$coefficients[2] /sd(combined[combined$treatment=="0",]$outgroup_index)
outgroup_se1<-m_otgrp_index$std.error[2]/ sd(combined[combined$treatment=="0",]$outgroup_index)
summary(m_otgrp_index)

covIn <- as.formula("~freq_usage")
m_princomp_index<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
summary(m_princomp_index)
outgroup_pc_coef1<-m_princomp_index$coefficients[2] /sd(combined[combined$treatment=="0",]$outgroup_princomp)
outgroup_pc_se1<-m_princomp_index$std.error[2]/ sd(combined[combined$treatment=="0",]$outgroup_princomp)

# Model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_otgrp_index2<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined)
summary(m_otgrp_index2)
outgroup_coef2<-m_otgrp_index2$coefficients[2] /sd(combined[combined$treatment=="0",]$outgroup_index)
outgroup_se2<-m_otgrp_index2$std.error[2]/ sd(combined[combined$treatment=="0",]$outgroup_index)

m_princomp_index2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
summary(m_princomp_index2)
outgroup_pc_coef2<-m_princomp_index2$coefficients[2] /sd(combined[combined$treatment=="0",]$outgroup_princomp)
outgroup_pc_se2<-m_princomp_index2$std.error[2]/ sd(combined[combined$treatment=="0",]$outgroup_princomp)

# Table 7
coef.vec_outgroup <- c(ft_coef,
              sd_coef,
              cpr_coef,
              perc_coef,
              otgrp_coef,
              outgroup_coef,
              outgroup_pc_coef)
se.vec_outgroup <- c(ft_se,
                       sd_se,
                       cpr_se,
                       perc_se,
                       otgrp_se,
                       outgroup_se,
                       outgroup_pc_se)
names<-c("Feeling Thermometer","Social Distance","Cooperation","Perception of out-group evaluations","Outgroup Traits","Outgroup Regard (sum of z-scores)","Outgroup Regard (principal components)")
merged_outgroup<- data.frame(names, coef.vec_outgroup, se.vec_outgroup)
merged_outgroup

coef.vec_outgroup1 <- c(ft_coef1,
                       sd_coef1,
                       cpr_coef1,
                       perc_coef1,
                       otgrp_coef1,
                       outgroup_coef1,
                       outgroup_pc_coef1)
se.vec_outgroup1 <- c(ft_se1,
                     sd_se1,
                     cpr_se1,
                     perc_se1,
                     otgrp_se1,
                     outgroup_se1,
                     outgroup_pc_se1)
merged_outgroup1<- data.frame(names, coef.vec_outgroup1, se.vec_outgroup1)
merged_outgroup1

coef.vec_outgroup2 <- c(ft_coef2,
                        sd_coef2,
                        cpr_coef2,
                        perc_coef2,
                        otgrp_coef2,
                        outgroup_coef2,
                        outgroup_pc_coef2)
se.vec_outgroup2 <- c(ft_se2,
                      sd_se2,
                      cpr_se2,
                      perc_se2,
                      otgrp_se2,
                      outgroup_se2,
                      outgroup_pc_se2)
merged_outgroup2<- data.frame(names, coef.vec_outgroup2, se.vec_outgroup2)
merged_outgroup2

# -------- *Table S6* ------------
join_all(list(merged_outgroup, merged_outgroup1, merged_outgroup2), by="names")
# -------- *Table S6* ------------


# ------------------------------------
#  FDR Adjusted Results: Table S7 & S8
# ------------------------------------
m_princomp_index2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
summary(m_princomp_index2)
# SWB Model 2: Adjusting for a full set of covariates
p.values_swb<-c(
  (satisfaction2)$p.value[2],
  (joy2)$p.value[2],
  (fulfillment2)$p.value[2],
  (nerv2)$p.value[2],
  (boredom2)$p.value[2],
  (loneliness2)$p.value[2],
  (depression2)$p.value[2],
  (isolation2)$p.value[2],
  (swb2)$p.value[2])
names(p.values_swb)<-c("Satisfaction","Joy","Fulfillment","Anxiety","Boredom","Loneliness", "Depression","Isolation","Well-Being index")
p.values_swb<-sort(p.values_swb)
adjusted.p_swb<-as.table(p.adjust(p.values_swb,"BH"))
adjusted.p_swb

# OutGroup Regard Model 2: Adjusting for a full set of covariates
p.values_outgroup <-c(
  (m_ft2)$p.value[2],
  (m_sd2)$p.value[2],
  (m_cpr2)$p.value[2],
  (m_perc2)$p.value[2],
  (m_otgrp2)$p.value[2],
  (m_otgrp_index2)$p.value[2],
  (m_princomp_index2)$p.value[2])
names(p.values_outgroup)<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                            "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
p.values_swb<-sort(p.values_swb)
adjusted_p_outgroup<-as.table(p.adjust(p.values.outgroup,"BH"))
adjusted_p_outgroup

# ------ Figure S6 (outgroup attitudes): Treatment of deactivation on the subsample of users who shared their online data
online_data <- combined[!is.na(combined$bosniak.3),]

# Model 2, adjusting for frequency of usage for all the 5 indicators of outgroup attitudes:
covIn <- as.formula("~freq_usage")

# composite outgroup index
m<-lm_lin(outgroup_index~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$outgroup_index)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$outgroup_index)

m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$outgroup_index)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$outgroup_index)

# feeling thermoemeter
m<-lm_lin(ft~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$ft)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$ft)

# social distnacce
m<-lm_lin(sd~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$sd)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$sd)

# cooperation
m<-lm_lin(multi_stat~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$multi_stat)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$multi_stat)

# perceptions of other's evaluations
m<-lm_lin(othchrct_online_data~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$othchrct_online_data)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$othchrct_online_data)

# other's characteristics
m<-lm_lin(inchrct_online_data~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$inchrct_online_data)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$inchrct_online_data)

# Plotting Figure S3
dev.off()
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#
par(mar=c(10,8,3,1)) #20 je desna strana
coef.vec<- c(-0.238, -0.160,-0.223,-0.034, -0.007, -0.237)
se.vec <- c(0.113,0.116, 0.112,0.114,0.107, 0.109,0.113)
var.names <- c("Feeling thermometer","Social closeness","Cooperation","Perception of \n out-group evaluations","Out-group traits","Out-group regard index")
results_df <- data.frame(term=var.names, estimate=coef.vec,std.error=se.vec)
y.axis <- c(length(coef.vec):1)
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)" , cex.lab=2.3,ylab = "", pch = 19,cex = 1,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), main = "", col=c("red","red","red","red","red","red","red","lightblue"),cex.lab=1)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.2, col = c("red","red","red","red","red","red","red","lightblue"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(7,0.5,0.5))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1.2) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="blue",lty=2,lwd=1.6)

dev.off()
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))
par(mar=c(10,8,3,1)) 
coef.vec<- c(-0.243,0.176,0.003 ,0.059, -0.009, -0.384,0.042,-0.220, -0.120,0.018)
se.vec <- c(0.108,0.106,0.116,0.106,0.107,0.108,0.111,0.106,0.113, 0.117)
var.names <- c("News Knowledge","Well-being index","Satisfaction","Joy","Fulfillment","Anxiety","Boredom","Loneliness", "Depression","Isolation")
results_df <- data.frame(term=var.names, estimate=coef.vec,
                         std.error=se.vec)
y.axis <- c(length(coef.vec):1) #par(mar=c(2, 13, 0, 0))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1,ylab = "", pch = 19,cex = 1,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "",mgp = c(3,.7,0.2), col=c("blue",
                                                                    "blue",
                                                                    "blue",
                                                                    "blue",
                                                                    "blue",
                                                                    "blue",
                                                                    "blue",
                                                                    "blue",
                                                                    "blue",
                                                                    "blue"))
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.1, col = c("blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1.2) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.6)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.6)
# draw dotted line through 0 
#box(bty = "l") #place box around plot


# ----- Figure S6 (news + swb): Treatment of deactivation on the subsample of users who shared their online data-
covIn <- as.formula("~freq_usage")

# news
m<-lm_lin(c_news~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$c_news)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$c_news)

# swb index + associated indicators
m<-lm_lin(swb~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$swb)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$swb)

m<-lm_lin(satisf~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$satisf)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$satisf)

m<-lm_lin(depression~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$depression)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$depression)

m<-lm_lin(loneliness~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$loneliness)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$loneliness)

m<-lm_lin(nerv~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$nerv)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$nerv)

m<-lm_lin(boredom~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$boredom)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$boredom)

m<-lm_lin(joy~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$joy)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$joy)

m<-lm_lin(isol~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$isol)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$isol)

m<-lm_lin(fulf~treatment, covariates=covIn, data=online_data)
m$coefficients[2]/sd(online_data[online_data$treatment=="0",]$fulf)
m$std.error[2]/ sd(online_data[online_data$treatment=="0",]$fulf)

