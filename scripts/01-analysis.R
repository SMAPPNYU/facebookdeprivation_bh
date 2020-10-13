# Set working directory
rootArg <-  if(Sys.info()["user"]=="nejlaasimovic"){
  setwd("/Users/nejlaasimovic/Desktop/data")
}

# Install or/and activate packages
# install.packages
library(ggplot2); library(ggpubr); library(readxl);
library(stringr); library(estimatr);library(plm)
library(skimr); library(Rmisc); library(devtools);
library(plotly); library(dplyr); library(Rmisc)
library(lmtest)

# Upload the main dataset
combined <- read.csv("bosnia_data_anonym.csv", stringsAsFactors = FALSE)
options(digits=3)

# -----------------------------------------------------------------
#    Table S1: DESCRIPTIVE STATISTICS - baseline characteristics
# -----------------------------------------------------------------

des.variables<-c("age","gender","educ","employ1","trust_media.1","time_usage","freq_usage","freq_news","numb_forums","freq_fbnews","politics_int", "imp_ethn","imp_cntry","imp_relg","imp_family","imp_neigh")
stargazer::stargazer(combined[des.variables], type="text")
# skim(combined[des.variables]) # looking at full distribution

# -------------------------------------------------------------------
#    Table S2: DESCRIPTIVE STATISTICS - ethnic composition
# --------------------------------------------------------------------

combined$ethn_t <- NA
combined[which(combined$ethn=="1" | combined$ethn=="-1"),]$ethn_t<- 1 #Bosniak
combined[which(combined$ethn=="2"|combined$ethn=="-4"),]$ethn_t<-2 #Serb
combined[which(combined$ethn=="3"|combined$ethn=="-5"),]$ethn_t<-3 #Croat
combined[which(combined$ethn=="-3"|combined$ethn=="-2"),]$ethn_t<-4 #Bosnian
combined[which(combined$ethn=="0"),]$ethn_t<-5 #Other/Do not want to say

# As their ethnic belonging, some participants indicated religion, some nationality, others
# chose not to respond. While we use these selections as different categories (when including the covariate in the main analysis)
# to allow for any nuances that may exist between them, below we combine the religion
# with the corresponding/most frequent ethnic category (as the two greatly overlap within the context)


table2<-table(combined$ethn_t) 
names(table2)<-c("Bosniak","Serb","Croat","Bosnian","Other/Do not want to say")
table2<-cbind(table2, as.vector(table2/353*100))
colnames(table2)<-c("No. of people","Share")
table2

# --------------------------------
#   Table 3: COVARIATE BALANCE
# ---------------------------------
# --- Table 3 as in the paper edited within Overleaf

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

# -----------------------------------------------
#   Table S4: ATTRITION - JT needs to provide comments first on attrition; can skip for now
# ------------------------------------------------

attrition_ctrl<-read_excel("sept18_active_attrition_c.xlsx")
attrition_treat<-read_excel("sept18_attrition_treatedgroup_c_sept20.xlsx")

attrition_ctrl<- combined[,bal.variables]
attrition_treat<-combined[,bal.variables]

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
# Reverse coding negative emotions, so that higher values indicate more positive subjective well-being
combined$nerv <- (-1) * combined$nerv 
combined$depression <- (-1) * combined$depression
combined$loneliness <- (-1) * combined$loneliness
combined$boredom <- (-1) * combined$boredom
combined$isol<- (-1) * combined$isol

# Checking the internal consistency of the scale
wellbeing_vars <- c("satisf", "joy","fulf","depression","boredom","isol","loneliness","nerv")
wellbeing_df <- combined[,wellbeing_vars]
psych::alpha(wellbeing_df, 'check.keys=TRUE')$total$std.alpha #0.829 - high
prcomp<- prcomp(wellbeing_df)
print(prcomp, digits=3) # Checking the loadings

# Observe relatively uniform loadings across variables, proceed by creating sum score index
                       
# NOTE FOR JIM [one possibility is to create the analysis function that will apply to all variables (below for Model 3, full covariate spec), but it seemed to me that going model by model
                       # for each separate outcome is easier for readers to follow; if you think it's too mundane/repetitive, I could use a function through which
                       # all outcomes could be run, as below on the example of news as the outcome]
                       
 # VERSION separate function for model 3                                  
ITT_res <- function(vUp="",
                    label_name="",
                    dataIn=NULL){
  formUp <- as.formula(paste0(vUp,
                              "~",
                              "treatment"))
  covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
  Model3 <- lm_lin(formUp,
                      covariates=covIn,
                      data=dataIn)
  fitOut <- tidy(Model3)
  results <- fitOut[fitOut[,"term"]=="treatment",]
  results$controlmean <- mean(dataIn[dataIn$treatment==0,vUp], na.rm=TRUE)
  results$controlsd <- sd(dataIn[dataIn$treatment==0,vUp], na.rm=TRUE)
  results$standardized_est<-fitOut$estimate[2]/  results$controlsd 
  results$standardized_sd<-fitOut$std.error[2]/  results$controlsd 
  results$N <- Model3$N
  results$label <- label_name
  return(results)
}                   
ITT_res("c_news","label",combined) # for example, for news outcome: results$standardized_est is the coef of interest
                       
# VERSION Model by Model                      
# Primary index - sum of z-scores
for (i in 1:nrow(combined)){
  combined$swb[i]<- scale(combined$satisf)[i]+scale(combined$joy)[i]+scale(combined$fulf)[i]+scale(combined$depression)[i] +scale(combined$loneliness)[i] + scale(combined$nerv)[i] 
  +scale(combined$boredom)[i]+scale(combined$isol[i])
}

#  model 1: baseline
swb0 <-lm((swb)~(treatment), data = combined)
swb_coef<-coeftest(swb0, vcov = vcovHC(swb0, type = "HC1"))[2,1]/sd(combined[combined$treatment=="0",]$swb)
swb_se<-coeftest(swb0, vcov = vcovHC(swb0, type = "HC1"))[2,2]/sd(combined[combined$treatment=="0",]$swb)

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
swb1<-lm_lin(swb~treatment, covariates=covIn, data=combined)
swb_coef1<-swb1$coefficients[2] /sd(combined[combined$treatment=="0",]$swb)
swb_se1<-swb1$std.error[2]/ sd(combined[combined$treatment=="0",]$swb)

# model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
swb2<-lm_lin(swb~treatment, covariates=covIn, data=combined)
swb_coef2<-swb2$coefficients[2] /sd(combined[combined$treatment=="0",]$swb)
swb_se2<-swb2$std.error[2]/ sd(combined[combined$treatment=="0",]$swb)


# --------------  Coefficients for all the separate outcomes 
# ------- Satisfaction, Model 1
satisfaction <-lm((satisf)~(treatment), data = combined)
satisfaction_coef<-coeftest(satisfaction , vcov = vcovHC(satisfaction, type = "HC1"))[2,1]/sd(combined[combined$treatment=="0",]$satisf)
satisfaction_se<-coeftest(satisfaction, vcov = vcovHC(satisfaction, type = "HC1"))[2,2]/sd(combined[combined$treatment=="0",]$satisf)

# Satisfaction, Model 2
covIn <- as.formula("~freq_usage") 
satisfaction1<- lm_lin(satisf~treatment, covariates=covIn, data=combined)
satisfaction_coef1<-satisfaction1$coefficients[2] /sd(combined[combined$treatment=="0",]$satisf)
satisfaction_se1<-satisfaction1$std.error[2]/ sd(combined[combined$treatment=="0",]$satisf)

# Satisfaction, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
satisfaction2 <- lm_lin(satisf~treatment, covariates=covIn, data=combined)
satisfaction_coef2<-satisfaction2$coefficients[2] /sd(combined[combined$treatment=="0",]$satisf)
satisfaction_se2<-satisfaction2$std.error[2]/ sd(combined[combined$treatment=="0",]$satisf)

# ------- Joy, Model 1
joy <-lm((joy)~(treatment), data = combined)
joy_coef<-coeftest(joy , vcov = vcovHC(joy, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$joy)))
joy_se<-coeftest(joy, vcov = vcovHC(joy, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$joy)))

# Joy, Model 2
covIn <- as.formula("~freq_usage")
joy1<-lm_lin(joy~treatment, covariates=covIn, data=combined)
joy_coef1<-joy1$coefficients[2]/sd(combined[combined$treatment=="0",]$joy)
joy_se1<-joy1$std.error[2]/ sd(combined[combined$treatment=="0",]$joy)

# Joy, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
joy2<-lm_lin(joy~treatment, covariates=covIn, data=combined)
joy_coef2<-joy2$coefficients[2]/sd(combined[combined$treatment=="0",]$joy)
joy_se2<-joy2$std.error[2]/ sd(combined[combined$treatment=="0",]$joy)

# -------  Fulfillment, Model 1
fulf <-lm(fulf~treatment, data = combined)
fulf_coef<-coeftest(fulf , vcov = vcovHC(fulf, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$fulf)))
fulf_se<-coeftest(fulf, vcov = vcovHC(fulf, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$fulf)))

# Fulfillment, Model 2
covIn <- as.formula("~freq_usage")
fulfillment1<-lm_lin(fulf~treatment, covariates=covIn, data=combined)
fulf_coef1<-fulfillment1$coefficients[2]/sd(combined[combined$treatment=="0",]$fulf)
fulf_se1<-fulfillment1$std.error[2]/ sd(combined[combined$treatment=="0",]$fulf)

# Fulfillment, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
fulfillment2<-lm_lin(fulf~treatment, covariates=covIn, data=combined)
fulf_coef2<-fulfillment2$coefficients[2]/sd(combined[combined$treatment=="0",]$fulf)
fulf_se2<-fulfillment2$std.error[2]/ sd(combined[combined$treatment=="0",]$fulf)

# -------  Anxiety, Model 1
nerv <-lm((nerv)~(treatment), data = combined)
nerv_coef<-coeftest(nerv , vcov = vcovHC(nerv, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$nerv)))
nerv_se<-coeftest(nerv, vcov = vcovHC(nerv, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$nerv)))

# Anxiety, Model 2
covIn <- as.formula("~freq_usage")
nerv1<-lm_lin(nerv~treatment, covariates=covIn, data=combined)
nerv_coef1<-nerv1$coefficients[2] /sd(combined[combined$treatment=="0",]$nerv)
nerv_se1<-nerv1$std.error[2]/ sd(combined[combined$treatment=="0",]$nerv)

# Anxiety, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
nerv2<-lm_lin(nerv~treatment, covariates=covIn, data=combined)
nerv_coef2<-nerv2$coefficients[2] /sd(combined[combined$treatment=="0",]$nerv)
nerv_se2<-nerv2$std.error[2]/ sd(combined[combined$treatment=="0",]$nerv)
summary(nerv2)

# ------- Boredom, Model 1
boredom <-lm((boredom)~(treatment), data = combined)
boredom_coef<-coeftest(boredom, vcov = vcovHC(boredom, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$boredom)))
boredom_se<-coeftest(boredom, vcov = vcovHC(boredom, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$boredom)))

# Boredom, Model 2
covIn <- as.formula("~freq_usage")
boredom1<-lm_lin(boredom~treatment, covariates=covIn, data=combined)
boredom_coef1<-boredom1$coefficients[2] /sd(combined[combined$treatment=="0",]$boredom)
boredom_se1<-boredom1$std.error[2]/ sd(combined[combined$treatment=="0",]$boredom)

# Boredom, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
boredom2<-lm_lin(boredom~treatment, covariates=covIn, data=combined)
boredom_coef2<-boredom2$coefficients[2] /sd(combined[combined$treatment=="0",]$boredom)
boredom_se2<-boredom2$std.error[2]/ sd(combined[combined$treatment=="0",]$boredom)
summary(boredom2)

# ------- Loneliness, Model 1
loneliness <-lm((loneliness)~(treatment), data = combined)
loneliness_coef<-coeftest(loneliness , vcov = vcovHC(loneliness, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$loneliness)))
loneliness_se<-coeftest(loneliness, vcov = vcovHC(loneliness, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$loneliness)))

# Loneliness, Model 2
covIn <- as.formula("~freq_usage")
loneliness1<-lm_lin(loneliness~treatment, covariates=covIn, data=combined)
loneliness_coef1<-loneliness1$coefficients[2] /sd(combined[combined$treatment=="0",]$loneliness)
loneliness_se1<-loneliness1$std.error[2]/ sd(combined[combined$treatment=="0",]$loneliness)

#  Loneliness, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
loneliness2<-lm_lin(loneliness~treatment, covariates=covIn, data=combined)
loneliness_coef2<-loneliness2$coefficients[2] /sd(combined[combined$treatment=="0",]$loneliness)
loneliness_se2<-loneliness2$std.error[2]/ sd(combined[combined$treatment=="0",]$loneliness)

# -------  Depression, Model 1
depression <-lm((depression)~(treatment), data = combined)
depression_coef<-coeftest(depression, vcov = vcovHC(depression, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$depression)))
depression_se<-coeftest(depression, vcov = vcovHC(depression, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$depression)))

# Depression, Model 2
covIn <- as.formula("~freq_usage")
depression1<-lm_lin(depression~treatment, covariates=covIn, data=combined)
depression_coef1<-depression1$coefficients[2] /sd(combined[combined$treatment=="0",]$depression)
depression_se1<-depression1$std.error[2]/ sd(combined[combined$treatment=="0",]$depression)

# Depression, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
depression2<-lm_lin(depression~treatment, covariates=covIn, data=combined)
depression_coef2<-depression2$coefficients[2] /sd(combined[combined$treatment=="0",]$depression)
depression_se2<-depression2$std.error[2]/ sd(combined[combined$treatment=="0",]$depression)

# -------  Isolation, Model 1
isol <-lm((isol)~(treatment), data = combined)
isol_coef<-coeftest(isol , vcov = vcovHC(isol, type = "HC1"))[2,1]/sd(na.omit((combined[combined$treatment=="0",]$isol)))
isol_se<-coeftest(isol, vcov = vcovHC(isol, type = "HC1"))[2,2]/sd(na.omit((combined[combined$treatment=="0",]$isol)))

# Isolation, Model 2
covIn <- as.formula("~freq_usage")
isolation1<-lm_lin(isol~treatment, covariates=covIn, data=combined)
isol_coef1<-isolation1$coefficients[2]/sd(combined[combined$treatment=="0",]$isol)
isol_se1<-isolation1$std.error[2]/ sd(combined[combined$treatment=="0",]$isol)

# Isolation, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
isolation2<-lm_lin(isol~treatment, covariates=covIn, data=combined)
isol_coef2<-isolation2$coefficients[2]/sd(combined[combined$treatment=="0",]$isol)
isol_se2<-isolation2$std.error[2]/ sd(combined[combined$treatment=="0",]$isol)

# ------------- Referenced in the main paper 
# Excluding the outliers
outlier.values<-boxplot.stats(combined$swb)$out  
no_swb_outliers<-combined[!combined$swb %in% outlier.values,]

# Model 3 (full covariate specification), treatment effect on swb excluding the outliers
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
swb2_nooutliers<-lm_lin(swb~treatment, covariates=covIn, data=no_swb_outliers)
summary(swb2_nooutliers)
swb2_nooutliers_coef2<-swb2_nooutliers$coefficients[2] /sd(no_swb_outliers[no_swb_outliers$treatment=="0",]$swb)
swb2_nooutliers_se2<-swb2_nooutliers$std.error[2]/ sd(no_swb_outliers[no_swb_outliers$treatment=="0",]$swb)

# Results of directional hypothesis in the direction of the hypothesis
t.test(no_swb_outliers[no_swb_outliers$treatment=="0",]$swb, no_swb_outliers[no_swb_outliers$treatment=="1",]$swb, alternative="less")


# ----------------------------
#    FACTUAL NEWS KNOWLEDGE
# -----------------------------
# Creating the news index as sum of correct/false responses
# (within the dataset, each  response coded -1 if a false response was given; 1 if true and 0 if "unsure" was selected)
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
news_coef1<-news1$coefficients[2]/sd(combined[combined$treatment=="0",]$c_news)
news_se1<-news1$std.error[2]/ sd(combined[combined$treatment=="0",]$c_news)

# News - Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
news2<-lm_lin(c_news~treatment, covariates=covIn, data=combined)
news_coef2<-news2$coefficients[2]/sd(combined[combined$treatment=="0",]$c_news)
news_se2<-news2$std.error[2]/ sd(combined[combined$treatment=="0",]$c_news)

# Table S5 - Model 1
# extract results of interest 
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
coef.vec1<- c(news_coef1,
              swb_coef1,
              satisfaction_coef1,
              joy_coef1,   
              fulf_coef1, 
              nerv_coef1,
              boredom_coef1,
              loneliness_coef1,
              depression_coef1, 
              isol_coef1)
se.vec1 <-  c(news_se1,
             swb_se1,
             satisfaction_se1,
             joy_se1,   
             fulf_se1, 
             nerv_se1,
             boredom_se1,
             loneliness_se1,
             depression_se1, 
             isol_se1)
merged1 <- data.frame(names, coef.vec1, se.vec1)
merged1

# Table S5 - Model 2
coef.vec2<- c(news_coef2,
              swb_coef2,
              satisfaction_coef2,
              joy_coef2,   
              fulf_coef2, 
              nerv_coef2,
              boredom_coef2,
              loneliness_coef2,
              depression_coef2, 
              isol_coef2)
se.vec2 <-  c(news_se2,
              swb_se2,
              satisfaction_se2,
              joy_se2,   
              fulf_se2, 
              nerv_se2,
              boredom_se2,
              loneliness_se2,
              depression_se2, 
              isol_se2)
merged2 <- data.frame(names, coef.vec2, se.vec2)
merged2

# -------- *Table S5* ------------
tableS5<-join_all(list(merged, merged1, merged2), by="names") 
tableS5
# -------- *Table S5* ------------

# -----------------------------------------
# MAIN RESULTS: INTER-ETHNIC ATTITUDES
# -----------------------------------------

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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
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
sd_coef1<-m_sd$coefficients[2]/sd(combined[combined$treatment=="0",]$sd)
sd_se1<-m_sd$std.error[2]/sd(combined[combined$treatment=="0",]$sd)

# Model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
m_sd2<-lm_lin(sd~treatment, covariates=covIn, data=combined)
sd_coef2<-m_sd2$coefficients[2] /sd(combined[combined$treatment=="0",]$sd)
sd_se2<-m_sd2$std.error[2]/ sd(combined[combined$treatment=="0",]$sd)

# ------- COOPERATION 
# creating cooperation index as a sum of z-scales from the agreement with two statements:
# 1.Multi-ethnic parties cannot secure the interest of my people.
# 2.It is only possible to cooperate with members of my ethnic group.
# statements <-c("agr_st5","agr_st7")
# There were 2 missing values for which we imputed the results (mode for that age group)
# combined[,"agr_st5"] <- sapply("agr_st5", function (x) combined[,x] <- imputeGroupMode (dataIn=combined, varIn = x, groupVar="age"))
for (i in 1:nrow(combined)){
combined$multi_stat[i] <- scale(combined$agr_st5)[i] + scale(combined$agr_st7)[i] 
 }

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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
m_cpr2<-lm_lin(multi_stat~treatment, covariates=covIn, data=combined)
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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
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

# Creating PC index of outgroup regard
group_attitudes_vars <- c("sd","multi_stat","ft","inchrct_combined","othchrct_combined")
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


# Model 3: full set of covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
m_otgrp_index2<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined)
outgroup_coef2<-m_otgrp_index2$coefficients[2] /sd(combined[combined$treatment=="0",]$outgroup_index)
outgroup_se2<-m_otgrp_index2$std.error[2]/ sd(combined[combined$treatment=="0",]$outgroup_index)

m_princomp_index2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
outgroup_pc_coef2<-m_princomp_index2$coefficients[2] /sd(combined[combined$treatment=="0",]$outgroup_princomp)
outgroup_pc_se2<-m_princomp_index2$std.error[2]/ sd(combined[combined$treatment=="0",]$outgroup_princomp)

# Table 6
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
tableS6<-join_all(list(merged_outgroup, merged_outgroup1, merged_outgroup2), by="names")
tableS6
# -------- *Table S6* ------------

# ------------------------------------
#  FDR Adjusted Results: Table S7 & S8
# ------------------------------------
# Table 7, Subjective Well Being: Adjusting for a full set of covariates
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

FDR_swb <- data.frame(p.values_swb, adjusted.p_swb)
colnames(FDR_swb) <- c("BH Adjusted p-value","Variable Name","Non-adjusted p-value")
print(FDR_swb, digits = 4)
FDR_swb # For table S7, coefficients and standard errors extracted from Table 5


# Table S8, Outgroup Regard: Adjusting for a full set of covariates
p.values_outgroup <-c(
  (m_ft2)$p.value[2],
  (m_sd2)$p.value[2],
  (m_cpr2)$p.value[2],
  (m_perc2)$p.value[2],
  (m_otgrp2)$p.value[2],
  (m_otgrp_index2)$p.value[2],
  (m_princomp_index2)$p.value[2])
names(p.values_outgroup)<-c("feeling thermometer", "sd","cooperation","perception of out-group evaluations",
                            "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
p.values_outgroup<-sort(p.values_outgroup)
adjusted.p_outgroup<-as.table(p.adjust(p.values_outgroup,"BH"))

FDR_outgroup <- data.frame(p.values_outgroup, adjusted.p_outgroup)
colnames(FDR_outgroup) <- c("BH Adjusted p-value","Variable Name","Non-adjusted p-value")
print(FDR_outgroup, digits = 4)
FDR_outgroup # For table S7, coefficients and standard errors extracted from Table 5



