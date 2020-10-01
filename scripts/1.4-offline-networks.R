# --------------------------------------
# Offline networks: TABLE S14-S18
# ---------------------------------------

# -------  Ethnic Fractionalization Index

# Calculated using the formula below; already incorporated in the dataset
# for (i in 1:length(combined$email)){
 # combined$fractionalization_offline_index[i] <- 1 - (combined$bosniak[i]^2+combined$serb[i]^2+combined$croat[i]^2)}
combined$ef_median_index<-ifelse(combined$fractionalization_offline_index== median(na.omit(combined$fractionalization_offline_index))|combined$fractionalization_offline_index>median(na.omit(combined$fractionalization_offline_index)),1,0) # smaller, more homogenous; 1 is more het

# ------- City Homogeneity: Majority Share
combined$ms_median_index <- ifelse(combined$largest_majority > median(combined$largest_majority) | combined$largest_majority==median(combined$largest_majority), 1, 0) 

# -------  Shannon Entropy
# Calculated using the formula below; already incorporated in the dataset; 
# where one group is 0 (for three cities; log undefined), value calculated manually
for (i in 1:length(combined$email)){
  combined$shannon_offline[i] <- -(combined$bosniak[i]*log(combined$bosniak[i])+combined$croat[i]*log(combined$croat[i])+combined$serb[i]*log(combined$serb[i]))}

combined[is.na(combined$shannon_offline),]$city # few NAs for values that have 0 for one of the ethnic groups
#-combined[is.na(combined$shannon_offline & combined$city=="Cazin"),]$bosniak*log(combined[is.na(combined$shannon_offline & combined$city=="Cazin"),]$bosniak)+combined[is.na(combined$shannon_offline & combined$city=="Cazin"),]$croat*log(combined[is.na(combined$shannon_offline & combined$city=="Cazin"),]$croat)
#-combined[is.na(combined$shannon_offline & combined$city=="Buzim"),]$bosniak*log(combined[is.na(combined$shannon_offline & combined$city=="Buzim"),]$bosniak)
#-combined[is.na(combined$shannon_offline & combined$city=="Grude"),]$croat*log(combined[is.na(combined$shannon_offline & combined$city=="Grude"),]$croat)+combined[is.na(combined$shannon_offline & combined$city=="Grude"),]$serb*log(combined[is.na(combined$shannon_offline & combined$city=="Grude"),]$serb)
# -combined[is.na(combined$shannon_offline & combined$city=="Prozor-Rama"),]$croat*log(combined[is.na(combined$shannon_offline & combined$city=="Prozor-Rama"),]$croat)+combined[is.na(combined$shannon_offline & combined$city=="Prozor-Rama"),]$bosniak*log(combined[is.na(combined$shannon_offline & combined$city=="Prozor-Rama"),]$bosniak)
combined[is.na(combined$shannon_offline & combined$city== "Prozor-Rama"),]$shannon_offline <- -0.1289234
combined[is.na(combined$shannon_offline & combined$city=="Cazin"),]$shannon_offline <-  0.01365618
combined[is.na(combined$shannon_offline & combined$city=="Buzim"),]$shannon_offline<- 0.006975443
combined[is.na(combined$shannon_offline & combined$city=="Buzim, BiH"),]$shannon_offline<-0.006975443
combined[combined$city=="Grude",]$shannon_offline <--0.001920276
combined[is.na(combined$shannon_offline),]$shannon_offline <-  0.01365618 # Cazin "

combined$se_median_index<-ifelse(combined$shannon_offline >   median(na.omit(combined$shannon_offline)) |  combined$shannon_offline == median(na.omit(combined$shannon_offline)), 1, 0)

# ------ Effects on the three main families of outcomes, within homogenous communities within 3 indices of diversity

# ---------------------------------
#      Ethnic Fractionalization
# ---------------------------------

# Outgroup Attitudes - homogenous [first with outgroup index formed as a sum of z-scores;
# second regression with outgroup index formed as a PC score]

m <-lm(outgroup_index~(treatment), data=combined[combined$ef_median_index==0,])
outgroup_ef_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$outgroup_index)
outgroup_ef_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$outgroup_index)

m_pc <-lm((outgroup_princomp)~(treatment), data=combined[combined$ef_median_index==0,])
outgroup_ef_pc_coef<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index=="0" & combined$treatment=="0",]$outgroup_princomp)
outgroup_ef_pc_se<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index=="0" & combined$treatment=="0",]$outgroup_princomp)

# more homogenous, one control -- ef_median_index==0 
covIn <- as.formula("~freq_usage")
outgroup_ef<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
outgroup_ef_coef1<-outgroup_ef$coefficients[2] /sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_index)
outgroup_ef_se1<-outgroup_ef$std.error[2]/ sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_pc_ef<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
outgroup_ef_pc_coef1<-outgroup_pc_ef$coefficients[2] /sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)
outgroup_ef_pc_se1<-outgroup_pc_ef$std.error[2]/ sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)

# more homogenous, all controls -- ef_median_index==0 
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
outgroup_ef_coef2<-m$coefficients[2] /sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$outgroup_index)
outgroup_ef_se2<-m$std.error[2]/ sd(combined[combined$ef_median_index==0 &combined$treatment==0,]$outgroup_index)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_pc<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
outgroup_ef_pc_coef2<-m_pc$coefficients[2] /sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)
outgroup_ef_pc_se2<-m_pc$std.error[2]/ sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)

# ------------- heteorogeneous
# offline heterog = 1
m <-lm((outgroup_princomp)~(treatment), data=combined[combined$ef_median_index==1,])
summary(m)
coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==1 & combined$treatment=="0",]$outgroup_index)
coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==1 & combined$treatment=="0",]$outgroup_index)

m_pc_htr <-lm((outgroup_princomp)~(treatment), data=combined[combined$ef_median_index==1,])
summary(m_pc_htr)
coeftest(m_pc_htr, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==1 & combined$treatment=="0",]$outgroup_princomp)
coeftest(m_pc_htr, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==1 & combined$treatment=="0",]$outgroup_princomp)

# more homogenous, one control -- ef_median_index==1 
covIn <- as.formula("~freq_usage")
outgroup_ef_htr<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ef_median_index==1,])
summary(outgroup_ef_htr)
outgroup_ef_htr$coef_htrficients[2] /sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_index)
outgroup_ef_htr$std.error[2]/ sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_pc_ef_htr<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ef_median_index==1,])
summary(outgroup_pc_ef_htr)
outgroup_pc_ef_htr$coef_coefficients[2] /sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_princomp)
outgroup_pc_ef_htr$std.error[2]/ sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_princomp)

# more heterogeneous, all controls -- ef_median_index==1 
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_htr<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$ef_median_index==1,])
m_htr$coefficients[2] /sd(combined[combined$ef_median_index==1 & combined$treatment==0,]$outgroup_index)
m_htr$std.error[2]/ sd(combined[combined$ef_median_index==1 &combined$treatment==0,]$outgroup_index)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_pc_htr<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ef_median_index==1,])
m_pc_htr$coefficients[2] /sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_princomp)
m_pc_htr$std.error[2]/ sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_princomp)

# Subjective Well-Being
m <-lm((swb)~(treatment), data=combined[combined$ef_median_index==0,])
coeftest(m , vcov = vcovHC(joy, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$swb)
coeftest(m , vcov = vcovHC(joy, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$swb)

covIn <- as.formula("~freq_usage")
m <-lm((swb)~(treatment), data=combined[combined$ef_median_index==0,])
coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$swb)
coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$swb)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m <-lm((swb)~(treatment), data=combined[combined$ef_median_index==0,])
coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$swb)
coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$swb)

# Political News
m <-lm((c_news)~(treatment), data=combined[combined$ef_median_index==0,])
coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$c_news)
coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$c_news)

covIn <- as.formula("~freq_usage")
m <-lm((c_news)~(treatment), data=combined[combined$ef_median_index==0,])
coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$c_news)
coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$c_news)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m <-lm((c_news)~(treatment), data=combined[combined$ef_median_index==0,])
coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$c_news)
coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$c_news)



# ---------  Ethnic Fractionalization: Out-group attitudes indicator-by-indicator analysis within homogenous communities

# ----- Feeling Thermometer
# model 1: baseline
ft_model1 <- lm(combined[combined$ef_median_index==0,]$ft~combined[combined$ef_median_index==0,]$treatment) 
ft_ef_coef<-coeftest(ft_model1 , vcov = vcovHC(ft_model1, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$ft)
ft_ef_se<-coeftest(ft_model1 , vcov = vcovHC(ft_model1, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$ft)

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
ft_model1_ef<-lm_lin(ft~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
ft_ef_coef1<-ft_model1_ef$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$ft)
ft_ef_se1<-ft_model1_ef$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$ft)

# model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m<-lm_lin(ft~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
ft_ef_coef2<-m$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$ft)
ft_ef_se2<-m$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$ft)

# ----- SD
# model 1: baseline
sd_model1 <- lm(combined[combined$ef_median_index==0,]$sd~combined[combined$ef_median_index==0,]$treatment) 
sd_ef_coef<-coeftest(sd_model1 , vcov = vcovHC(sd_model1, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$sd)
sd_ef_se<-coeftest(sd_model1 , vcov = vcovHC(sd_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$ef_median_index==0 & combined$treatment==0,]$sd))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
sd_model1_ef<-lm_lin(sd~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
sd_ef_coef1<-sd_model1_ef$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$sd)
sd_ef_se1<-sd_model1_ef$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$sd)

# model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m<-lm_lin(sd~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
sd_ef_coef2<-m$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$sd)
sd_ef_se2<-m$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$sd)

# ----- Cooperation
# model 1: baseline
multi_stat_model1 <- lm(combined[combined$ef_median_index==0,]$multi_stat~combined[combined$ef_median_index==0,]$treatment) 
cprn_ef_coef<-coeftest(multi_stat_model1 , vcov = vcovHC(multi_stat_model1, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$multi_stat)
cprn_ef_se<-coeftest(multi_stat_model1 , vcov = vcovHC(multi_stat_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$ef_median_index==0 & combined$treatment==0,]$multi_stat))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
cp_model1_ef<-lm_lin(multi_stat~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
cprn_ef_coef1<-cp_model1_ef$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$multi_stat)
cprn_ef_se1<-cp_model1_ef$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$multi_stat)

# model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
cp_model2_ef<-lm_lin(multi_stat~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
cprn_ef_coef2<-cp_model2_ef$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$multi_stat)
cprn_ef_se2<-cp_model2_ef$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$multi_stat)

# ----- Perception of Out-Group Evaluations
# model 1: baseline
inchrct_combined_model1 <- lm(combined[combined$ef_median_index==0,]$inchrct_combined~combined[combined$ef_median_index==0,]$treatment) 
prcp_ef_coef<-coeftest(inchrct_combined_model1 , vcov = vcovHC(inchrct_combined_model1, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$inchrct_combined)
prcp_ef_se<-coeftest(inchrct_combined_model1 , vcov = vcovHC(inchrct_combined_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$ef_median_index==0 & combined$treatment==0,]$inchrct_combined))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
prcp_model1_ef<-lm_lin(inchrct_combined~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
prcp_ef_coef1<-prcp_model1_ef$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$inchrct_combined)
prcp_ef_se1<-prcp_model1_ef$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$inchrct_combined)


# model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m<-lm_lin(inchrct_combined~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
prcp_ef_coef2<-m$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$inchrct_combined)
prcp_ef_se2<-m$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$inchrct_combined)


# ----- Out-Group Traits
# model 1: baseline
othchrct_combined_model1 <- lm(combined[combined$ef_median_index==0,]$othchrct_combined~combined[combined$ef_median_index==0,]$treatment) 
otgr_ef_coef<-coeftest(othchrct_combined_model1 , vcov = vcovHC(othchrct_combined_model1, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$othchrct_combined)
otgr_ef_se<-coeftest(othchrct_combined_model1 , vcov = vcovHC(othchrct_combined_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$ef_median_index==0 & combined$treatment==0,]$othchrct_combined))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
traits_model1_ef<-lm_lin(othchrct_combined~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
otgr_ef_coef1<-traits_model1_ef$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$othchrct_combined)
otgr_ef_se1<-traits_model1_ef$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$othchrct_combined)

# model 3: full covariates

summary(lm(othchrct_combined~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$ef_median_index=="0",]))
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m<-lm_lin(othchrct_combined~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
otgr_ef_coef2<-m$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$othchrct_combined)
otgr_ef_se2<-m$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$othchrct_combined)
summary(m)
# Putting estimates + standard errors into the table
coef.vec_ef <- c(outgroup_ef_coef,
              outgroup_ef_pc_coef,
              ft_ef_coef,
              sd_ef_coef,
              cprn_ef_coef,
              prcp_ef_coef,
             otgr_ef_coef)

se.vec_ef <- c(outgroup_ef_se,
              outgroup_ef_pc_se,
              ft_ef_se,
              sd_ef_se,
              cprn_ef_se,
              prcp_ef_se,
              otgr_ef_se)
names <- c("out-group regard (sum of z-scores)",'out-group regard (PC)',"feeling thermometer","social closeness",
           "cooperation","perception of out-group evaluations","out-group traits")
merged_ef <- data.frame(names, coef.vec_ef, se.vec_ef)

coef.vec_ef1 <- c(outgroup_ef_coef1,
                 outgroup_ef_pc_coef1,
                 ft_ef_coef1,
                 sd_ef_coef1,
                 cprn_ef_coef1,
                 prcp_ef_coef1,
                 otgr_ef_coef1)

se.vec_ef1 <- c(outgroup_ef_se1,
               outgroup_ef_pc_se1,
               ft_ef_se1,
               sd_ef_se1,
               cprn_ef_se1,
               prcp_ef_se1,
               otgr_ef_se1)
merged_ef1 <- data.frame(names, coef.vec_ef1, se.vec_ef1)

coef.vec_ef2 <- c(outgroup_ef_coef2,
                  outgroup_ef_pc_coef2,
                  ft_ef_coef2,
                  sd_ef_coef2,
                  cprn_ef_coef2,
                  prcp_ef_coef2,
                  otgr_ef_coef2)

se.vec_ef2 <- c(outgroup_ef_se2,
                outgroup_ef_pc_se2,
                ft_ef_se2,
                sd_ef_se2,
                cprn_ef_se2,
                prcp_ef_se2,
                otgr_ef_se2)
merged_ef2 <- data.frame(names, coef.vec_ef2, se.vec_ef2)

# -------- *Table S9* ------------
join_all(list(merged_ef, merged_ef1, merged_ef2), by="names")
# -------- *Table S9* ------------


# ---------------------------
#  SHANNON ENTROPY
# ----------------------------
# ---------  Shannon Entropy: Out-group attitudes indicator-by-indicator analysis within homogenous communities

m <-lm((outgroup_index)~(treatment), data=combined[combined$se_median_index==0,])
outgroup_se_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$outgroup_index)
outgroup_se_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$outgroup_index)

m <-lm((outgroup_princomp)~(treatment), data=combined[combined$se_median_index==0,])
outgroup_se_pc_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index=="0" & combined$treatment=="0",]$outgroup_princomp)
outgroup_se_pc_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index=="0" & combined$treatment=="0",]$outgroup_princomp)

covIn <- as.formula("~freq_usage")
outgroup_se<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
outgroup_se_coef1<-outgroup_se$coefficients[2] /sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_index)
outgroup_se_se1<-outgroup_se$std.error[2]/ sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_pc_se<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
outgroup_se_pc_coef1<-outgroup_pc_se$coefficients[2] /sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)
outgroup_se_pc_se1<-outgroup_pc_se$std.error[2]/ sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)

m<-lm(outgroup_index~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$se_median_index==0,])
outgroup_se_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$outgroup_index)
outgroup_se_se2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$outgroup_index)

m<-lm(outgroup_princomp~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$se_median_index==0,])
outgroup_se_pc_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$outgroup_princomp)
outgroup_se_pc_se2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$outgroup_princomp)


# ---------  Shannon Entropy: Out-group attitudes indicator-by-indicator analysis within homogenous communities

# Feeling Thermometer
# model 1: baseline
ft_model1 <- lm(combined[combined$se_median_index==0,]$ft~combined[combined$se_median_index==0,]$treatment) 
ft_se_coef<-coeftest(ft_model1 , vcov = vcovHC(ft_model1, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$ft)
ft_se_se<-coeftest(ft_model1 , vcov = vcovHC(ft_model1, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$ft)

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
ft_model1_se<-lm_lin(ft~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
ft_se_coef1<-ft_model1_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$ft)
ft_se_se1<-ft_model1_se$std.error[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$ft)

# model 3: full covariates
m<-lm(ft~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$se_median_index==0,])
ft_se_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$ft)
ft_se_se2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$ft)

# ----- SD
# model 1: baseline
sd_model1 <- lm(combined[combined$se_median_index==0,]$sd~combined[combined$se_median_index==0,]$treatment) 
sd_se_coef<-coeftest(sd_model1 , vcov = vcovHC(sd_model1, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$sd)
sd_se_se<-coeftest(sd_model1 , vcov = vcovHC(sd_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$se_median_index==0 & combined$treatment==0,]$sd))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
sd_model1_se<-lm_lin(sd~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
sd_se_coef1<-sd_model1_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$sd)
sd_se_se1<-sd_model1_se$std.error[2]/sd(na.omit(combined[combined$se_median_index==0 & combined$treatment==0,]$sd))

# model 3: full covariates
m<-lm(sd~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$se_median_index==0,])
sd_se_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$sd)
sd_se_se2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$sd)


# -----  Cooperation
# model 1: baseline
multi_stat_model1 <- lm(combined[combined$se_median_index==0,]$multi_stat~combined[combined$se_median_index==0,]$treatment) 
cprn_se_coef<-coeftest(multi_stat_model1 , vcov = vcovHC(multi_stat_model1, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$multi_stat)
cprn_se_se<-coeftest(multi_stat_model1 , vcov = vcovHC(multi_stat_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$se_median_index==0 & combined$treatment==0,]$multi_stat))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
cp_model1_se<-lm_lin(multi_stat~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
cprn_se_coef1<-cp_model1_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$multi_stat)
cprn_se_se1<-cp_model1_se$std.error[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$multi_stat)

# model 3: full covariates
m<-lm(multi_stat~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$se_median_index==0,])
cprn_se_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$multi_stat)
cprn_se_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$multi_stat)

# ----- Perception of Out-Group Evaluations
# model 1: baseline
inchrct_combined_model1 <- lm(combined[combined$se_median_index==0,]$inchrct_combined~combined[combined$se_median_index==0,]$treatment) 
prcp_se_coef<-coeftest(inchrct_combined_model1 , vcov = vcovHC(inchrct_combined_model1, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$inchrct_combined)
prcp_se_se<-coeftest(inchrct_combined_model1 , vcov = vcovHC(inchrct_combined_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$se_median_index==0 & combined$treatment==0,]$inchrct_combined))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
prcp_model1_se<-lm_lin(inchrct_combined~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
prcp_se_coef1<-prcp_model1_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$inchrct_combined)
prcp_se_se1<-prcp_model1_se$std.error[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$inchrct_combined)

# model 3: full covariates
m<-lm(inchrct_combined~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$se_median_index==0,])
prcp_se_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$inchrct_combined)
prcp_se_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$inchrct_combined)

# -----  Out-Group Traits
# model 1: baseline
othchrct_combined_model1 <- lm(combined[combined$se_median_index==0,]$othchrct_combined~combined[combined$se_median_index==0,]$treatment) 
otgr_se_coef<-coeftest(othchrct_combined_model1, vcov = vcovHC(othchrct_combined_model1, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$othchrct_combined)
otgr_se_se<-coeftest(othchrct_combined_model1, vcov = vcovHC(othchrct_combined_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$se_median_index==0 & combined$treatment==0,]$othchrct_combined))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
traits_model1_se<-lm_lin(othchrct_combined~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
otgr_se_coef1<-traits_model1_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$othchrct_combined)
otgr_se_se1<-traits_model1_se$std.error[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$othchrct_combined)

# model 3: full covariates
m<-lm(othchrct_combined~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$se_median_index==0,])
otgr_se_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$othchrct_combined)
otgr_se_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$othchrct_combined)


# Putting estimates + standard errors into the table
coef.vec_se <- c(outgroup_se_coef,
                 outgroup_se_pc_coef,
                 ft_se_coef,
                 sd_se_coef,
                 cprn_se_coef,
                 prcp_se_coef,
                 otgr_se_coef)

se.vec_se <- c(outgroup_se_se,
               outgroup_se_pc_se,
               ft_se_se,
               sd_se_se,
               cprn_se_se,
               prcp_se_se,
               otgr_se_se)
merged_se <- data.frame(names, coef.vec_se, se.vec_se)
merged_se 
coef.vec_se1 <- c(outgroup_se_coef1,
                  outgroup_se_pc_coef1,
                  ft_se_coef1,
                  sd_se_coef1,
                  cprn_se_coef1,
                  prcp_se_coef1,
                  otgr_se_coef1)
se.vec_se1 <- c(outgroup_se_se1,
                outgroup_se_pc_se1,
                ft_se_se1,
                sd_se_se1,
                cprn_se_se1,
                prcp_se_se1,
                otgr_se_se1)
merged_se1 <- data.frame(names, coef.vec_se1, se.vec_se1)

coef.vec_se2 <- c(outgroup_se_coef2,
                  outgroup_se_pc_coef2,
                  ft_se_coef2,
                  sd_se_coef2,
                  cprn_se_coef2,
                  prcp_se_coef2,
                  otgr_se_coef2)
se.vec_se2 <- c(outgroup_se_se2,
                outgroup_se_pc_se2,
                ft_se_se2,
                sd_se_se2,
                cprn_se_se2,
                prcp_se_se2,
                otgr_se_se2)
merged_se2 <- data.frame(names, coef.vec_se2, se.vec_se2)
# -------- *Table S10* ------------
join_all(list(merged_se, merged_se1, merged_se2), by="names")
# -------- *Table S10* ------------

# -------------------------------
#        MAJORITY GROUP SHARE
# -------------------------------
# -------- Effect on the three main families of outcomes, within homogenous communities within 3 indices of diversity

# Outgroup Attitudes
m <-lm((outgroup_index)~(treatment), data=combined[combined$ms_median_index==1,])
outgroup_ms_coef<-coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_index)
outgroup_ms_se<-coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_ms<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
outgroup_ms_coef1<-outgroup_ms$coefficients[2] /sd(combined[combined$ms_median_index == 1 &combined$treatment=="0",]$outgroup_index)
outgroup_ms_se1<-outgroup_ms$std.error[2]/ sd(combined[combined$ms_median_index == 1&combined$treatment=="0",]$outgroup_index)

m<-lm(outgroup_index~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$ms_median_index==1,])
outgroup_ms_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_index)
outgroup_ms_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_index)

# Outgroup Attitudes (PC index)
m <-lm((outgroup_princomp)~(treatment), data=combined[combined$ms_median_index==1,])
outgroup_ms_pc_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_princomp)
outgroup_ms_pc_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_princomp)

covIn <- as.formula("~freq_usage")
outgroup_pc_ms<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
outgroup_ms_pc_coef1<-outgroup_pc_ms$coefficients[2] /sd(combined[combined$ms_median_index == 1&combined$treatment=="0",]$outgroup_princomp)
outgroup_ms_pc_se1<-outgroup_pc_ms$std.error[2]/ sd(combined[combined$ms_median_index == 1&combined$treatment=="0",]$outgroup_princomp)

m<-lm(outgroup_princomp~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$ms_median_index==1,])
outgroup_ms_pc_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_princomp)
outgroup_ms_pc_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_princomp)

# ---------  Majority Group Share: Out-group attitudes indicator-by-indicator analysis within homogenous communities

# ---- Feeling Thermometer
# model 1: baseline
ft_model1 <- lm(combined[combined$ms_median_index==1,]$ft~combined[combined$ms_median_index==1,]$treatment) 
ft_ms_coef<-coeftest(ft_model1 , vcov = vcovHC(ft_model1, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$ft)
ft_ms_se<-coeftest(ft_model1 , vcov = vcovHC(ft_model1, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$ft)

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
ft_model1_ms<-lm_lin(ft~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
ft_ms_coef1<-ft_model1_ms$coefficients[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$ft)
ft_ms_se1<-ft_model1_ms$std.error[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$ft)

# model 3: full covariates
m<-lm(ft~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$ms_median_index==1,])
ft_ms_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$ft)
ft_ms_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$ft)


# ---- SD
# model 1: baseline
sd_model1 <- lm(combined[combined$ms_median_index==1,]$sd~combined[combined$ms_median_index==1,]$treatment) 
sd_ms_coef<-coeftest(sd_model1 , vcov = vcovHC(sd_model1, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$sd)
sd_ms_se<-coeftest(sd_model1 , vcov = vcovHC(sd_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$ms_median_index==1 & combined$treatment==0,]$sd))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
sd_model1_ms<-lm_lin(sd~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
sd_ms_coef1<-sd_model1_ms$coefficients[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$sd)
sd_ms_se1<-sd_model1_ms$std.error[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$sd)

# model 3: full covariates
m<-lm(sd~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$ms_median_index==1,])
sd_ms_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$sd)
sd_ms_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$sd)


# ---- Cooperation
# model 1: baseline
multi_stat_model1 <- lm(combined[combined$ms_median_index==1,]$multi_stat~combined[combined$ms_median_index==1,]$treatment) 
cprn_ms_coef<-coeftest(multi_stat_model1 , vcov = vcovHC(multi_stat_model1, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$multi_stat)
cprn_ms_se<-coeftest(multi_stat_model1 , vcov = vcovHC(multi_stat_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$ms_median_index==1 & combined$treatment==0,]$multi_stat))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
cp_model1_ms<-lm_lin(multi_stat~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
cprn_ms_coef1<-cp_model1_ms$coefficients[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$multi_stat)
cprn_ms_se1<-cp_model1_ms$std.error[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$multi_stat)

# model 3: full covariates
m<-lm(multi_stat~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$ms_median_index==1,])
cprn_ms_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$multi_stat)
cprn_ms_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$multi_stat)

# ---- Perception of Out-Group Evalautions
# model 1: baseline
inchrct_combined_model1 <- lm(combined[combined$ms_median_index==1,]$inchrct_combined~combined[combined$ms_median_index==1,]$treatment) 
inchrct_ms_coef<-coeftest(inchrct_combined_model1 , vcov = vcovHC(inchrct_combined_model1, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$inchrct_combined)
inchrct_ms_se<-coeftest(inchrct_combined_model1 , vcov = vcovHC(inchrct_combined_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$ms_median_index==1 & combined$treatment==0,]$inchrct_combined))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
inchrct_model1_ms<-lm_lin(inchrct_combined~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
inchrct_ms_coef1<-inchrct_model1_ms$coefficients[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$inchrct_combined)
inchrct_ms_se1<-inchrct_model1_ms$std.error[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$inchrct_combined)

# model 3: full covariates
m<-lm(inchrct_combined~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$ms_median_index==1,])
inchrct_ms_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$inchrct_combined)
inchrct_ms_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$inchrct_combined)

# ---- Outgroup traits
# model 1: baseline
othchrct_combined_model1 <- lm(combined[combined$ms_median_index==1,]$othchrct_combined~combined[combined$ms_median_index==1,]$treatment) 
prcp_ms_coef<-coeftest(othchrct_combined_model1 , vcov = vcovHC(othchrct_combined_model1, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==0 & combined$treatment==0,]$othchrct_combined)
prcp_ms_se<-coeftest(othchrct_combined_model1 , vcov = vcovHC(othchrct_combined_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$ms_median_index==0 & combined$treatment==0,]$othchrct_combined))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
traits_model1_ms<-lm_lin(othchrct_combined~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
prcp_ms_coef1<-traits_model1_ms$coefficients[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$othchrct_combined)
prcp_ms_se1<-traits_model1_ms$std.error[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$othchrct_combined)

# model 3: full covariates
m<-lm(othchrct_combined~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$ms_median_index==1,])
prcp_ms_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$othchrct_combined)
prcp_ms_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$othchrct_combined)

# Putting estimates + standard errors into the table
coef.vec_ms <- c(outgroup_ms_coef,
                 outgroup_ms_pc_coef,
                 ft_ms_coef,
                 sd_ms_coef,
                 cprn_ms_coef,
                 inchrct_ms_coef,
                 prcp_ms_coef)
se.vec_ms <- c(outgroup_ms_se,
               outgroup_ms_pc_se,
               ft_ms_se,
               sd_ms_se,
               cprn_ms_se,
               inchrct_ms_se,
               prcp_ms_se)
merged_ms <- data.frame(names, coef.vec_ms, se.vec_ms)
merged_ms

coef.vec_ms1 <- c(outgroup_ms_coef1,
                  outgroup_ms_pc_coef1,
                  ft_ms_coef1,
                  sd_ms_coef1,
                  cprn_ms_coef1,
                  inchrct_ms_coef1,
                  prcp_ms_coef1)
se.vec_ms1 <- c(outgroup_ms_se1,
                outgroup_ms_pc_se1,
                ft_ms_se1,
                sd_ms_se1,
                cprn_ms_se1,
                inchrct_ms_se1,
                prcp_ms_se1)
merged_ms1 <- data.frame(names, coef.vec_ms1, se.vec_ms1)
merged_ms1

coef.vec_ms2 <- c(outgroup_ms_coef2,
                  outgroup_ms_pc_coef2,
                  ft_ms_coef2,
                  sd_ms_coef2,
                  cprn_ms_coef2,
                  inchrct_ms_coef2,
                  prcp_ms_coef2)
se.vec_ms2 <- c(outgroup_ms_se2,
                outgroup_ms_pc_se2,
                ft_ms_se2,
                sd_ms_se2,
                cprn_ms_se2,
                inchrct_ms_se2,
                prcp_ms_se2)
names <- c("out-group regard (sum of z-scores)",'out-group regard (PC)',"feeling thermometer","social closeness",
           "cooperation","perception of out-group evaluations","out-group traits")
merged_ms2 <- data.frame(names, coef.vec_ms2, se.vec_ms2)
merged_ms2 
# -------- *Table S11* ------------
join_all(list(merged_ms, merged_ms1, merged_ms2), by="names")
# -------- *Table S11s* ------------

# ------------------------------------------------
# Table S12: FDR ADJUSTING WITHIN HOMOGENOUS AREAS
# -------------------------------------------------
# Table S12: Ethnic fractionalization
p.values_ef <-c(
  summary(ft_model1_ef)$coefficients[14],
  summary(sd_model1_ef)$coefficients[14],
  summary(cp_model1_ef)$coefficients[14],
  summary(prcp_model1_ef)$coefficients[14],
  summary(traits_model1_ef)$coefficients[14],
  summary(outgroup_ef)$coefficients[14],
  summary(outgroup_pc_ef)$coefficients[14])
p.values_ef<-sort(p.values_ef)
adjusted_p_ef<-as.table(p.adjust(p.values_ef,"BH"))
rownames(adjusted_p)<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                        "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
adjusted_p

# Table S13: Shannon Entropy - update
p.values_se <-as.table(c(
  summary(ft_model1_se)$coefficients[14],
  summary(sd_model1_se)$coefficients[14],
  summary(cp_model1_se)$coefficients[14],
  summary(prcp_model1_se)$coefficients[14],
  summary(traits_model1_se)$coefficients[14],
  summary(outgroup_se)$coefficients[14],
  summary(outgroup_pc_se)$coefficients[14]
))
rownames(p.values_se)<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                         "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
p.values_se<-sort(p.values_se)
adjusted.p_se<-as.table(p.adjust(p.values_se,"BH"))
rownames(adjusted.p_se)<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                           "out-group traits","out-group regard (z-scores)","out-group regard (pc)")

# Table S14: Majority Group Share
p.values_ms<-c(
  summary(ft_model1_ms)$coefficients[14],
  summary(sd_model1_ms)$coefficients[14],
  summary(cp_model1_ms)$coefficients[14],
  summary(prcp_model1_ms)$coefficients[14],
  summary(traits_model1_ms)$coefficients[14],
  summary(outgroup_ms)$coefficients[14],
  summary(outgroup_pc_ms)$coefficients[14])
p.values_ms<-sort(p.values_ms)
adjusted.p_ms<-as.table(p.adjust(p.values_ms,"BH"))
rownames(adjusted.p_ms)<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                           "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
adjusted_p

# ------------------------------------------------------
#    INTERACTION w/ CONTINOUS MEASURE: TABLE S17 & S18
# -----------------------------------------------------

# Ethnic Fractionalization
ef_interaction <- lm(outgroup_princomp ~ treatment +fractionalization_offline_index+ treatment:fractionalization_offline_index,
                                      data=combined)
ef_interaction_coef<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_se<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+fractionalization_offline_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_interaction_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_se1<- m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+fractionalization_offline_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_interaction_coef2<-m$coefficients[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_se2<-m$std.error[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Shannon Entropy
se_interaction <- lm(outgroup_princomp ~ treatment+ shannon_offline + treatment:shannon_offline,
                     data=combined)
se_interaction_coef<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_se<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))
# summary(se_interaction)

covIn <- as.formula("~freq_usage+shannon_offline")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_interaction_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+shannon_offline")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_interaction_coef2<-m$coefficients[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_se2<-m$std.error[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Largest Majority
ms_interaction <- lm(outgroup_princomp ~ treatment+ largest_majority + treatment:largest_majority,
                     data=combined)
ms_interaction_coef<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_se<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+largest_majority")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_interaction_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+largest_majority")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_interaction_coef2<-m$coefficients[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_se2<-m$std.error[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)


coef.vec_intr <- c(ef_interaction_coef,
                   ms_interaction_coef,
                   se_interaction_coef)
se.vec_intr <- c(ef_interaction_se,
                 ms_interaction_se,
                 se_interaction_se)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr <- data.frame(names, coef.vec_intr,se.vec_intr)

coef.vec_intr1 <- c(ef_interaction_coef1,
                    ms_interaction_coef1,
                    se_interaction_coef1)
se.vec_intr1 <- c(ef_interaction_se1,
                  ms_interaction_se1,
                  se_interaction_se1)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr1 <- data.frame(names, coef.vec_intr1,se.vec_intr1)

coef.vec_intr2 <- c(ef_interaction_coef2,
                    ms_interaction_coef2,
                    se_interaction_coef2)
se.vec_intr2 <- c(ef_interaction_se2,
                  ms_interaction_se2,
                  se_interaction_se2)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr2 <- data.frame(names, coef.vec_intr2,se.vec_intr2)

# -------- *Table S17* ------------
join_all(list(merged_intr, merged_intr1, merged_intr2), by="names")
# -------- *Table S17* ------------


# --------------------
# ABOVE/BELOW MEDIAN
# --------------------

combined$ef_median_index_rev<-ifelse(combined$fractionalization_offline_index== median(na.omit(combined$fractionalization_offline_index))|combined$fractionalization_offline_index>median(na.omit(combined$fractionalization_offline_index)),0,1) 

# 1 is more homogenous within the rev index
ef_interaction_rev <- lm(outgroup_princomp ~ treatment +ef_median_index_rev+ treatment:ef_median_index_rev,
                     data=combined)
ef_median_intr_coef<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[2,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_se<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[2,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+ef_median_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_median_intr_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+ef_median_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_median_intr_coef2<-m$coefficients[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_se2<-m$std.error[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)


# shannon entropy
combined$se_median_index_rev<-ifelse(combined$shannon_offline >   median(na.omit(combined$shannon_offline)) |  combined$shannon_offline == median(na.omit(combined$shannon_offline)), 0, 1)

se_interaction <- lm(outgroup_princomp ~ treatment+ se_median_index_rev + treatment:se_median_index_rev,
                     data=combined)
se_median_intr_coef<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_se<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+se_median_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_median_intr_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_coef1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+se_median_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_median_intr_coef2<-m$coefficients[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_coef2<-m$std.error[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# majority group share
combined$ms_median_index_rev <- ifelse(combined$largest_majority > median(combined$largest_majority) | combined$largest_majority==median(combined$largest_majority), 1, 0) 

ms_interaction <- lm(outgroup_princomp ~ treatment+ ms_median_index_rev  + treatment:ms_median_index_rev,
                     data=combined)
ms_median_intr_coef<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_se<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+ms_median_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_median_intr_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+ms_median_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_median_intr_coef2<-m$coefficients[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_coef2<-m$std.error[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)

coef.vec_intr <- c(ef_median_intr_coef,
                   ms_median_intr_coef,
                   se_median_intr_coef)
se.vec_intr <- c(ef_median_intr_se,
                 ms_median_intr_se,
                 se_median_intr_se)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr <- data.frame(names, coef.vec_intr,se.vec_intr)

coef.vec_intr1 <- c(ef_median_intr_coef1,
                    ms_median_intr_coef1,
                    se_median_intr_coef1)
se.vec_intr1 <- c(ef_median_intr_se1,
                  ms_median_intr_se1,
                  se_median_intr_se1)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr1 <- data.frame(names, coef.vec_intr1,se.vec_intr1)

coef.vec_intr2 <- c(ef_median_intr_coef2,
                    ms_median_intr_coef2,
                    se_median_intr_coef2)
se.vec_intr2 <- c(ef_median_intr_se2,
                  ms_median_intr_se2,
                  se_median_intr_se2)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr2 <- data.frame(names, coef.vec_intr2,se.vec_intr2)


# -------- *Table S18* ------------
join_all(list(merged_intr, merged_intr1, merged_intr2), by="names")
# -------- *Table S18* ------------

