# --------------------------------------
# Offline networks: TABLE S14-S18
# ---------------------------------------

# -------  Ethnic Fractionalization Index

# Calculated using the formula below (and on p. 15 of SI); already incorporated into the dataset
# for (i in 1:length(combined$X)){
 # combined$fractionalization_offline_index[i] <- 1 - (combined$bosniak[i]^2+combined$serb[i]^2+combined$croat[i]^2)}
combined$ef_median_index<-ifelse(combined$fractionalization_offline_index== median(combined$fractionalization_offline_index)|combined$fractionalization_offline_index>median(combined$fractionalization_offline_index),1,0) # smaller, more homogenous; 1 is more het

# ------- City Homogeneity: Majority Share
combined$ms_median_index <- ifelse(combined$largest_majority > median(combined$largest_majority) | combined$largest_majority==median(combined$largest_majority), 1, 0) 

# -------  Shannon Entropy
# Calculated using the formula below; already incorporated in the dataset; 
# where one group is 0 (for three cities; log undefined), value calculated manually
# for (i in 1:length(combined$X)){
  # combined_old$shannon_offline[i] <- -(combined$bosniak[i]*log(combined$bosniak[i])+combined$croat[i]*log(combined$croat[i])+combined$serb[i]*log(combined$serb[i]))}
combined$se_median_index<-ifelse(combined$shannon_offline >   median(na.omit(combined$shannon_offline)) |  combined$shannon_offline == median(na.omit(combined$shannon_offline)), 1, 0)

# ------ Effects on the three main families of outcomes, within homogenous communities within 3 indices of diversity

# ----------------------------------------------------
#     Models for Table S9 - Ethnic Fractionalization
# ----------------------------------------------------

# Outgroup Attitudes - homogenous [first with outgroup index formed as a sum of z-scores;
# second regression with outgroup index formed as a PC score]

# no covariate adjustment, Model 1
m <-lm(outgroup_index~(treatment), data=combined[combined$ef_median_index==0,])
outgroup_ef_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$outgroup_index)
outgroup_ef_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==0 & combined$treatment=="0",]$outgroup_index)

m_pc <-lm((outgroup_princomp)~(treatment), data=combined[combined$ef_median_index==0,])
summary(m_pc)
outgroup_ef_pc_coef<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index=="0" & combined$treatment=="0",]$outgroup_princomp)
outgroup_ef_pc_se<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index=="0" & combined$treatment=="0",]$outgroup_princomp)

# controlling for frequency usage, Model 2
covIn <- as.formula("~freq_usage")
outgroup_ef<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
outgroup_ef_coef1<-outgroup_ef$coefficients[2] /sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_index)
outgroup_ef_se1<-outgroup_ef$std.error[2]/ sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_pc_ef<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
outgroup_ef_pc_coef1<-outgroup_pc_ef$coefficients[2] /sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)
outgroup_ef_pc_se1<-outgroup_pc_ef$std.error[2]/ sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)

# full covariate adjustment, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
m<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
outgroup_ef_coef2<-m$coefficients[2] /sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$outgroup_index)
outgroup_ef_se2<-m$std.error[2]/ sd(combined[combined$ef_median_index==0 &combined$treatment==0,]$outgroup_index)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
m_pc<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
outgroup_ef_pc_coef2<-m_pc$coefficients[2] /sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)
outgroup_ef_pc_se2<-m_pc$std.error[2]/ sd(combined[combined$ef_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)

# ------------- heteorogeneous
# offline heterog = 1

# # het - no covariate adjustment, Model 1
m <-lm((outgroup_princomp)~(treatment), data=combined[combined$ef_median_index==1,])
summary(m)
coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==1 & combined$treatment=="0",]$outgroup_index)
coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==1 & combined$treatment=="0",]$outgroup_index)

m_pc_htr <-lm((outgroup_princomp)~(treatment), data=combined[combined$ef_median_index==1,])
summary(m_pc_htr)
coeftest(m_pc_htr, vcov = vcovHC(m_pc_htr , type = "HC1"))[2,1]/sd(combined[combined$ef_median_index==1 & combined$treatment=="0",]$outgroup_princomp)
coeftest(m_pc_htr, vcov = vcovHC(m_pc_htr , type = "HC1"))[2,2]/sd(combined[combined$ef_median_index==1 & combined$treatment=="0",]$outgroup_princomp)

# het - controlling for frequency usage, Model 2
covIn <- as.formula("~freq_usage")
outgroup_ef_htr<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ef_median_index==1,])
summary(outgroup_ef_htr)
outgroup_ef_htr$coefficients[2] /sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_index)
outgroup_ef_htr$std.error[2]/ sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_pc_ef_htr<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$ef_median_index==1,])
summary(outgroup_pc_ef_htr)
outgroup_pc_ef_htr$coef_coefficients[2] /sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_princomp)
outgroup_pc_ef_htr$std.error[2]/ sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_princomp)

# het - full covariate adjustment, Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
m_htr<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$ef_median_index==1,])
m_htr$coefficients[2] /sd(combined[combined$ef_median_index==1 & combined$treatment==0,]$outgroup_index)
m_htr$std.error[2]/ sd(combined[combined$ef_median_index==1 &combined$treatment==0,]$outgroup_index)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
m_pc_htr<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ef_median_index==1,])
outgroup_ef_pc_htr_coef2<-m_pc_htr$coefficients[2] /sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_princomp)
outgroup_ef_pc_htr_se2<-m_pc_htr$std.error[2]/ sd(combined[combined$ef_median_index==1&combined$treatment=="0",]$outgroup_princomp)


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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
ft_model2_ef<-lm_lin(ft~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
ft_ef_coef2<-ft_model2_ef$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$ft)
ft_ef_se2<-ft_model2_ef$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$ft)

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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
sd_model2_ef<-lm_lin(sd~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
sd_ef_coef2<-sd_model2_ef$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$sd)
sd_ef_se2<-sd_model2_ef$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$sd)

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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
cp_model2_ef<-lm_lin(multi_stat~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
summary(cp_model2_ef)
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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
prcp_model2_ef<-lm_lin(inchrct_combined~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
prcp_ef_coef2<-prcp_model2_ef$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$inchrct_combined)
prcp_ef_se2<-prcp_model2_ef$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$inchrct_combined)


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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
otgrp_model2_ef<-lm_lin(othchrct_combined~treatment, covariates=covIn, data=combined[combined$ef_median_index==0,])
otgr_ef_coef2<-otgrp_model2_ef$coefficients[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$othchrct_combined)
otgr_ef_se2<-otgrp_model2_ef$std.error[2]/sd(combined[combined$ef_median_index==0 & combined$treatment==0,]$othchrct_combined)

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
# Final version within supplementary material edited directly within Overleaf 
# -------- *Table S9* ------------


# -------------------------------------------
#  Models for Table 10 - SHANNON ENTROPY
# ------------------------------------------
# ---------  Shannon Entropy: Main outcome (out-group regard index)  analysis within homogenous communities

m <-lm((outgroup_index)~(treatment), data=combined[combined$se_median_index==0,])
outgroup_se_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$outgroup_index)
outgroup_se_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==0 & combined$treatment=="0",]$outgroup_index)

m <-lm((outgroup_princomp)~(treatment), data=combined[combined$se_median_index==0,])
outgroup_se_pc_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index=="0" & combined$treatment=="0",]$outgroup_princomp)
outgroup_se_pc_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index=="0" & combined$treatment=="0",]$outgroup_princomp)

covIn <- as.formula("~freq_usage")
outgroup_se1<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
outgroup_se_coef1<-outgroup_se1$coefficients[2] /sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_index)
outgroup_se_se1<-outgroup_se1$std.error[2]/ sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_pc_se1<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
outgroup_se_pc_coef1<-outgroup_pc_se1$coefficients[2] /sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)
outgroup_se_pc_se1<-outgroup_pc_se1$std.error[2]/ sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)


covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
outgroup_se2<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
summary(outgroup_se)
outgroup_se_coef2<-outgroup_se2$coefficients[2] /sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_index)
outgroup_se_se2<-outgroup_se2$std.error[2]/ sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
outgroup_pc_se2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
summary(outgroup_pc_se)
outgroup_se_pc_coef2<-outgroup_pc_se2$coefficients[2] /sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)
outgroup_se_pc_se2<-outgroup_pc_se2$std.error[2]/ sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)


# ---------  Shannon Entropy: Main outcome (out-group regard index)  analysis within heterogenous communities
m <-lm((outgroup_index)~(treatment), data=combined[combined$se_median_index==1,])
outgroup_se_htr_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==1 & combined$treatment=="0",]$outgroup_index)
outgroup_se_htr_se_htr<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index==1 & combined$treatment=="0",]$outgroup_index)

m <-lm((outgroup_princomp)~(treatment), data=combined[combined$se_median_index==1,])
outgroup_se_pc_htr_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$se_median_index=="0" & combined$treatment=="0",]$outgroup_princomp)
outgroup_se_pc_htr_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$se_median_index=="0" & combined$treatment=="0",]$outgroup_princomp)

covIn <- as.formula("~freq_usage")
outgroup_se_htr1<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$se_median_index==1,])
outgroup_se_htr_coef1<-outgroup_se_htr1$coefficients[2] /sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_index)
outgroup_se_htr_se1<-outgroup_se_htr1$std.error[2]/ sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_pc_se_htr1<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$se_median_index==1,])
outgroup_se_pc_htr_coef1<-outgroup_pc_se_htr1$coefficients[2] /sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)
outgroup_se_pc_htr_se1<-outgroup_pc_se_htr1$std.error[2]/ sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
outgroup_se_htr2<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$se_median_index==1,])
outgroup_se_htr_coef2<-outgroup_se_htr2$coefficients[2] /sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_index)
outgroup_se_htr_se2<-outgroup_se_htr2$std.error[2]/ sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
outgroup_pc_se_htr2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$se_median_index==1,])
outgroup_se_pc_htr_coef2<-outgroup_pc_se_htr2$coefficients[2] /sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)
outgroup_se_pc_htr_se2<-outgroup_pc_se_htr2$std.error[2]/ sd(combined[combined$se_median_index=="0"&combined$treatment=="0",]$outgroup_princomp)


# ---------  Shannon Entropy: Out-group attitudes (indicator-by-indicator) analysis within homogenous communities

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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
ft_model2_se<-lm_lin(ft~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
ft_se_coef2<-ft_model2_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$ft)
ft_se_se2<-ft_model2_se$std.error[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$ft)

# ----- SD
# model 1: baseline
sd_model1 <- lm(combined[combined$se_median_index==0,]$sd~combined[combined$se_median_index==0,]$treatment) 
sd_se_coef<-coeftest(sd_model1 , vcov = vcovHC(sd_model1, type = "HC1"))[2,1]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$sd)
sd_se_se<-coeftest(sd_model1 , vcov = vcovHC(sd_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$se_median_index==0 & combined$treatment==0,]$sd))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
sd_model1_se<-lm_lin(sd~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
summary(sd_model1_se)
sd_se_coef1<-sd_model1_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$sd)
sd_se_se1<-sd_model1_se$std.error[2]/sd(na.omit(combined[combined$se_median_index==0 & combined$treatment==0,]$sd))

# model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
sd_model2_se<-lm_lin(sd~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
summary(sd_model2_se)
sd_se_coef2<-sd_model2_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$sd)
sd_se_se2<-sd_model2_se$std.error[2]/sd(na.omit(combined[combined$se_median_index==0 & combined$treatment==0,]$sd))



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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
cp_model2_se<-lm_lin(multi_stat~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
cprn_se_coef2<-cp_model2_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$multi_stat)
cprn_se_se2<-cp_model2_se$std.error[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$multi_stat)

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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
prcp_model2_se<-lm_lin(inchrct_combined~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
prcp_se_coef2<-prcp_model2_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$inchrct_combined)
prcp_se_se2<-prcp_model2_se$std.error[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$inchrct_combined)

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
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
traits_model2_se<-lm_lin(othchrct_combined~treatment, covariates=covIn, data=combined[combined$se_median_index==0,])
otgr_se_coef2<-traits_model2_se$coefficients[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$othchrct_combined)
otgr_se_se2<-traits_model2_se$std.error[2]/sd(combined[combined$se_median_index==0 & combined$treatment==0,]$othchrct_combined)


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
# Edited directly within Overleaf
# -------- *Table S10* ------------

# -------------------------------------------------------
#      Models for Table 11 -  MAJORITY GROUP SHARE
# -------------------------------------------------------
# ---------  Majority Group Share: Main outcome (out-group regard index)  analysis within homogenous communities
# Outgroup Attitudes
outgroup_model_ms<-lm((outgroup_index)~(treatment), data=combined[combined$ms_median_index==1,])
outgroup_ms_coef<-coeftest(outgroup_model_ms , vcov = vcovHC(outgroup_model_ms, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_index)
outgroup_ms_se<-coeftest(outgroup_model_ms, vcov = vcovHC(outgroup_model_ms, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_model1_ms<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
outgroup_ms_coef1<-outgroup_model1_ms$coefficients[2] /sd(combined[combined$ms_median_index == 1 &combined$treatment=="0",]$outgroup_index)
outgroup_ms_se1<-outgroup_model1_ms$std.error[2]/ sd(combined[combined$ms_median_index == 1&combined$treatment=="0",]$outgroup_index)

outgroup_model2_ms <-lm(outgroup_index~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined[combined$ms_median_index==1,])
outgroup_ms_coef2<-coeftest(outgroup_model2_ms, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_index)
outgroup_ms_se2<-coeftest (outgroup_model2_ms, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_index)

# Outgroup Attitudes (PC index)
outgroup_pc_model_ms <-lm((outgroup_princomp)~(treatment), data=combined[combined$ms_median_index==1,])
outgroup_ms_pc_coef<-coeftest(outgroup_pc_model_ms, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_princomp)
outgroup_ms_pc_se<-coeftest(outgroup_pc_model_ms, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_princomp)

covIn <- as.formula("~freq_usage")
outgroup_pc_model1_ms<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
outgroup_ms_pc_coef1<-outgroup_pc_model1_ms$coefficients[2] /sd(combined[combined$ms_median_index == 1&combined$treatment=="0",]$outgroup_princomp)
outgroup_ms_pc_se1<-outgroup_pc_model1_ms$std.error[2]/ sd(combined[combined$ms_median_index == 1&combined$treatment=="0",]$outgroup_princomp)

outgroup_pc_model2_ms<-lm(outgroup_princomp~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined[combined$ms_median_index==1,])
outgroup_ms_pc_coef2<-coeftest(outgroup_pc_model2_ms, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_princomp)
outgroup_ms_pc_se2<-coeftest (outgroup_pc_model2_ms, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$outgroup_princomp)

# ---------  Majority Group Share: Main outcome (out-group regard index)  analysis within heterogenous communities
# Outgroup Attitudes
m <-lm((outgroup_index)~(treatment), data=combined[combined$ms_median_index==0,])
outgroup_ms_coef<-coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==0 & combined$treatment=="0",]$outgroup_index)
outgroup_ms_se<-coeftest(m , vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==0 & combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_ms<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$ms_median_index==0,])
outgroup_ms_coef1<-outgroup_ms$coefficients[2] /sd(combined[combined$ms_median_index==0 &combined$treatment=="0",]$outgroup_index)
outgroup_ms_se1<-outgroup_ms$std.error[2]/ sd(combined[combined$ms_median_index==0&combined$treatment=="0",]$outgroup_index)

m<-lm(outgroup_index~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn, data=combined[combined$ms_median_index==0,])
outgroup_ms_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==0 & combined$treatment=="0",]$outgroup_index)
outgroup_ms_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==0 & combined$treatment=="0",]$outgroup_index)

# Outgroup Attitudes (PC index)
m <-lm((outgroup_princomp)~(treatment), data=combined[combined$ms_median_index==0,])
outgroup_ms_pc_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==0 & combined$treatment=="0",]$outgroup_princomp)
outgroup_ms_pc_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==0 & combined$treatment=="0",]$outgroup_princomp)

covIn <- as.formula("~freq_usage")
outgroup_pc_ms<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$ms_median_index==0,])
outgroup_ms_pc_coef1<-outgroup_pc_ms$coefficients[2] /sd(combined[combined$ms_median_index==0&combined$treatment=="0",]$outgroup_princomp)
outgroup_ms_pc_se1<-outgroup_pc_ms$std.error[2]/ sd(combined[combined$ms_median_index==0&combined$treatment=="0",]$outgroup_princomp)

m<-lm(outgroup_princomp~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined[combined$ms_median_index==0,])
outgroup_ms_htr_pc_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==0 & combined$treatment=="0",]$outgroup_princomp)
outgroup_ms_htr_pc_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==0 & combined$treatment=="0",]$outgroup_princomp)

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
ft_model2_ms<-lm(ft~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined[combined$ms_median_index==1,])
ft_ms_coef2<-coeftest(ft_model2_ms, vcov = vcovHC(ft_model2_ms, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$ft)
ft_ms_se2<-coeftest (ft_model2_ms, vcov = vcovHC(ft_model2_ms, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$ft)


# ---- SD
# model 1: baseline
sd_model_ms <- lm(combined[combined$ms_median_index==1,]$sd~combined[combined$ms_median_index==1,]$treatment) 
sd_ms_coef<-coeftest(sd_model_ms , vcov = vcovHC(sd_model_ms, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$sd)
sd_ms_se<-coeftest(sd_model_ms , vcov = vcovHC(sd_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$ms_median_index==1 & combined$treatment==0,]$sd))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
sd_model1_ms<-lm_lin(sd~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
sd_ms_coef1<-sd_model1_ms$coefficients[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$sd)
sd_ms_se1<-sd_model1_ms$std.error[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$sd)

# model 3: full covariates
sd_model2_ms<-lm(sd~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined[combined$ms_median_index==1,])
sd_ms_coef2<-coeftest(sd_model2_ms, vcov = vcovHC(sd_model2_ms, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$sd)
sd_ms_se2<-coeftest (sd_model2_ms, vcov = vcovHC(sd_model2_ms, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$sd)


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
cp_model2_ms<-lm(multi_stat~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined[combined$ms_median_index==1,])
cprn_ms_coef2<-coeftest(cp_model2_ms, vcov = vcovHC(cp_model2_ms, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$multi_stat)
cprn_ms_se2<-coeftest (cp_model2_ms, vcov = vcovHC(cp_model2_ms, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$multi_stat)

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
inchrct_model2_ms<-lm(inchrct_combined~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined[combined$ms_median_index==1,])
inchrct_ms_coef2<-coeftest(inchrct_model2_ms, vcov = vcovHC(inchrct_model2_ms, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$inchrct_combined)
inchrct_ms_se2<-coeftest (inchrct_model2_ms, vcov = vcovHC(inchrct_model2_ms, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$inchrct_combined)

# ---- Outgroup traits
# model 1: baseline
othchrct_combined_model1 <- lm(combined[combined$ms_median_index==1,]$othchrct_combined~combined[combined$ms_median_index==1,]$treatment) 
prcp_ms_coef<-coeftest(othchrct_combined_model1 , vcov = vcovHC(othchrct_combined_model1, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==0 & combined$treatment==0,]$othchrct_combined)
prcp_ms_se<-coeftest(othchrct_combined_model1 , vcov = vcovHC(othchrct_combined_model1, type = "HC1"))[2,2]/sd(na.omit(combined[combined$ms_median_index==0 & combined$treatment==0,]$othchrct_combined))

# model 2: includes freq_usage
covIn <- as.formula("~freq_usage")
prcp_model1_ms<-lm_lin(othchrct_combined~treatment, covariates=covIn, data=combined[combined$ms_median_index==1,])
prcp_ms_coef1<-traits_model1_ms$coefficients[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$othchrct_combined)
prcp_ms_se1<-traits_model1_ms$std.error[2]/sd(combined[combined$ms_median_index==1 & combined$treatment==0,]$othchrct_combined)

# model 3: full covariates
prcp_model2_ms<-lm(othchrct_combined~treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined[combined$ms_median_index==1,])
prcp_ms_coef2<-coeftest(prcp_model2_ms, vcov = vcovHC(prcp_model2_ms, type = "HC1"))[2,1]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$othchrct_combined)
prcp_ms_se2<-coeftest (prcp_model2_ms, vcov = vcovHC(prcp_model2_ms, type = "HC1"))[2,2]/sd(combined[combined$ms_median_index==1 & combined$treatment=="0",]$othchrct_combined)

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

# -------------------------------------------------------------
# Table S12, S13 and S14: FDR ADJUSTING WITHIN HOMOGENOUS AREAS
# --------------------------------------------------------------

# Table S12: Ethnic fractionalization
p.values_ef <-as.table(c(
  summary(ft_model1_ef)$coefficients[14],
  summary(sd_model1_ef)$coefficients[14],
  summary(cp_model1_ef)$coefficients[14],
  summary(prcp_model1_ef)$coefficients[14],
  summary(traits_model1_ef)$coefficients[14],
  summary(outgroup_ef)$coefficients[14],
  summary(outgroup_pc_ef)$coefficients[14]))
rownames(p.values_ef)<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                           "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
p.values_ef<-sort(p.values_ef)
adjusted_p_ef<-as.table(p.adjust(p.values_ef,"BH"))
adjusted_p_ef

# Table S13: Shannon Entropy - update
p.values_se <-as.table(c(
  summary(ft_model1_se)$coefficients[14],
  summary(sd_model1_se)$coefficients[14],
  summary(cp_model1_se)$coefficients[14],
  summary(prcp_model1_se)$coefficients[14],
  summary(traits_model1_se)$coefficients[14],
  summary(outgroup_se1)$coefficients[14],
  summary(outgroup_pc_se1)$coefficients[14]
))
rownames(p.values_se)<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                         "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
p.values_se
p.values_se<-sort(p.values_se)
adjusted.p_se<-as.table(p.adjust(p.values_se,"BH"))
adjusted.p_se


# Table S14: Majority Group Share
p.values_ms<-as.table(c(
  summary(ft_model1_ms)$coefficients[14],
  summary(sd_model1_ms)$coefficients[14],
  summary(cp_model1_ms)$coefficients[14],
  summary(inchrct_model1_ms)$coefficients[14],
  summary(prcp_model1_ms)$coefficients[14],
  summary(outgroup_model1_ms)$coefficients[14],
  summary(outgroup_pc_model1_ms)$coefficients[14]))
rownames(p.values_ms)<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                         "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
p.values_ms
p.values_ms<-sort(p.values_ms)
adjusted.p_ms<-as.table(p.adjust(p.values_ms,"BH"))
adjusted.p_ms
# For table S12-14, coefficients and standard errors extracted from Tables 9-11

# ------------------------------------------------------
#    INTERACTION w/ CONTINOUS MEASURE: TABLE S15 & S16
# -----------------------------------------------------
# ------ Ethnic Fractionalization
# Model 1, no covariates
ef_interaction <- lm(outgroup_princomp ~ treatment +fractionalization_offline_index+ treatment:fractionalization_offline_index,
                                      data=combined)
ef_interaction_coef<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_se<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+fractionalization_offline_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_interaction_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_se1<- m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3, full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry+fractionalization_offline_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_interaction_coef2<-m$coefficients[26]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_se2<-m$std.error[26]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# ------ Shannon Entropy
# Model 1, no covariates
se_interaction <- lm(outgroup_princomp ~ treatment+ shannon_offline + treatment:shannon_offline,
                     data=combined)
se_interaction_coef<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_se<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+shannon_offline")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_interaction_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3, full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry+shannon_offline")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_interaction_coef2<-m$coefficients[26]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_se2<-m$std.error[26]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# ------  Largest Majority
# Model 1, no covariates
ms_interaction <- lm(outgroup_princomp ~ treatment+ largest_majority + treatment:largest_majority,
                     data=combined)
ms_interaction_coef<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_se<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+largest_majority")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_interaction_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3, full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry+largest_majority")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_interaction_coef2<-m$coefficients[26]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_se2<-m$std.error[26]/sd(combined[combined$treatment==0,]$outgroup_princomp)


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

# -------- *Table S15* ------------
join_all(list(merged_intr, merged_intr1, merged_intr2), by="names")
# -------- *Table S15* ------------


# ------------------------------------------
# Table S16: EQUAL AND ABOVE/BELOW MEDIAN
# -------------------------------------------
# Re-creating the indices so that 1 is more homogenous area across
combined$ef_median_index_rev<-ifelse(combined$fractionalization_offline_index== median(na.omit(combined$fractionalization_offline_index))|combined$fractionalization_offline_index>median(na.omit(combined$fractionalization_offline_index)),1,0) 
combined$ms_median_index <- ifelse(combined$largest_majority > median(combined$largest_majority) | combined$largest_majority==median(combined$largest_majority), 1, 0) 
combined$se_median_index_rev<-ifelse(combined$shannon_offline >   median(na.omit(combined$shannon_offline)) |  combined$shannon_offline == median(na.omit(combined$shannon_offline)), 1, 0)

# 1 is more homogenous within the rev index
ef_interaction <- lm(outgroup_princomp ~ treatment +ef_median_index_rev+treatment:ef_median_index_rev,
                     data=combined)
ef_median_intr_coef<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[2,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_se<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[2,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+ef_median_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_median_intr_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+ef_median_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_median_intr_coef2<-m$coefficients[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_se2<-m$std.error[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# shannon entropy
se_interaction <- lm(outgroup_princomp ~ treatment+ se_median_index_rev + treatment:se_median_index_rev,
                     data=combined)
se_median_intr_coef<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_se<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+se_median_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_median_intr_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+se_median_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_median_intr_coef2<-m$coefficients[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_se2<-m$std.error[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# majority group share
ms_interaction <- lm(outgroup_princomp ~ treatment+ ms_median_index  + treatment:ms_median_index,
                     data=combined)
ms_median_intr_coef<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_se<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+ms_median_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_median_intr_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+ms_median_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_median_intr_coef2<-m$coefficients[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_se2<-m$std.error[24]/sd(combined[combined$treatment==0,]$outgroup_princomp)

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
merged_intr1 
coef.vec_intr2 <- c(ef_median_intr_coef2,
                    ms_median_intr_coef2,
                    se_median_intr_coef2)
se.vec_intr2 <- c(ef_median_intr_se2,
                  ms_median_intr_se2,
                  se_median_intr_se2)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr2 <- data.frame(names, coef.vec_intr2,se.vec_intr2)
merged_intr2 

# -------- *Table S16* ------------
join_all(list(merged_intr, merged_intr1, merged_intr2), by="names")
# -------- *Table S16s* ------------

