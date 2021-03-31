#########################################################################################
# File-Name: 1.3-offline-networks.R
# Date: 2021
# Purpose: Analyzing the effects of the treatment of deactivation on outgroup regard,
#          depending on the composition of users' offline networks (Census info incorporated
#          into the dataset)
# Machine: MacOS High Sierra
###########################################################################################

source("01-analysis.R") 

# ------------------------------------------------
#   Creating indices of offline heterogeneity
# -------------------------------------------------

# -------  Ethnic Fractionalization Index
# Calculated using the formula indicated in SI; already incorporated into the dataset
final_data$ef_median_index <- ifelse(final_data$fractionalization_offline_index >= median(final_data$fractionalization_offline_index),0,1) # 1 is more homogenous

# ------- City Homogeneity: Majority Share
dat<-final_data[,c("bosniak","croat","serb")]
final_data$largest_majority <- apply(dat, 1, max) # identify maximum value/largest majority share as that is the measure of interest

final_data$ms_median_index <- ifelse(final_data$largest_majority >= median(final_data$largest_majority), 1, 0) # 1 is more homogenous (i.e, one group is constitutes a larger % of the population)

# -------  Shannon Entropy
# Calculated using the formula indicated in SI; already incorporated into the dataset;
final_data$se_median_index <- ifelse(final_data$shannon_offline >=   median(final_data$shannon_offline), 0, 1)  # 1 is more homogenous

# Below we calculate the ITT effects on the three main families of outcomes, 
# within homogenous communities (below median value, as calculated with 3 indices of diversity above)

# ----------------------------------------------------
#     Models for Table S10 - Ethnic Fractionalization
# ----------------------------------------------------

# No covariate adjustment
TableS10.1 <- do.call(data.frame,rbind(
  reg_res_mod1("outgroup_index",final_data[final_data$ef_median_index==1,]),
  reg_res_mod1("outgroup_princomp",final_data[final_data$ef_median_index==1,]),
  reg_res_mod1("ft", final_data[final_data$ef_median_index==1,]),
  reg_res_mod1("sd",final_data[final_data$ef_median_index==1,]),
  reg_res_mod1("multi_stat", final_data[final_data$ef_median_index==1,]),
  reg_res_mod1("inchrct_final_data",final_data[final_data$ef_median_index==1,]),
  reg_res_mod1("othchrct_final_data",final_data[final_data$ef_median_index==1,])))

# Adjusting for users' self-reported weekly frequency of accessing Facebook
TableS10.2 <- do.call(data.frame,rbind(
  reg_res_mod2("outgroup_index",final_data[final_data$ef_median_index==1,]),
  reg_res_mod2("outgroup_princomp",final_data[final_data$ef_median_index==1,]),
  reg_res_mod2("ft", final_data[final_data$ef_median_index==1,]),
  reg_res_mod2("sd",final_data[final_data$ef_median_index==1,]),
  reg_res_mod2("multi_stat", final_data[final_data$ef_median_index==1,]),
  reg_res_mod2("inchrct_final_data",final_data[final_data$ef_median_index==1,]),
  reg_res_mod2("othchrct_final_data",final_data[final_data$ef_median_index==1,])))

# Full covariate specification
TableS10.3 <- do.call(data.frame,rbind(
  reg_res_mod3_ethn("outgroup_index",final_data[final_data$ef_median_index==1,]),
  reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$ef_median_index==1,]),
  reg_res_mod3_ethn("ft", final_data[final_data$ef_median_index==1,]),
  reg_res_mod3_ethn("sd",final_data[final_data$ef_median_index==1,]),
  reg_res_mod3_ethn("multi_stat", final_data[final_data$ef_median_index==1,]),
  reg_res_mod3_ethn("inchrct_final_data",final_data[final_data$ef_median_index==1,]),
  reg_res_mod3_ethn("othchrct_final_data",final_data[final_data$ef_median_index==1,])))

# -------- *Table S10* ------------
tableS10 <- join_all(list(TableS10.1, TableS10.2, TableS10.3), by="outcome")
print(tableS10, digits=3)
# -------- *Table S10* ------------

# -------------------------------------------
#  Models for Table 11 - SHANNON ENTROPY
# ------------------------------------------

# No covariate adjustment
TableS11.1 <- do.call(data.frame,rbind(
  reg_res_mod1("outgroup_index",final_data[final_data$se_median_index==1,]),
  reg_res_mod1("outgroup_princomp",final_data[final_data$se_median_index==1,]),
  reg_res_mod1("ft", final_data[final_data$se_median_index==1,]),
  reg_res_mod1("sd",final_data[final_data$se_median_index==1,]),
  reg_res_mod1("multi_stat", final_data[final_data$se_median_index==1,]),
  reg_res_mod1("inchrct_final_data",final_data[final_data$se_median_index==1,]),
  reg_res_mod1("othchrct_final_data",final_data[final_data$se_median_index==1,])))

# Adjusting for users' self-reported weekly frequency of accessing Facebook
TableS11.2 <- do.call(data.frame,rbind(
  reg_res_mod2("outgroup_index",final_data[final_data$se_median_index==1,]),
  reg_res_mod2("outgroup_princomp",final_data[final_data$se_median_index==1,]),
  reg_res_mod2("ft", final_data[final_data$se_median_index==1,]),
  reg_res_mod2("sd",final_data[final_data$se_median_index==1,]),
  reg_res_mod2("multi_stat", final_data[final_data$se_median_index==1,]),
  reg_res_mod2("inchrct_final_data",final_data[final_data$se_median_index==1,]),
  reg_res_mod2("othchrct_final_data",final_data[final_data$se_median_index==1,])))

# Full covariate specification
TableS11.3 <- do.call(data.frame,rbind(
  reg_res_mod3_ethn("outgroup_index",final_data[final_data$se_median_index==1,]),
  reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$se_median_index==1,]),
  reg_res_mod3_ethn("ft", final_data[final_data$se_median_index==1,]),
  reg_res_mod3_ethn("sd",final_data[final_data$se_median_index==1,]),
  reg_res_mod3_ethn("multi_stat", final_data[final_data$se_median_index==1,]),
  reg_res_mod3_ethn("inchrct_final_data",final_data[final_data$se_median_index==1,]),
  reg_res_mod3_ethn("othchrct_final_data",final_data[final_data$se_median_index==1,])))


# -------- *Table S11* ------------
tableS11 <- join_all(list(TableS11.1, TableS11.2, TableS11.3), by="outcome")
print(tableS11, digits=3)
# -------- *Table S11* ------------

# ---------------------------------------------
#  Models for Table 12 - MAJORITY GROUP SHARE
# ---------------------------------------------

# No covariate adjustment
TableS12.1 <- do.call(data.frame,rbind(
  reg_res_mod1("outgroup_index",final_data[final_data$ms_median_index==1,]),
  reg_res_mod1("outgroup_princomp",final_data[final_data$ms_median_index==1,]),
  reg_res_mod1("ft", final_data[final_data$ms_median_index==1,]),
  reg_res_mod1("sd",final_data[final_data$ms_median_index==1,]),
  reg_res_mod1("multi_stat", final_data[final_data$ms_median_index==1,]),
  reg_res_mod1("inchrct_final_data",final_data[final_data$ms_median_index==1,]),
  reg_res_mod1("othchrct_final_data",final_data[final_data$ms_median_index==1,])))

# Adjusting for users' self-reported weekly frequency of accessing Facebook
TableS12.2 <- do.call(data.frame,rbind(
  reg_res_mod2("outgroup_index",final_data[final_data$ms_median_index==1,]),
  reg_res_mod2("outgroup_princomp",final_data[final_data$ms_median_index==1,]),
  reg_res_mod2("ft", final_data[final_data$ms_median_index==1,]),
  reg_res_mod2("sd",final_data[final_data$ms_median_index==1,]),
  reg_res_mod2("multi_stat", final_data[final_data$ms_median_index==1,]),
  reg_res_mod2("inchrct_final_data",final_data[final_data$ms_median_index==1,]),
  reg_res_mod2("othchrct_final_data",final_data[final_data$ms_median_index==1,])))

# Full covariate specification
TableS12.3 <- do.call(data.frame,rbind(
  reg_res_mod3_ethn("outgroup_index",final_data[final_data$ms_median_index==1,]),
  reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$ms_median_index==1,]),
  reg_res_mod3_ethn("ft", final_data[final_data$ms_median_index==1,]),
  reg_res_mod3_ethn("sd",final_data[final_data$ms_median_index==1,]),
  reg_res_mod3_ethn("multi_stat", final_data[final_data$ms_median_index==1,]),
  reg_res_mod3_ethn("inchrct_final_data",final_data[final_data$ms_median_index==1,]),
  reg_res_mod3_ethn("othchrct_final_data",final_data[final_data$ms_median_index==1,])))

# -------- *Table S12* ------------
tableS12 <- join_all(list(TableS12.1, TableS12.2, TableS12.3), by="outcome")
print(tableS12, digits=3)
# -------- *Table S12* ------------

# -------------------------------------------------------------
# Table S13, S14 and S15: FDR ADJUSTING WITHIN HOMOGENOUS AREAS
# --------------------------------------------------------------
# Table S13: Ethnic fractionalization
p.values_ef <- as.table(c(
  reg_res_mod3_ethn("ft", final_data[final_data$ef_median_index==1,])$p.value,
  reg_res_mod3_ethn("sd", final_data[final_data$ef_median_index==1,])$p.value,
  reg_res_mod3_ethn("multi_stat", final_data[final_data$ef_median_index==1,])$p.value,
  reg_res_mod3_ethn("inchrct_final_data", final_data[final_data$ef_median_index==1,])$p.value,
  reg_res_mod3_ethn("othchrct_final_data", final_data[final_data$ef_median_index==1,])$p.value,
  reg_res_mod3_ethn("outgroup_index", final_data[final_data$ef_median_index==1,])$p.value,
  reg_res_mod3_ethn("outgroup_princomp", final_data[final_data$ef_median_index==1,])$p.value))
names <- c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
           "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
names(p.values_ef) <- names
adjusted.p_ef <- as.table(p.adjust(sort(p.values_ef),"BH"))
FDR_ef <- cbind(p.values_ef[names], adjusted.p_ef[names])
colnames(FDR_ef) <- c("Non-adjusted p-value","BH Adjusted p-value")
# Estimates and std.errors are taken from the table with main results

# -------- *Table S13* ------------
print(FDR_ef, digits=2) 
# -------- *Table S13* ------------

# Table S14: Shannon Entropy
p.values_se <- c(
  reg_res_mod3_ethn("ft", final_data[final_data$se_median_index==1,])$p.value,
  reg_res_mod3_ethn("sd", final_data[final_data$se_median_index==1,])$p.value,
  reg_res_mod3_ethn("multi_stat", final_data[final_data$se_median_index==1,])$p.value,
  reg_res_mod3_ethn("othchrct_final_data", final_data[final_data$se_median_index==1,])$p.value,
  reg_res_mod3_ethn("inchrct_final_data", final_data[final_data$se_median_index==1,])$p.value,
  reg_res_mod3_ethn("outgroup_index", final_data[final_data$se_median_index==1,])$p.value,
  reg_res_mod3_ethn("outgroup_princomp", final_data[final_data$se_median_index==1,])$p.value)
names <-  c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
            "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
names(p.values_se) <- names
adjusted.p_se <- as.table(p.adjust(sort(p.values_se),"BH"))
FDR_se <- cbind(p.values_se[names], adjusted.p_se[names])
colnames(FDR_se) <- c("Non-adjusted p-value","BH Adjusted p-value")
# Estimates and std.errors are taken from the table with main results

# -------- *Table S14* ------------
print(FDR_se, digits=2)
# -------- *Table S14* ------------

# Table S15: Majority Group Share
p.values_ms <- c(
  reg_res_mod3_ethn("ft", final_data[final_data$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("sd", final_data[final_data$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("multi_stat", final_data[final_data$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("othchrct_final_data", final_data[final_data$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("inchrct_final_data", final_data[final_data$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("outgroup_index", final_data[final_data$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("outgroup_princomp", final_data[final_data$ms_median_index==1,])$p.value)
names <- c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                        "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
names(p.values_ms) <- names
adjusted.p_ms <- as.table(p.adjust(sort(p.values_ms),"BH"))
FDR_ms <- cbind(p.values_ms[names], adjusted.p_ms[names])
colnames(FDR_ms) <- c("Non-adjusted p-value","BH Adjusted p-value")
# Estimates and std.errors are taken from the table with main results

# -------- *Table S15* ------------
print(FDR_ms, digits=1)
# -------- *Table S15* ------------

# ------------------------------------------------------
#    INTERACTION w/ CONTINOUS MEASURE: TABLE S16 & S17
# ------------------------------------------------------
sd_pc <- sd(final_data[final_data$treatment==0,]$outgroup_princomp)

# ------ Ethnic Fractionalization
# Model 1, no covariates

# Higher values of fractionalization index indicate higher heterogeneity levels; 
# for the ease of interpretation and alignment with our paper dicscussion, we reverse code the index
# so that higher values indicate higher homogeneity level
final_data$fractionalization_offline_index_rev <- 1 - final_data$fractionalization_offline_index 

# Model 1, no covariates
ef_interaction <- lm(outgroup_princomp ~ treatment + fractionalization_offline_index_rev + treatment:fractionalization_offline_index_rev, data=final_data)
ef_interaction_coef <- coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,1]/sd_pc
ef_interaction_se <- coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,2]/sd_pc

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+fractionalization_offline_index_rev")
ef_interaction1 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
ef_interaction_coef1 <- ef_interaction1$coefficients[6]/sd_pc
ef_interaction_se1 <- ef_interaction1$std.error[6]/sd_pc

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+fractionalization_offline_index_rev")
ef_interaction2 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
ef_interaction_coef2 <- ef_interaction2$coefficient[18]/sd_pc
ef_interaction_se2 <- ef_interaction2$std.error[18]/sd_pc


# ------ Shannon Entropy
# As above: higher values of Shannon Entropy indicate higher heterogeneity levels; 
# for the ease of interpretation and alignment with our paper dicscussion, we reverse code the index
# so that higher values indicate higher homogeneity level

# Model 1, no covariates
final_data$shannon_offline_rev <- 1 - final_data$shannon_offline

# Model 1, no covariates
se_interaction <- lm(outgroup_princomp ~ treatment+ shannon_offline_rev + treatment:shannon_offline_rev, data=final_data)
se_interaction_coef <- coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,1]/sd_pc
se_interaction_se <- coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,2]/sd_pc

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+shannon_offline_rev")
se_interaction1 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
se_interaction_coef1 <- se_interaction1$coefficients[6]/sd_pc
se_interaction_se1 <- se_interaction1$std.error[6]/sd_pc

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+shannon_offline_rev")
se_interaction2 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
se_interaction_coef2 <- se_interaction2$coefficients[18]/sd_pc
se_interaction_se2 <- se_interaction2$std.error[18]/sd_pc


# ------  Largest Majority
# Model 1, no covariates
ms_interaction <- lm(outgroup_princomp ~ treatment+ largest_majority + treatment:largest_majority,
                     data=final_data)
ms_interaction_coef <- coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,1]/sd_pc
ms_interaction_se <- coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,2]/sd_pc

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+largest_majority")
ms_interaction1 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
ms_interaction_coef1 <- ms_interaction1$coefficients[6]/sd_pc
ms_interaction_se1 <- ms_interaction1$std.error[6]/sd_pc

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+largest_majority")
ms_interaction2 <-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
ms_interaction_coef2 <- ms_interaction2$coefficients[18]/sd_pc
ms_interaction_se2 <- ms_interaction2$std.error[18]/sd_pc

# Putting it together
coef.vec_intr <- c(ef_interaction_coef,
                   ms_interaction_coef,
                   se_interaction_coef)
se.vec_intr <- c(ef_interaction_se,
                 ms_interaction_se,
                 se_interaction_se)
names <- c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr <- data.frame(names, coef.vec_intr,se.vec_intr)

coef.vec_intr1 <- c(ef_interaction_coef1,
                    ms_interaction_coef1,
                    se_interaction_coef1)
se.vec_intr1 <- c(ef_interaction_se1,
                  ms_interaction_se1,
                  se_interaction_se1)
names <- c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr1 <- data.frame(names, coef.vec_intr1,se.vec_intr1)

coef.vec_intr2 <- c(ef_interaction_coef2,
                    ms_interaction_coef2,
                    se_interaction_coef2)
se.vec_intr2 <- c(ef_interaction_se2,
                  ms_interaction_se2,
                  se_interaction_se2)
names <- c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr2 <- data.frame(names, coef.vec_intr2,se.vec_intr2)

# -------- *Table S16* ------------
join_all(list(merged_intr, merged_intr1, merged_intr2), by="names")
# -------- *Table S16* ------------


# ------------------------------------------
# Table S17: EQUAL AND ABOVE/BELOW MEDIAN
# -------------------------------------------
# ------ Ethnic Fractionalization
# Model 1, no covariates
ef_interaction <- lm(outgroup_princomp ~ treatment +ef_median_index+treatment:ef_median_index,
                     data=final_data)
ef_median_intr_coef <- coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,1]/sd_pc
ef_median_intr_se <- coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,2]/sd_pc

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+ef_median_index")
ef_interaction1 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
ef_median_intr_coef1 <- ef_interaction1$coefficients[6]/sd_pc
ef_median_intr_se1 <- ef_interaction1$std.error[6]/sd_pc

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+ef_median_index")
ef_interaction2 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
ef_median_intr_coef2 <- ef_interaction2$coefficients[18]/sd_pc
ef_median_intr_se2 <- ef_interaction2$std.error[18]/sd_pc

# ------ Shannon Entropy
# Model 1, no covariates
se_interaction <- lm(outgroup_princomp ~ treatment+ se_median_index + treatment:se_median_index,
                     data=final_data)
se_median_intr_coef <- coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,1]/sd_pc
se_median_intr_se <- coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,2]/sd_pc

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+se_median_index")
se_interaction1 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
se_median_intr_coef1 <- se_interaction1$coefficients[6]/sd_pc
se_median_intr_se1 <- se_interaction1$std.error[6]/sd_pc

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+se_median_index")
se_interaction2 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
se_median_intr_coef2 <- se_interaction2$coefficients[18]/sd_pc
se_median_intr_se2 <- se_interaction2$std.error[18]/sd_pc

# ------ Majority Group Share
# Model 1, no covariates
ms_interaction <- lm(outgroup_princomp ~ treatment+ ms_median_index  + treatment:ms_median_index,
                     data=final_data)
ms_median_intr_coef <- coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4]/sd_pc
ms_median_intr_se <- coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,2]/sd_pc

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+ms_median_index")
ms_interaction1 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
ms_median_intr_coef1 <- ms_interaction1$coefficients[6]/sd_pc
ms_median_intr_se1 <- ms_interaction1$std.error[6]/sd_pc

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+ms_median_index")
ms_interaction2 <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
ms_median_intr_coef2 <- ms_interaction2$coefficients[18]/sd_pc
ms_median_intr_se2 <- ms_interaction2$std.error[18]/sd_pc

# Putting it all together
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


# -------- *Table S17* ------------
join_all(list(merged_intr, merged_intr1, merged_intr2), by="names")
# -------- *Table S17* ------------

