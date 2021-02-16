# --------------------------------------
#           Offline networks
# ---------------------------------------

# -------  Ethnic Fractionalization Index
# Calculated using the formula below; already incorporated into the dataset
# for (i in 1:length(combined$X)){
 # combined$fractionalization_offline_index[i] <- 1 - (combined$bosniak[i]^2+combined$serb[i]^2+combined$croat[i]^2)}
combined$ef_median_index<-ifelse(combined$fractionalization_offline_index >= median(combined$fractionalization_offline_index),1,0) #  1 is more heterogenous

# ------- City Homogeneity: Majority Share
combined$ms_median_index <- ifelse(combined$largest_majority >= median(combined$largest_majority), 1, 0) # 0 is more heterogenous 

# -------  Shannon Entropy
# Calculated using the formula below; already incorporated in the dataset; 
# where one group is 0 (for three cities; log undefined), value calculated manually
# for (i in 1:length(combined$X)){
  # combined_old$shannon_offline[i] <- -(combined$bosniak[i]*log(combined$bosniak[i])+combined$croat[i]*log(combined$croat[i])+combined$serb[i]*log(combined$serb[i]))}
combined$se_median_index <- ifelse(combined$shannon_offline >=   median(combined$shannon_offline), 1, 0)  # 1 is more heterogenous

# ------ Effects on the three main families of outcomes, within homogenous communities with 3 indices of diversity

# ----------------------------------------------------
#     Models for Table S9 - Ethnic Fractionalization
# ----------------------------------------------------

# ------ Table S9
TableS9.1<-do.call(data.frame,rbind(
  reg_res_mod1("outgroup_index",combined[combined$ef_median_index==0,]),
  reg_res_mod1("outgroup_princomp",combined[combined$ef_median_index==0,]),
  reg_res_mod1("ft", combined[combined$ef_median_index==0,]),
  reg_res_mod1("sd",combined[combined$ef_median_index==0,]),
  reg_res_mod1("multi_stat", combined[combined$ef_median_index==0,]),
  reg_res_mod1("inchrct_combined",combined[combined$ef_median_index==0,]),
  reg_res_mod1("othchrct_combined",combined[combined$ef_median_index==0,])))

TableS9.2<-do.call(data.frame,rbind(
  reg_res_mod2("outgroup_index",combined[combined$ef_median_index==0,]),
  reg_res_mod2("outgroup_princomp",combined[combined$ef_median_index==0,]),
  reg_res_mod2("ft", combined[combined$ef_median_index==0,]),
  reg_res_mod2("sd",combined[combined$ef_median_index==0,]),
  reg_res_mod2("multi_stat", combined[combined$ef_median_index==0,]),
  reg_res_mod2("inchrct_combined",combined[combined$ef_median_index==0,]),
  reg_res_mod2("othchrct_combined",combined[combined$ef_median_index==0,])))

TableS9.3<-do.call(data.frame,rbind(
  reg_res_mod3_ethn("outgroup_index",combined[combined$ef_median_index==0,]),
  reg_res_mod3_ethn("outgroup_princomp",combined[combined$ef_median_index==0,]),
  reg_res_mod3_ethn("ft", combined[combined$ef_median_index==0,]),
  reg_res_mod3_ethn("sd",combined[combined$ef_median_index==0,]),
  reg_res_mod3_ethn("multi_stat", combined[combined$ef_median_index==0,]),
  reg_res_mod3_ethn("inchrct_combined",combined[combined$ef_median_index==0,]),
  reg_res_mod3_ethn("othchrct_combined",combined[combined$ef_median_index==0,])))

# -------- *Table S9* ------------
tableS9<-join_all(list(TableS9.1, TableS9.2, TableS9.3), by="outcome")
print(tableS9, digits=3)
# -------- *Table S9* ------------

# -------------------------------------------
#  Models for Table 10 - SHANNON ENTROPY
# ------------------------------------------

# ------ Table S10
TableS10.1<-do.call(data.frame,rbind(
  reg_res_mod1("outgroup_index",combined[combined$se_median_index==0,]),
  reg_res_mod1("outgroup_princomp",combined[combined$se_median_index==0,]),
  reg_res_mod1("ft", combined[combined$se_median_index==0,]),
  reg_res_mod1("sd",combined[combined$se_median_index==0,]),
  reg_res_mod1("multi_stat", combined[combined$se_median_index==0,]),
  reg_res_mod1("inchrct_combined",combined[combined$se_median_index==0,]),
  reg_res_mod1("othchrct_combined",combined[combined$se_median_index==0,])))

TableS10.2<-do.call(data.frame,rbind(
  reg_res_mod2("outgroup_index",combined[combined$se_median_index==0,]),
  reg_res_mod2("outgroup_princomp",combined[combined$se_median_index==0,]),
  reg_res_mod2("ft", combined[combined$se_median_index==0,]),
  reg_res_mod2("sd",combined[combined$se_median_index==0,]),
  reg_res_mod2("multi_stat", combined[combined$se_median_index==0,]),
  reg_res_mod2("inchrct_combined",combined[combined$se_median_index==0,]),
  reg_res_mod2("othchrct_combined",combined[combined$se_median_index==0,])))

TableS10.3<-do.call(data.frame,rbind(
  reg_res_mod3_ethn("outgroup_index",combined[combined$se_median_index==0,]),
  reg_res_mod3_ethn("outgroup_princomp",combined[combined$se_median_index==0,]),
  reg_res_mod3_ethn("ft", combined[combined$se_median_index==0,]),
  reg_res_mod3_ethn("sd",combined[combined$se_median_index==0,]),
  reg_res_mod3_ethn("multi_stat", combined[combined$se_median_index==0,]),
  reg_res_mod3_ethn("inchrct_combined",combined[combined$se_median_index==0,]),
  reg_res_mod3_ethn("othchrct_combined",combined[combined$se_median_index==0,])))


# -------- *Table S10* ------------
tableS10<-join_all(list(TableS10.1, TableS10.2, TableS10.3), by="outcome")
print(tableS10, digits=3)
# -------- *Table S10* ------------

# ----------------------------------------------
#  Models for Table 11 - MAJORITY GROUP SHARE
# ---------------------------------------------

TableS11.1<-do.call(data.frame,rbind(
  reg_res_mod1("outgroup_index",combined[combined$ms_median_index==1,]),
  reg_res_mod1("outgroup_princomp",combined[combined$ms_median_index==1,]),
  reg_res_mod1("ft", combined[combined$ms_median_index==1,]),
  reg_res_mod1("sd",combined[combined$ms_median_index==1,]),
  reg_res_mod1("multi_stat", combined[combined$ms_median_index==1,]),
  reg_res_mod1("inchrct_combined",combined[combined$ms_median_index==1,]),
  reg_res_mod1("othchrct_combined",combined[combined$ms_median_index==1,])))

TableS11.2<-do.call(data.frame,rbind(
  reg_res_mod2("outgroup_index",combined[combined$ms_median_index==1,]),
  reg_res_mod2("outgroup_princomp",combined[combined$ms_median_index==1,]),
  reg_res_mod2("ft", combined[combined$ms_median_index==1,]),
  reg_res_mod2("sd",combined[combined$ms_median_index==1,]),
  reg_res_mod2("multi_stat", combined[combined$ms_median_index==1,]),
  reg_res_mod2("inchrct_combined",combined[combined$ms_median_index==1,]),
  reg_res_mod2("othchrct_combined",combined[combined$ms_median_index==1,])))

TableS11.3<-do.call(data.frame,rbind(
  reg_res_mod3_ethn("outgroup_index",combined[combined$ms_median_index==1,]),
  reg_res_mod3_ethn("outgroup_princomp",combined[combined$ms_median_index==1,]),
  reg_res_mod3_ethn("ft", combined[combined$ms_median_index==1,]),
  reg_res_mod3_ethn("sd",combined[combined$ms_median_index==1,]),
  reg_res_mod3_ethn("multi_stat", combined[combined$ms_median_index==1,]),
  reg_res_mod3_ethn("inchrct_combined",combined[combined$ms_median_index==1,]),
  reg_res_mod3_ethn("othchrct_combined",combined[combined$ms_median_index==1,])))

# -------- *Table S11* ------------
tableS11<-join_all(list(TableS11.1, TableS11.2, TableS11.3), by="outcome")
print(tableS11, digits=3)
# -------- *Table S11* ------------

# -------------------------------------------------------------
# Table S12, S13 and S14: FDR ADJUSTING WITHIN HOMOGENOUS AREAS
# --------------------------------------------------------------
# Table S12: Ethnic fractionalization
p.values_ef <- as.table(c(
  reg_res_mod3_ethn("ft", combined[combined$ef_median_index==0,])$p.value,
  reg_res_mod3_ethn("sd", combined[combined$ef_median_index==0,])$p.value,
  reg_res_mod3_ethn("multi_stat", combined[combined$ef_median_index==0,])$p.value,
  reg_res_mod3_ethn("inchrct_combined", combined[combined$ef_median_index==0,])$p.value,
  reg_res_mod3_ethn("othchrct_combined", combined[combined$ef_median_index==0,])$p.value,
  reg_res_mod3_ethn("outgroup_index", combined[combined$ef_median_index==0,])$p.value,
  reg_res_mod3_ethn("outgroup_princomp", combined[combined$ef_median_index==0,])$p.value))
names<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                      "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
names(p.values_ef) <- names
adjusted.p_ef<-as.table(p.adjust(sort(p.values_ef),"BH"))
FDR_ef<-cbind(p.values_ef[names], adjusted.p_ef[names])
colnames(FDR_ef)<-c("Non-adjusted p-value","BH Adjusted p-value")
# Estimates and std.errors are taken from the table with main results

# -------- *Table S12* ------------
print(FDR_ef, digits=2) 
# -------- *Table S12* ------------

# Table S13: Shannon Entropy
p.values_se <- c(
  reg_res_mod3_ethn("ft", combined[combined$se_median_index==0,])$p.value,
  reg_res_mod3_ethn("sd", combined[combined$se_median_index==0,])$p.value,
  reg_res_mod3_ethn("multi_stat", combined[combined$se_median_index==0,])$p.value,
  reg_res_mod3_ethn("othchrct_combined", combined[combined$se_median_index==0,])$p.value,
  reg_res_mod3_ethn("inchrct_combined", combined[combined$se_median_index==0,])$p.value,
  reg_res_mod3_ethn("outgroup_index", combined[combined$se_median_index==0,])$p.value,
  reg_res_mod3_ethn("outgroup_princomp", combined[combined$se_median_index==0,])$p.value)
names<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
         "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
names(p.values_se)<- names
adjusted.p_se<-as.table(p.adjust(sort(p.values_se),"BH"))
FDR_se<-cbind(p.values_se[names], adjusted.p_se[names])
colnames(FDR_se)<-c("Non-adjusted p-value","BH Adjusted p-value")
# Estimates and std.errors are taken from the table with main results

# -------- *Table S13* ------------
print(FDR_se, digits=2)
# -------- *Table S13* ------------

# Table S14: Majority Group Share
p.values_ms <- c(
  reg_res_mod3_ethn("ft", combined[combined$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("sd", combined[combined$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("multi_stat", combined[combined$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("othchrct_combined", combined[combined$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("inchrct_combined", combined[combined$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("outgroup_index", combined[combined$ms_median_index==1,])$p.value,
  reg_res_mod3_ethn("outgroup_princomp", combined[combined$ms_median_index==1,])$p.value)
names<-c("feeling thermometer", "sd","cooperation","perception of out-group attitudes",
                      "out-group traits","out-group regard (z-scores)","out-group regard (pc)")
names(p.values_ms) <- names
adjusted.p_ms<-as.table(p.adjust(sort(p.values_ms),"BH"))
FDR_ms<-cbind(p.values_ms[names], adjusted.p_ms[names])
colnames(FDR_ms)<-c("Non-adjusted p-value","BH Adjusted p-value")
# Estimates and std.errors are taken from the table with main results

# -------- *Table S14* ------------
print(FDR_ms, digits=2)
# -------- *Table S14* ------------

# ------------------------------------------------------
#    INTERACTION w/ CONTINOUS MEASURE: TABLE S15 & S16
# ------------------------------------------------------

# ------ Ethnic Fractionalization
# Model 1, no covariates
ef_interaction <- lm(outgroup_princomp ~ treatment +fractionalization_offline_index+ treatment:fractionalization_offline_index,
                                      data=combined)
ef_interaction_coef<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_se<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+fractionalization_offline_index")
ef_interaction1 <-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_interaction_coef1<-ef_interaction1$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_se1<- ef_interaction1$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+fractionalization_offline_index")
ef_interaction2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_interaction_coef2<-ef_interaction2$coefficient[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_se2<-ef_interaction2$std.error[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)


# ------ Shannon Entropy
# Model 1, no covariates
se_interaction <- lm(outgroup_princomp ~ treatment+ shannon_offline + treatment:shannon_offline,
                     data=combined)
se_interaction_coef<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_se<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+shannon_offline")
se_interaction1<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_interaction_coef1<-se_interaction1$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_se1<-se_interaction1$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+shannon_offline")
se_interaction2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_interaction_coef2<-se_interaction2$coefficients[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_se2<-se_interaction2$std.error[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)


# ------  Largest Majority
# Model 1, no covariates
ms_interaction <- lm(outgroup_princomp ~ treatment+ largest_majority + treatment:largest_majority,
                     data=combined)
ms_interaction_coef<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_se<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+largest_majority")
ms_interaction1<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_interaction_coef1<-ms_interaction1$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_se1<-ms_interaction1$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+largest_majority")
ms_interaction2 <-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_interaction_coef2<-ms_interaction2$coefficients[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_se2<-ms_interaction2$std.error[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Putting it together
coef.vec_intr <- c(-ef_interaction_coef,
                   ms_interaction_coef,
                   -se_interaction_coef)
se.vec_intr <- c(ef_interaction_se,
                 ms_interaction_se,
                 se_interaction_se)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr <- data.frame(names, coef.vec_intr,se.vec_intr)

coef.vec_intr1 <- c(-ef_interaction_coef1,
                    ms_interaction_coef1,
                    -se_interaction_coef1)
se.vec_intr1 <- c(ef_interaction_se1,
                  ms_interaction_se1,
                  se_interaction_se1)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr1 <- data.frame(names, coef.vec_intr1,se.vec_intr1)

coef.vec_intr2 <- c(-ef_interaction_coef2,
                    ms_interaction_coef2,
                    -se_interaction_coef2)
se.vec_intr2 <- c(-ef_interaction_se2,
                  ms_interaction_se2,
                  -se_interaction_se2)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr2 <- data.frame(names, coef.vec_intr2,se.vec_intr2)

# -------- *Table S15* ------------
join_all(list(merged_intr, merged_intr1, merged_intr2), by="names")
# -------- *Table S15* ------------


# ------------------------------------------
# Table S16: EQUAL AND ABOVE/BELOW MEDIAN
# -------------------------------------------
# Re-creating the indices so that 1 is more homogenous area across [for the ease of interpretation]
combined$ef_median_index_rev <- ifelse(combined$fractionalization_offline_index >= median(na.omit(combined$fractionalization_offline_index)),0,1) 
combined$ms_median_index <- ifelse(combined$largest_majority >= median(na.omit(combined$largest_majority)), 1,0) # as it was earlier
combined$se_median_index_rev <- ifelse(combined$shannon_offline >=  median(na.omit(combined$shannon_offline)), 0,1)

# ------ Ethnic Fractionalization
# Model 1, no covariates
ef_interaction <- lm(outgroup_princomp ~ treatment +ef_median_index_rev+treatment:ef_median_index_rev,
                     data=combined)
ef_median_intr_coef<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_se<-coeftest(ef_interaction, vcov = vcovHC(ef_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+ef_median_index_rev")
ef_interaction1<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_median_intr_coef1<-ef_interaction1$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_se1<-ef_interaction1$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+ef_median_index_rev")
ef_interaction2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_median_intr_coef2<-ef_interaction2$coefficients[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_se2<-ef_interaction2$std.error[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# ------ Shannon Entropy
# Model 1, no covariates
se_interaction <- lm(outgroup_princomp ~ treatment+ se_median_index_rev + treatment:se_median_index_rev,
                     data=combined)
se_median_intr_coef<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_se<-coeftest(se_interaction, vcov = vcovHC(se_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+se_median_index_rev")
se_interaction1<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_median_intr_coef1<-se_interaction1$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_se1<-se_interaction1$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+se_median_index_rev")
se_interaction2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_median_intr_coef2<-se_interaction2$coefficients[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_se2<-se_interaction2$std.error[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# ------ Majority Group Share
# Model 1, no covariates
ms_interaction <- lm(outgroup_princomp ~ treatment+ ms_median_index  + treatment:ms_median_index,
                     data=combined)
ms_median_intr_coef<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_se<-coeftest(ms_interaction, vcov = vcovHC(ms_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2, controlling for baseline freq of usage
covIn <- as.formula("~freq_usage+ms_median_index")
ms_interaction1<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_median_intr_coef1<-ms_interaction1$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_se1<-ms_interaction1$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3, full covariates
covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry+ms_median_index")
ms_interaction2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_median_intr_coef2<-ms_interaction2$coefficients[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_se2<-ms_interaction2$std.error[18]/sd(combined[combined$treatment==0,]$outgroup_princomp)

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


# -------- *Table S16* ------------
join_all(list(merged_intr, merged_intr1, merged_intr2), by="names")
# -------- *Table S16* ------------

