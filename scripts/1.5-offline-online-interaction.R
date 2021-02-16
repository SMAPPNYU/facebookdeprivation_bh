# This script analyzes the interaction of users' offline and online network heterogeneity

# -----------------------------
#           Table S19
# -----------------------------
# Continous measure - subtracting values of heterogeneity indices of users' offline vs. online networks:
# 1. First column: Outgroup regard index, formed as principal component score, as the dependent variable
# 2. Second column: Outgroup regard index, formed as a sum score, as the dependent variable

## PRINCIPAL COMPONENT SCORE

# -------- Majority Group Share
combined$majority_group_difference <- combined$largest_majority-combined$largest_majority_online # higher - offline more homogenous
covIn <- as.formula("~freq_usage+majority_group_difference")
m<-lm_lin(outgroup_princomp~treatment+treatment, covariates=covIn, data=combined)
inter_ms_coef<-m$coefficients[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))
inter_ms_se<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))

# --------- Ethnic Fractionalization
combined$fract_difference <- combined$online_index-combined$fractionalization_offline_index # higher - offline more homogenous
covIn <- as.formula("~freq_usage+fract_difference")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
inter_ef_coef<-m$coefficients[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))
inter_ef_se<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))

# --------- Shannon Entropy
combined$shannon_difference <- combined$shannon_online-combined$shannon_offline # higher - offline more homogenous
covIn <- as.formula("~freq_usage+shannon_difference")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
inter_sh_coef<-m$coefficients[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))
inter_sh_se<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))

# Table S19 [first column]
coef.vec_intr_onl <- c(inter_sh_coef, inter_ms_coef, inter_ef_coef)
se.vec_intr_onl <- c(inter_sh_se, inter_ms_se, inter_ef_se)
names<-c("Out-Group Index (SE)","Out-Group Index (MS)", "Out-Group Index (EF)")
interaction_princp <- data.frame(names, coef.vec_intr_onl,se.vec_intr_onl)

## SUM SCORE

# -------- Majority Group Share
covIn <- as.formula("~freq_usage+majority_group_difference")
m<-lm_lin(outgroup_index~treatment+treatment, covariates=covIn, data=combined)
inter_ms_coef2<-m$coefficients[6]/sd((combined[which( combined$treatment=="0"),]$outgroup_index))
inter_ms_se2<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_index))

# --------- Ethnic Fractionalization
covIn <- as.formula("~freq_usage+fract_difference")
m<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined)
inter_ef_coef2<-m$coefficients[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_index))
inter_ef_se2<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_index))

# --------- Shannon Entropy
covIn <- as.formula("~freq_usage+shannon_difference")
m<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined)
inter_sh_coef2<-m$coefficients[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_index))
inter_sh_se2<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_index))

# Table S19 [second column]
coef.vec_intr_onl2 <- c(inter_sh_coef2, inter_ms_coef2, inter_ef_coef2)
se.vec_intr_onl2 <- c(inter_sh_se2, inter_ms_se2, inter_ef_se2)
names<-c("Out-Group Index (SE)","Out-Group Index (MS)", "Out-Group Index (EF)")
interaction_princp2 <- data.frame(names, coef.vec_intr_onl2,se.vec_intr_onl2)


# ---------------------------- Table S19  ---------------------------- 
tableS19<-join_all(list(interaction_princp,interaction_princp2), by="names") 
tableS19
# ---------------------------- Table S19  ---------------------------- 


# ----------------------------------------------------------------
#                   Table S20 - Binary Indicator
# -----------------------------------------------------------------

# Creating a binary indicator comparing indices capturing the heterogeneity of users' offline vs. online networks 
# 1. First column: Outgroup regard index, formed as principal component score, as the dependent variable
# 2. Second column: Outgroup regard index, formed as a sum score, as the dependent variable

combined$majority_group_difference <- ifelse(combined$largest_majority>=combined$largest_majority_online, 1, 0) # 1 ~ offline is more homogenous
combined$fract_difference <- ifelse(combined$fractionalization_offline_index>=combined$online_index,0,1) #1 ~ offline more homogenous
combined$shannon_difference <- ifelse(combined$shannon_offline>=combined$shannon_online,0,1)  #1 ~ offline more homogenous

## PRINCIPAL COMPONENT SCORE

# -------- Majority Group Share
covIn <- as.formula("~freq_usage+majority_group_difference")
m<-lm_lin(outgroup_princomp~treatment+treatment, covariates=covIn, data=combined)
inter_ms_coef<-m$coefficients[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))
inter_ms_se<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))

# --------- Ethnic Fractionalization
covIn <- as.formula("~freq_usage+fract_difference")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
inter_ef_coef<-m$coefficients[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))
inter_ef_se<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))

# --------- Shannon Entropy
covIn <- as.formula("~freq_usage+shannon_difference")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
inter_sh_coef<-m$coefficients[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))
inter_sh_se<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_princomp))

# Table S20 [first column]
coef.vec_intr_onl <- c(inter_sh_coef, inter_ms_coef, inter_ef_coef)
se.vec_intr_onl <- c(inter_sh_se, inter_ms_se, inter_ef_se)
names<-c("Out-Group Index (SE)","Out-Group Index (MS)", "Out-Group Index (EF)")
interaction_princp <- data.frame(names, coef.vec_intr_onl,se.vec_intr_onl)

## SUM SCORE

# -------- Majority Group Share
covIn <- as.formula("~freq_usage+majority_group_difference")
m<-lm_lin(outgroup_index~treatment+treatment, covariates=covIn, data=combined)
inter_ms_coef2<-m$coefficients[6]/sd((combined[which( combined$treatment=="0"),]$outgroup_index))
inter_ms_se2<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_index))

# --------- Ethnic Fractionalization
covIn <- as.formula("~freq_usage+fract_difference")
m<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined)
inter_ef_coef2<-m$coefficients[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_index))
inter_ef_se2<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_index))

# --------- Shannon Entropy
covIn <- as.formula("~freq_usage+shannon_difference")
m<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined)
inter_sh_coef2<-m$coefficients[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_index))
inter_sh_se2<-m$std.error[6]/sd((combined[which(combined$treatment=="0"),]$outgroup_index))

# Table S20 [second column]
coef.vec_intr_onl2 <- c(inter_sh_coef2, inter_ms_coef2, inter_ef_coef2)
se.vec_intr_onl2 <- c(inter_sh_se2, inter_ms_se2, inter_ef_se2)
names<-c("Out-Group Index (SE)","Out-Group Index (MS)", "Out-Group Index (EF)")
interaction_princp2 <- data.frame(names, coef.vec_intr_onl2,se.vec_intr_onl2)

# ---------------------------- Table S20  ---------------------------- 
tableS20<-join_all(list(interaction_princp,interaction_princp2), by="names") 
tableS20
# ---------------------------- Table S20 ---------------------------- 