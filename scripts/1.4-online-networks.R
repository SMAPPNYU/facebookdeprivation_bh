#########################################################################################
# File-Name: 1.4-online-networks_r.R
# Date: 2021
# Purpose: Analyzing the effects of the treatment of deactivation on outgroup regard,
#          depending on the composition of users' online networks (calculated percentages 
#          incorporated into the dataset)
# Machine: MacOS High Sierra
###########################################################################################

source("1.3-offline-networks_r.R") 

# ------------- Obtain proportions to be used in the analysis 
#               (percentages in the original form)
final_data$online_bosniak <- final_data$percentage_bosniak.3/100
final_data$online_serbian <- final_data$percentage_serbian.3/100
final_data$online_croatian <- final_data$percentage_croatian.3/100

# All of the below diversity indices are calculated following formulas presented on page 17 of Supplementary Information
# ------------- Ethnic Fractionalization
# First calculate continous measure of the diversity score
for (i in 1:length(final_data$code)){
  final_data$online_ef_index[i] <- 1 - (final_data$online_bosniak[i]^2+final_data$online_serbian[i]^2+final_data$online_croatian[i]^2)
}
# Create binary equal and above/below median index
final_data$online_ef_median_index <- ifelse(final_data$online_ef_index>=median(na.omit(final_data$online_ef_index)),0,1) # 1 is more homogeneous

# -------------- Shannon Entropy:  forming diversity continous measure and binary equal and above/below median index
# First calculate continous measure of the diversity score
for (i in 1:length(final_data$code)){
  final_data$shannon_online[i] <- -(final_data$online_bosniak[i]*log(final_data$online_bosniak[i])+final_data$online_croatian[i]*log(final_data$online_croatian[i])+final_data$online_serbian[i]*log(final_data$online_serbian[i]))}
# For one user [row 278], one of the percentages is 0;
# because that would include (log0), we calculate the index manually using the formula in SI:
final_data$shannon_online[278] <- -(0  + final_data$online_croatian[i]*log(final_data$online_croatian[278])+final_data$online_serbian[278]*log(final_data$online_serbian[278]))

# Create binary equal and above/below median index
final_data$online_sh_median_index <-ifelse(final_data$shannon_online >= median(na.omit(final_data$shannon_online)),0,1) # 1 is more homogeneous

# ----------- Majority Share Online: forming diversity continous measure and binary equal and above/below median index
# First calculate continous measure of the diversity score
dat<-final_data[,c("online_bosniak","online_croatian","online_serbian")]
final_data$largest_majority_online <- apply(dat, 1, max) # identify maximum value/largest majority share as that is the measure of interest

# Create binary equal and above/below median index
final_data$online_ms_median_index <- ifelse(final_data$largest_majority_online >= median(na.omit(final_data$largest_majority_online)), 1, 0) # 1 is more homogeneous

# ----------------------------------------------------------------------------------
# Table S17: Difference between people who sent vs did not send their online data
# ----------------------------------------------------------------------------------
bal.variables <- c("gender","age","educ","employ1","trust_media.1","freq_news","freq_fbnews","politics_int","numb_forums","time_usage", "freq_usage","imp_ethn","imp_family","imp_cntry","imp_relg","imp_career","imp_neigh","imp_hobby")

p.values <- cbind(sapply(bal.variables, function (x) t.test(final_data[final_data$online_data=="1",][,x], final_data[final_data$online_data=="0",][,x]))[3,])
online_offline_table <- do.call(data.frame, # creating a table combining the mean values and p-values
                    list(OnlineData = apply( final_data[,bal.variables][final_data$online_data=="1",], 2, mean, na.rm=TRUE),
                    NoOnlineData =  apply( final_data[,bal.variables][final_data$online_data=="0",], 2, mean, na.rm=TRUE),
                    OnlineData_SD = apply( final_data[,bal.variables][final_data$online_data=="1",], 2, sd, na.rm=TRUE),
                    NoOnlineData_SD =  apply( final_data[,bal.variables][final_data$online_data=="0",], 2, sd, na.rm=TRUE),
                    t.test=p.values))
online_offline_table

# ------------------------------------
#             FIGURE S5
# ------------------------------------
# Treatment of deactivation on the subsample of users who shared their online data
online_data <- final_data[final_data$online_data=="1",]

# Plot: Fig S5 - top part
# pdf(file = "figureS6_top.pdf",   # The directory you want to save the file in
#     width = 5.8, # The width of the plot in inches
#     height = 5.2) 

layout(matrix(c(2,1),1,2), 
       widths = c(1.5, 5))
par(mar=c(10,8,3,1)) 

coef.vec <- c(reg_res_mod1("ft",online_data)$estimate, reg_res_mod1("sd",online_data)$estimate, reg_res_mod1("multi_stat",online_data)$estimate, reg_res_mod1("othchrct_final_data",online_data)$estimate, reg_res_mod1("inchrct_final_data",online_data)$estimate, reg_res_mod1("outgroup_princomp",online_data)$estimate)
se.vec <- c(reg_res_mod1("ft",online_data)$std.error, reg_res_mod1("sd",online_data)$std.error, reg_res_mod1("multi_stat",online_data)$std.error, reg_res_mod1("othchrct_final_data",online_data)$std.error, reg_res_mod1("inchrct_final_data",online_data)$std.error, reg_res_mod1("outgroup_princomp",online_data)$std.error)

var.names <- c("Feeling thermometer","Social closeness","Cooperation","Perception of \n out-group evaluations","Out-group traits","Out-group regard index")
results_df <- data.frame(term=var.names, estimate=coef.vec,std.error=se.vec)
y.axis <- c(length(coef.vec):1)
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)" , cex.lab=2.3,ylab = "", pch = 19,cex = 1,
     xlim = c(-1,1), main = "", col=c("red","red","red","red","red","red","red","red"),cex.lab=1)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.2, col = c("red","red","red","red","red","red","red","red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 0.9, mgp = c(7,0.5,0.5))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1.2)
abline(h = 0, v=0, col="blue",lty=2,lwd=1.6)


# Plot: Fig S5 - bottom part
dev.off()
# pdf(file = "figureS6_bottom.pdf",   # The directory you want to save the file in
#     width = 5.8, # The width of the plot in inches
#     height = 5.2) 

coef.vec <- c(reg_res_mod1("c_news",online_data)$estimate, reg_res_mod1("swb",online_data)$estimate, reg_res_mod1("satisf",online_data)$estimate, reg_res_mod1("depression",online_data)$estimate, reg_res_mod1("loneliness",online_data)$estimate, reg_res_mod1("nerv",online_data)$estimate, reg_res_mod1("boredom",online_data)$estimate,reg_res_mod1("joy",online_data)$estimate,reg_res_mod1("isol",online_data)$estimate, reg_res_mod1("fulf",online_data)$estimate)
se.vec <- c(reg_res_mod1("c_news",online_data)$std.error, reg_res_mod1("swb",online_data)$std.error, reg_res_mod1("satisf",online_data)$std.error, reg_res_mod1("depression",online_data)$std.error, reg_res_mod1("loneliness",online_data)$std.error, reg_res_mod1("nerv",online_data)$std.error, reg_res_mod1("boredom",online_data)$std.error,reg_res_mod1("joy",online_data)$std.error,reg_res_mod1("isol",online_data)$std.error, reg_res_mod1("fulf",online_data)$std.error)

var.names <- c("News Knowledge","Well-being index","Satisfaction","Depression","Loneliness","Anxiety","Boredom","Joy","Isolation","Fulfillment")
results_df <- data.frame(term=var.names, estimate=coef.vec,
                         std.error=se.vec)
y.axis <- c(length(coef.vec):1) #par(mar=c(2, 13, 0, 0))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1,ylab = "", pch = 19,cex = 1,
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
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 0.9, mgp = c(3,.7,0.2))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1.2) 
abline(h = 0, v=0, col="gray",lty=2,lwd=1.6)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.6)


# ---------------------------------------------------------
#    INTERACTION w/ CONTINOUS MEASURE [online]: TABLE S19
# ---------------------------------------------------------
# Reverse coding ethnic fractionalization and Shannon entropy indices, so that higher values
# indicate higher level of ethnic homogeneity
final_data$online_ef_index_rev <- 1 - final_data$online_ef_index
final_data$shannon_online_rev <- 1 - final_data$shannon_online

# standard deviation of the control group, for standardizing the results below
sd_pc <- sd(final_data[final_data$treatment==0,]$outgroup_princomp)

# ----------- Ethnic Fractionalization
ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_ef_index_rev+ treatment:online_ef_index_rev, data=final_data)
ef_interaction_onl_coef <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[4,1]/sd_pc
ef_interaction_onl_se <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[4,2]/sd_pc

ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_ef_index_rev+ treatment:online_ef_index_rev+freq_usage, data=final_data)
ef_interaction_onl_coef1 <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[5,1]/sd_pc
ef_interaction_onl_se1 <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[5,2]/sd_pc

ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_ef_index_rev+ treatment:online_ef_index_rev+freq_usage
                             +gender+age+educ+employ1+imp_ethn+imp_cntry, data=final_data)
ef_interaction_onl_coef2 <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[11,1]/sd_pc
ef_interaction_onl_se2 <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[11,2]/sd_pc

# --------- Shannon Entropy
se_onl_interaction <- lm(outgroup_princomp ~ treatment+ shannon_online_rev+ treatment:shannon_online_rev, data=final_data)
se_interaction_onl_coef <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[4,1]/sd_pc
se_interaction_onl_se <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[4,2]/sd_pc

se_onl_interaction <- lm(outgroup_princomp ~ treatment+ shannon_online_rev+ treatment:shannon_online_rev+freq_usage, data=final_data)
se_interaction_onl_coef1 <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[5,1]/sd_pc
se_interaction_onl_se1 <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[5,2]/sd_pc

se_onl_interaction <- lm(outgroup_princomp ~ treatment+ shannon_online_rev+ treatment:shannon_online_rev+freq_usage
                         +gender+age+educ+employ1+imp_ethn+imp_cntry, data=final_data)
se_interaction_onl_coef2 <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[11,1]/sd_pc
se_interaction_onl_se2 <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[11,2]/sd_pc

# ---------  Majority Group Share
ms_onl_interaction <- lm(outgroup_princomp ~ treatment+largest_majority_online + treatment:largest_majority_online, data=final_data)
ms_interaction_onl_coef <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,1]/sd_pc
ms_interaction_onl_se <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,2]/sd_pc

ms_onl_interaction <- lm(outgroup_princomp ~ treatment+largest_majority_online + treatment:largest_majority_online+freq_usage, data=final_data)
ms_interaction_onl_coef1 <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[5,1]/sd_pc
ms_interaction_onl_se1 <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[5,2]/sd_pc

ms_onl_interaction <- lm(outgroup_princomp ~ treatment+largest_majority_online + treatment:largest_majority_online+freq_usage
                         +gender+age+educ+employ1+imp_ethn+imp_cntry, data=final_data)
ms_interaction_onl_coef2 <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[11,1]/sd_pc
ms_interaction_onl_se2 <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[11,2]/sd_pc


# Creating Table S19
coef.vec_intr_onl <- c(ef_interaction_onl_coef, 
                   ms_interaction_onl_coef,
                   se_interaction_onl_coef)
se.vec_intr_onl <- c(ef_interaction_onl_se,
                 ms_interaction_onl_se,
                 se_interaction_onl_se)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr <- data.frame(names, coef.vec_intr_onl,se.vec_intr_onl)

coef.vec_intr_onl1 <- c(ef_interaction_onl_coef1,
                   ms_interaction_onl_coef1,
                   se_interaction_onl_coef1)
se.vec_intr_onl1 <- c(ef_interaction_onl_se1,
                 ms_interaction_onl_se1,
                 se_interaction_onl_se1)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr1 <- data.frame(names, coef.vec_intr_onl1,se.vec_intr_onl1)

coef.vec_intr_onl2 <- c(ef_interaction_onl_coef2,
                        ms_interaction_onl_coef2,
                        se_interaction_onl_coef2)
se.vec_intr_onl2 <- c(ef_interaction_onl_se2,
                      ms_interaction_onl_se2,
                      se_interaction_onl_se2)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr2 <- data.frame(names, coef.vec_intr_onl2,se.vec_intr_onl2)


# -------- *Table S19* ------------
TableS19 <- join_all(list(merged_intr, merged_intr1, merged_intr2), by="names") 
TableS19
# -------- *Table S19* ------------


# -------------------------------------------------------------
#       INTERACTION WITH ABOVE/BELOW MEDIAN INDEX [online]
# --------------------------------------------------------------

# ----------- Ethnic Fractionalization
ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_ef_median_index+ treatment:online_ef_median_index, data=final_data)
ef_interaction_onl_coef <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[4,1]/sd_pc
ef_interaction_onl_se <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[4,2]/sd_pc

ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_ef_median_index+ treatment:online_ef_median_index+freq_usage, data=final_data)
ef_interaction_onl_coef1 <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[5,1]/sd_pc
ef_interaction_onl_se1 <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[5,2]/sd_pc

ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_ef_median_index+ treatment:online_ef_median_index+freq_usage
                             +gender+age+educ+employ1+imp_ethn+imp_cntry, data=final_data)
ef_interaction_onl_coef2 <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[11,1]/sd_pc
ef_interaction_onl_se2 <- coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[11,2]/sd_pc

# --------- Shannon Entropy
se_onl_interaction <- lm(outgroup_princomp ~ treatment+ online_sh_median_index+ treatment:online_sh_median_index, data=final_data)
se_interaction_onl_coef <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[4,1]/sd_pc
se_interaction_onl_se <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[4,2]/sd_pc

se_onl_interaction <- lm(outgroup_princomp ~ treatment+ online_sh_median_index+ treatment:online_sh_median_index+freq_usage, data=final_data)
se_interaction_onl_coef1 <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[5,1]/sd_pc
se_interaction_onl_se1 <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[5,2]/sd_pc

se_onl_interaction <- lm(outgroup_princomp ~ treatment+ online_sh_median_index+ treatment:online_sh_median_index+freq_usage
                         +gender+age+educ+employ1+imp_ethn+imp_cntry, data=final_data)
se_interaction_onl_coef2 <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[11,1]/sd_pc
se_interaction_onl_se2 <- coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[11,2]/sd_pc


# ---------  Majority Group Share
ms_onl_interaction <- lm(outgroup_princomp ~ treatment+online_ms_median_index + treatment:online_ms_median_index, data=final_data)
ms_interaction_onl_coef <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,1]/sd_pc
ms_interaction_onl_se <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,2]/sd_pc

ms_onl_interaction <- lm(outgroup_princomp ~ treatment+online_ms_median_index + treatment:online_ms_median_index+freq_usage, data=final_data)
ms_interaction_onl_coef1 <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[5,1]/sd_pc
ms_interaction_onl_se1 <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[5,2]/sd_pc

ms_onl_interaction <- lm(outgroup_princomp ~ treatment+online_ms_median_index + treatment:online_ms_median_index+freq_usage
                         +gender+age+educ+employ1+imp_ethn+imp_cntry, data=final_data)
ms_interaction_onl_coef2 <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[11,1]/sd_pc
ms_interaction_onl_se2 <- coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[11,2]/sd_pc


# Creating Table S20
coef.vec_intr_onl <- c(ef_interaction_onl_coef, 
                       ms_interaction_onl_coef,
                       se_interaction_onl_coef)
se.vec_intr_onl <- c(ef_interaction_onl_se,
                     ms_interaction_onl_se,
                     se_interaction_onl_se)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr <- data.frame(names, coef.vec_intr_onl,se.vec_intr_onl)

coef.vec_intr_onl1 <- c(ef_interaction_onl_coef1,
                        ms_interaction_onl_coef1,
                        se_interaction_onl_coef1)
se.vec_intr_onl1 <- c(ef_interaction_onl_se1,
                      ms_interaction_onl_se1,
                      se_interaction_onl_se1)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr1 <- data.frame(names, coef.vec_intr_onl1,se.vec_intr_onl1)

coef.vec_intr_onl2 <- c(ef_interaction_onl_coef2,
                        ms_interaction_onl_coef2,
                        se_interaction_onl_coef2)
se.vec_intr_onl2 <- c(ef_interaction_onl_se2,
                      ms_interaction_onl_se2,
                      se_interaction_onl_se2)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr2 <- data.frame(names, coef.vec_intr_onl2,se.vec_intr_onl2)


# -------- *Table S20* ------------
TableS20 <- join_all(list(merged_intr, merged_intr1, merged_intr2), by="names") 
TableS20
# -------- *Table S20* ------------






