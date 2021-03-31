#########################################################################################
# File-Name: 1.4-online-networks_r.R
# Date: 2021
# Purpose: Analyzing the interaction of users' offline and online network heterogeneity
###########################################################################################

source("1.4-online-networks_r.R") 

# -----------------------------
#           Table S21
# -----------------------------
# Continous measure - subtracting values of heterogeneity indices of users' offline vs. online networks:
# 1. First column: Outgroup regard index, formed as principal component score, as the dependent variable
# 2. Second column: Outgroup regard index, formed as a sum score, as the dependent variable

## PRINCIPAL COMPONENT SCORE

# -------- Majority Group Share
final_data$majority_group_difference <- final_data$largest_majority-final_data$largest_majority_online # higher values indicate that offline
                                                                                                 # network more homogenous
covIn <- as.formula("~freq_usage+majority_group_difference")
int_ms_pc <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
inter_ms_coef <- int_ms_pc$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))
inter_ms_se <- int_ms_pc$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))


# --------- Ethnic Fractionalization
final_data$fract_difference <- final_data$online_ef_index-final_data$fractionalization_offline_index # higher values indicate that offline
                                                                                                # network more homogenous
covIn <- as.formula("~freq_usage+fract_difference")
int_ef_pc <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
inter_ef_coef <- int_ef_pc$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))
inter_ef_se <- int_ef_pc$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))

# --------- Shannon Entropy
final_data$shannon_difference <- final_data$shannon_online-final_data$shannon_offline # higher values indicate that offline
                                                                                # network more homogenous
covIn <- as.formula("~freq_usage+shannon_difference")
int_sh_pc <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
inter_sh_coef <- int_sh_pc$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))
inter_sh_se <- int_sh_pc$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))

# Table S21 [first column]
coef.vec_intr_onl <- c(inter_sh_coef, inter_ms_coef, inter_ef_coef)
se.vec_intr_onl <- c(inter_sh_se, inter_ms_se, inter_ef_se)
names <- c("Out-Group Index (SE)","Out-Group Index (MS)", "Out-Group Index (EF)")
interaction_princp <- data.frame(names, coef.vec_intr_onl,se.vec_intr_onl)

## SUM SCORE

# -------- Majority Group Share
covIn <- as.formula("~freq_usage+majority_group_difference")
int_ms_sum <- lm_lin(outgroup_index~treatment+treatment, covariates=covIn, data=final_data)
inter_ms_coef2 <- int_ms_sum $coefficients[6]/sd((final_data[which( final_data$treatment=="0"),]$outgroup_index))
inter_ms_se2 <- int_ms_sum $std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))

# --------- Ethnic Fractionalization
covIn <- as.formula("~freq_usage+fract_difference")
int_ef_sum <- lm_lin(outgroup_index~treatment, covariates=covIn, data=final_data)
inter_ef_coef2 <- int_ef_sum$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))
inter_ef_se2 <- int_ef_sum$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))

# --------- Shannon Entropy
covIn <- as.formula("~freq_usage+shannon_difference")
int_sh_sum <- lm_lin(outgroup_index~treatment, covariates=covIn, data=final_data)
inter_sh_coef2 <- int_sh_sum$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))
inter_sh_se2 <- int_sh_sum$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))

# Table S21 [second column]
coef.vec_intr_onl2 <- c(inter_sh_coef2, inter_ms_coef2, inter_ef_coef2)
se.vec_intr_onl2 <- c(inter_sh_se2, inter_ms_se2, inter_ef_se2)
names<-c("Out-Group Index (SE)","Out-Group Index (MS)", "Out-Group Index (EF)")
interaction_princp2 <- data.frame(names, coef.vec_intr_onl2,se.vec_intr_onl2)


# ---------------------------- *Table S21*  ---------------------------- 
tableS21 <- join_all(list(interaction_princp,interaction_princp2), by="names") 
tableS21
# ---------------------------- *Table S21*  ---------------------------- 


# ----------------------------------------------------------------
#                   Table S22 - Binary Indicator
# -----------------------------------------------------------------

# Creating a binary indicator comparing indices capturing the heterogeneity of users' offline vs. online networks 
# 1. First column: Outgroup regard index, formed as principal component score, as the dependent variable
# 2. Second column: Outgroup regard index, formed as a sum score, as the dependent variable

final_data$majority_group_difference <- ifelse(final_data$largest_majority>=final_data$largest_majority_online, 1, 0) # 1 indicates that offline network is more homogenous
final_data$fract_difference <- ifelse(final_data$fractionalization_offline_index>=final_data$online_ef_index,0,1) # 1 indicates that offline network is more homogenous
final_data$shannon_difference <- ifelse(final_data$shannon_offline>=final_data$shannon_online,0,1)  # 1 indicates that offline network is more homogenous

## PRINCIPAL COMPONENT SCORE

# -------- Majority Group Share
covIn <- as.formula("~freq_usage+majority_group_difference")
binary_ms_pc <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
inter_ms_coef <- binary_ms_pc$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))
inter_ms_se <- binary_ms_pc$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))

# --------- Ethnic Fractionalization
covIn <- as.formula("~freq_usage+fract_difference")
binary_ef_pc <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
inter_ef_coef <- binary_ef_pc$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))
inter_ef_se <- binary_ef_pc$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))

# --------- Shannon Entropy
covIn <- as.formula("~freq_usage+shannon_difference")
binary_sh_pc <- lm_lin(outgroup_princomp~treatment, covariates=covIn, data=final_data)
inter_sh_coef <- binary_sh_pc$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))
inter_sh_se <- binary_sh_pc$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_princomp))

# Table S22 [first column]
coef.vec_intr_onl <- c(inter_sh_coef, inter_ms_coef, inter_ef_coef)
se.vec_intr_onl <- c(inter_sh_se, inter_ms_se, inter_ef_se)
names<-c("Out-Group Index (SE)","Out-Group Index (MS)", "Out-Group Index (EF)")
interaction_princp <- data.frame(names, coef.vec_intr_onl,se.vec_intr_onl)

## SUM SCORE

# -------- Majority Group Share
covIn <- as.formula("~freq_usage+majority_group_difference")
binary_ms_sum <- lm_lin(outgroup_index~treatment, covariates=covIn, data=final_data)
inter_ms_coef2 <- binary_ms_sum$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))
inter_ms_se2 <- binary_ms_sum$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))

# --------- Ethnic Fractionalization
covIn <- as.formula("~freq_usage+fract_difference")
binary_ef_sum <- lm_lin(outgroup_index~treatment, covariates=covIn, data=final_data)
inter_ef_coef2 <- binary_ef_sum$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))
inter_ef_se2 <- binary_ef_sum$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))

# --------- Shannon Entropy
covIn <- as.formula("~freq_usage+shannon_difference")
binary_sh_sum <- lm_lin(outgroup_index~treatment, covariates=covIn, data=final_data)
inter_sh_coef2 <- binary_sh_sum$coefficients[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))
inter_sh_se2 <- binary_sh_sum$std.error[6]/sd((final_data[which(final_data$treatment=="0"),]$outgroup_index))

# Table S22 [second column]
coef.vec_intr_onl2 <- c(inter_sh_coef2, inter_ms_coef2, inter_ef_coef2)
se.vec_intr_onl2 <- c(inter_sh_se2, inter_ms_se2, inter_ef_se2)
names<-c("Out-Group Index (SE)","Out-Group Index (MS)", "Out-Group Index (EF)")
interaction_princp2 <- data.frame(names, coef.vec_intr_onl2,se.vec_intr_onl2)

# ---------------------------- Table S22  ---------------------------- 
tableS22 <- join_all(list(interaction_princp,interaction_princp2), by="names") 
tableS22
# ---------------------------- Table S22 ---------------------------- 

dev.off()

# ---------- SUM INDEX
par(mfrow=c(1,2), mar=c(10, 6, 6, 6)) 
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1)

coef.vec_fig3 <- c(reg_res_mod2("outgroup_index",final_data[final_data$shannon_difference==1,])$estimate, reg_res_mod2("outgroup_index",final_data[final_data$majority_group_difference==1,])$estimate, reg_res_mod2("outgroup_index",final_data[final_data$fract_difference==1,])$estimate)
se.vec_fig3 <-  c(reg_res_mod2("outgroup_index",final_data[final_data$shannon_difference==1,])$std.error, reg_res_mod2("outgroup_index",final_data[final_data$majority_group_difference==1,])$std.error, reg_res_mod2("outgroup_index",final_data[final_data$fract_difference==1,])$std.error)

plot(coef.vec_fig3, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Offline network more homogenous", col=c("black","blue","red"), cex.lab=0.9)
segments(coef.vec_fig3-qnorm(.975)*se.vec_fig3, y.axis, coef.vec_fig3+qnorm(.975)*se.vec_fig3, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

# heterogenous - right panel

coef.vecfigx <- c(reg_res_mod2("outgroup_index",final_data[final_data$shannon_difference==0,])$estimate, reg_res_mod2("outgroup_index",final_data[final_data$majority_group_difference==0,])$estimate, reg_res_mod2("outgroup_index",final_data[final_data$fract_difference==0,])$estimate)
se.vecfigx <-  c(reg_res_mod2("outgroup_index",final_data[final_data$shannon_difference==0,])$std.error, reg_res_mod2("outgroup_index",final_data[final_data$majority_group_difference==0,])$std.error, reg_res_mod2("outgroup_index",final_data[final_data$fract_difference==0,])$std.error)

plot(coef.vecfigx, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Offline network more heterogenous", col=c("black","blue","red"),cex.lab=0.9)
segments(coef.vecfigx-qnorm(.975)*se.vecfigx, y.axis, coef.vecfigx+qnorm(.975)*se.vecfigx, y.axis, lwd = 1.6, c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

legend(x=-5, y=-0.3,
       legend = c("Shannon Entropy","Majority Group Share","Ethnic Fractionalization"), 
       col=c("black","blue","red"), xpd=NA,
       pch = c(19,17,12), 
       bty = T,
       pt.cex = 1.1, 
       cex = 0.8, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE)
title("Outgroup Regard [sum] Index", line = -2,adj=0.5, outer = TRUE)
#dev.off()


# ---------- PC INDEX

dev.off()
par(mfrow=c(1,2), mar=c(10, 6, 6, 6)) 
var.names <- c("Out-Group \n princomp (SE)","Out-Group \n princomp (MS)","Out-Group \n princomp (EF)")
y.axis <- c(length(var.names):1)

coef.vec_fig3 <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$shannon_difference==1,])$estimate, reg_res_mod2("outgroup_princomp",final_data[final_data$majority_group_difference==1,])$estimate, reg_res_mod2("outgroup_princomp",final_data[final_data$fract_difference==1,])$estimate)
se.vec_fig3 <-  c(reg_res_mod2("outgroup_princomp",final_data[final_data$shannon_difference==1,])$std.error, reg_res_mod2("outgroup_princomp",final_data[final_data$majority_group_difference==1,])$std.error, reg_res_mod2("outgroup_princomp",final_data[final_data$fract_difference==1,])$std.error)

plot(coef.vec_fig3, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Offline network more homogenous", col=c("black","blue","red"), cex.lab=0.9)
segments(coef.vec_fig3-qnorm(.975)*se.vec_fig3, y.axis, coef.vec_fig3+qnorm(.975)*se.vec_fig3, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

# heterogenous - right panel
coef.vecfigx <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$shannon_difference==0,])$estimate, reg_res_mod2("outgroup_princomp",final_data[final_data$majority_group_difference==0,])$estimate, reg_res_mod2("outgroup_princomp",final_data[final_data$fract_difference==0,])$estimate)
se.vecfigx <-  c(reg_res_mod2("outgroup_princomp",final_data[final_data$shannon_difference==0,])$std.error, reg_res_mod2("outgroup_princomp",final_data[final_data$majority_group_difference==0,])$std.error, reg_res_mod2("outgroup_princomp",final_data[final_data$fract_difference==0,])$std.error)

plot(coef.vecfigx, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Offline network more heterogenous", col=c("black","blue","red"),cex.lab=0.9)
segments(coef.vecfigx-qnorm(.975)*se.vecfigx, y.axis, coef.vecfigx+qnorm(.975)*se.vecfigx, y.axis, lwd = 1.6, c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)

y.axis <- c(length(var.names):1)
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

legend(x=-5, y=-0.3,
       legend = c("Shannon Entropy","Majority Group Share","Ethnic Fractionalization"), 
       col=c("black","blue","red"), xpd=NA,
       pch = c(19,17,12), 
       bty = T,
       pt.cex = 1.1, 
       cex = 0.8, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE)
title("Outgroup Regard [PC] index", line = -2,adj=0.5, outer = TRUE)

