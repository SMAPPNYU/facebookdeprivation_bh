combined$online_index<-NA

# Obtain proportions to be used in the analysis
combined$percentage_bosniak.3 <- combined$percentage_bosniak.3/100
combined$percentage_serbian.3 <- combined$percentage_serbian.3/100
combined$percentage_croatian.3 <- combined$percentage_croatian.3/100

for (i in 1:length(combined$email)){
  combined$online_index[i] <- 1 - (combined$percentage_bosniak.3[i]^2+combined$percentage_serbian.3[i]^2+combined$percentage_croatian.3[i]^2)
}
combined$online_ef_median_index<-ifelse(combined$online_index>median(na.omit(combined$online_index)),1,0) 

# Shannon Entropy
for (i in 1:length(combined$email)){
  combined$shannon_online[i] <- -(combined$percentage_bosniak.3[i]*log(combined$percentage_bosniak.3[i])+combined$percentage_croatian.3[i]*log(combined$percentage_croatian.3[i])+combined$percentage_serbian.3[i]*log(combined$percentage_serbian.3[i]))}
combined$online_sh_median_index <-ifelse(combined$shannon_online> median(na.omit(combined$shannon_online)),1,0) 


# Majority Share Online
combined$online_ms_median_index <- ifelse(combined$largest_majority_online> median(combined$largest_majority_online) | combined$largest_majority_online==median(combined$largest_majority_online), 1, 0) 

# ----- Excluding overlapping names as robustness check; creation of the three indices
combined$percentage_bosniak.4 <- combined$percentage_bosniak.4/100
combined$percentage_serbian.4 <- combined$percentage_serbian.4/100
combined$percentage_croatian.4 <- combined$percentage_croatian.4/100

# Ethnic Fractionalization
for (i in 1:length(combined$email)){
  combined$online_index_4[i] <- 1 - (combined$percentage_bosniak.4[i]^2+combined$percentage_serbian.4[i]^2+combined$percentage_croatian.4[i]^2)
}
combined$online_ef_median_index<-ifelse(combined$online_index_4>median(na.omit(combined$online_index_4)),1,0) 

# Shannon Entropy
for (i in 1:length(combined$email)){
  combined$shannon_online_4[i] <- -(combined$percentage_bosniak.4[i]*log(combined$percentage_bosniak.4[i])+combined$percentage_croatian.4[i]*log(combined$percentage_croatian.4[i])+combined$percentage_serbian.4[i]*log(combined$percentage_serbian.4[i]))}
combined$online_sh_median_index <-ifelse(combined$shannon_online_4> median(na.omit(combined$shannon_online_4)),1,0) 

#  Majority Share Online
combined$online_ms_median_index_4 <- ifelse(combined$largest_majority_online> median(combined$largest_majority_online_4) | combined$largest_majority_online_4==median(combined$largest_majority_online_4), 1, 0) 

# ----------------------------------------------------------------------------------
# Table S19: Difference between people who sent vs did not send their online data
# ----------------------------------------------------------------------------------

bal.variables<-c("gender","age","educ","employ1","trust_media","freq_news","no_friends","freq_fbnews","politics_int","numb_forums","time_usage", "freq_usage","imp_ethn","imp_family","imp_cntry","imp_relg","imp_hobby","imp_neigh","imp_career")
df <- combined[,bal.variables]

p.values<-cbind(sapply(bal.variables, function (x) t.test(combined[is.na(combined$bosniak.3),][,x], combined[!is.na(combined$bosniak.3),][,x]))[3,])
online_offline_table <- do.call(data.frame, # creating a table combining the mean values and p-values
                    list(OnlineData = apply(df[!is.na(combined$bosniak.3),], 2, mean, na.rm=TRUE),
                    NoOnlineData =  apply(df[is.na(combined$bosniak.3),], 2, mean, na.rm=TRUE),
                    OnlineData = apply(df[!is.na(combined$bosniak.3),], 2, sd, na.rm=TRUE),
                    NoOnlineData =  apply(df[is.na(combined$bosniak.3),], 2, sd, na.rm=TRUE),
                    t.test=p.values))


# ----------------------------------------------------------------------------------
#      Online Network: Ethnic Fractionalization
# ----------------------------------------------------------------------------------
for (i in 1:length(combined$email)){
  combined$online_index[i] <- 1 - (combined$percentage_bosniak.3[i]^2+combined$percentage_serbian.3[i]^2+combined$percentage_croatian.3[i]^2)
}
combined$online_ef_median_index<-ifelse(combined$online_index > median(na.omit(combined$online_index)) | combined$online_index == median(na.omit(combined$online_index)), 1, 0)

# Model 1
m <-lm((outgroup_princomp)~(treatment), data=combined[combined$online_ef_median_index==0,])
online_outgroup_ef_hom_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_ef_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ef_hom_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_ef_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment), data=combined[combined$online_ef_median_index==1,])
online_outgroup_ef_het_coef<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_ef_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ef_het_se<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_ef_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)

# Model 2
covIn <- as.formula("~freq_usage")
outgroup_ef<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[which(combined$online_ef_median_index==0),])
online_outgroup_ef_hom_coef1<-outgroup_ef$coefficients[2] /sd(combined[combined$online_ef_median_index==0 &combined$treatment=="0",]$outgroup_index)
online_outgroup_ef_hom_se1<-outgroup_ef$std.error[2]/ sd(combined[combined$online_ef_median_index==0 &combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_ef<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$online_ef_median_index==1,])
online_outgroup_ef_het_coef1<-outgroup_ef$coefficients[2] /sd(combined[combined$online_ef_median_index==1 &combined$treatment=="0",]$outgroup_index)
online_outgroup_ef_het_se1<outgroup_ef$std.error[2]/ sd(combined[combined$online_ef_median_index==1 &combined$treatment=="0",]$outgroup_index)

# Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
outgroup_ef2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$online_ef_median_index==0,])
online_outgroup_ef_hom_coef2<-outgroup_ef2$coefficients[2] /sd(combined[combined$online_ef_median_index==0 &combined$treatment=="0",]$outgroup_index)
online_outgroup_ef_hom_se2<-outgroup_ef2$std.error[2]/ sd(combined[combined$online_ef_median_index==0 &combined$treatment=="0",]$outgroup_index)

outgroup_ef2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$online_ef_median_index==1,])
online_outgroup_ef_het_coef2<-outgroup_ef2$coefficients[2] /sd(combined[combined$online_ef_median_index==1 &combined$treatment=="0",]$outgroup_index)
online_outgroup_ef_het_se2<-outgroup_ef2$std.error[2]/ sd(combined[combined$online_ef_median_index==1 &combined$treatment=="0",]$outgroup_index)


# ----------------------------------------------------------------------------------
#       Online Majority Share
# ----------------------------------------------------------------------------------

combined$online_ms_median_index <- ifelse(combined$largest_majority_online > median(na.omit(combined$largest_majority_online)) | combined$largest_majority_online==median(na.omit(combined$largest_majority_online)), 0, 1) 
# if majority online is higher, 1 is more homogenous -> go offline, more positive

# Model 1
m <-lm((outgroup_princomp)~(treatment), data=combined[combined$online_ms_median_index==1,])
online_outgroup_ms_het_coef<-online_outgroup_ms_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_ms_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ms_het_se<-online_outgroup_ms_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_ms_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment), data=combined[combined$online_ms_median_index==0,])
online_outgroup_ms_hom_coef<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_ms_median_index=="0" & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ms_hom_coef <-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_ms_median_index=="0" & combined$treatment=="0"),]$outgroup_princomp)

# Model 2
covIn <- as.formula("~freq_usage")
outgroup_ef<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$online_ms_median_index==1,])
online_outgroup_ms_het_coef1<-outgroup_ef$coefficients[2] /sd(combined[combined$online_ms_median_index==1 &combined$treatment=="0",]$outgroup_index)
online_outgroup_ms_het_se1<-outgroup_ef$std.error[2]/ sd(combined[combined$online_ms_median_index==1 &combined$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_pc_ef<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$online_ms_median_index==0,])
online_outgroup_ms_hom_coef2<-outgroup_pc_ef$coefficients[2] /sd(combined[combined$online_ms_median_index==0 &combined$treatment=="0",]$outgroup_princomp)
online_outgroup_ms_hom_se2<-outgroup_pc_ef$std.error[2]/ sd(combined[combined$online_ms_median_index==0 &combined$treatment=="0",]$outgroup_princomp)

# Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[which(combined$online_ms_median_index==1),])
online_outgroup_ms_het_coef1<-m$coefficients[2] /sd(combined[combined$online_ms_median_index==1 & combined$treatment==0,]$outgroup_index)
online_outgroup_ms_het_se1<-m$std.error[2]/ sd(combined[combined$online_ms_median_index==1 &combined$treatment==0,]$outgroup_index)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_pc<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[which(combined$online_ms_median_index==0),])
online_outgroup_ms_hom_coef2<-m_pc$coefficients[2] /sd(combined[combined$online_ms_median_index==0 &combined$treatment=="0",]$outgroup_princomp)
online_outgroup_ms_hom_se2<-m_pc$std.error[2]/ sd(combined[combined$online_ms_median_index==0 &combined$treatment=="0",]$outgroup_princomp)

# ----------------------------------------------------------------------------------
#       Online Shannon Entropy
# ----------------------------------------------------------------------------------

for (i in 1:length(combined$email)){
  combined$shannon_online[i] <- -(combined$percentage_bosniak.3[i]*log(combined$percentage_bosniak.3[i])+combined$percentage_croatian.3[i]*log(combined$percentage_croatian.3[i])+combined$percentage_serbian.3[i]*log(combined$percentage_serbian.3[i]))}
combined$online_sh_median_index <-ifelse(combined$shannon_online > median(na.omit(combined$shannon_online))|combined$shannon_onlin==median(na.omit(combined$shannon_online)),1,0) 

# Model 1
m <-lm((outgroup_princomp)~(treatment), data=combined[combined$online_sh_median_index==1,])
online_outgroup_se_het_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_sh_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_se_het_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_sh_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment), data=combined[combined$online_sh_median_index==0,])
online_outgroup_se_hom_coef<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_sh_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_se_hom_se<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_sh_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)

# Model 2
covIn <- as.formula("~freq_usage")
outgroup_ef<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$online_sh_median_index==1,])
online_outgroup_se_hom_coef1<-outgroup_ef$coefficients[2] /sd(combined[which(combined$online_sh_median_index==1 &combined$treatment=="0"),]$outgroup_index)
online_outgroup_se_hom_se1<-outgroup_ef$std.error[2]/ sd(combined[which(combined$online_sh_median_index==1 &combined$treatment=="0"),]$outgroup_index)

covIn <- as.formula("~freq_usage")
outgroup_pc_ef<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$online_sh_median_index==0,])
online_outgroup_se_het_coef1<-outgroup_pc_ef$coefficients[2] /sd(combined[which(combined$online_sh_median_index==0 &combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_se_het_se1<-outgroup_pc_ef$std.error[2]/ sd(combined[which(combined$online_sh_median_index==0 &combined$treatment=="0"),]$outgroup_princomp)

# Model 3
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[which(combined$online_sh_median_index==1),])
online_outgroup_se_hom_coef2<-m$coefficients[2] /sd(combined[combined$online_sh_median_index==1 & combined$treatment==0,]$outgroup_index)
online_outgroup_se_hom_se2<-m$std.error[2]/ sd(combined[combined$online_sh_median_index==1 &combined$treatment==0,]$outgroup_index)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn")
m_pc<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[which(combined$online_sh_median_index)==0,])
online_outgroup_se_het_coef2<-m_pc$coefficients[2] /sd(combined[combined$online_sh_median_index==0 &combined$treatment=="0",]$outgroup_princomp)
online_outgroup_se_het_coef2<-m_pc$std.error[2]/ sd(combined[combined$online_sh_median_index==0 &combined$treatment=="0",]$outgroup_princomp)


# ----------------------------------------------------------------------------------
#       Plotting effects within online networks
# ----------------------------------------------------------------------------------

par(mfrow=c(1,2)) 
par(mar=c(9, 5, 6, 1))

coef.vec.2<-c(online_outgroup_se_hom_coef2,online_outgroup_ms_hom_coef1,online_outgroup_ef_hom_coef1)
se.vec.2<-c(online_outgroup_se_hom_ef1,online_outgroup_ms_hom_ef1,online_outgroup_ef_hom_ef1)
adjust = 0.26
plot(coef.vec.2, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 0.72,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Homogenous", col=c("black","blue","red"),cex.lab=0.7, cex.main=0.85) 
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis, coef.vec.2+qnorm(.975)*se.vec.2, y.axis, lwd = 1.6, c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.8, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.8) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
#par(mfrow=c(1,2),oma=c(5,0,0,0),xpd=NA)
#coord <- par("usr")

coef.vec.2<-c(online_outgroup_se_het_coef1,online_outgroup_ms_het_coef1,online_outgroup_ef_het_coef1)
coef.vec.2<-c(online_outgroup_se_het_ef1,online_outgroup_ms_het_ef1,online_outgroup_ef_het_ef1)

#coef.vec<- c(0.04725468,  0.02827728, 0.048795)
#se.vec <- c( 0.2354524,0.2358452,  0.2318477)
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Heterogeneous", col=c("black","blue","red"), cex.lab=0.7, cex.main=0.85)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.8, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.8) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis


legend(x=-5, y=-0.5,
       legend = c("Shannon Entropy", "Majority Group Share","Ethnic Fractionalization"), 
       col=c("black","blue","red"), xpd=NA,
       pch = c(19,17,12), 
       bty = T,
       pt.cex = 1, 
       cex = 0.4, 
       text.col = "black",   box.lty=1.2, box.lwd=0.5,
       horiz=TRUE)
title("Online Networks", line = -1, cex.main=0.95  ,outer = TRUE)


# ---------------------------------------------------------
#    INTERACTION w/ CONTINOUS MEASURE [online]: TABLE S20
# ---------------------------------------------------------
# ----------- ethnic fractionalization
for (i in 1:length(combined$email)){
  combined$online_index[i] <- 1 - (combined$percentage_bosniak.3[i]^2+combined$percentage_serbian.3[i]^2+combined$percentage_croatian.3[i]^2)
}
combined$online_index_rev <- - combined$online_index 

# 1 is more homogenous
ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_index_rev+ treatment:online_index, data=combined)
ef_interaction_onl_coef<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_onl_se<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+online_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_interaction_onl_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_onl_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+online_index_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_interaction_onl_coef2<-m$coefficients[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_interaction_onl_se2<-m$std.error[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# --------- shannon entropy
combined$shannon_online_rev<--combined$shannon_online

se_onl_interaction <- lm(outgroup_princomp ~ treatment+ shannon_online_rev+ treatment:shannon_online,
                         data=combined)
se_interaction_onl_coef<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_onl_se<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+shannon_online_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_interaction_onl_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_onl_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+online_index_rev+shannon_online_rev")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_interaction_onl_coef2<-m$coefficients[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_onl_coef2<-m$std.error[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# ---------  majority group share
combined$online_ms_median_index <- ifelse(combined$largest_majority_online > median(combined$largest_majority_online ) | combined$largest_majority_online==median(combined$largest_majority_online), 1, 0) 
ms_onl_interaction <- lm(outgroup_princomp ~ treatment+largest_majority_online + treatment:largest_majority_online,
                         data=combined)
ms_interaction_onl_coef<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_onl_se<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

covIn <- as.formula("~freq_usage+largest_majority_online")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_interaction_onl_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_onl_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+online_index_rev+largest_majority_online")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_interaction_onl_coef2<-m$coefficients[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_onl_se2<-m$std.error[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)


# Creating a table
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

# ------------------------------------------------
#    ONLINE NETWORK: Above/Below Median [Table 21]
# -------------------------------------------------

# ----- Ethnic Fractionalization Online
for (i in 1:length(combined$email)){
  combined$online_index[i] <- 1 - (combined$percentage_bosniak.3[i]^2+combined$percentage_serbian.3[i]^2+combined$percentage_croatian.3[i]^2)
}
combined$online_ef_median_index<-ifelse(combined$online_index >  median(na.omit(combined$online_index)) |  combined$online_index== median(na.omit(combined$online_index)), 0, 1)
# 1 is more homogenous

# Model 1: no covariates
ef_onl_interaction_rev_m <- lm(outgroup_princomp ~ treatment +online_ef_median_index+ treatment:online_ef_median_index, data=combined)
ef_median_intr_onl_coef<-coeftest(ef_onl_interaction_rev_m, vcov = vcovHC(ef_onl_interaction_rev_m, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_onl_se<-coeftest(ef_onl_interaction_rev_m, vcov = vcovHC(ef_onl_interaction_rev_m, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2: frequency of usage
covIn <- as.formula("~freq_usage+online_ef_median_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_median_intr_onl_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_onl_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3: all covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+online_index_rev+online_ef_median_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ef_median_intr_onl_coef1<-m$coefficients[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_onl_se1<-m$std.error[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)


# --------- Shannon Entropy
for (i in 1:length(combined$email)){
  combined$shannon_online[i] <- -(combined$percentage_bosniak.3[i]*log(combined$percentage_bosniak.3[i])+combined$percentage_croatian.3[i]*log(combined$percentage_croatian.3[i])+combined$percentage_serbian.3[i]*log(combined$percentage_serbian.3[i]))}
combined$online_sh_median_index <-ifelse(combined$shannon_online> median(na.omit(combined$shannon_online)),0,1) 

# Model 1: no covariates
se_onl_interaction_m <- lm(outgroup_princomp ~ treatment+ online_sh_median_index+ treatment:online_sh_median_index,
                           data=combined)
se_median_intr_onl_coef<-coeftest(se_onl_interaction_m, vcov = vcovHC(se_onl_interaction_m, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_onl_se<-coeftest(se_onl_interaction_m, vcov = vcovHC(se_onl_interaction_m, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 2: with frequency of usage
covIn <- as.formula("~freq_usage+online_sh_median_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_median_intr_onl_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_onl_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+online_index_rev+online_sh_median_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
se_median_intr_onl_coef2<-m$coefficients[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_onl_se2<-m$std.error[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# ---------  Majority Group Share Online Index
combined$online_ms_median_index <- ifelse(combined$largest_majority_online > median(na.omit(combined$largest_majority_online)) | combined$largest_majority_online==median(na.omit(combined$largest_majority_online)), 1, 0) 

# Model 1: no covariates
ms_onl_interaction <- lm(outgroup_princomp ~ treatment+online_ms_median_index + treatment:online_ms_median_index,
                         data=combined)
ms_median_intr_onl_coef<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_onl_se<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,2]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 2: with frequency of usage
covIn <- as.formula("~freq_usage+online_ms_median_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_median_intr_onl_coef1<-m$coefficients[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_onl_se1<-m$std.error[6]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 3: full covariates
covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+online_index_rev+online_ms_median_index")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined)
ms_median_intr_onl_coef2<-m$coefficients[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_onl_se2<-m$std.error[12]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Creating a table [Table 21]
coef.vec_intr_onl <- c(ef_median_intr_onl_coef,
                       ms_median_intr_onl_coef,
                       se_median_intr_onl_coef)
se.vec_intr_onl <- c(ef_median_intr_onl_se,
                     ms_median_intr_onl_se,
                     se_median_intr_onl_se)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr <- data.frame(names, coef.vec_intr_onl,se.vec_intr_onl)

coef.vec_intr_onl1 <- c(ef_median_intr_onl_coef1,
                       ms_median_intr_onl_coef1,
                       se_median_intr_onl_coef1)
se.vec_intr_onl1 <- c(ef_median_intr_onl_se1,
                     ms_median_intr_onl_se1,
                     se_median_intr_onl_se1)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr1 <- data.frame(names, coef.vec_intr_onl1,se.vec_intr_onl1)

coef.vec_intr_onl2 <- c(ef_median_intr_onl_coef2,
                        ms_median_intr_onl_coef2,
                        se_median_intr_onl_coef2)
se.vec_intr_onl2 <- c(ef_median_intr_onl_se2,
                      ms_median_intr_onl_se2,
                      se_median_intr_onl_se2s)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr2 <- data.frame(names, coef.vec_intr_onl2,se.vec_intr_onl2)
