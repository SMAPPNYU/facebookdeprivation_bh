combined$online_index<-NA

# Obtain proportions to be used in the analysis
combined$percentage_bosniak.3 <- combined$percentage_bosniak.3/100
combined$percentage_serbian.3 <- combined$percentage_serbian.3/100
combined$percentage_croatian.3 <- combined$percentage_croatian.3/100

# Ethnic heterogeneity: diversity continous measure and binary equal and above/below median index
for (i in 1:length(combined$code)){
  combined$online_index[i] <- 1 - (combined$percentage_bosniak.3[i]^2+combined$percentage_serbian.3[i]^2+combined$percentage_croatian.3[i]^2)
}
combined$online_ef_median_index<-ifelse(combined$online_index>median(na.omit(combined$online_index))|combined$online_index==median(na.omit(combined$online_index)),1,0) 

# Shannon Entropy:  diversity continous measure and binary equal and above/below median index
for (i in 1:length(combined$code)){
  combined$shannon_online[i] <- -(combined$percentage_bosniak.3[i]*log(combined$percentage_bosniak.3[i])+combined$percentage_croatian.3[i]*log(combined$percentage_croatian.3[i])+combined$percentage_serbian.3[i]*log(combined$percentage_serbian.3[i]))}
combined$online_sh_median_index <-ifelse(combined$shannon_online> median(na.omit(combined$shannon_online))|combined$shannon_online==median(na.omit(combined$shannon_online)),1,0) 


# Majority Share Online:  diversity continous measure and binary equal and above/below median index
combined$online_ms_median_index <- ifelse(combined$largest_majority_online> median(combined$largest_majority_online) | combined$largest_majority_online==median(combined$largest_majority_online), 1, 0) 

# ----- Excluding overlapping names as robustness check; creation of the three indices
combined$percentage_bosniak.4 <- combined$percentage_bosniak.4/100
combined$percentage_serbian.4 <- combined$percentage_serbian.4/100
combined$percentage_croatian.4 <- combined$percentage_croatian.4/100

# Ethnic Fractionalization
for (i in 1:length(combined$email)){
  combined$online_index_4[i] <- 1 - (combined$percentage_bosniak.4[i]^2+combined$percentage_serbian.4[i]^2+combined$percentage_croatian.4[i]^2)
}
combined$online_ef_median_index4<-ifelse(combined$online_index_4>median(na.omit(combined$online_index_4))|combined$online_index_4==median(na.omit(combined$online_index_4)),1,0) 

# Shannon Entropy
for (i in 1:length(combined$email)){
  combined$shannon_online_4[i] <- -(combined$percentage_bosniak.4[i]*log(combined$percentage_bosniak.4[i])+combined$percentage_croatian.4[i]*log(combined$percentage_croatian.4[i])+combined$percentage_serbian.4[i]*log(combined$percentage_serbian.4[i]))}
combined$online_sh_median_index <-ifelse(combined$shannon_online_4> median(na.omit(combined$shannon_online_4))|combined$shannon_online_4==median(na.omit(combined$shannon_online_4)),1,0) 

#  Majority Share Online
combined$online_ms_median_index_4 <- ifelse(combined$largest_majority_online> median(combined$largest_majority_online_4) | combined$largest_majority_online_4==median(combined$largest_majority_online_4), 1, 0) 

# ----------------------------------------------------------------------------------
# Table S17: Difference between people who sent vs did not send their online data
# ----------------------------------------------------------------------------------

bal.variables<-c("gender","age","educ","employ1","trust_media.1","freq_news","freq_fbnews","politics_int","numb_forums","time_usage", "freq_usage","imp_ethn","imp_family","imp_cntry","imp_relg","imp_hobby","imp_neigh","imp_career")
df <- combined[,bal.variables]

p.values<-cbind(sapply(bal.variables, function (x) t.test(combined[is.na(combined$bosniak.3),][,x], combined[!is.na(combined$bosniak.3),][,x]))[3,])
online_offline_table <- do.call(data.frame, # creating a table combining the mean values and p-values
                    list(OnlineData = apply(df[!is.na(combined$bosniak.3),], 2, mean, na.rm=TRUE),
                    NoOnlineData =  apply(df[is.na(combined$bosniak.3),], 2, mean, na.rm=TRUE),
                    OnlineData = apply(df[!is.na(combined$bosniak.3),], 2, sd, na.rm=TRUE),
                    NoOnlineData =  apply(df[is.na(combined$bosniak.3),], 2, sd, na.rm=TRUE),
                    t.test=p.values))
online_offline_table

# ----------------------------------------------------------------------------------
#      Online Network: Ethnic Fractionalization
# ----------------------------------------------------------------------------------
for (i in 1:length(combined$code)){
  combined$online_index[i] <- 1 - (combined$percentage_bosniak.3[i]^2+combined$percentage_serbian.3[i]^2+combined$percentage_croatian.3[i]^2)
}
combined$online_ef_median_index<-ifelse(combined$online_index > median(na.omit(combined$online_index)) | combined$online_index == median(na.omit(combined$online_index)), 1, 0)

# Model 1
m <-lm((outgroup_princomp)~(treatment), data=combined[which(combined$online_ef_median_index==0),])
online_outgroup_ef_hom_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_ef_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ef_hom_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_ef_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment), data=combined[which(combined$online_ef_median_index==1),])
online_outgroup_ef_het_coef<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_ef_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ef_het_se<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_ef_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)

# Model 2
m <-lm((outgroup_princomp)~(treatment+freq_usage), data=combined[which(combined$online_ef_median_index==0),])
online_outgroup_ef_hom_coef1<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_ef_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ef_hom_se1<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_ef_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment+freq_usage), data=combined[which(combined$online_ef_median_index==1),])
online_outgroup_ef_het_coef1<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_ef_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ef_het_se1<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_ef_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)


# Model 3
m <-lm((outgroup_princomp)~(treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry), data=combined[which(combined$online_ef_median_index==0),])
online_outgroup_ef_hom_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_ef_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ef_hom_se2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_ef_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment+freq_usage+freq_usage:treatment+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry), data=combined[which(combined$online_ef_median_index==1),])
online_outgroup_ef_het_coef2<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_ef_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ef_het_se2<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_ef_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)


# ----------------------------------------------------------------------------------
#       Online Majority Share
# ----------------------------------------------------------------------------------

combined$online_ms_median_index <- ifelse(combined$largest_majority_online > median(na.omit(combined$largest_majority_online)) | combined$largest_majority_online==median(na.omit(combined$largest_majority_online)), 0, 1) 
# if majority online is higher, 1 is more homogenous -> go offline, more positive

# Model 1
m <-lm((outgroup_princomp)~(treatment), data=combined[which(combined$online_ms_median_index==1),])
online_outgroup_ms_het_coef<-online_outgroup_ms_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_ms_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ms_het_se<-online_outgroup_ms_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_ms_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment), data=combined[which(combined$online_ms_median_index==0),])
online_outgroup_ms_hom_coef<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_ms_median_index=="0" & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ms_hom_coef <-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_ms_median_index=="0" & combined$treatment=="0"),]$outgroup_princomp)

# Model 2
m <-lm((outgroup_princomp)~(treatment+freq_usage), data=combined[which(combined$online_ms_median_index==1),])
online_outgroup_ms_het_coef1<-online_outgroup_ms_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_ms_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ms_het_se1<-online_outgroup_ms_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_ms_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment+freq_usage), data=combined[which(combined$online_ms_median_index==0),])
online_outgroup_ms_hom_coef1<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_ms_median_index=="0" & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ms_hom_se1 <-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_ms_median_index=="0" & combined$treatment=="0"),]$outgroup_princomp)

# Model 3
m <-lm((outgroup_princomp)~(treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry), data=combined[which(combined$online_ms_median_index==1),])
online_outgroup_ms_het_coef2<-online_outgroup_ms_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_ms_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ms_het_se2<-online_outgroup_ms_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_ms_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry), data=combined[which(combined$online_ms_median_index==0),])
online_outgroup_ms_hom_coef2<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_ms_median_index=="0" & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_ms_hom_se2<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_ms_median_index=="0" & combined$treatment=="0"),]$outgroup_princomp)


# ----------------------------------------------------------------------------------
#       Online Shannon Entropy
# ----------------------------------------------------------------------------------

for (i in 1:length(combined$code)){
  combined$shannon_online[i] <- -(combined$percentage_bosniak.3[i]*log(combined$percentage_bosniak.3[i])+combined$percentage_croatian.3[i]*log(combined$percentage_croatian.3[i])+combined$percentage_serbian.3[i]*log(combined$percentage_serbian.3[i]))}
combined$online_sh_median_index <-ifelse(combined$shannon_online > median(na.omit(combined$shannon_online))|combined$shannon_online==median(na.omit(combined$shannon_online)),1,0) 

# Model 1
m <-lm((outgroup_princomp)~(treatment), data=combined[which(combined$online_sh_median_index==1),])
online_outgroup_se_het_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_sh_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_se_het_se<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_sh_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment), data=combined[which(combined$online_sh_median_index==0),])
online_outgroup_se_hom_coef<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_sh_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_se_hom_se<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_sh_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)

# Model 2
m <-lm((outgroup_princomp)~(treatment+freq_usage), data=combined[which(combined$online_sh_median_index==1),])
online_outgroup_se_het_coef1<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_sh_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_se_het_se1<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_sh_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment+freq_usage), data=combined[which(combined$online_sh_median_index==0),])
online_outgroup_se_hom_coef1<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_sh_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_se_hom_se1<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_sh_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)

# Model 3
m <-lm((outgroup_princomp)~(treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry), data=combined[which(combined$online_sh_median_index==1),])
online_outgroup_se_het_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$online_sh_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_se_het_se2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$online_sh_median_index==1 & combined$treatment=="0"),]$outgroup_princomp)

m_pc <-lm((outgroup_princomp)~(treatment+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry), data=combined[which(combined$online_sh_median_index==0),])
online_outgroup_se_hom_coef2<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,1]/sd(combined[which(combined$online_sh_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)
online_outgroup_se_hom_se2<-coeftest(m_pc, vcov = vcovHC(m_pc, type = "HC1"))[2,2]/sd(combined[which(combined$online_sh_median_index==0 & combined$treatment=="0"),]$outgroup_princomp)


# ----------------------------------------------------------------------------------
#       Figure S5: Plotting effects within online networks
# ----------------------------------------------------------------------------------
dev.off()
pdf(file = "/Users/nejlaasimovic/Desktop/figureS5.pdf",   # The directory you want to save the file in
    width = 5.8, # The width of the plot in inches
    height = 5.2) s
par(mfrow=c(1,2)) s
par(mar=c(9, 5, 6, 1))
coef.vec.2<-c(online_outgroup_ef_hom_coef1,online_outgroup_ms_hom_coef1,online_outgroup_se_hom_coef1)
se.vec.2<-c(online_outgroup_se_het_se1,online_outgroup_ms_hom_se1,online_outgroup_se_hom_se1)
adjust = 0.26
plot(coef.vec.2, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1,#plot coefficients as points, turning off axes and labels.
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
se.vec.2<-c(online_outgroup_ef_het_se1,online_outgroup_ms_het_se1,online_outgroup_ef_het_se1)
#coef.vec<- c(0.04725468,  0.02827728, 0.048795)
#se.vec <- c( 0.2354524,0.2358452,  0.2318477)
plot(coef.vec.2, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Heterogeneous", col=c("black","blue","red"), cex.lab=0.7, cex.main=0.85)
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis, coef.vec.2+qnorm(.975)*se.vec.2, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.8, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.8) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

legend(x=-4,  y=-0.1,
       legend = c("Shannon Entropy", "Majority Group Share","Ethnic Fractionalization"), 
       col=c("black","blue","red"), xpd=NA,
       pch = c(19,17,12), 
       bty = T,
       pt.cex = 1, 
       cex = 0.4, 
       text.col = "black",   box.lty=1.2, box.lwd=0.5,
       horiz=TRUE)
title("Online Networks", line = -1, cex.main=0.95  ,outer = TRUE)
dev.off()

# ---------------------------------------------------------
#    INTERACTION w/ CONTINOUS MEASURE [online]: TABLE S18
# ---------------------------------------------------------
# ----------- ethnic fractionalization
for (i in 1:length(combined$email)){
  combined$online_index[i] <- 1 - (combined$percentage_bosniak.3[i]^2+combined$percentage_serbian.3[i]^2+combined$percentage_croatian.3[i]^2)
}
combined$online_index_rev <- - combined$online_index 

# 1 is more homogenous
ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_index_rev+ treatment:online_index, data=combined)
ef_interaction_onl_coef<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[4,1]/sd(combined[which(combined$treatment==0),]$outgroup_princomp)
ef_interaction_onl_se<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[4,2]/sd((combined[which(combined$treatment==0),]$outgroup_princomp))

ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_index_rev+ treatment:online_index+freq_usage, data=combined)
ef_interaction_onl_coef1<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[5,1]/sd(combined[which(combined$treatment==0),]$outgroup_princomp)
ef_interaction_onl_se1<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[5,2]/sd((combined[which(combined$treatment==0),]$outgroup_princomp))

ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_index_rev+ treatment:online_index+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined)
ef_interaction_onl_coef2<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[15,1]/sd(combined[which(combined$treatment==0),]$outgroup_princomp)
ef_interaction_onl_se2<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[15,2]/sd((combined[which(combined$treatment==0),]$outgroup_princomp))



# --------- shannon entropy
combined$shannon_online_rev<--combined$shannon_online

se_onl_interaction <- lm(outgroup_princomp ~ treatment+ shannon_online_rev+ treatment:shannon_online,
                         data=combined)
se_interaction_onl_coef<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_onl_se<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

se_onl_interaction <- lm(outgroup_princomp ~ treatment+ shannon_online_rev+ treatment:shannon_online+freq_usage,
                         data=combined)
se_interaction_onl_coef1<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[5,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_onl_se1<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[5,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

se_onl_interaction <- lm(outgroup_princomp ~ treatment+ shannon_online_rev+ treatment:shannon_online+freq_usage
                         +as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+online_index_rev,
                         data=combined)
se_interaction_onl_coef2<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[15,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_onl_se2<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[15,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))


# ---------  majority group share
combined$online_ms_median_index <- ifelse(combined$largest_majority_online > median(combined$largest_majority_online) | combined$largest_majority_online==median(combined$largest_majority_online), 1, 0) 

ms_onl_interaction <- lm(outgroup_princomp ~ treatment+largest_majority_online + treatment:largest_majority_online,
                         data=combined)
ms_interaction_onl_coef<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_onl_se<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

ms_onl_interaction <- lm(outgroup_princomp ~ treatment+largest_majority_online + treatment:largest_majority_online+freq_usage,
                         data=combined)
ms_interaction_onl_coef1<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[5,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_onl_se1<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[5,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

ms_onl_interaction <- lm(outgroup_princomp ~ treatment+largest_majority_online + treatment:largest_majority_online+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry,
                         data=combined)
ms_interaction_onl_coef2<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[15,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_onl_se2<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[15,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))


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
merged_intr2
# -------- *Table S18* ------------
TableS18<-join_all(list(merged_intr, merged_intr1, merged_intr2), by="names") 
TableS18
# -------- *Table S18* ------------

# ----------------------------------------------------------
#    ONLINE NETWORK: Equal and above/Below Median [Table 19]
# ------------------------------------------------------------

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
ef_onl_interaction_rev_m <- lm(outgroup_princomp ~ treatment +online_ef_median_index+ treatment:online_ef_median_index+freq_usage, data=combined)
ef_median_intr_onl_coef1<-coeftest(ef_onl_interaction_rev_m, vcov = vcovHC(ef_onl_interaction_rev_m, type = "HC1"))[5,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_onl_se1<-coeftest(ef_onl_interaction_rev_m, vcov = vcovHC(ef_onl_interaction_rev_m, type = "HC1"))[5,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# Model 3: all covariates
ef_onl_interaction_rev_m <- lm(outgroup_princomp ~ treatment +online_ef_median_index+ treatment:online_ef_median_index+freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry+online_ef_median_index, data=combined)
ef_median_intr_onl_coef2<-coeftest(ef_onl_interaction_rev_m, vcov = vcovHC(ef_onl_interaction_rev_m, type = "HC1"))[15,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ef_median_intr_onl_se2<-coeftest(ef_onl_interaction_rev_m, vcov = vcovHC(ef_onl_interaction_rev_m, type = "HC1"))[15,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# --------- Shannon Entropy
for (i in 1:length(combined$email)){
  combined$shannon_online[i] <- -(combined$percentage_bosniak.3[i]*log(combined$percentage_bosniak.3[i])+combined$percentage_croatian.3[i]*log(combined$percentage_croatian.3[i])+combined$percentage_serbian.3[i]*log(combined$percentage_serbian.3[i]))}
combined$online_sh_median_index <-ifelse(combined$shannon_online> median(na.omit(combined$shannon_online)),0,1) 

# Model 1: no covariates
se_onl_interaction_m <- lm(outgroup_princomp ~ treatment+ online_sh_median_index+ treatment:online_sh_median_index,
                           data=combined)
se_median_intr_onl_coef<-coeftest(se_onl_interaction_m, vcov = vcovHC(se_onl_interaction_m, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_onl_se<-coeftest(se_onl_interaction_m, vcov = vcovHC(se_onl_interaction_m, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

se_onl_interaction_m <- lm(outgroup_princomp ~ treatment+ online_sh_median_index+ treatment:online_sh_median_index+freq_usage,
                           data=combined)
se_median_intr_onl_coef1<-coeftest(se_onl_interaction_m, vcov = vcovHC(se_onl_interaction_m, type = "HC1"))[5,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_onl_se1<-coeftest(se_onl_interaction_m, vcov = vcovHC(se_onl_interaction_m, type = "HC1"))[5,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

se_onl_interaction_m <- lm(outgroup_princomp ~ treatment+treatment:online_sh_median_index+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry+online_sh_median_index,
                           data=combined)
se_median_intr_onl_coef2<-coeftest(se_onl_interaction_m, vcov = vcovHC(se_onl_interaction_m, type = "HC1"))[15,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_median_intr_onl_se2<-coeftest(se_onl_interaction_m, vcov = vcovHC(se_onl_interaction_m, type = "HC1"))[15,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

# ---------  Majority Group Share Online Index
combined$online_ms_median_index <- ifelse(combined$largest_majority_online > median(na.omit(combined$largest_majority_online)) | combined$largest_majority_online==median(na.omit(combined$largest_majority_online)), 1, 0) 

# Model 1: no covariates
ms_onl_interaction <- lm(outgroup_princomp ~ treatment+online_ms_median_index + treatment:online_ms_median_index,
                         data=combined)
ms_median_intr_onl_coef<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_onl_se<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,2]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Model 2: with frequency of usage
ms_onl_interaction <- lm(outgroup_princomp ~ treatment+online_ms_median_index + treatment:online_ms_median_index+freq_usage,
                         data=combined)
ms_median_intr_onl_coef1<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[5,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_onl_se1<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[5,2]/sd(combined[combined$treatment==0,]$outgroup_princomp)


# Model 3: full covariates
ms_onl_interaction <- lm(outgroup_princomp ~ treatment+online_ms_median_index + treatment:online_ms_median_index+freq_usage
                         +as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined)
ms_median_intr_onl_coef2<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[15,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_median_intr_onl_se2<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[15,2]/sd(combined[combined$treatment==0,]$outgroup_princomp)

# Creating a table [Table 19]
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
                      se_median_intr_onl_se2)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr2 <- data.frame(names, coef.vec_intr_onl2,se.vec_intr_onl2)

# -------- *Table S19* ------------
tableS19<-join_all(list(merged_intr, merged_intr1, merged_intr2), by="names") 
tableS19
# -------- *Table S19* ------------

# ------------------------------------
#             FIGURE S6
# ------------------------------------

# ------ Treatment of deactivation on the subsample of users who shared their online data
online_data <- combined[!is.na(combined$bosniak.3),]

# Model 2, adjusting for frequency of usage for all the 5 indicators of outgroup attitudes:
model_figS6<- lm(outgroup_princomp ~ treatment, data=online_data)
summary(model_figS6)
model_figS6_coef<-coeftest(model_figS6, vcov = vcovHC(model_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$outgroup_princomp)
model_figS6_se<-coeftest(model_figS6, vcov = vcovHC(model_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$outgroup_princomp))


# feeling thermoemeter
ft_figS6<- lm(ft ~ treatment, data=online_data)
summary(ft_figS6)
ft_figS6_coef<-coeftest(ft_figS6, vcov = vcovHC(ft_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$ft)
ft_figS6_se<-coeftest(ft_figS6, vcov = vcovHC(ft_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$ft))

# social distnace
sd_figS6<- lm(sd ~ treatment, data=online_data)
summary(sd_figS6)
sd_figS6_coef<-coeftest(sd_figS6, vcov = vcovHC(sd_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$sd)
sd_figS6_se<-coeftest(sd_figS6, vcov = vcovHC(sd_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$sd))


# cooperation
multi_stat_figS6<- lm(multi_stat ~ treatment, data=online_data)
summary(multi_stat_figS6)
multi_stat_figS6_coef<-coeftest(multi_stat_figS6, vcov = vcovHC(multi_stat_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$multi_stat)
multi_stat_figS6_se<-coeftest(multi_stat_figS6, vcov = vcovHC(multi_stat_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$multi_stat))

# perceptions of other's evaluations
othchrct_combined_figS6<- lm(othchrct_combined~ treatment, data=online_data)
summary(othchrct_combined_figS6)
othchrct_combined_figS6_coef<-coeftest(othchrct_combined_figS6, vcov = vcovHC(othchrct_combined_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$othchrct_combined)
othchrct_combined_figS6_se<-coeftest(othchrct_combined_figS6, vcov = vcovHC(othchrct_combined_figS6, type = "HC1"))[4]/sd(online_data[which(online_data$treatment==0),]$othchrct_combined)

# other's characteristics
inchrct_combined_figS6<- lm(inchrct_combined ~ treatment, data=online_data)
summary(inchrct_combined_figS6)
inchrct_combined_figS6_coef<-coeftest(inchrct_combined_figS6, vcov = vcovHC(inchrct_combined_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$inchrct_combined)
inchrct_combined_figS6_se<-coeftest(inchrct_combined_figS6, vcov = vcovHC(inchrct_combined_figS6, type = "HC1"))[4]/sd(online_data[which(online_data$treatment==0),]$inchrct_combined)


# Plotting Figure S6 - top part
dev.off()
pdf(file = "/Users/nejlaasimovic/Desktop/figureS6_top.pdf",   # The directory you want to save the file in
    width = 5.8, # The width of the plot in inches
    height = 5.2) 
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#
par(mar=c(10,8,3,1)) #20 je desna strana
coef.vec<-c(ft_figS6_coef, sd_figS6_coef, multi_stat_figS6_coef, othchrct_combined_figS6_coef, inchrct_combined_figS6_coef,model_figS6_coef)
se.vec<-c(ft_figS6_se, sd_figS6_se, multi_stat_figS6_se, othchrct_combined_figS6_se, inchrct_combined_figS6_se,model_figS6_se)
var.names <- c("Feeling thermometer","Social closeness","Cooperation","Perception of \n out-group evaluations","Out-group traits","Out-group regard index")
results_df <- data.frame(term=var.names, estimate=coef.vec,std.error=se.vec)
y.axis <- c(length(coef.vec):1)
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)" , cex.lab=2.3,ylab = "", pch = 19,cex = 1,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), main = "", col=c("red","red","red","red","red","red","red","red"),cex.lab=1)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.2, col = c("red","red","red","red","red","red","red","red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(7,0.5,0.5))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1.2) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="blue",lty=2,lwd=1.6)

# ----- Figure S6 (news + swb): Treatment of deactivation on the subsample of users who shared their online data-)
online_data <- combined[!is.na(combined$bosniak.3),]

# news
c_news_figS6<- lm(c_news ~ treatment, data=online_data)
summary(c_news_figS6)
c_news_figS6_coef<-coeftest(c_news_figS6, vcov = vcovHC(c_news_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$c_news)
c_news_figS6_se<-coeftest(c_news_figS6, vcov = vcovHC(c_news_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$c_news))

# swb index + associated indicators
swb_figS6<- lm(swb ~ treatment, data=online_data)
summary(swb_figS6)
swb_figS6_coef<-coeftest(swb_figS6, vcov = vcovHC(swb_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$swb)
swb_figS6_se<-coeftest(swb_figS6, vcov = vcovHC(swb_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$swb))

satisf_figS6<- lm(satisf ~ treatment, data=online_data)
summary(satisf_figS6)
satisf_figS6_coef<-coeftest(satisf_figS6, vcov = vcovHC(satisf_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$satisf)
satisf_figS6_se<-coeftest(satisf_figS6, vcov = vcovHC(satisf_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$satisf))

depression_figS6<- lm(depression ~ treatment, data=online_data)
summary(depression_figS6)
depression_figS6_coef<-coeftest(depression_figS6, vcov = vcovHC(depression_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$depression)
depression_figS6_se<-coeftest(depression_figS6, vcov = vcovHC(depression_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$depression))

loneliness_figS6<- lm(loneliness ~ treatment, data=online_data)
summary(loneliness_figS6)
loneliness_figS6_coef<-coeftest(loneliness_figS6, vcov = vcovHC(loneliness_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$loneliness)
loneliness_figS6_se<-coeftest(loneliness_figS6, vcov = vcovHC(loneliness_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$loneliness))

nerv_figS6<- lm(nerv ~ treatment, data=online_data)
summary(nerv_figS6)
nerv_figS6_coef<-coeftest(nerv_figS6, vcov = vcovHC(nerv_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$nerv)
nerv_figS6_se<-coeftest(nerv_figS6, vcov = vcovHC(nerv_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$nerv))

boredom_figS6<- lm(boredom ~ treatment, data=online_data)
summary(boredom_figS6)
boredom_figS6_coef<-coeftest(boredom_figS6, vcov = vcovHC(boredom_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$boredom)
boredom_figS6_se<-coeftest(boredom_figS6, vcov = vcovHC(boredom_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$boredom))

joy_figS6<- lm(joy ~ treatment, data=online_data)
summary(joy_figS6)
joy_figS6_coef<-coeftest(joy_figS6, vcov = vcovHC(joy_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$joy)
joy_figS6_se<-coeftest(joy_figS6, vcov = vcovHC(joy_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$joy))

isol_figS6<- lm(isol ~ treatment, data=online_data)
summary(isol_figS6)
isol_figS6_coef<-coeftest(isol_figS6, vcov = vcovHC(isol_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$isol)
isol_figS6_se<-coeftest(isol_figS6, vcov = vcovHC(isol_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$isol))

fulf_figS6<- lm(fulf ~ treatment, data=online_data)
summary(fulf_figS6)
fulf_figS6_coef<-coeftest(fulf_figS6, vcov = vcovHC(fulf_figS6, type = "HC1"))[2]/sd(online_data[which(online_data$treatment==0),]$fulf)
fulf_figS6_se<-coeftest(fulf_figS6, vcov = vcovHC(fulf_figS6, type = "HC1"))[4]/sd((online_data[which(online_data$treatment==0),]$fulf))

# Plot: Fig S6 - bottom
dev.off()
pdf(file = "/Users/nejlaasimovic/Desktop/figureS6_bottom.pdf",   # The directory you want to save the file in
    width = 5.8, # The width of the plot in inches
    height = 5.2) 
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))
par(mar=c(10,8,3,1)) 
coef.vec<-c(c_news_figS6_coef, swb_figS6_coef, satisf_figS6_coef, depression_figS6_coef, loneliness_figS6_coef, nerv_figS6_coef, boredom_figS6_coef, joy_figS6_coef, isol_figS6_coef, fulf_figS6_coef)
se.vec<-c(c_news_figS6_se, swb_figS6_se, satisf_figS6_se, depression_figS6_se, loneliness_figS6_se, nerv_figS6_se, boredom_figS6_se, joy_figS6_se, isol_figS6_se, fulf_figS6_se)
var.names <- c("News Knowledge","Well-being index","Satisfaction","Depression","Loneliness","Anxiety","Boredom","Joy","Isolation","Fulfillment")
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

