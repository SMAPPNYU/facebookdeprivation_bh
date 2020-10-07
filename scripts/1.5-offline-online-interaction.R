# ---- Majority Group Share
combined$majority_group_difference <- ifelse(combined$largest_majority>combined$largest_majority_online|combined$largest_majority==combined$largest_majority_online, 0, 1) # 0=offline is more homogenous
 
# Online network more heterogenous
covIn <- as.formula("~freq_usage")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$majority_group_difference==0,])
onl_off_ms_coef<-m$coefficients[2]/sd((combined[which(combined$majority_group_difference==0 & combined$treatment=="0"),]$outgroup_princomp))
onl_off_ms_se<-m$std.error[2]/sd((combined[which(combined$majority_group_difference==0 & combined$treatment=="0"),]$outgroup_princomp))

# Offline network more heterogeneous
covIn <- as.formula("~freq_usage")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$majority_group_difference==1,])
onl_off_ms_coef2<-m$coefficients[2]/sd(combined[which(combined$majority_group_difference==1 & combined$treatment=="0"),]$outgroup_princomp)
onl_off_ms_se2<-m$std.error[2]/sd(combined[which(combined$majority_group_difference==1 & combined$treatment=="0"),]$outgroup_princomp)

# ---- Ethnic Fractionalization
combined$fract_difference <- ifelse(combined$fractionalization_offline_index>combined$online_index|combined$fractionalization_offline_index==combined$online_index,1,0) #0=offline more homogenous

# Online network more heterogenous
covIn <- as.formula("~freq_usage")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$majority_group_difference==0,])
onl_off_ef_coef<-m$coefficients[2]/sd(combined[which(combined$fract_difference==0 & combined$treatment=="0"),]$outgroup_princomp)
onl_off_ef_se<-m$std.error[2]/sd(combined[which(combined$fract_difference==0 & combined$treatment=="0"),]$outgroup_princomp)

# Offline network more heterogeneous
covIn <- as.formula("~freq_usage")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$majority_group_difference==1,])
onl_off_ef_coef2<-m$coefficients[2]/sd(combined[which(combined$fract_difference==1 & combined$treatment=="0"),]$outgroup_princomp)
onl_off_ef_se2<-m$std.error[2]/sdh(combined[which(combined$fract_difference==1 & combined$treatment=="0"),]$outgroup_princomp)


# ----- Shannon Entropy
combined$shannon_difference_ct <-combined$shannon_offline-combined$shannon_online
combined$shannon_difference <- ifelse(combined$shannon_offline>combined$shannon_online|combined$shannon_offline==combined$shannon_online,1,0)

# Online network more heterogenous
covIn <- as.formula("~freq_usage")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$shannon_difference==0,])
onl_off_sh_coef<-m$coefficients[2]/sd(combined[which(combined$shannon_difference ==0 & combined$treatment=="0"),]$outgroup_princomp)
onl_off_sh_se<-m$std.error[2]/sd(combined[which(combined$shannon_difference ==0 & combined$treatment=="0"),]$outgroup_princomp)

# Offline network more heterogeneous
covIn <- as.formula("~freq_usage")
m<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$shannon_difference==1,])
summary(m)
onl_off_sh_coef2<-m$coefficients[2]/sd(combined[which(combined$shannon_difference == 1 & combined$treatment=="0"),]$outgroup_princomp)
onl_off_sh_se2<-m$std.error[2]/sd(combined[which(combined$shannon_difference == 1 & combined$treatment=="0"),]$outgroup_princomp)


# ----------------------------
#       Plot Figure S7
# ----------------------------
#pdf(file = "/Users/nejlaasimovic/Desktop/figureS7.pdf",   # The directory you want to save the file in
 #   width = 5.8, # The width of the plot in inches
  #  height = 5.2)

# ------- Left Panel
par(mfrow=c(1,2)) 
par(mar=c(9, 7, 5, 1))
# par(mar=c(6, 3, 1, 5)) #20 je desna strana # 2 s lijeve gura, zadnja desno, 3 skuplja
coef.vec<-c(onl_off_sh_coef2,onl_off_ms_coef2,onl_off_ef_coef2)
se.vec<-c(onl_off_sh_se2,onl_off_ms_se2,onl_off_ef_se2)
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1.5,1.5), xaxs = "r", main = "Offline network more \n heterogenous", col=c("black","blue","red"), cex.lab=0.9, cex.main=0.9)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1.5,1.5,by=1.5), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1)#draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

# ------  Right Panel
coef.vec.2<-c(onl_off_sh_coef,onl_off_ms_coef,onl_off_ef_coef)
se.vec.2<-c(onl_off_sh_se,onl_off_ms_se,onl_off_ef_se)
adjust = 0.26
plot(coef.vec.2, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1.5,1.5), xaxs = "r", main = "Online network more \n heterogenous",col=c("black","blue","red"),cex.lab=0.9, cex.main=0.9)
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis, coef.vec.2+qnorm(.975)*se.vec.2, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at =  seq(-1.5,1.5,by=1.5), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
legend(x=-8, y=-0.01,
       legend = c("Shannon Entropy", "Majority Group Share","Ethnic Fractionalization"), 
       col=c("black","blue","red"), xpd=NA,
       pch = c(19,17,12), 
       bty = T,
       pt.cex = 1, 
       cex = 0.6, 
       text.col = "black",   box.lty=1.2, box.lwd=0.5,
       horiz=TRUE)
dev.off()

# -------------- OPTION B (not included): Figure S7, but with outgroup index as sum of z-scores rather than PC index
# ---- Majority Group Share
combined$majority_group_difference <- ifelse(combined$largest_majority>combined$largest_majority_online|combined$largest_majority==combined$largest_majority_online, 0, 1) # 0=offline is more homogenous

# Online network more heterogenous
m<-lm(outgroup_index~treatment+freq_usage, data=combined[which(combined$majority_group_difference==0),])
onl_off_ms_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$majority_group_difference==0 & combined$treatment=="0"),]$outgroup_index)
onl_off_ms_se<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$majority_group_difference==0 & combined$treatment=="0"),]$outgroup_index)

# Offline network more heterogeneous
m<-lm(outgroup_index~treatment+freq_usage, data=combined[combined$majority_group_difference==1,])
onl_off_ms_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$majority_group_difference==1 & combined$treatment=="0"),]$outgroup_index)
onl_off_ms_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$majority_group_difference==1 & combined$treatment=="0"),]$outgroup_index)

# ---- Ethnic Fractionalization
combined$fract_difference <- ifelse(combined$fractionalization_offline_index>combined$online_index|combined$fractionalization_offline_index==combined$online_index,1,0) #0=offline more homogenous

# Online network more heterogenous
m<-lm(outgroup_index~treatment+freq_usage, data=combined[combined$majority_group_difference==0,])
onl_off_ef_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$fract_difference==0 & combined$treatment=="0"),]$outgroup_index)
onl_off_ef_se<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$fract_difference==0 & combined$treatment=="0"),]$outgroup_index)

# Offline network more heterogeneous
m<-lm(outgroup_index~treatment+freq_usage, data=combined[combined$majority_group_difference==1,])
onl_off_ef_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$fract_difference==1 & combined$treatment=="0"),]$outgroup_index)
onl_off_ef_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$fract_difference==1 & combined$treatment=="0"),]$outgroup_index)

# ----- Shannon Entropy
combined$shannon_difference_ct <-combined$shannon_offline-combined$shannon_online
combined$shannon_difference <- ifelse(combined$shannon_offline>combined$shannon_online|combined$shannon_offline==combined$shannon_online,1,0)

# Online network more heterogenous
m<-lm(outgroup_index~treatment+freq_usage, data=combined[combined$shannon_difference==0,])
onl_off_sh_coef<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$shannon_difference==0 & combined$treatment=="0"),]$outgroup_index)
onl_off_sh_se<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$shannon_difference==0 & combined$treatment=="0"),]$outgroup_index)

# Offline network more heterogeneous
m<-lm(outgroup_index~treatment+freq_usage, data=combined[combined$shannon_difference==1,])
onl_off_sh_coef2<-coeftest(m, vcov = vcovHC(m, type = "HC1"))[2,1]/sd(combined[which(combined$shannon_difference==1 & combined$treatment=="0"),]$outgroup_index)
onl_off_sh_se2<-coeftest (m, vcov = vcovHC(m, type = "HC1"))[2,2]/sd(combined[which(combined$shannon_difference==1 & combined$treatment=="0"),]$outgroup_index)



# ----------------------------------------------------
#       Plot Figure S7 [Option B; sum of z-scores]
# -------------------------------------------------------

#pdf(file = "/Users/nejlaasimovic/Desktop/figureS7_zscore.pdf",   # The directory you want to save the file in
 #   width = 5.8, # The width of the plot in inches
  #  height = 5.2)

# Left Panel
par(mfrow=c(1,2)) 
par(mar=c(9, 7, 5, 1))
coef.vec<-c(onl_off_sh_coef2,onl_off_ms_coef2,onl_off_ef_coef2)
se.vec<-c(onl_off_sh_se2,onl_off_ms_se2,onl_off_ef_se2)
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1.5,1.5), xaxs = "r", main = "Offline network more \n heterogenous", col=c("black","blue","red"), cex.lab=0.9, cex.main=0.9)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1.5,1.5,by=1.5), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1)#draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

# Right Panel
coef.vec.2<-c(onl_off_sh_coef,onl_off_ms_coef,onl_off_ef_coef)
se.vec.2<-c(onl_off_sh_se,onl_off_ms_se,onl_off_ef_se)
adjust = 0.26
plot(coef.vec.2, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1.5,1.5), xaxs = "r", main = "Online network more \n heterogenous",col=c("black","blue","red"),cex.lab=0.9, cex.main=0.9)
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis, coef.vec.2+qnorm(.975)*se.vec.2, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at =  seq(-1.5,1.5,by=1.5), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
legend(x=-8, y=-0.01,
       legend = c("Shannon Entropy", "Majority Group Share","Ethnic Fractionalization"), 
       col=c("black","blue","red"), xpd=NA,
       pch = c(19,17,12), 
       bty = T,
       pt.cex = 1, 
       cex = 0.6, 
       text.col = "black",   box.lty=1.2, box.lwd=0.5,
       horiz=TRUE)
dev.off()

# ----------------------------------------------------
#       Figure S8
# -------------------------------------------------------
# indices, repeating as below to make it easier to follow
#combined$majority_group_difference <- ifelse(combined$largest_majority>combined$largest_majority_online, 0, 1) # offline is more homogenous
#combined$fract_difference <- ifelse(combined$fractionalization_offline_index>combined$online_index,1,0)
#combined$shannon_difference <- ifelse(combined$shannon_offline>combined$shannon_online,1,0)

# Ethnic Fractionalization - Part A
dev.off() 
#pdf(file = "/Users/nejlaasimovic/Desktop/figureS8_EF.pdf",   # The directory you want to save the file in
 #   width = 5.8, # The width of the plot in inches
  #  height = 5.2)

comb = summarySE(combined[!is.na(combined$fract_difference),], measurevar="outgroup_index", groupvars=c("fract_difference","treatment"))
comb$factor_diff<-as.factor(comb$fract_difference)
comb$treatment<-as.factor(comb$treatment)
pd = position_dodge(0.5)
ggplot(comb, aes(x=factor_diff, #1 is online more het than the offline one
                 y=outgroup_index,
                 color=treatment)) +
  geom_errorbar(aes(ymin=outgroup_index-se,
                    ymax=outgroup_index+se),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("black", "blue"))+
  scale_x_discrete(labels=c("1" = "Online more \n heterogenous", "0" = "Offline more\n heterogenous"))+
  xlab("") + ylab("OutGroup Index")+ggtitle("Ethnic Fractionalization")


# Majority Group Share - Part B
dev.off() 
# pdf(file = "/Users/nejlaasimovic/Desktop/figureS8_MS.pdf",   # The directory you want to save the file in
  #  width = 5.8, # The width of the plot in inches
   # height = 5.2)
comb = summarySE(combined[!is.na(combined$majority_group_difference),], measurevar="outgroup_index", groupvars=c("majority_group_difference","treatment"))
comb$factor_diff<-as.factor(comb$majority_group_difference)
comb$treatment<-as.factor(comb$treatment)
pd = position_dodge(0.5)
ggplot(comb, aes(x=factor_diff, #1 is online more het than the offline one
                 y=outgroup_index,
                 color=treatment)) +
  geom_errorbar(aes(ymin=outgroup_index-se,
                    ymax=outgroup_index+se),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold"),
  ) +
  scale_color_manual(values = c("black", "blue"))+
  scale_x_discrete(labels=c("1" = "Online more\n heterogenous", "0" ="Offline more\n heterogenous" ))+
  xlab("") + ylab("OutGroup Index")+ggtitle("Majority Group Share")

# Shannon Entropy - Part C
dev.off() 
# pdf(file = "/Users/nejlaasimovic/Desktop/figureS8_SE.pdf",   # The directory you want to save the file in
  #  width = 5.8, # The width of the plot in inches
  # height = 5.2)
comb = summarySE(combined[!is.na(combined$shannon_difference),], measurevar="outgroup_index", groupvars=c("shannon_difference","treatment"))
comb$factor_diff<-as.factor(comb$shannon_difference)
comb$treatment<-as.factor(comb$treatment)
pd = position_dodge(0.5)
ggplot(comb, aes(x=factor_diff, #1 is online more het than the offline one
                 y=outgroup_index,
                 color=treatment)) +
  geom_errorbar(aes(ymin=outgroup_index-se,
                    ymax=outgroup_index+se),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold"),
  ) +
  scale_color_manual(values = c("black", "blue"))+
  scale_x_discrete(labels=c("1" = "Offline more\n heterogenous", "0" ="Online more\n heterogenous" ))+
  xlab("") + ylab("OutGroup Index")+ggtitle("Shannon Entropy")
dev.off()


# ---------- Offline/Online interaction tables - TO ADD



