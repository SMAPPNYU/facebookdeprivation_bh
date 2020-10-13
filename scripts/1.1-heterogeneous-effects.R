# -----------------------------------
#   Figure S2: Heterogeneous Effects
# ------------------------------------

# --- GENDER

# gender = 1
covIn <- as.formula("~freq_usage")
outgroup_he_gender<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$gender==1,])
outgroup_he_gender_coef<-outgroup_he_gender$coefficients[2]/sd(combined[combined[combined$gender==1,]$treatment=="0",]$outgroup_princomp)
outgroup_he_gender_se<-outgroup_he_gender$std.error[2]/ sd(combined[combined[combined$gender==1,]$treatment=="0",]$outgroup_princomp)

covIn <- as.formula("~freq_usage")
swb_he_gender<-lm_lin(swb~treatment, covariates=covIn, data=combined[combined$gender==1,])
swb_he_gender_coef<-swb_he_gender$coefficients[2]/sd(combined[combined[combined$gender==1,]$treatment=="0",]$swb)
swb_he_gender_se<-swb_he_gender$std.error[2]/ sd(combined[combined[combined$gender==1,]$treatment=="0",]$swb)

covIn <- as.formula("~freq_usage")
cnews_he_gender<-lm_lin(c_news~treatment, covariates=covIn, data=combined[combined$gender==1,])
cnews_he_gender_coef<-cnews_he_gender$coefficients[2]/sd(combined[combined[combined$gender==1,]$treatment=="0",]$c_news)
cnews_he_gender_se<-cnews_he_gender$std.error[2]/ sd(combined[combined[combined$gender==1,]$treatment=="0",]$c_news)

# gender = 2
covIn <- as.formula("~freq_usage")
outgroup_he_gender2<-lm_lin(outgroup_princomp~treatment, covariates=covIn, data=combined[combined$gender==2,])
outgroup_he_gender_coef2<-outgroup_he_gender2$coefficients[2]/sd(combined[combined[combined$gender==2,]$treatment=="0",]$outgroup_princomp)
outgroup_he_gender_se2<-outgroup_he_gender2$std.error[2]/ sd(combined[combined[combined$gender==2,]$treatment=="0",]$outgroup_princomp)

covIn <- as.formula("~freq_usage")
swb_he_gender2<-lm_lin(swb~treatment, covariates=covIn, data=combined[combined$gender==2,])
swb_he_gender_coef2<-swb_he_gender2$coefficients[2]/sd(combined[combined[combined$gender==2,]$treatment=="0",]$swb)
swb_he_gender_se2<-swb_he_gender2$std.error[2]/ sd(combined[combined[combined$gender==2,]$treatment=="0",]$swb)

covIn <- as.formula("~freq_usage")
cnews_he_gender2<-lm_lin(c_news~treatment, covariates=covIn, data=combined[combined$gender==2,])
cnews_he_gender_coef2<-cnews_he_gender2$coefficients[2]/sd(combined[combined[combined$gender==2,]$treatment=="0",]$c_news)
cnews_he_gender_se2<-cnews_he_gender2$std.error[2]/ sd(combined[combined[combined$gender==2,]$treatment=="0",]$c_news)

# PLOT: HETEROGENOUS EFFECT [GENDER]
pdpdf(file = "/Users/nejlaasimovic/Desktop/figS2_gender.pdf",   # The directory you want to save the file in
    width = 5.5, # The width of the plot in inches
    height = 3.8) 

adjust = 0.06
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))
coef.vec<-c(outgroup_he_gender_coef, swb_he_gender_coef, cnews_he_gender_coef)
coef.vec.2<-c(outgroup_he_gender_coef2, swb_he_gender_coef2, cnews_he_gender_coef2)
se.vec<-c(outgroup_he_gender_se, swb_he_gender_se, cnews_he_gender_se)
se.vec.2<-c(outgroup_he_gender_se2, swb_he_gender_se2, cnews_he_gender_se2)
var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 
par(mgp=c(2.5,1,0))   # x axis closer
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,
     xlim = c(-1,1), xaxs = "r", main = "Gender", col="red",cex.lab=0.8)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 1, mgp = c(4,1.5,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) 
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust,lwd = 1.5, col = c("blue"))
points(coef.vec.2, y.axis-adjust,pch = 17, cex = 1, col = c("blue")) 
legend("bottom", 
       legend = c("Male", "Female"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.5))
dev.off()


# --- INTEREST IN POLITICS
combined$politics_int_ct <- ifelse(combined$politics_int >= median(combined$politics_int), 1, 0) 

# politics_int_ct = 0
covIn <- as.formula("~freq_usage")
outgroup_he_politics_int<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$politics_int_ct==1 ,])
outgroup_he_politics_int_coef<-outgroup_he_politics_int$coefficients[2]/sd(combined[combined[combined$politics_int_ct==1 ,]$treatment=="0",]$outgroup_index)
outgroup_he_politics_int_se<-outgroup_he_politics_int$std.error[2]/ sd(combined[combined[combined$politics_int_ct==1 ,]$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
swb_he_politics_int<-lm_lin(swb~treatment, covariates=covIn, data=combined[combined$politics_int_ct==1 ,])
swb_he_politics_int_coef<-swb_he_politics_int$coefficients[2]/sd(combined[combined[combined$politics_int_ct==1 ,]$treatment=="0",]$swb)
swb_he_politics_int_se<-swb_he_politics_int$std.error[2]/ sd(combined[combined[combined$politics_int_ct==1 ,]$treatment=="0",]$swb)

covIn <- as.formula("~freq_usage")
news_he_politics_int<-lm_lin(c_news~treatment, covariates=covIn, data=combined[combined$politics_int_ct==1 ,])
news_he_politics_int_coef<-news_he_politics_int$coefficients[2]/sd(combined[combined[combined$politics_int_ct==1 ,]$treatment=="0",]$c_news)
news_he_politics_int_se<-news_he_politics_int$std.error[2]/ sd(combined[combined[combined$politics_int_ct==1 ,]$treatment=="0",]$c_news)

# politics_int_ct = 1
outgroup_he_politics_int2<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$politics_int_ct==0,])
outgroup_he_politics_int_coef2<-outgroup_he_politics_int2$coefficients[2]/sd(combined[combined[combined$politics_int_ct==0,]$treatment=="0",]$outgroup_index)
outgroup_he_politics_int_se2<-outgroup_he_politics_int2$std.error[2]/ sd(combined[combined[combined$politics_int_ct==0,]$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
swb_he_politics_int2<-lm_lin(swb~treatment, covariates=covIn, data=combined[combined$politics_int_ct==0,])
swb_he_politics_int_coef2<-swb_he_politics_int2$coefficients[2]/sd(combined[combined[combined$politics_int_ct==0,]$treatment=="0",]$swb)
swb_he_politics_int_se2<-swb_he_politics_int2$std.error[2]/ sd(combined[combined[combined$politics_int_ct==0,]$treatment=="0",]$swb)

covIn <- as.formula("~freq_usage")
news_he_politics_int2<-lm_lin(c_news~treatment, covariates=covIn, data=combined[combined$politics_int_ct==0,])
news_he_politics_int_coef2<-news_he_politics_int2$coefficients[2]/sd(combined[combined[combined$politics_int_ct==0,]$treatment=="0",]$c_news)
news_he_politics_int_se2<-news_he_politics_int2$std.error[2]/ sd(combined[combined[combined$politics_int_ct==0,]$treatment=="0",]$c_news)
summary(news_he_politics_int2)


# PLOT: HETEROGENOUS EFFECT [INTEREST IN POLITICS]
pdf(file = "/Users/nejlaasimovic/Desktop/figS2_intpolitics.pdf",   # The directory you want to save the file in
    width = 5.5, # The width of the plot in inches
    height = 3.8) 

coef.vec<-c(outgroup_he_politics_int_coef, swb_he_politics_int_coef, news_he_politics_int_coef)
coef.vec.2<-c(outgroup_he_politics_int_coef2, swb_he_politics_int_coef2, news_he_politics_int_coef2)

se.vec<-c(outgroup_he_politics_int_se, swb_he_politics_int_se, news_he_politics_int_se)
se.vec.2<-c(outgroup_he_politics_int_se2, swb_he_politics_int_se2, news_he_politics_int_se2)

layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#w
var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 
adjust = 0.06
par(mgp=c(2.5,1,0))   # x axis closer
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,
     xlim = c(-1,1), xaxs = "r", main = "Interest in Politics", col="red",cex.lab=0.8)
par(mar=c(5,4,4,2)+0.1)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 1, mgp = c(4,1.5,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) 
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)

segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust,lwd = 1.5, col = c("blue"))
points(coef.vec.2, y.axis-adjust,pch = 17, cex = 1, col = c("blue"))
legend("bottom", 
       legend = c("Below median", "Above median"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.5))
dev.off()

# ------------- AGE
combined$age_ct <- ifelse(combined$age >= 23, 1, 0)

# age_ct = 0
covIn <- as.formula("~freq_usage")
outgroup_he_age<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$age_ct==1 ,])
outgroup_he_age_coef<-outgroup_he_age$coefficients[2]/sd(combined[combined[combined$age_ct==1 ,]$treatment=="0",]$outgroup_index)
outgroup_he_age_se<-outgroup_he_age$std.error[2]/ sd(combined[combined[combined$age_ct==1 ,]$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
swb_he_age<-lm_lin(swb~treatment, covariates=covIn, data=combined[combined$age_ct==1 ,])
swb_he_age_coef<-swb_he_age$coefficients[2]/sd(combined[combined[combined$age_ct==1 ,]$treatment=="0",]$swb)
swb_he_age_se<-swb_he_age$std.error[2]/ sd(combined[combined[combined$age_ct==1 ,]$treatment=="0",]$swb)

covIn <- as.formula("~freq_usage")
news_he_age<-lm_lin(c_news~treatment, covariates=covIn, data=combined[combined$age_ct==1 ,])
news_he_age_coef<-news_he_age$coefficients[2]/sd(combined[combined[combined$age_ct==1 ,]$treatment=="0",]$c_news)
news_he_age_se<-news_he_age$std.error[2]/ sd(combined[combined[combined$age_ct==1 ,]$treatment=="0",]$c_news)

# age_ct = 1
outgroup_he_age2<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[combined$age_ct==0,])
outgroup_he_age_coef2<-outgroup_he_age2$coefficients[2]/sd(combined[combined[combined$age_ct==0,]$treatment=="0",]$outgroup_index)
outgroup_he_age_se2<-outgroup_he_age2$std.error[2]/ sd(combined[combined[combined$age_ct==0,]$treatment=="0",]$outgroup_index)

covIn <- as.formula("~freq_usage")
swb_he_age2<-lm_lin(swb~treatment, covariates=covIn, data=combined[combined$age_ct==0,])
swb_he_age_coef2<-swb_he_age2$coefficients[2]/sd(combined[combined[combined$age_ct==0,]$treatment=="0",]$swb)
swb_he_age_se2<-swb_he_age2$std.error[2]/ sd(combined[combined[combined$age_ct==0,]$treatment=="0",]$swb)

covIn <- as.formula("~freq_usage")
news_he_age2<-lm_lin(c_news~treatment, covariates=covIn, data=combined[combined$age_ct==0,])
news_he_age_coef2<-news_he_age2$coefficients[2]/sd(combined[combined[combined$age_ct==0,]$treatment=="0",]$c_news)
news_he_age_se2<-news_he_age2$std.error[2]/ sd(combined[combined[combined$age_ct==0,]$treatment=="0",]$c_news)


# PLOT: HETEROGENOUS EFFECT [AGE]
dev.off()
pdf(file = "/Users/nejlaasimovic/Desktop/figS2_age.pdf",   # The directory you want to save the file in
    width = 5.5, # The width of the plot in inches
    height = 3.8) 
#par(mar=c(3,2,3,4))
coef.vec<-c(outgroup_he_age_coef, swb_he_age_coef, news_he_age_coef)
se.vec<-c(outgroup_he_age_coef, swb_he_age_se, news_he_age_se)

coef.vec2<-c(outgroup_he_age_coef2, swb_he_age_coef2, news_he_age_coef2)
se.vec2<-c(outgroup_he_age_se2, swb_he_age_se2, news_he_age_se2)

adjust = 0.06

layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))
var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 
par(mgp=c(2.5,1,0))   # x axis closer
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,
     xlim = c(-1,1), xaxs = "r", main = "Age", col="red",cex.lab=0.8)
par(mar=c(5,4,4,2)+0.1)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 1, mgp = c(4,1.5,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1)
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
# draw dotted line through 0 
#box(bty = "l") #place box around plot
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust,lwd = 1.5, col = c("blue"))
points(coef.vec.2, y.axis-adjust,pch = 17, cex = 1, col = c("blue")) 
legend("bottom", 
       legend = c("Above median", "Below median"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.5))
dev.off()

# ---------- ETHNICITY
# creating ethnic categories including those who self-identify by their religion
# Bosniak or Islam
combined$ethn=="1" | combined$ethn=="-1"

# Serb or Orthodox
combined$ethn=="2" | combined$ethn=="-4"

# --- Bosniak
outgroup_he_ethn<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[which(combined$ethn=="1" | combined$ethn=="-1"),])
outgroup_he_ethn_coef<-outgroup_he_ethn$coefficients[2]/sd(combined[which(combined$ethn=="1" | combined$ethn=="-1"),]$outgroup_index)
outgroup_he_ethn_se<-outgroup_he_ethn$std.error[2]/sd(combined[which(combined$ethn=="1" | combined$ethn=="-1"),]$outgroup_index)

swb_he_ethn<-lm_lin(swb~treatment, covariates=covIn, data=combined[which(combined$ethn=="1" | combined$ethn=="-1"),])
swb_he_ethn_coef<-outgroup_he_ethn$coefficients[2]/sd(combined[which(combined$ethn=="1" | combined$ethn=="-1"),]$swb)
swb_he_ethn_se<-outgroup_he_ethn$std.error[2]/sd(combined[which(combined$ethn=="1" | combined$ethn=="-1"),]$swb)

cnews_he_ethn<-lm_lin(c_news~treatment, covariates=covIn, data=combined[which(combined$ethn=="1" | combined$ethn=="-1"),])
cnews_he_ethn_coef<-cnews_he_ethn$coefficients[2]/sd(combined[which(combined$ethn=="1" | combined$ethn=="-1"),]$c_news)
cnews_he_ethn_se<-cnews_he_ethn$std.error[2]/sd(combined[which(combined$ethn=="1" | combined$ethn=="-1"),]$c_news)

# --- Serb

outgroup_he_ethn<-lm_lin(outgroup_index~treatment, covariates=covIn, data=combined[which(combined$ethn=="2" | combined$ethn=="-4"),])
outgroup_he_ethn_coef2<-outgroup_he_ethn$coefficients[2]/sd(combined[which(combined$ethn=="2" | combined$ethn=="-4"),]$outgroup_index)
outgroup_he_ethn_se2<-outgroup_he_ethn$std.error[2]/sd(combined[which(combined$ethn=="2" | combined$ethn=="-4"),]$outgroup_index)

swb_he_ethn<-lm_lin(swb~treatment, covariates=covIn, data=combined[which(combined$ethn=="2" | combined$ethn=="-4"),])
swb_he_ethn_coef2<-outgroup_he_ethn$coefficients[2]/sd(combined[which(combined$ethn=="2" | combined$ethn=="-4"),]$swb)
swb_he_ethn_se2<-outgroup_he_ethn$std.error[2]/sd(combined[which(combined$ethn=="2" | combined$ethn=="-4"),]$swb)

cnews_he_ethn<-lm_lin(c_news~treatment, covariates=covIn, data=combined[which(combined$ethn=="2" | combined$ethn=="-4"),])
cnews_he_ethn_coef2<-cnews_he_ethn$coefficients[2]/sd(combined[which(combined$ethn=="2" | combined$ethn=="-4"),]$c_news)
cnews_he_ethn_se2<-cnews_he_ethn$std.error[2]/sd(combined[which(combined$ethn=="2" | combined$ethn=="-4"),]$c_news)

# PLOT: HETEROGENOUS EFFECT [ETHNICITY]
dev.off()
pdf(file = "/Users/nejlaasimovic/Desktop/figS2_ethnicity.pdf",   # The directory you want to save the file in
    width = 5.5, # The width of the plot in inches
    height = 3.8) 
#par(mar=c(3,2,3,4))
coef.vec<-c(outgroup_he_ethn_coef, swb_he_ethn_coef, cnews_he_ethn_coef)
coef.vec2<-c(outgroup_he_ethn_coef2, swb_he_ethn_coef2, cnews_he_ethn_coef2)

se.vec<-c(outgroup_he_ethn_se, swb_he_ethn_se, cnews_he_ethn_se)
se.vec2<-c(outgroup_he_ethn_se2, swb_he_ethn_se2, cnews_he_ethn_se2)

adjust = 0.06

layout(matrix(c(2,1),1,2), 
       widths = c(1.5, 5))
var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 
par(mgp=c(2.5,1,0))   # x axis closer
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,
     xlim = c(-1,1), xaxs = "r", main = "Ethnicity", col="red",cex.lab=0.8)
par(mar=c(5,4,4,2)+0.1)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 1, mgp = c(4,1.5,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) 
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust,lwd = 1.5, col = c("blue"))
points(coef.vec.2, y.axis-adjust,pch = 17, cex = 1, col = c("blue"))
legend("bottom", 
       legend = c("Bosniak", "Serb"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.5))
dev.off()

# ---- INTENSITY OF USAGE
# by heavy vs light users
combined$freq_usage_ht <- ifelse(combined$freq_usage >= median(combined$freq_usage), 1, 0) 

# out-group attitudes
m_g<-lm(combined[combined$freq_usage_ht==0,]$outgroup_princomp~combined[combined$freq_usage_ht==0,]$treatment)
het_int_coef<-coeftest(m_g)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$outgroup_princomp))
het_int_se<-coeftest(m_g)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$outgroup_princomp))

m_g2<-lm(combined[combined$freq_usage_ht==1,]$outgroup_princomp~combined[combined$freq_usage_ht==1,]$treatment)
het_int_coef2<-coeftest(m_g2)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$outgroup_princomp))
het_int_se2<-coeftest(m_g2)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$outgroup_princomp))

# subjective well-being
m_g<-lm(combined[combined$freq_usage_ht==0,]$swb~combined[combined$freq_usage_ht==0,]$treatment)
swb_het_int_coef<-coeftest(m_g)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$swb))
swb_het_int_se<-coeftest(m_g)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$swb))

m_g2<-lm(combined[combined$freq_usage_ht==1,]$swb~combined[combined$freq_usage_ht==1,]$treatment)
swb_het_int_coef2<-coeftest(m_g2)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$swb))
swb_het_int_se2<-coeftest(m_g2)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$swb))

# political news
m_g<-lm(combined[combined$freq_usage_ht==0,]$c_news~combined[combined$freq_usage_ht==0,]$treatment)
news_het_int_coef<-coeftest(m_g)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$c_news))
news_het_int_se<-coeftest(m_g)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$c_news))

m_g2<-lm(combined[combined$freq_usage_ht==1,]$c_news~combined[combined$freq_usage_ht==1,]$treatment)
news_het_int_coef2<-coeftest(m_g2)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$c_news))
news_het_int_se2<-coeftest(m_g2)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$c_news))

# ------- Exploratory (not pre-registered): by ENTITY
# 1 Federation of BiH
# 2. Republika Srpska

m_g<-lm(combined[combined$entity==1,]$outgroup_princomp~combined[combined$entity==1,]$treatment)
het_entity_coef<-coeftest(m_g)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$outgroup_princomp))
het_entity_se<-coeftest(m_g)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$outgroup_princomp))

m_g2<-lm(combined[combined$entity==2,]$outgroup_princomp~combined[combined$entity==2,]$treatment)
het_entity_coef2<-coeftest(m_g2)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$outgroup_princomp))
het_entity_se2<-coeftest(m_g2)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$outgroup_princomp))

# subjective well-being
m_g<-lm(combined[combined$entity==1,]$swb~combined[combined$entity==1,]$treatment)
swb_het_entity_coef<-coeftest(m_g)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$swb))
swb_het_entity_se<-coeftest(m_g)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$swb))

m_g2<-lm(combined[combined$entity==2,]$swb~combined[combined$entity==2,]$treatment)
swb_het_entity_coef2<-coeftest(m_g2)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$swb))
swb_het_entity_se2<-coeftest(m_g2)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$swb))

# political news
m_g<-lm(combined[combined$entity==1,]$c_news~combined[combined$entity==1,]$treatment)
cnews_het_entity_coef<-coeftest(m_g)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$c_news))
cnews_het_entity_se<-coeftest(m_g)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$c_news))

m_g2<-lm(combined[combined$entity==2,]$c_news~combined[combined$entity==2,]$treatment)
cnews_het_entity_coef2<-coeftest(m_g2)[2,1]/sd(na.omit(combined[combined$treatment=="0",]$c_news))
cnews_het_entity_se2<-coeftest(m_g2)[2,2]/sd(na.omit(combined[combined$treatment=="0",]$c_news))

# PLOT: HETEROGENOUS EFFECT [ENTITY]
dev.off()
pdf(file = "/Users/nejlaasimovic/Desktop/figS2_entity.pdf",   # The directory you want to save the file in
    width = 5.5, # The width of the plot in inches
    height = 3.8) 

coef.vec<-c(het_entity_coef, swb_het_entity_coef, cnews_het_entity_coef)
coef.vec2<-c(het_entity_coef2, swb_het_entity_coef2, cnews_het_entity_coef2)

se.vec<-c(het_entity_se, swb_het_entity_se, cnews_het_entity_se)
se.vec2<-c(het_entity_se2, swb_het_entity_se2, cnews_het_entity_se2)

adjust = 0.06
layout(matrix(c(2,1),1,2), 
       widths = c(1.5, 5))
var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 
par(mgp=c(2.5,1,0))   # x axis closer
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,
     xlim = c(-1,1), xaxs = "r", main = "Entity", col="red",cex.lab=0.8)
par(mar=c(5,4,4,2)+0.1)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 1, mgp = c(4,1.5,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1)
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
# draw dotted line through 0 
#box(bty = "l") #place box around plot
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust,lwd = 1.5, col = c("blue"))
points(coef.vec.2, y.axis-adjust,pch = 17, cex = 1, col = c("blue"))
legend("bottom", 
       legend = c("Federation", "RS"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.5))
dev.off()

