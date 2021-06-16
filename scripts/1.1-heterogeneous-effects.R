#########################################################################################
# File-Name: 1.1-heterogeneous-effects.R
# Date: 2021
# Purpose: Analysis of heterogeneous effects by gender,
#          interest in politics, age, ethnicity, entity (geographical/political subunit)
###########################################################################################

source("~/Desktop/facebookdeprivation_bh-master/scripts/01-analysis.R") # set your directory

# ----------------------------------------
#     Figure S2: Heterogeneous Effects
# ----------------------------------------

## GENDER

# pdf(file = "figS2_gender.pdf",   # The directory you want to save the file in
# width = 5.5, # The width of the plot in inches
# height = 3.8) 


coef.vec <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$gender==1,])$estimate, reg_res_mod2("swb",final_data[final_data$gender==1,])$estimate, reg_res_mod2("c_news",final_data[final_data$gender==1,])$estimate)
coef.vec2 <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$gender==2,])$estimate, reg_res_mod2("swb",final_data[final_data$gender==2,])$estimate, reg_res_mod2("c_news",final_data[final_data$gender==2,])$estimate)

se.vec <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$gender==1,])$std.error, reg_res_mod2("swb",final_data[final_data$gender==1,])$std.error, reg_res_mod2("c_news",final_data[final_data$gender==1,])$std.error)
se.vec2 <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$gender==2,])$std.error, reg_res_mod2("swb",final_data[final_data$gender==2,])$std.error, reg_res_mod2("c_news",final_data[final_data$gender==2,])$std.error)

var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 

# ---------- Plot Figure S2 [heterogeneous effect by gender]
par(mar=c(8, 10, 5, 5))  
adjust = 0.06
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.9,
     xlim = c(-1,1), xaxs = "r", main = "Gender", col="red",cex.lab=0.8)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 1, mgp = c(4,0.5,0.5))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) 
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
segments(coef.vec2-qnorm(.975)*se.vec2, y.axis-adjust, coef.vec2+qnorm(.975)*se.vec2, y.axis-adjust,lwd = 1.5, col = c("blue"))
points(coef.vec2, y.axis-adjust,pch = 17, cex = 1, col = c("blue")) 
legend("bottom", 
       legend = c("Male", "Female"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black", box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.8))
#dev.off()

## INTEREST IN POLITICS

# Dividing the sample into below and above median value
final_data$politics_int_ct <- ifelse(final_data$politics_int >= median(final_data$politics_int), 1, 0) 

# pdf(file = "figS2_intpolitics.pdf",   # The directory you want to save the file in
# width = 5.5, # The width of the plot in inches
# height = 3.8) 

coef.vec <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$politics_int_ct==1,])$estimate, reg_res_mod2("swb",final_data[final_data$politics_int_ct==1,])$estimate, reg_res_mod2("c_news",final_data[final_data$politics_int_ct==1,])$estimate)
coef.vec2 <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$politics_int_ct==0,])$estimate, reg_res_mod2("swb",final_data[final_data$politics_int_ct==0,])$estimate, reg_res_mod2("c_news",final_data[final_data$politics_int_ct==0,])$estimate)

se.vec <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$politics_int_ct==1,])$std.error, reg_res_mod2("swb",final_data[final_data$politics_int_ct==1,])$std.error, reg_res_mod2("c_news",final_data[final_data$politics_int_ct==1,])$std.error)
se.vec2 <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$politics_int_ct==0,])$std.error, reg_res_mod2("swb",final_data[final_data$politics_int_ct==0,])$std.error, reg_res_mod2("c_news",final_data[final_data$politics_int_ct==0,])$std.error)

var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 

#  ---------- Plot Figure S2  [heterogeneous effect by interest in politics]
par(mar=c(8, 10, 5, 5))    
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Interest in Politics", col="red",cex.lab=0.8)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(1,0.5,0.3))#reduce label size, moves labels closer to tick marks; zadnji u mgp spusta x liniju nize
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
segments(coef.vec2-qnorm(.975)*se.vec2, y.axis-adjust, coef.vec2+qnorm(.975)*se.vec2, y.axis-adjust,lwd = 1.5, col = c("blue"))#draw lines connecting 95% confidence intervals
points(coef.vec2, y.axis-adjust,pch = 17, cex = 1, col = c("blue")) #add point estimates for 2nd model; pch = 21 uses for overlay points, and "white" for white color
legend("bottom", 
       legend = c("Median/Above median", "Below median"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.5))

## AGE

# Divide the sample into above and below the year of 24
# We pre-registered testing the difference for people born during/before the war
# (i.e, below 24, and year 24 and older)
final_data$age_ct <- ifelse(final_data$age >= 24, 1, 0)

coef.vec <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$age_ct==1,])$estimate, reg_res_mod2("swb",final_data[final_data$age_ct==1,])$estimate, reg_res_mod2("c_news",final_data[final_data$age_ct==1,])$estimate)
coef.vec.2 <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$age_ct==0,])$estimate, reg_res_mod2("swb",final_data[final_data$age_ct==0,])$estimate, reg_res_mod2("c_news",final_data[final_data$age_ct==0,])$estimate)

se.vec <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$age_ct==1,])$std.error, reg_res_mod2("swb",final_data[final_data$age_ct==1,])$std.error, reg_res_mod2("c_news",final_data[final_data$age_ct==1,])$std.error)
se.vec.2 <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$age_ct==0,])$std.error, reg_res_mod2("swb",final_data[final_data$age_ct==0,])$std.error, reg_res_mod2("c_news",final_data[final_data$age_ct==0,])$std.error)

var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 

#  ---------- Plot Figure S2  [heterogeneous effect by age]
# pdf(file = "figS2_age.pdf",   # The directory you want to save the file in
# width = 5.5, # The width of the plot in inches
# height = 3.8) 

par(mar=c(8, 10, 5, 5))    
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,
     xlim = c(-1,1), xaxs = "r", main = "Age", col="red",cex.lab=0.8)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 1, mgp = c(2,1.5,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) 
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust,lwd = 1.5, col = c("blue"))
points(coef.vec.2, y.axis-adjust,pch = 17, cex = 1, col = c("blue"))
legend("bottom", 
       legend = c("Born after the war", "Born during/prior to war"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.5))
# dev.off()

## ETHNICITY
# Including those who self-identified by their religion 
# 1. Bosniak or Islam: final_data$ethn=="1" | final_data$ethn=="-1"
# 2. Serb or Orthodox: final_data$ethn=="2" | final_data$ethn=="-4"

coef.vec <- c(reg_res_mod2("outgroup_princomp",final_data[which(final_data$ethn=="1" | final_data$ethn=="-1"),])$estimate, reg_res_mod2("swb",final_data[which(final_data$ethn=="1" | final_data$ethn=="-1"),])$estimate, reg_res_mod2("c_news",final_data[which(final_data$ethn=="1" | final_data$ethn=="-1"),])$estimate)
coef.vec.2 <- c(reg_res_mod2("outgroup_princomp",final_data[which(final_data$ethn=="2" | final_data$ethn=="-4"),])$estimate, reg_res_mod2("swb",final_data[which(final_data$ethn=="2" | final_data$ethn=="-4"),])$estimate, reg_res_mod2("c_news",final_data[which(final_data$ethn=="2" | final_data$ethn=="-4"),])$estimate)

se.vec <- c(reg_res_mod2("outgroup_princomp",final_data[which(final_data$ethn=="1" | final_data$ethn=="-1"),])$std.error, reg_res_mod2("swb",final_data[which(final_data$ethn=="1" | final_data$ethn=="-1"),])$std.error, reg_res_mod2("c_news",final_data[which(final_data$ethn=="1" | final_data$ethn=="-1"),])$std.error)
se.vec.2 <- c(reg_res_mod2("outgroup_princomp",final_data[which(final_data$ethn=="2" | final_data$ethn=="-4"),])$std.error, reg_res_mod2("swb",final_data[which(final_data$ethn=="2" | final_data$ethn=="-4"),])$std.error, reg_res_mod2("c_news",final_data[which(final_data$ethn=="2" | final_data$ethn=="-4"),])$std.error)

var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 

#  ---------- Plot Figure S2  [heterogeneous effect by ethnicity]

# pdf(file = "figS2_ethnicity.pdf", 
# width = 5.5, # The width of the plot in inches
# height = 3.8) 
par(mar=c(8, 10, 5, 5))   
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,
     xlim = c(-1.5,1.5), xaxs = "r", main = "Ethnicity", col="red",cex.lab=0.8)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1.5,1.5,by=1.5), labels =c(-1.5,0,1.5) , tick = T,
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
# dev.off()

## EXPLORATORY [not-preregistered]: Heterogeneous effect by entity
# 1 Federation of BiH 
# 2. Republika Srpska

#  ---------- Plot Figure S2 -- Heterogeneous effect by entity
coef.vec <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$entity==1,])$estimate, reg_res_mod2("swb",final_data[final_data$entity==1,])$estimate, reg_res_mod2("c_news",final_data[final_data$entity==1,])$estimate)
coef.vec.2 <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$entity==2,])$estimate, reg_res_mod2("swb",final_data[final_data$entity==2,])$estimate, reg_res_mod2("c_news",final_data[final_data$entity==2,])$estimate)

se.vec <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$entity==1,])$std.error, reg_res_mod2("swb",final_data[final_data$entity==1,])$std.error, reg_res_mod2("c_news",final_data[final_data$entity==1,])$std.error)
se.vec.2 <- c(reg_res_mod2("outgroup_princomp",final_data[final_data$entity==2,])$std.error, reg_res_mod2("swb",final_data[final_data$entity==2,])$std.error, reg_res_mod2("c_news",final_data[final_data$entity==2,])$std.error)

var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 

# pdf(file = "figS2_entity.pdf",  
# width = 5.5, 
# height = 3.8) 
par(mar=c(8, 10, 5, 5))   
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,
     xlim = c(-1,1), xaxs = "r", main = "Entity", col="red",cex.lab=0.8)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 1, mgp = c(4,1.5,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1)
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust,lwd = 1.5, col = c("blue"))
points(coef.vec.2, y.axis-adjust,pch = 17, cex = 1, col = c("blue")) 
legend("bottom", 
       legend = c("Federation", "RS"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black",   box.lty=1, box.lwd=0.3,
       horiz=TRUE, inset=c(0.5,-0.5))
#dev.off()

