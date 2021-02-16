# -----------------------------------
#   Figure S2: Heterogeneous Effects
# ------------------------------------

# --- GENDER
# gender = 1, male
reg_res_mod2("outgroup_princomp",combined[combined$gender==1,])
reg_res_mod2("swb",combined[combined$gender==1,])
reg_res_mod2("c_news",combined[combined$gender==1,])

# gender = 2, female
reg_res_mod2("outgroup_princomp",combined[combined$gender==2,])
reg_res_mod2("swb",combined[combined$gender==2,])
reg_res_mod2("c_news",combined[combined$gender==2,])

#  ---------- Plot Figure S2 [heterogeneous effect by gender]
# pdf(file = "/Users/nejlaasimovic/Desktop/figS2_gender.pdf",   # The directory you want to save the file in
# width = 5.5, # The width of the plot in inches
# height = 3.8) 

adjust = 0.06
layout(matrix(c(2,1),1,2),
       widths = c(1.5, 5))
coef.vec<-c(reg_res_mod2("outgroup_princomp",combined[combined$gender==1,])$estimate, reg_res_mod2("swb",combined[combined$gender==1,])$estimate, reg_res_mod2("c_news",combined[combined$gender==1,])$estimate)
coef.vec2<-c(reg_res_mod2("outgroup_princomp",combined[combined$gender==2,])$estimate, reg_res_mod2("swb",combined[combined$gender==2,])$estimate, reg_res_mod2("c_news",combined[combined$gender==2,])$estimate)
se.vec<-c(reg_res_mod2("outgroup_princomp",combined[combined$gender==1,])$std.error, reg_res_mod2("swb",combined[combined$gender==1,])$std.error, reg_res_mod2("c_news",combined[combined$gender==1,])$std.error)
se.vec2<-c(reg_res_mod2("outgroup_princomp",combined[combined$gender==2,])$std.error, reg_res_mod2("swb",combined[combined$gender==2,])$std.error, reg_res_mod2("c_news",combined[combined$gender==2,])$std.error)
var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 
par(mar=c(9, 4, 5, 1))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,
     xlim = c(-1,1), xaxs = "r", main = "Gender", col="red",cex.lab=0.8)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,
     cex.axis = 1, mgp = c(4,1.5,1))#
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
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.5))
#dev.off()


#  ---------- Plot Figure S2  [heterogeneous effect by interest in politics]
combined$politics_int_ct <- ifelse(combined$politics_int >= median(combined$politics_int), 1, 0) 

# politics_int_ct = 0
reg_res_mod2("outgroup_princomp",combined[combined$politics_int_ct==1,])
reg_res_mod2("swb",combined[combined$politics_int_ct==1 ,])
reg_res_mod2("c_news",combined[combined$politics_int_ct==1 ,])

# politics_int_ct = 1
reg_res_mod2("outgroup_princomp",combined[combined$politics_int_ct==1 ,])
reg_res_mod2("swb",combined[combined$politics_int_ct==1 ,])
reg_res_mod2("c_news",combined[combined$politics_int_ct==1 ,])

#  ---------- Plot Figure S2  [heterogeneous effect by interest in politics]
dev.off()
# pdf(file = "/Users/nejlaasimovic/Desktop/figS2_intpolitics.pdf",   # The directory you want to save the file in
# width = 5.5, # The width of the plot in inches
# height = 3.8) 

coef.vec<-c(reg_res_mod2("outgroup_princomp",combined[combined$politics_int_ct==1,])$estimate, reg_res_mod2("swb",combined[combined$politics_int_ct==1,])$estimate, reg_res_mod2("c_news",combined[combined$politics_int_ct==1,])$estimate)
coef.vec2<-c(reg_res_mod2("outgroup_princomp",combined[combined$politics_int_ct==0,])$estimate, reg_res_mod2("swb",combined[combined$politics_int_ct==0,])$estimate, reg_res_mod2("c_news",combined[combined$politics_int_ct==0,])$estimate)
se.vec<-c(reg_res_mod2("outgroup_princomp",combined[combined$politics_int_ct==1,])$std.error, reg_res_mod2("swb",combined[combined$politics_int_ct==1,])$std.error, reg_res_mod2("c_news",combined[combined$politics_int_ct==1,])$std.error)
se.vec2<-c(reg_res_mod2("outgroup_princomp",combined[combined$politics_int_ct==0,])$std.error, reg_res_mod2("swb",combined[combined$politics_int_ct==0,])$std.error, reg_res_mod2("c_news",combined[combined$politics_int_ct==0,])$std.error)

layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#w
var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 
adjust = 0.06
par(mgp=c(1.5,1,0))   # x axis closer
par(mar=c(9, 4, 5, 1))
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
#dev.off()

# ------------- AGE
combined$age_ct <- ifelse(combined$age >= 24, 1, 0)

coef.vec<-c(reg_res_mod1("outgroup_princomp",combined[combined$age_ct==1,])$estimate, reg_res_mod1("swb",combined[combined$age_ct==1,])$estimate, reg_res_mod1("c_news",combined[combined$age_ct==1,])$estimate)
coef.vec.2<-c(reg_res_mod1("outgroup_princomp",combined[combined$age_ct==0,])$estimate, reg_res_mod1("swb",combined[combined$age_ct==0,])$estimate, reg_res_mod1("c_news",combined[combined$age_ct==0,])$estimate)
se.vec<-c(reg_res_mod1("outgroup_princomp",combined[combined$age_ct==1,])$std.error, reg_res_mod1("swb",combined[combined$age_ct==1,])$std.error, reg_res_mod1("c_news",combined[combined$age_ct==1,])$std.error)
se.vec.2<-c(reg_res_mod1("outgroup_princomp",combined[combined$age_ct==0,])$std.error, reg_res_mod1("swb",combined[combined$age_ct==0,])$std.error, reg_res_mod1("c_news",combined[combined$age_ct==0,])$std.error)


#  ---------- Plot Figure S2  [heterogeneous effect by age]
dev.off()
# pdf(file = "/Users/nejlaasimovic/Desktop/figS2_age.pdf",   # The directory you want to save the file in
# width = 5.5, # The width of the plot in inches
# height = 3.8) 

coef.vec<-c(reg_res_mod2("outgroup_princomp",combined[combined$age_ct==1,])$estimate, reg_res_mod2("swb",combined[combined$age_ct==1,])$estimate, reg_res_mod2("c_news",combined[combined$age_ct==1,])$estimate)
coef.vec.2<-c(reg_res_mod2("outgroup_princomp",combined[combined$age_ct==0,])$estimate, reg_res_mod2("swb",combined[combined$age_ct==0,])$estimate, reg_res_mod2("c_news",combined[combined$age_ct==0,])$estimate)
se.vec<-c(reg_res_mod2("outgroup_princomp",combined[combined$age_ct==1,])$std.error, reg_res_mod2("swb",combined[combined$age_ct==1,])$std.error, reg_res_mod2("c_news",combined[combined$age_ct==1,])$std.error)
se.vec.2<-c(reg_res_mod2("outgroup_princomp",combined[combined$age_ct==0,])$std.error, reg_res_mod2("swb",combined[combined$age_ct==0,])$std.error, reg_res_mod2("c_news",combined[combined$age_ct==0,])$std.error)

adjust = 0.06
layout(matrix(c(2,1),1,2), 
       widths = c(1.5, 5))
var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 
par(mgp=c(2,1,0))  # x axis closer
par(mar=c(9, 5, 5, 1))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Age", col="red",cex.lab=0.8)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(2,1.5,1))#reduce label size, moves labels closer to tick marks; zadnji u mgp spusta x liniju nize
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
# draw dotted line through 0 
#box(bty = "l") #place box around plot
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust,lwd = 1.5, col = c("blue"))#draw lines connecting 95% confidence intervals
points(coef.vec.2, y.axis-adjust,pch = 17, cex = 1, col = c("blue")) #add point estimates for 2nd model; pch = 21 uses for overlay points, and "white" for white color
legend("bottom", 
       legend = c("Above median", "Below median"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.5))
# dev.off()

# ---------- ETHNICITY
# creating ethnic categories including those who self-identify by their religion
# Bosniak or Islam
combined$ethn=="1" | combined$ethn=="-1"

# Serb or Orthodox
combined$ethn=="2" | combined$ethn=="-4"

# Bosniak/Islam
reg_res_mod2("outgroup_princomp",combined[which(combined$ethn=="1" | combined$ethn=="-1"),])
reg_res_mod2("swb",combined[which(combined$ethn=="1" | combined$ethn=="-1"),])
reg_res_mod2("c_news",combined[which(combined$ethn=="1" | combined$ethn=="-1"),])

# Serb/Orthodox
reg_res_mod2("outgroup_princomp",combined[which(combined$ethn=="2" | combined$ethn=="-4"),])
reg_res_mod2("swb",combined[which(combined$ethn=="2" | combined$ethn=="-4"),])
reg_res_mod2("c_news",combined[which(combined$ethn=="2" | combined$ethn=="-4"),])


#  ---------- Plot Figure S2  [heterogeneous effect by ethnicity]
dev.off()
# pdf(file = "/Users/nejlaasimovic/Desktop/figS2_ethnicity.pdf",   # The directory you want to save the file in
# width = 5.5, # The width of the plot in inches
# height = 3.8) 
coef.vec<-c(reg_res_mod2("outgroup_princomp",combined[which(combined$ethn=="1" | combined$ethn=="-1"),])$estimate, reg_res_mod2("swb",combined[which(combined$ethn=="1" | combined$ethn=="-1"),])$estimate, reg_res_mod2("c_news",combined[which(combined$ethn=="1" | combined$ethn=="-1"),])$estimate)
coef.vec.2<-c(reg_res_mod2("outgroup_princomp",combined[which(combined$ethn=="2" | combined$ethn=="-4"),])$estimate, reg_res_mod2("swb",combined[which(combined$ethn=="2" | combined$ethn=="-4"),])$estimate, reg_res_mod2("c_news",combined[which(combined$ethn=="2" | combined$ethn=="-4"),])$estimate)
se.vec<-c(reg_res_mod2("outgroup_princomp",combined[which(combined$ethn=="1" | combined$ethn=="-1"),])$std.error, reg_res_mod2("swb",combined[which(combined$ethn=="1" | combined$ethn=="-1"),])$std.error, reg_res_mod2("c_news",combined[which(combined$ethn=="1" | combined$ethn=="-1"),])$std.error)
se.vec.2<-c(reg_res_mod2("outgroup_princomp",combined[which(combined$ethn=="2" | combined$ethn=="-4"),])$std.error, reg_res_mod2("swb",combined[which(combined$ethn=="2" | combined$ethn=="-4"),])$std.error, reg_res_mod2("c_news",combined[which(combined$ethn=="2" | combined$ethn=="-4"),])$std.error)

adjust = 0.06

layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#w
var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 
par(mgp=c(2,1,0))  # x axis closer
par(mar=c(9, 5, 5, 1))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1.5,1.5), xaxs = "r", main = "Ethnicity", col="red",cex.lab=0.8)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1.5,1.5,by=1.5), labels =c(-1.5,0,1.5) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,1.5,1))#reduce label size, moves labels closer to tick marks; zadnji u mgp spusta x liniju nize
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust,lwd = 1.5, col = c("blue"))#draw lines connecting 95% confidence intervals
points(coef.vec.2, y.axis-adjust,pch = 17, cex = 1, col = c("blue")) #add point estimates for 2nd model; pch = 21 uses for overlay points, and "white" for white color
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

#  ---------- EXPLORATORY [not-preregistered]: Heterogeneous effect by entity
# 1 Federation of BiH
# 2. Republika Srpska

# Federation = 1
reg_res_mod2("outgroup_princomp",combined[combined$entity==1,])
reg_res_mod2("swb",combined[combined$entity==1,])
reg_res_mod2("c_news",combined[combined$entity==1,])

# RS = 2
reg_res_mod2("outgroup_princomp",combined[combined$entity==2,])
reg_res_mod2("swb",combined[combined$entity==2,])
reg_res_mod2("c_news",combined[combined$entity==2,])

#  ---------- Plot Figure S2 -- Heterogeneous effect by entity
dev.off()
# pdf(file = "/Users/nejlaasimovic/Desktop/figS2_entity.pdf",   # The directory you want to save the file in
# width = 5.5, # The width of the plot in inches
# height = 3.8) 

coef.vec<-c(reg_res_mod2("outgroup_princomp",combined[combined$entity==1,])$estimate, reg_res_mod2("swb",combined[combined$entity==1,])$estimate, reg_res_mod2("c_news",combined[combined$entity==1,])$estimate)
coef.vec.2<-c(reg_res_mod2("outgroup_princomp",combined[combined$entity==2,])$estimate, reg_res_mod2("swb",combined[combined$entity==2,])$estimate, reg_res_mod2("c_news",combined[combined$entity==2,])$estimate)
se.vec<-c(reg_res_mod2("outgroup_princomp",combined[combined$entity==1,])$std.error, reg_res_mod2("swb",combined[combined$entity==1,])$std.error, reg_res_mod2("c_news",combined[combined$entity==1,])$std.error)
se.vec.2<-c(reg_res_mod2("outgroup_princomp",combined[combined$entity==2,])$std.error, reg_res_mod2("swb",combined[combined$entity==2,])$std.error, reg_res_mod2("c_news",combined[combined$entity==2,])$std.error)

adjust = 0.06
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot
       widths = c(1.5, 5))#w
var.names <- c("Out-Group Attitudes","Subjective Well-Being", "News Knowledge")
y.axis <- c(length(var.names):1) 
par(mgp=c(2,1,0))  # x axis closer
par(mar=c(9, 5, 5, 1))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = 19,cex = 0.8,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Entity", col="red",cex.lab=0.8)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,1.5,1))#reduce label size, moves labels closer to tick marks; zadnji u mgp spusta x liniju nize
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=2.5)
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust,lwd = 1.5, col = c("blue"))#draw lines connecting 95% confidence intervals
points(coef.vec.2, y.axis-adjust,pch = 17, cex = 1, col = c("blue")) #add point estimates for 2nd model; pch = 21 uses for overlay points, and "white" for white color
legend("bottom", 
       legend = c("Federation", "RS"), 
       col = c("red", "blue"), xpd=TRUE,
       pch = c(16,17), 
       bty = T,
       pt.cex = 1, 
       cex = 0.5, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE, inset=c(0.5,-0.5))
#dev.off()

