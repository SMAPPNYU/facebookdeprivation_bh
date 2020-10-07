# -----------------------------------------------
#  Facebook substitutes - overall sample: Fig S3
# -----------------------------------------------
combined$inst<-str_count(combined$activ, "1")
combined$oth_online <- str_count(combined$activ, "2")
combined$tv <-str_count(combined$activ, "3")
combined$fam <- str_count(combined$activ, "4")
combined$notech <- str_count(combined$activ, "5")
combined$online_news <- str_count(combined$activ, "6")

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
inst_fs<-lm_lin(inst~treatment, covariates=covIn, data=combined)
inst_fs_coef<-inst_fs$coefficients[2]/sd(combined[combined$treatment=="0",]$inst)
inst_fs_se<-inst_fs$std.error[2]/sd(combined[combined$treatment=="0",]$inst)

oth_online_fs<-lm_lin(oth_online~treatment, covariates=covIn, data=combined)
oth_online_fs_coef<-oth_online_fs$coefficients[2]/sd(combined[combined$treatment=="0",]$oth_online)
oth_online_fs_se<-oth_online_fs$std.error[2]/sd(combined[combined$treatment=="0",]$oth_online)

tv_fs<-lm_lin(tv~treatment, covariates=covIn, data=combined)
tv_fs_coef<-tv_fs$coefficients[2]/sd(combined[combined$treatment=="0",]$tv)
tv_fs_se<-tv_fs$std.error[2]/sd(combined[combined$treatment=="0",]$tv)

fam_fs<-lm_lin(fam~treatment, covariates=covIn, data=combined)
fam_fs_coef<-fam_fs$coefficients[2]/sd(combined[combined$treatment=="0",]$fam)
fam_fs_se<-fam_fs$std.error[2]/sd(combined[combined$treatment=="0",]$fam)

notech_fs<-lm_lin(notech~treatment, covariates=covIn, data=combined)
notech_fs_coef<-notech_fs$coefficients[2]/sd(combined[combined$treatment=="0",]$notech)
notech_fs_se<-notech_fs$std.error[2]/sd(combined[combined$treatment=="0",]$notech)

online_news_fs<-lm_lin(online_news~treatment, covariates=covIn, data=combined)
online_news_fs_coef<-online_news_fs$coefficients[2]/sd(combined[combined$treatment=="0",]$online_news)
online_news_fs_se<-online_news_fs$std.error[2]/sd(combined[combined$treatment=="0",]$online_news)


# -------- *Plot S3* ------------

dev.off() #
#pdf("figureS3.pdf")
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#
par(mar=c(10,8,3,1)) 
coef.vec<- c(inst_fs_coef,oth_online_fs_coef,tv_fs_coef,fam_fs_coef,notech_fs_coef,online_news_fs_coef)
se.vec <- c(inst_fs_se,oth_online_fs_se,tv_fs_se,fam_fs_se,notech_fs_se,online_news_fs_se)
var.names <- c("Instagram","Other Online Activities","TV","Family & Friends","No Tech","Online News")
results_df <- data.frame(term=var.names, estimate=coef.vec,
                         std.error=se.vec)
y.axis <- c(length(coef.vec):1) #par(mar=c(2, 13, 0, 0))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1,ylab = "", pch = 19,cex = 0.8,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Facebook Substitutes", cex.main=0.8 , mgp = c(3,.7,0.2), col=c("blue",
                                                                                                        "blue",
                                                                                                        "blue",
                                                                                                    "blue",
                                                                                                        "blue",
                                                                                                        "blue",
                                                                                                        "blue",
                                                                                                        "blue",
                                                                                                        "blue",
                                                                                                        "blue"),cex.lab=0.8)

axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.8) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.4)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.4)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis,lwd = 1.5, col = c("blue"))
#dev.off()

# --------------------------------------------------
#  Facebook substitutes - online vs. offline: Fig S4
# --------------------------------------------------

# Facebook substitutes: Sent online info
combined$inst<-str_count(combined$activ, "1")
combined$oth_online <- str_count(combined$activ, "2")
combined$tv <-str_count(combined$activ, "3")
combined$fam <- str_count(combined$activ, "4")
combined$notech <- str_count(combined$activ, "5")
combined$online_news <- str_count(combined$activ, "6")
pal <- colorRampPalette(colors = c("lightblue", "blue"))(7)

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")

inst_fs_online<-lm_lin(inst~treatment, covariates=covIn, data=combined[!is.na(combined$bosniak.3),])
inst_fs_online_coef<-inst_fs_online$coefficients[2]/sd(combined[combined$treatment=="0" & !is.na(combined$bosniak.3),]$inst)
inst_fs_online_se<-inst_fs_online$std.error[2]/sd(combined[combined$treatment=="0"& !is.na(combined$bosniak.3),]$inst)

oth_online_fs_online <-lm_lin(oth_online~treatment, covariates=covIn, data=combined[!is.na(combined$bosniak.3),])
oth_online_fs_online_coef<-oth_online_fs_online$coefficients[2]/sd(combined[combined$treatment=="0" & !is.na(combined$bosniak.3),]$oth_online)
oth_online_fs_online_se<-oth_online_fs_online$std.error[2]/sd(combined[combined$treatment=="0"& !is.na(combined$bosniak.3),]$oth_online)

tv_fs_online<-lm_lin(tv~treatment, covariates=covIn, data=combined[!is.na(combined$bosniak.3),])
tv_fs_online_coef<-tv_fs_online$coefficients[2]/sd(combined[combined$treatment=="0" & !is.na(combined$bosniak.3),]$tv)
tv_fs_online_se<-tv_fs_online$std.error[2]/sd(combined[combined$treatment=="0"& !is.na(combined$bosniak.3),]$tv)

fam_fs_online<-lm_lin(fam~treatment, covariates=covIn, data=combined[!is.na(combined$bosniak.3),])
fam_fs_online_coef<-fam_fs_online$coefficients[2]/sd(combined[combined$treatment=="0" & !is.na(combined$bosniak.3),]$fam)
fam_fs_online_se<-fam_fs_online$std.error[2]/sd(combined[combined$treatment=="0"& !is.na(combined$bosniak.3),]$fam)

notech_fs_online<-lm_lin(notech~treatment, covariates=covIn, data=combined[!is.na(combined$bosniak.3),])
notech_fs_online_coef<-notech_fs_online$coefficients[2]/sd(combined[combined$treatment=="0" & !is.na(combined$bosniak.3),]$notech)
notech_fs_online_se<-notech_fs_online$std.error[2]/sd(combined[combined$treatment=="0"& !is.na(combined$bosniak.3),]$notech)

online_news_fs_online<-lm_lin(online_news~treatment, covariates=covIn, data=combined[!is.na(combined$bosniak.3),])
online_news_fs_online_coef<-online_news_fs_online$coefficients[2]/sd(combined[combined$treatment=="0" & !is.na(combined$bosniak.3),]$online_news)
online_news_fs_online_se<-online_news_fs_online$std.error[2]/sd(combined[combined$treatment=="0"& !is.na(combined$bosniak.3),]$online_news)

 
# -------- *Plot S4 [top panel]* ------------

dev.off() #
#pdf(file = "/Users/nejlaasimovic/Desktop/fig4_top.pdf",   # The directory you want to save the file in
  #  width = 5.5, # The width of the plot in inches
   # height = 3.8) 

layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#
par(mgp=c(2.5,1,0)) 
coef.vec<- c(inst_fs_online_coef,oth_online_fs_online_coef,tv_fs_online_coef,fam_fs_online_coef,notech_fs_online_coef,online_news_fs_online_coef)
se.vec<- c(inst_fs_online_se,oth_online_fs_online_se,tv_fs_online_se,fam_fs_online_se,notech_fs_online_se,online_news_fs_online_se)
var.names <- c("Instagram","Other Online Activities","TV","Family & Friends","No Tech","Online News")
results_df <- data.frame(term=var.names, estimate=coef.vec,
                         std.error=se.vec)
y.axis <- c(length(coef.vec):1) #par(mar=c(2, 13, 0, 0))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1,ylab = "", pch = 19,cex = 0.9,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Facebook Substitutes: Sent Online Info", cex.main=0.9 , mgp = c(3,.7,0.2), col=c("blue",
                                                                                                                                  "blue","blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue"),cex.lab=0.8)

segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.1, col = c("blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.9) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.4)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.4)
# dev.off()



# Facebook substitutes: Did not send online info

inst_fs_online2<-lm_lin(inst~treatment, covariates=covIn, data=combined[is.na(combined$bosniak.3),])
inst_fs_online_coef2<-inst_fs_online2$coefficients[2]/sd(combined[combined$treatment=="0" & is.na(combined$bosniak.3),]$inst)
inst_fs_online_se2<-inst_fs_online2$std.error[2]/sd(combined[combined$treatment=="0"& is.na(combined$bosniak.3),]$inst)

oth_online_fs_online2 <-lm_lin(oth_online~treatment, covariates=covIn, data=combined[is.na(combined$bosniak.3),])
oth_online_fs_online_coef2<-oth_online_fs_online2$coefficients[2]/sd(combined[combined$treatment=="0" & is.na(combined$bosniak.3),]$oth_online)
oth_online_fs_online_se2<-oth_online_fs_online2$std.error[2]/sd(combined[combined$treatment=="0"& is.na(combined$bosniak.3),]$oth_online)

tv_fs_online2<-lm_lin(tv~treatment, covariates=covIn, data=combined[is.na(combined$bosniak.3),])
tv_fs_online_coef2<-tv_fs_online2$coefficients[2]/sd(combined[combined$treatment=="0" & is.na(combined$bosniak.3),]$tv)
tv_fs_online_se2<-tv_fs_online2$std.error[2]/sd(combined[combined$treatment=="0"& is.na(combined$bosniak.3),]$tv)

fam_fs_online2<-lm_lin(fam~treatment, covariates=covIn, data=combined[is.na(combined$bosniak.3),])
fam_fs_online_coef2<-fam_fs_online2$coefficients[2]/sd(combined[combined$treatment=="0" & is.na(combined$bosniak.3),]$fam)
fam_fs_online_se2<-fam_fs_online2$std.error[2]/sd(combined[combined$treatment=="0"& is.na(combined$bosniak.3),]$fam)

notech_fs_online2<-lm_lin(notech~treatment, covariates=covIn, data=combined[is.na(combined$bosniak.3),])
notech_fs_online_coef2<-notech_fs_online2$coefficients[2]/sd(combined[combined$treatment=="0" & is.na(combined$bosniak.3),]$notech)
notech_fs_online_se2<-notech_fs_online2$std.error[2]/sd(combined[combined$treatment=="0"& is.na(combined$bosniak.3),]$notech)

online_news_fs_online2<-lm_lin(online_news~treatment, covariates=covIn, data=combined[is.na(combined$bosniak.3),])
online_news_fs_online_coef2<-online_news_fs_online2$coefficients[2]/sd(combined[combined$treatment=="0" & is.na(combined$bosniak.3),]$online_news)
online_news_fs_online_se2<-online_news_fs_online2$std.error[2]/sd(combined[combined$treatment=="0"& is.na(combined$bosniak.3),]$online_news)


# -------- *Plot S4 [bottom panel]* ------------
dev.off() #
# pdf(file = "/Users/nejlaasimovic/Desktop/fig4_bottom.pdf",   # The directory you want to save the file in
  #  width = 5.5, # The width of the plot in inches
  #  height = 3.8) 
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#
par(mgp=c(2.5,1,0)) 
coef.vec2<- c(inst_fs_online_coef2,oth_online_fs_online_coef2,tv_fs_online_coef2,fam_fs_online_coef2,notech_fs_online_coef2,online_news_fs_online_coef2)
se.vec2<- c(inst_fs_online_se2,oth_online_fs_online_se2,tv_fs_online_se2,fam_fs_online_se2,notech_fs_online_se2,online_news_fs_online_se2)
var.names <- c("Instagram","Other Online Activities","TV","Family & Friends","No Tech","Online News")
results_df <- data.frame(term=var.names, estimate=coef.vec2,
                         std.error=se.vec2)
y.axis <- c(length(coef.vec2):1) #par(mar=c(2, 13, 0, 0))
plot(coef.vec2, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1.4,ylab = "", pch = 19,cex = 0.9,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Facebook Substitutes: Did Not Send Online Info", cex.main=0.9 , mgp = c(3,.7,0.2), col=c("blue",
                                                                                                                                  "blue","blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue",
                                                                                                                                  "blue"),cex.lab=0.8)

segments(coef.vec2-qnorm(.975)*se.vec2, y.axis, coef.vec2+qnorm(.975)*se.vec2, y.axis, lwd = 1.4, col = c("blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.9) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.7)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.7)
# dev.off()

















