#########################################################################################
# File-Name: 1.2-fb-substitutes_r.R
# Date: 2021
# Purpose: Explores the activities users engaged in during the treatment week
# Machine: MacOS High Sierra
###########################################################################################

source("01-analysis_r.R") 

# -----------------------------------------------
#  Facebook Substitutes -  Fig S3
# -----------------------------------------------
# The question of activities users engage in during the treatment week was multiple-choice
# hence, we need to split it up

final_data$inst <- str_count(final_data$activ, "1")
final_data$oth_online <- str_count(final_data$activ, "2")
final_data$tv <- str_count(final_data$activ, "3")
final_data$fam <- str_count(final_data$activ, "4")
final_data$notech <- str_count(final_data$activ, "5")
final_data$online_news <- str_count(final_data$activ, "6")

covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")

# extracting and naming coefficients and standard errors for easier plotting below
inst_fs<-lm_lin(inst~treatment, covariates=covIn, data=final_data)
inst_fs_coef<-inst_fs$coefficients[2]/sd(final_data[final_data$treatment=="0",]$inst)
inst_fs_se<-inst_fs$std.error[2]/sd(final_data[final_data$treatment=="0",]$inst,na.rm=T)

oth_online_fs<-lm_lin(oth_online~treatment, covariates=covIn, data=final_data)
oth_online_fs_coef<-oth_online_fs$coefficients[2]/sd(final_data[final_data$treatment=="0",]$oth_online,na.rm=T)
oth_online_fs_se<-oth_online_fs$std.error[2]/sd(final_data[final_data$treatment=="0",]$oth_online,na.rm=T)

tv_fs<-lm_lin(tv~treatment, covariates=covIn, data=final_data)
tv_fs_coef<-tv_fs$coefficients[2]/sd(final_data[final_data$treatment=="0",]$tv,na.rm=T)
tv_fs_se<-tv_fs$std.error[2]/sd(final_data[final_data$treatment=="0",]$tv,na.rm=T)

fam_fs<-lm_lin(fam~treatment, covariates=covIn, data=final_data)
fam_fs_coef<-fam_fs$coefficients[2]/sd(final_data[final_data$treatment=="0",]$fam,na.rm=T)
fam_fs_se<-fam_fs$std.error[2]/sd(final_data[final_data$treatment=="0",]$fam,na.rm=T)

notech_fs<-lm_lin(notech~treatment, covariates=covIn, data=final_data)
notech_fs_coef<-notech_fs$coefficients[2]/sd(final_data[final_data$treatment=="0",]$notech,na.rm=T)
notech_fs_se<-notech_fs$std.error[2]/sd(final_data[final_data$treatment=="0",]$notech,na.rm=T)

online_news_fs<-lm_lin(online_news~treatment, covariates=covIn, data=final_data)
online_news_fs_coef<-online_news_fs$coefficients[2]/sd(final_data[final_data$treatment=="0",]$online_news,na.rm=T)
online_news_fs_se<-online_news_fs$std.error[2]/sd(final_data[final_data$treatment=="0",]$online_news,na.rm=T)


# -------- *Plot S3* ------------
#pdf(file = "figureS3.pdf",   # The directory you want to save the file in
#width = 5.5, # The width of the plot in inches
#height = 3.8) 

par(mar=c(12,8,3,1)) 
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
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.8) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.4)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.4)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis,lwd = 1.5, col = c("blue"))
# dev.off()

# --------------------------------------------------
#  Facebook substitutes - online vs. offline: Fig S4
# --------------------------------------------------
# -------- *Plot S4 [right panel]* ------------

dev.off() 
# pdf(file = "fig4_top.pdf",   # The directory you want to save the file in
  #  width = 5.5, # The width of the plot in inches
   # height = 3.8) 

layout(matrix(c(2,1),1,2), 
       widths = c(1.5, 5))
par(mfrow=c(1,2)) 
coef.vec <- c(reg_res_mod3("inst",final_data[is.na(final_data$percentage_bosniak.3),])$estimate,
             reg_res_mod3("oth_online",final_data[is.na(final_data$percentage_bosniak.3),])$estimate,
             reg_res_mod3("tv",final_data[is.na(final_data$percentage_bosniak.3),])$estimate,
             reg_res_mod3("fam",final_data[is.na(final_data$percentage_bosniak.3),])$estimate,
             reg_res_mod3("notech",final_data[is.na(final_data$percentage_bosniak.3),])$estimate,
             reg_res_mod3("online_news",final_data[is.na(final_data$percentage_bosniak.3),])$estimate)
se.vec <- c(reg_res_mod3("inst",final_data[is.na(final_data$percentage_bosniak.3),])$std.error,
           reg_res_mod3("oth_online",final_data[is.na(final_data$percentage_bosniak.3),])$std.error,
           reg_res_mod3("tv",final_data[is.na(final_data$percentage_bosniak.3),])$std.error,
           reg_res_mod3("fam",final_data[is.na(final_data$percentage_bosniak.3),])$std.error,
           reg_res_mod3("notech",final_data[is.na(final_data$percentage_bosniak.3),])$std.error,
           reg_res_mod3("online_news",final_data[is.na(final_data$percentage_bosniak.3),])$std.error)
var.names <- c("Instagram","Other Online Activities","TV","Family & Friends","No Tech","Online News")
results_df <- data.frame(term=var.names, estimate=coef.vec,
                         std.error=se.vec)
y.axis <- c(length(coef.vec):1) #par(mar=c(2, 13, 0, 0))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1,ylab = "", pch = 19,cex = 0.9,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Facebook Substitutes: No Online Information Sent", cex.main=0.8 , mgp = c(3,.7,0.2), col=c("blue",
                                                                                                                                    "blue","blue",
                                                                                                                                    "blue",
                                                                                                                                    "blue",
                                                                                                                                    "blue"),cex.lab=0.8)

segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.1, col = c("blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "blue"))
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.9) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.4)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.4)
# dev.off()


# -------- *Plot S4 [left panel]* ------------
# Facebook substitutes: Did not send online info
# pdf(fig4_top.pdf",   # The directory you want to save the file in
# width = 5.5, # The width of the plot in inches
# height = 3.8) 

coef.vec<- c(reg_res_mod3("inst",final_data[!is.na(final_data$percentage_bosniak.3),])$estimate,
             reg_res_mod3("oth_online",final_data[!is.na(final_data$percentage_bosniak.3),])$estimate,
             reg_res_mod3("tv",final_data[!is.na(final_data$percentage_bosniak.3),])$estimate,
             reg_res_mod3("fam",final_data[!is.na(final_data$percentage_bosniak.3),])$estimate,
             reg_res_mod3("notech",final_data[!is.na(final_data$percentage_bosniak.3),])$estimate,
             reg_res_mod3("online_news",final_data[!is.na(final_data$percentage_bosniak.3),])$estimate)
se.vec<- c(reg_res_mod3("inst",final_data[!is.na(final_data$percentage_bosniak.3),])$std.error,
           reg_res_mod3("oth_online",final_data[!is.na(final_data$percentage_bosniak.3),])$std.error,
           reg_res_mod3("tv",final_data[!is.na(final_data$percentage_bosniak.3),])$std.error,
           reg_res_mod3("fam",final_data[!is.na(final_data$percentage_bosniak.3),])$std.error,
           reg_res_mod3("notech",final_data[!is.na(final_data$percentage_bosniak.3),])$std.error,
           reg_res_mod3("online_news",final_data[!is.na(final_data$percentage_bosniak.3),])$std.error)
var.names <- c("Instagram","Other Online Activities","TV","Family & Friends","No Tech","Online News")
results_df <- data.frame(term=var.names, estimate=coef.vec,
                         std.error=se.vec)
y.axis <- c(length(coef.vec):1) 
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1,ylab = "", pch = 19,cex = 0.9,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Facebook Substitutes: Online Information Sent", cex.main=0.8, mgp = c(3,.7,0.2), col=c("blue",
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
                                                                                                      "blue"))
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.9) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.4)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.4)
# dev.off()








