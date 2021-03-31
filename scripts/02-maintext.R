#########################################################################################
# File-Name: 02-maintext_r.R
# Date: 2021
# Purpose: # This script replicates the figures and tables presented in the main text
# Machine: MacOS High Sierra
###########################################################################################

source("1.3-offline-networks.R") # loading the previous script

# ----------------------
#     FIGURE 1
# ----------------------

# Figure 1 (Panel A)

#pdf(file = "figure1_A.pdf",   # The directory you want to save the file in
    #width = 5.4, # The width of the plot in inches
    #height = 4.4) 
par(mfrow=c(1,2), mar=c(15, 12, 4, 1)) 
coef.vec <- c(reg_res_mod3("c_news",final_data)$estimate,
               reg_res_mod3("swb",final_data)$estimate,
               reg_res_mod3("satisf",final_data)$estimate,
               reg_res_mod3("joy",final_data)$estimate,
               reg_res_mod3("fulf",final_data)$estimate,
               -reg_res_mod3("nerv",final_data)$estimate,
               -reg_res_mod3("boredom",final_data)$estimate,
               -reg_res_mod3("loneliness",final_data)$estimate,
               -reg_res_mod3("depression",final_data)$estimate,
               -reg_res_mod3("isol",final_data)$estimate)
# "negative" emotions were reverse coded in the regression tables and creation of index so that higher number indicates more positive attribution across;
# returning it to original direction for clearer visualization
se.vec <- c(reg_res_mod3("c_news",final_data)$std.error,
            reg_res_mod3("swb",final_data)$std.error,
            reg_res_mod3("satisf",final_data)$std.error,
            reg_res_mod3("joy",final_data)$std.error,
            reg_res_mod3("fulf",final_data)$std.error,
            reg_res_mod3("nerv",final_data)$std.error,
            reg_res_mod3("boredom",final_data)$std.error,
            reg_res_mod3("loneliness",final_data)$std.error,
            reg_res_mod3("depression",final_data)$std.error,
            reg_res_mod3("isol",final_data)$std.error)

var.names <- c("News Knowledge","Well-being index","Satisfaction","Joy","Fulfillment","Anxiety","Boredom","Loneliness", "Depression","Isolation")
results_df <- data.frame(term=var.names, estimate=coef.vec,
                         std.error=se.vec)
y.axis <- c(length(coef.vec):1) 
par(mar=c(7, 10, 0, 0))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1,ylab = "", pch = 19,cex = 1,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "", mgp = c(3,.7,0.2), col=c("blue",
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
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.6)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.6)
# dev.off()


# Figure 1 (Panel B)
# dev.off()
# pdf(file = "figure1_B.pdf",   # The directory you want to save the file in
    # width = 5.8, # The width of the plot in inches
    # height = 5.1) 
coef.vec_outgroup <- c(reg_res_mod3("ft",final_data)$estimate,
                       reg_res_mod3("sd",final_data)$estimate,
                       reg_res_mod3("multi_stat",final_data)$estimate,
                       reg_res_mod3("othchrct_final_data",final_data)$estimate,
                       reg_res_mod3("inchrct_final_data",final_data)$estimate,
                       reg_res_mod3("outgroup_princomp",final_data)$estimate)
se.vec_outgroup <- c(reg_res_mod3("ft",final_data)$std.error,
                     reg_res_mod3("sd",final_data)$std.error,
                     reg_res_mod3("multi_stat",final_data)$std.error,
                     reg_res_mod3("othchrct_final_data",final_data)$std.error,
                     reg_res_mod3("inchrct_final_data",final_data)$std.error,
                     reg_res_mod3("outgroup_princomp",final_data)$std.error)

var.names <- c("Feeling thermometer","Social closeness","Cooperation","Perception of \n out-group evaluations","Out-group traits","Out-group regard index")
results_df <- data.frame(term=var.names, estimate=coef.vec_outgroup,std.error=se.vec_outgroup)
y.axis <- c(length(coef.vec_outgroup):1)

plot(coef.vec_outgroup, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)" , cex.lab=1,ylab = "", pch = 19,cex = 1,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), main = "", col=c("red","red","red","red","red","red"),mgp = c(3,.7,0.2))
segments(coef.vec_outgroup-qnorm(.975)*se.vec_outgroup, y.axis, coef.vec_outgroup+qnorm(.975)*se.vec_outgroup, y.axis, lwd = 1.1, col = c("red","red","red","red","red","red"))
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1,  mgp = c(3,.7,0.2))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.6)
# dev.off()


# ------------
# FIGURE 2
# ------------
inst <- sum(str_count(final_data[final_data$treatment == 1,]$activ, "1"))/nrow(final_data[final_data$treatment == 1,])
oth_online <- sum(str_count(final_data[final_data$treatment == 1,]$activ, "2"))/nrow(final_data[final_data$treatment == 1,])
tv <- sum(str_count(final_data[final_data$treatment == 1 ,]$activ, "3"))/nrow(final_data[final_data$treatment == 1,])
fam <- sum(str_count(final_data[final_data$treatment == 1,]$activ, "4"))/nrow(final_data[final_data$treatment == 1,])
notech <- sum(str_count(final_data[final_data$treatment == 1,]$activ, "5"))/nrow(final_data[final_data$treatment == 1,])
online_news <- sum(str_count(final_data[final_data$treatment == 1,]$activ, "6"))/nrow(final_data[final_data$treatment == 1,])
pal <- colorRampPalette(colors = c("lightblue", "blue"))(7)

# png("figure2.png",   # The directory you want to save the file in
 # width = 7, # The width of the plot in inches
  #  height = 4.1) 
par(mar = c(10, 30, 5, 10))
x <- c("Friends & Family", "Instagram","No Tech", "News online","Other online","TV")
y <- c(fam, inst,notech,online_news,oth_online,tv)
data <- data.frame(x, y, stringsAsFactors = FALSE)
data$x <- factor(data$x, levels = unique(data$x)[order(data$y, decreasing = TRUE)])
plot_ly(data,x=~x, y=~y, 
        name="FDs",type="bar",
        marker = list(color = c("rgba(70, 120, 150, 5)",
                                "rgba(70, 120, 150, 0.9)",
                                "rgba(70, 120, 150, 0.8)",
                                "rgba(50, 120, 150, 0.6)",
                                "rgba(50, 120, 150, 0.4)",
                                "rgba(50, 120, 150, 0.2"))) %>% layout(xaxis = list(title = "", tickangle = -45,categoryorder = "array",tickfont=list(size=18)),
                                                                       yaxis = list(title = "Proportion of responses",categoryorder = "array",tickfont=list(size=18), mgp=c(5,1,0),titlefont = list(size=20),line=2))
# dev.off()

# ------------
# FIGURE 3
# ------------
dev.off()
par(mfrow=c(1,2), mar=c(10, 6, 6, 0.1)) 

coef.vec_fig3 <- c(reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$se_median_index==1,])$estimate, reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$ms_median_index==1,])$estimate, reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$ef_median_index==1,])$estimate)
se.vec_fig3 <-  c(reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$se_median_index==1,])$std.error, reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$ms_median_index==1,])$std.error, reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$ef_median_index==1,])$std.error)

var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1)
plot(coef.vec_fig3, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Homogenous", col=c("black","blue","red"), cex.lab=0.9)
segments(coef.vec_fig3-qnorm(.975)*se.vec_fig3, y.axis, coef.vec_fig3+qnorm(.975)*se.vec_fig3, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

# heterogenous - right panel
coef.vec_fig3 <- c(reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$se_median_index==0,])$estimate, reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$ms_median_index==0,])$estimate, reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$ef_median_index==0,])$estimate)
se.vec_fig3 <-  c(reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$se_median_index==0,])$std.error, reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$ms_median_index==0,])$std.error, reg_res_mod3_ethn("outgroup_princomp",final_data[final_data$ef_median_index==0,])$std.error)
adjust = 0.26
plot(coef.vec_fig3, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Heterogenous", col=c("black","blue","red"),cex.lab=0.9)
segments(coef.vec_fig3-qnorm(.975)*se.vec_fig3, y.axis, coef.vec_fig3+qnorm(.975)*se.vec_fig3, y.axis, lwd = 1.6, c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)

y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

legend(x=-4, y=-0.3,
       legend = c("Shannon Entropy","Majority Group Share","Ethnic Fractionalization"), 
       col=c("black","blue","red"), xpd=NA,
       pch = c(19,17,12), 
       bty = T,
       pt.cex = 1.1, 
       cex = 0.8, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE)
title("Offline Networks", line = -1.6,adj=0.2, outer = TRUE)
dev.off()
