# Figures and Tables from the main text [coefficients and standard values derived
# from the analysis in 01-analysis.R and 1-3-offline-networks.R]

# ------------
# FIGURE 1
# ------------

# Figure 1 (Panel A)
dev.off()
#pdf(file = "/Users/nejlaasimovic/Desktop/figure1_A.pdf",   # The directory you want to save the file in
    #width = 5.4, # The width of the plot in inches
    #height = 4.4) 
par(mar=c(4.1, 9.1, 2.1, 1.1))
coef.vec <- c(news_coef2,
                  swb_coef2,
                  satisfaction_coef2,
                  joy_coef2,
                  fulf_coef2,
                  -nerv_coef2, # "negative" emotions were reverse coded in the regression tables
              # and creation of index so that higher number indicates more positive attribution across;
              # returning it to original direction for clearer visualization
                  -boredom_coef2,
                  -loneliness_coef2,
                  -depression_coef2,
                  -isol_coef2)
se.vec <- c(news_se2,
                  swb_se2,
                  satisfaction_se2,
                  joy_se2,
                  fulf_se2,
                  nerv_se2,
                  boredom_se2,
                  loneliness_se2,
                  depression_se2,
                  isol_se2)

var.names <- c("News Knowledge","Well-being index","Satisfaction","Joy","Fulfillment","Anxiety","Boredom","Loneliness", "Depression","Isolation")
results_df <- data.frame(term=var.names, estimate=coef.vec,
                         std.error=se.vec)
y.axis <- c(length(coef.vec):1) 
#par(mar=c(2, 13, 0, 0))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1.2,ylab = "", pch = 19,cex = 1,#plot coefficients as points, turning off axes and labels.
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
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1.2) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.6)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.6)
# dev.off()


# Figure 1 (Panel B)
dev.off()
# pdf(file = "/Users/nejlaasimovic/Desktop/figure1_B.pdf",   # The directory you want to save the file in
    # width = 5.8, # The width of the plot in inches
    # height = 5.1) 
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#
par(mar=c(10,8,3,1)) #20 je desna strana

coef.vec_outgroup <- c(ft_coef2,
                       sd_coef2,
                       cpr_coef2,
                       perc_coef2,
                       otgrp_coef2,
                       outgroup_pc_coef2)
se.vec_outgroup <- c(ft_se2,
                     sd_se2,
                     cpr_se2,
                     perc_se2,
                     otgrp_se2,
                     outgroup_pc_se2)
var.names <- c("Feeling thermometer","Social closeness","Cooperation","Perception of \n out-group evaluations","Out-group traits","Out-group regard index")
results_df <- data.frame(term=var.names, estimate=coef.vec_outgroup,std.error=se.vec_outgroup)
y.axis <- c(length(coef.vec_outgroup):1)
plot(coef.vec_outgroup, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)" , cex.lab=2.3,ylab = "", pch = 19,cex = 1,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), main = "", col=c("red","red","red","red","red","red","red","red"),cex.lab=1)
segments(coef.vec_outgroup-qnorm(.975)*se.vec_outgroup, y.axis, coef.vec_outgroup+qnorm(.975)*se.vec_outgroup, y.axis, lwd = 1.2, col = c("red","red","red","red","red","red","red","red"))
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(7,0.5,0.5))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1.2) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.6)
# dev.off()


# ------------
# FIGURE 2
# ------------
inst<-sum(str_count(combined[combined$treatment == 1,]$activ, "1"))/nrow(combined[combined$treatment == 1,])
oth_online <- sum(str_count(combined[combined$treatment == 1,]$activ, "2"))/nrow(combined[combined$treatment == 1,])
tv <- sum(str_count(combined[combined$treatment == 1 ,]$activ, "3"))/nrow(combined[combined$treatment == 1,])
fam <- sum(str_count(combined[combined$treatment == 1,]$activ, "4"))/nrow(combined[combined$treatment == 1,])
notech <- sum(str_count(combined[combined$treatment == 1,]$activ, "5"))/nrow(combined[combined$treatment == 1,])
online_news <- sum(str_count(combined[combined$treatment == 1,]$activ, "6"))/nrow(combined[combined$treatment == 1,])
pal <- colorRampPalette(colors = c("lightblue", "blue"))(7)

dev.off()
# png("/Users/nejlaasimovic/Desktop/figure2.png",   # The directory you want to save the file in
 # width = 7, # The width of the plot in inches
  #  height = 4.1) 
par(mar = c(10, 30, 5, 10))
x <- c("Friends & Fam", "Instagram","No Tech", "News online","Other online","TV")
y<-c(fam, inst,notech,online_news,oth_online,tv)
#y <- c(83/159,79/159,53/159,45/159,34/159,22/159)
data<-data.frame(x, y, stringsAsFactors = FALSE)
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
# homogenous - left panel
# pdf(file = "/Users/nejlaasimovic/Desktop/figure3.pdf")  # The directory you want to save the file in
par(mfrow=c(1,2))
par(mar=c(10, 6, 6, 0.1))
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1)
coef.vec_fig3 <- c(outgroup_se_pc_coef2, outgroup_ms_pc_coef2, outgroup_ef_pc_coef2)
se.vec_fig3 <- c(outgroup_se_pc_se2, outgroup_ms_pc_se2, outgroup_ef_pc_se2)
plot(coef.vec_fig3, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Homogenous", col=c("black","blue","red"), cex.lab=0.9)
segments(coef.vec_fig3-qnorm(.975)*se.vec_fig3, y.axis, coef.vec_fig3+qnorm(.975)*se.vec_fig3, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
 #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

# heterogenous - right panel
coef.vec_fig3 <- c(outgroup_se_pc_htr_coef2,outgroup_ms_htr_pc_coef2, outgroup_ef_htr_pc_coef2)
se.vec_fig3 <- c(outgroup_se_pc_htr_se2, outgroup_ms_htr_pc_se2, outgroup_ef_htr_pc_se2)
adjust = 0.26
plot(coef.vec_fig3, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Heterogenous", col=c("black","blue","red"),cex.lab=0.9)
segments(coef.vec_fig3-qnorm(.975)*se.vec_fig3, y.axis, coef.vec_fig3+qnorm(.975)*se.vec_fig3, y.axis, lwd = 1.6, c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)

y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

legend(x=-3.7, y=0.01,
       legend = c("Shannon Entropy","Majority Group Share","Ethnic Fractionalization"), 
       col=c("black","blue","red"), xpd=NA,
       pch = c(19,17,12), 
       bty = T,
       pt.cex = 1, 
       cex = 0.7, 
       text.col = "black",   box.lty=1, box.lwd=0.5,
       horiz=TRUE)
title("Offline Networks", line = -1.6,adj=0.6, outer = TRUE)
dev.off()
