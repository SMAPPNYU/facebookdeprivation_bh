# Figures and Tables from the main text [coefficient and standard values derived
# from the analysis in code_review_megan_september.R]

# Set working directory
rootArg <-  if(Sys.info()["user"]=="nejlaasimovic"){
  setwd("~/Users/nejlaasimovic/Desktop")
}

# Install or/and activate packages
library(dplyr); library(plotly)
# ------------
# FIGURE 1
# ------------

# Figure 1 (Panel A)
dev.off()
png(filename="figure1_A.png")
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))
par(mar=c(10,8,3,1)) 
coef.vec<- c(-0.243,0.176,0.003 ,0.059, -0.009, -0.384,0.042,-0.220, -0.120,0.018)
se.vec <- c(0.108,0.106,0.116,0.106,0.107,0.108,0.111,0.106,0.113, 0.117)
var.names <- c("News Knowledge","Well-being index","Satisfaction","Joy","Fulfillment","Anxiety","Boredom","Loneliness", "Depression","Isolation")
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
# draw dotted line through 0 
#box(bty = "l") #place box around plot

# Figure 1 (Panel B)
dev.off()
png(filename="figure1_B.png")
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#
par(mar=c(10,8,3,1)) #20 je desna strana
coef.vec<- c(-0.238, -0.160,-0.223,-0.038, -0.007, -0.237)
se.vec <- c(0.113,0.116, 0.112,0.110,0.107, 0.113)
var.names <- c("Feeling thermometer","Social closeness","Cooperation","Perception of \n out-group evaluations","Out-group traits","Out-group regard index")
results_df <- data.frame(term=var.names, estimate=coef.vec,std.error=se.vec)
y.axis <- c(length(coef.vec):1)
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)" , cex.lab=2.3,ylab = "", pch = 19,cex = 1,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), main = "", col=c("red","red","red","red","red","red","red","lightblue"),cex.lab=1)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.2, col = c("red","red","red","red","red","red","red","lightblue"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(7,0.5,0.5))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1.2) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="blue",lty=2,lwd=1.6)

# ------------
# FIGURE 2
# ------------
inst<-sum(str_count(combined[combined$treatment == 1 & combined$mediancateg==0,]$activ, "1"))
oth_online <- sum(str_count(combined[combined$treatment == 1,]$activ, "2"))
tv <- sum(str_count(combined[combined$treatment == 1 & combined$mediancateg==0,]$activ, "3"))
fam <- sum(str_count(combined[combined$treatment == 1& combined$mediancateg==0,]$activ, "4"))
notech <- sum(str_count(combined[combined$treatment == 1& combined$mediancateg==0,]$activ, "5"))
online_news <- sum(str_count(combined[combined$treatment == 1& combined$mediancateg==0,]$activ, "6"))
pal <- colorRampPalette(colors = c("lightblue", "blue"))(7)

dev.off()
png(filename="figure1_A.png")
par(mar = c(10, 30, 5, 10))
x <- c("Friends & Fam", "Instagram","No Tech", "News online","Other online","TV")
y = c(83/159,79/159,53/159,45/159,34/159,22/159)
data<-data.frame(x, y, stringsAsFactors = FALSE)
data$x <- factor(data$x, levels = unique(data$x)[order(data$y, decreasing = TRUE)])
plot_ly(data,x=~x, y=~y, 
        name="FDs",type="bar",
        marker = list(color = c("rgba(80, 120, 150, 1)",
                                "rgba(60, 120, 150, 3)",
                                "rgba(70, 120, 150, 0.8)",
                                "rgba(50, 120, 150, 0.6)",
                                "rgba(50, 120, 150, 0.4)",
                                "rgba(50, 120, 150, 0.2"))) %>% layout(xaxis = list(title = "", tickangle = -45,categoryorder = "array",tickfont=list(size=18)),
                                                                       yaxis = list(title = "Proportion of responses",categoryorder = "array",tickfont=list(size=18), mgp=c(5,1,0),titlefont = list(size=20),line=2))

# ------------
# FIGURE 3
# ------------
dev.off()
png(filename="figure3png")
par(mfrow=c(1,2)) 
par(mar=c(9, 7, 5, 1))
# par(mar=c(6, 3, 1, 5)) #20 je desna strana # 2 s
coef.vec<- c(-0.424281, -0.246915,  -0.4960176)
se.vec <- c(0.157716,0.1242222,  0.1580626)
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Homogenous", col=c("black","blue","red"), cex.lab=0.9)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.6, col = c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis

coef.vec.2<-c(-0.07709326,-0.166649, -0.01065272)
se.vec.2<-c(0.1503582, 0.2293604,0.1496785)
adjust = 0.26
plot(coef.vec.2, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", ylab = "", pch = c(19,17,12),cex = 1.2,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "Heterogenous", col=c("black","blue","red"),cex.lab=0.9)
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis, coef.vec.2+qnorm(.975)*se.vec.2, y.axis, lwd = 1.6, c("black","blue","red"))
abline(h = 0, v=0, col="grey",lty=2,lwd=2.5)
var.names <- c("Out-Group \n Index (SE)","Out-Group \n Index (MS)","Out-Group \n Index (EF)")
y.axis <- c(length(var.names):1) #par(mar=c(2, 13, 0, 0))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 1, mgp = c(4,2,1))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
#par(mfrow=c(1,2),oma=c(5,0,0,0),xpd=NA)
#coord <- par("usr")

legend(x=-4, y=-0.5,
       legend = c("Shannon Entropy", "Majority Group Share","Ethnic Fractionalization"), 
       col=c("black","blue","red"), xpd=NA,
       pch = c(19,17,12), 
       bty = T,
       pt.cex = 1, 
       cex = 0.6, 
       text.col = "black",   box.lty=1.2, box.lwd=0.5,
       horiz=TRUE)
title("Offline Networks", line = -1, outer = TRUE)
dev.off()
