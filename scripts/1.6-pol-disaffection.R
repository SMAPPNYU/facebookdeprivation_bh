###################################################################################################################
# File-Name: 1.6-pol-disaffection.R
# Date: 2021
# Purpose: Analyzing the effect of the treatment of deactivation on the outcome of political disaffection
###################################################################################################################

source("1.5-offline-online-interaction.R") 

# ----------------------------------------------
#   POLITICAL DISAFFECTION [in SI, Section 14]
# -----------------------------------------------
# Creating indices as sums of z-scores

# cynicism
final_data$cynicism <- scale(final_data$agr_st1)+scale(final_data$agr_st2)+scale(final_data$agr_st3) # higher more positive attribution

# apathy
final_data$apathy <- scale(final_data$agr_st6)+scale(final_data$agr_st8)+scale(final_data$agr_st4) # higher more positive attribution

# skepticism
final_data$agr_st9 <- -final_data$agr_st9 # recoding, so that higher values indicate more positive attribution 
final_data$agr_st10 <- -final_data$agr_st10

final_data$skepticism <- scale(final_data$agr_st9) + scale(final_data$agr_st10) 

# --- Disaffection as PC index
disaffection <- c("agr_st1","agr_st2","agr_st3","agr_st4","agr_st6",
                  "agr_st8","agr_st9","agr_st10")
disaffection_pc <- final_data[,disaffection]
psych::alpha(disaffection_pc, 'check.keys=TRUE')$total$std.alpha 
prcomp(na.omit(disaffection_pc,na.action))
final_data$disaffection_pc_index <- pc_index(dataIn=disaffection_pc,
                                                 varList=disaffection)



# ------ Table S23
TableS23.1<-do.call(data.frame,rbind(
  reg_res_mod1("apathy",final_data),
  reg_res_mod1("cynicism",final_data),
  reg_res_mod1("skepticism",final_data),
  reg_res_mod1("disaffection_pc_index",final_data)))

TableS23.2<-do.call(data.frame,rbind(
  reg_res_mod2("apathy",final_data),
  reg_res_mod2("cynicism",final_data),
  reg_res_mod2("skepticism",final_data),
  reg_res_mod2("disaffection_pc_index",final_data)))

TableS23.3<-do.call(data.frame,rbind(
  reg_res_mod3("apathy",final_data),
  reg_res_mod3("cynicism",final_data),
  reg_res_mod3("skepticism",final_data),
  reg_res_mod3("disaffection_pc_index",final_data)))

# -------- *Table S23* ------------
tableS23 <- join_all(list(TableS23.1, TableS23.2, TableS23.3), by="outcome")
print(tableS23, digits=3)
# In Table 23, variables not reverse coded (hence the difference in signs) to make it easier to follow the meaning of estimates
# [i.e. positive estimate indicates an increase in apathy etc.]

# ------ Explanations

# external political efficacy

final_data$agr_st15 <- -final_data$agr_st15 # recoding, so that higher values indicate more positive attribution 
final_data$ext_efficacy <- scale(final_data$agr_st14)+scale(final_data$agr_st15)

# government efficacy
final_data$pol_efficacy <- scale(final_data$agr_st11)+scale(final_data$agr_st12)+scale(final_data$agr_st13)

# trust in institutions
final_data$trust <- scale(final_data$trust_media)+scale(final_data$trust_pol)+scale(final_data$trust_inst)+scale(final_data$trust_relg)+scale(final_data$trust_ngo)+scale(final_data$trust_gov)

# ---------- Plot [Figure S7]

dev.off() 
#pdf(file = "/Users/nejlaasimovic/Desktop/fig8.pdf",   # The directory you want to save the file in
#  width = 5.5, # The width of the plot in inches
# height = 3.8) 

layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#
par(mar=c(8, 5, 10, 4))
coef.vec <- c(-reg_res_mod3("apathy", final_data)$estimate, # variables were reverse coded in the creation of index
             # so that across variables higher number indicates more positive purposes,
             # but for visualization purposes, clearer that the coefficients indicate
             # what the name does i.e. increase in cynicisim as clearly negative
             -reg_res_mod3("cynicism", final_data)$estimate,
             -reg_res_mod3("skepticism", final_data)$estimate,
             -reg_res_mod3("disaffection_pc_index", final_data)$estimate)
se.vec <- c(reg_res_mod3("apathy", final_data)$std.error,
           reg_res_mod3("cynicism", final_data)$std.error,
           reg_res_mod3("skepticism", final_data)$std.error,
           reg_res_mod3("disaffection_pc_index", final_data)$std.error)
var.names <- c("Apathy","Cynicism","Skepticism","Disaffection (PC index)")
results_df <- data.frame(term=var.names, estimate=coef.vec,
                         std.error=se.vec)
y.axis <- c(length(coef.vec):1) 
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1,ylab = "", pch = 19,cex = 0.9,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "", cex.main=0.9 , mgp = c(3,.7,0.2), col=c("blue",
                                                                                    "blue","blue",
                                                                                    "red"),cex.lab=0.8)
title("Political Disaffection", line = 2)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.1, col = c("blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "red"))
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.9) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.4)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.4)
# dev.off()


# -------- Plotting explanatory/additional disaffection variables
dev.off() #
#pdf(file = "/Users/nejlaasimovic/Desktop/fig9.pdf",   # The directory you want to save the file in
#  width = 5.5, # The width of the plot in inches
# height = 3.8) 

layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#
par(mar=c(8, 5, 10, 4))
coef.vec<- c(reg_res_mod3("ext_efficacy", final_data)$estimate,
             reg_res_mod3("pol_efficacy", final_data)$estimate,
             reg_res_mod3("trust", final_data)$estimate)
se.vec<- c(reg_res_mod3("ext_efficacy", final_data)$std.error,
           reg_res_mod3("pol_efficacy", final_data)$std.error,
           reg_res_mod3("trust", final_data)$std.error)
var.names <- c("External Efficacy","Political Efficacy","Trust")
results_df <- data.frame(term=var.names, estimate=coef.vec,
                         std.error=se.vec)
y.axis <- c(length(coef.vec):1) 
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)", cex.lab=1,ylab = "", pch = 19,cex = 0.9,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), xaxs = "r", main = "", cex.main=0.9 , mgp = c(3,.7,0.2), col=c("blue",
                                                                                    "blue","blue",
                                                                                    "red"),cex.lab=0.8)
title("Additional Political Attitudes", line = 2)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.1, col = c("blue",
                                                                                                      "blue",
                                                                                                      "blue",
                                                                                                      "red"))
axis(1, at = seq(-1,1,by=0.5), labels =c(-1,-0.5,0,0.5,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(3,.7,0.2))#reduce label size, moves labels closer to tick marks
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 0.9) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="gray",lty=2,lwd=1.4)
abline(h = 9.5, v=0, col="gray",lty=2,lwd=1.4)
# dev.off()




