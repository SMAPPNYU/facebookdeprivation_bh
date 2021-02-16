# Analyzing online networks
combined$online_index <- NA

# Obtain proportions to be used in the analysis
combined$percentage_bosniak.3 <- combined$percentage_bosniak.3/100
combined$percentage_serbian.3 <- combined$percentage_serbian.3/100
combined$percentage_croatian.3 <- combined$percentage_croatian.3/100

# ------------- Ethnic heterogeneity: diversity continous measure and binary equal and above/below median index
for (i in 1:length(combined$code)){
  combined$online_index[i] <- 1 - (combined$percentage_bosniak.3[i]^2+combined$percentage_serbian.3[i]^2+combined$percentage_croatian.3[i]^2)
}
combined$online_ef_median_index<-ifelse(combined$online_index>=median(na.omit(combined$online_index)),0,1) # 1 is more homogeneous

# -------------- Shannon Entropy:  diversity continous measure and binary equal and above/below median index
for (i in 1:length(combined$code)){
  combined$shannon_online[i] <- -(combined$percentage_bosniak.3[i]*log(combined$percentage_bosniak.3[i])+combined$percentage_croatian.3[i]*log(combined$percentage_croatian.3[i])+combined$percentage_serbian.3[i]*log(combined$percentage_serbian.3[i]))}
combined$online_sh_median_index <-ifelse(combined$shannon_online >= median(na.omit(combined$shannon_online)),0,1) # 1 is more homogeneous

# Cannot calculate when one is 0 (log0) which is the case for code 278 so have to do it manually:
combined$shannon_online[278] <- -(0  + combined$percentage_croatian.3[i]*log(combined$percentage_croatian.3[278])+combined$percentage_serbian.3[278]*log(combined$percentage_serbian.3[278]))
combined$shannon_online[278]

# ----------- Majority Share Online:  diversity continous measure and binary equal and above/below median index
combined$online_ms_median_index <- ifelse(combined$largest_majority_online >= median(na.omit(combined$largest_majority_online)), 1, 0) # 1 is more homogeneous


# ----------------------------------------------------------------------------------
# Table S17: Difference between people who sent vs did not send their online data
# ----------------------------------------------------------------------------------

bal.variables<-c("gender","age","educ","employ1","trust_media.1","freq_news","freq_fbnews","politics_int","numb_forums","time_usage", "freq_usage","imp_ethn","imp_family","imp_cntry","imp_relg","imp_career","imp_neigh","imp_hobby")
df <- combined[,bal.variables]

p.values<-cbind(sapply(bal.variables, function (x) t.test(combined[is.na(combined$bosniak.3),][,x], combined[!is.na(combined$bosniak.3),][,x]))[3,])
online_offline_table <- do.call(data.frame, # creating a table combining the mean values and p-values
                    list(OnlineData = apply(df[!is.na(combined$bosniak.3),], 2, mean, na.rm=TRUE),
                    NoOnlineData =  apply(df[is.na(combined$bosniak.3),], 2, mean, na.rm=TRUE),
                    OnlineData = apply(df[!is.na(combined$bosniak.3),], 2, sd, na.rm=TRUE),
                    NoOnlineData =  apply(df[is.na(combined$bosniak.3),], 2, sd, na.rm=TRUE),
                    t.test=p.values))
online_offline_table

# ------------------------------------
#             FIGURE S5
# ------------------------------------
# Treatment of deactivation on the subsample of users who shared their online data
online_data <- combined[!is.na(combined$bosniak.3),]

# Plot: Fig S5 - top part
dev.off()
#pdf(file = "/Users/nejlaasimovic/Desktop/figureS6_top.pdf",   # The directory you want to save the file in
#   width = 5.8, # The width of the plot in inches
#    height = 5.2) 
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))#
par(mar=c(10,8,3,1)) #20 je desna strana
coef.vec<-c(reg_res_mod1("ft",online_data)$estimate, reg_res_mod1("sd",online_data)$estimate, reg_res_mod1("multi_stat",online_data)$estimate, reg_res_mod1("othchrct_combined",online_data)$estimate, reg_res_mod1("inchrct_combined",online_data)$estimate, reg_res_mod1("outgroup_princomp",online_data)$estimate)
se.vec<-c(reg_res_mod1("ft",online_data)$std.error, reg_res_mod1("sd",online_data)$std.error, reg_res_mod1("multi_stat",online_data)$std.error, reg_res_mod1("othchrct_combined",online_data)$std.error, reg_res_mod1("inchrct_combined",online_data)$std.error, reg_res_mod1("outgroup_princomp",online_data)$std.error)
var.names <- c("Feeling thermometer","Social closeness","Cooperation","Perception of \n out-group evaluations","Out-group traits","Out-group regard index")
results_df <- data.frame(term=var.names, estimate=coef.vec,std.error=se.vec)
y.axis <- c(length(coef.vec):1)
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "Treatment effect (SD)" , cex.lab=2.3,ylab = "", pch = 19,cex = 1,#plot coefficients as points, turning off axes and labels.
     xlim = c(-1,1), main = "", col=c("red","red","red","red","red","red","red","red"),cex.lab=1)
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd = 1.2, col = c("red","red","red","red","red","red","red","red"))
axis(1, at = seq(-1,1,by=1), labels =c(-1,0,1) , tick = T,#draw x-axis and labels with tick marks
     cex.axis = 0.9, mgp = c(7,0.5,0.5))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, mgp = c(1.6,.7,0),cex.axis = 1.2) #draw y-axis with tick marks, make labels perpendicular to axis and closer to axis
abline(h = 0, v=0, col="blue",lty=2,lwd=1.6)


# Plot: Fig S5 - bottom part
dev.off()
#pdf(file = "/Users/nejlaasimovic/Desktop/figureS6_bottom.pdf",   # The directory you want to save the file in
 #   width = 5.8, # The width of the plot in inches
  #  height = 5.2) 
layout(matrix(c(2,1),1,2), #in order to add variable categories and braces to left side of plot, 
       widths = c(1.5, 5))
par(mar=c(10,8,3,1)) 
coef.vec<-c(reg_res_mod1("c_news",online_data)$estimate, reg_res_mod1("swb",online_data)$estimate, reg_res_mod1("satisf",online_data)$estimate, reg_res_mod1("depression",online_data)$estimate, reg_res_mod1("loneliness",online_data)$estimate, reg_res_mod1("nerv",online_data)$estimate, reg_res_mod1("boredom",online_data)$estimate,reg_res_mod1("joy",online_data)$estimate,reg_res_mod1("isol",online_data)$estimate, reg_res_mod1("fulf",online_data)$estimate)
se.vec<-c(reg_res_mod1("c_news",online_data)$std.error, reg_res_mod1("swb",online_data)$std.error, reg_res_mod1("satisf",online_data)$std.error, reg_res_mod1("depression",online_data)$std.error, reg_res_mod1("loneliness",online_data)$std.error, reg_res_mod1("nerv",online_data)$std.error, reg_res_mod1("boredom",online_data)$std.error,reg_res_mod1("joy",online_data)$std.error,reg_res_mod1("isol",online_data)$estimate, reg_res_mod1("fulf",online_data)$std.error)
var.names <- c("News Knowledge","Well-being index","Satisfaction","Depression","Loneliness","Anxiety","Boredom","Joy","Isolation","Fulfillment")
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


# ---------------------------------------------------------
#    INTERACTION w/ CONTINOUS MEASURE [online]: TABLE S18
# ---------------------------------------------------------
# Reverse coding ethnic fractionalization and Shannon entropy indices, so that higher values
# indicate higher level of ethnic homogeneity
combined$online_index_rev <- 1-combined$online_index
combined$shannon_online_rev <- 1-combined$shannon_online

# ----------- Ethnic Fractionalization
ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_index_rev+ treatment:online_index_rev, data=combined)
ef_interaction_onl_coef<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[4,1]/sd(combined[which(combined$treatment==0),]$outgroup_princomp)
ef_interaction_onl_se<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[4,2]/sd((combined[which(combined$treatment==0),]$outgroup_princomp))

ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_index_rev+ treatment:online_index_rev+freq_usage, data=combined)
ef_interaction_onl_coef1<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[5,1]/sd(combined[which(combined$treatment==0),]$outgroup_princomp)
ef_interaction_onl_se1<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[5,2]/sd((combined[which(combined$treatment==0),]$outgroup_princomp))

ef_onl_interaction_rev <- lm(outgroup_princomp ~ treatment +online_index_rev+ treatment:online_index_rev+freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined)
ef_interaction_onl_coef2<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[11,1]/sd(combined[which(combined$treatment==0),]$outgroup_princomp)
ef_interaction_onl_se2<-coeftest(ef_onl_interaction_rev, vcov = vcovHC(ef_onl_interaction_rev, type = "HC1"))[11,2]/sd((combined[which(combined$treatment==0),]$outgroup_princomp))

# --------- Shannon Entropy
se_onl_interaction <- lm(outgroup_princomp ~ treatment+ shannon_online_rev+ treatment:shannon_online_rev, data=combined)
se_interaction_onl_coef<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[4,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_onl_se<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

se_onl_interaction <- lm(outgroup_princomp ~ treatment+ shannon_online_rev+ treatment:shannon_online_rev+freq_usage, data=combined)
se_interaction_onl_coef1<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[5,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_onl_se1<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[5,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

se_onl_interaction <- lm(outgroup_princomp ~ treatment+ shannon_online_rev+ treatment:shannon_online_rev+freq_usage
                         +gender+age+educ+employ1+imp_ethn+imp_cntry,
                         data=combined)
se_interaction_onl_coef2<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[11,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
se_interaction_onl_se2<-coeftest(se_onl_interaction, vcov = vcovHC(se_onl_interaction, type = "HC1"))[11,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))


# ---------  Majority Group Share
ms_onl_interaction <- lm(outgroup_princomp ~ treatment+largest_majority_online + treatment:largest_majority_online, data=combined)
ms_interaction_onl_coef<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_onl_se<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[4,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

ms_onl_interaction <- lm(outgroup_princomp ~ treatment+largest_majority_online + treatment:largest_majority_online+freq_usage, data=combined)
ms_interaction_onl_coef1<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[5,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_onl_se1<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[5,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))

ms_onl_interaction <- lm(outgroup_princomp ~ treatment+largest_majority_online + treatment:largest_majority_online+freq_usage+gender+age+educ+employ1+imp_ethn+imp_cntry, data=combined)
ms_interaction_onl_coef2<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[11,1]/sd(combined[combined$treatment==0,]$outgroup_princomp)
ms_interaction_onl_se2<-coeftest(ms_onl_interaction, vcov = vcovHC(ms_onl_interaction, type = "HC1"))[11,2]/sd(na.omit(combined[combined$treatment==0,]$outgroup_princomp))


# Creating Table S18
coef.vec_intr_onl <- c(ef_interaction_onl_coef, 
                   ms_interaction_onl_coef,
                   se_interaction_onl_coef)
se.vec_intr_onl <- c(ef_interaction_onl_se,
                 ms_interaction_onl_se,
                 se_interaction_onl_se)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr <- data.frame(names, coef.vec_intr_onl,se.vec_intr_onl)

coef.vec_intr_onl1 <- c(ef_interaction_onl_coef1,
                   ms_interaction_onl_coef1,
                   se_interaction_onl_coef1)
se.vec_intr_onl1 <- c(ef_interaction_onl_se1,
                 ms_interaction_onl_se1,
                 se_interaction_onl_se1)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr1 <- data.frame(names, coef.vec_intr_onl1,se.vec_intr_onl1)

coef.vec_intr_onl2 <- c(ef_interaction_onl_coef2,
                        ms_interaction_onl_coef2,
                        se_interaction_onl_coef2)
se.vec_intr_onl2 <- c(ef_interaction_onl_se2,
                      ms_interaction_onl_se2,
                      se_interaction_onl_se2)
names<-c("Treatment x Fractionalization Offline (rev.)","Treatment x Majority Group Share","Treatment x Shannon Entropy (rev.)")
merged_intr2 <- data.frame(names, coef.vec_intr_onl2,se.vec_intr_onl2)


# -------- *Table S18* ------------
TableS18<-join_all(list(merged_intr, merged_intr1, merged_intr2), by="names") 
TableS18
# -------- *Table S18* ------------
