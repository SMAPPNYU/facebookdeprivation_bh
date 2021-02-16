pc_index <- function(dataIn=NULL,
                           varList=NULL){
  x <- dataIn[,varList]
  res <- princomp(na.omit(x))
  index <- as.vector(predict(na.omit(res), newdata=x)[,1])
  return(index)
}

reg_res_mod1<- function(outcome="", dataIn=NULL){
  Model1 <- lm(as.formula(paste0(outcome,"~","treatment")), data=dataIn)
  results <- tidy(Model1)
  results <- results[results[,"term"]=="treatment",]
  results$estimate <- coeftest(Model1 , vcov = vcovHC(Model1, type = "HC1"))[2,1]/sd(dataIn[dataIn$treatment==0,outcome], na.rm=TRUE)
  results$std.error <- coeftest(Model1 , vcov = vcovHC(Model1, type = "HC1"))[2,2]/sd(dataIn[dataIn$treatment==0,outcome], na.rm=TRUE)
  results$N <- nrow(dataIn)
  results$outcome <- paste0(outcome)
  return(results[c("estimate","std.error","p.value", "N","outcome")])
}  

reg_res_mod2<- function(outcome="", dataIn=NULL){
  covIn <- as.formula("~freq_usage")
  Model2 <- lm_lin(as.formula(paste0(outcome,"~","treatment")), covariates=covIn, data=dataIn)
  results <- tidy(Model2)
  results <- results[results[,"term"]=="treatment",]
  results$estimate2 <- results$estimate/sd(dataIn[dataIn$treatment==0,outcome], na.rm=TRUE)
  results$std.error2 <- results$std.error/sd(dataIn[dataIn$treatment==0,outcome], na.rm=TRUE)
  results$N <- Model2$nobs
  return(results[c("estimate2","std.error2","p.value", "N","outcome")])
}  

reg_res_mod3 <- function(outcome="", dataIn=NULL){
  covIn <- as.formula("~freq_usage+as.factor(ethn_t)+gender+age+educ+employ1+imp_ethn+imp_cntry")
  Model3 <- lm_lin(as.formula(paste0(outcome,"~","treatment")), covariates=covIn, data=dataIn)
  results <- tidy(Model3)
  results <- results[results[,"term"]=="treatment",]
  results$estimate3<-results$estimate/sd(dataIn[dataIn$treatment==0,outcome], na.rm=TRUE)
  results$std.error3<-results$std.error/sd(dataIn[dataIn$treatment==0,outcome], na.rm=TRUE)
  results$N <- Model3$nobs
  return(results[c("estimate3","std.error3","p.value", "N","outcome")])
}  


reg_res_mod3_ethn<- function(outcome="", dataIn=NULL){
  covIn <- as.formula("~freq_usage+gender+age+educ+employ1+imp_cntry+imp_ethn")
  Model3 <- lm_lin(as.formula(paste0(outcome,"~","treatment")), covariates=covIn, data=dataIn)
  results <- tidy(Model3)
  results <- results[results[,"term"]=="treatment",]
  results$estimate3<-results$estimate/sd(dataIn[dataIn$treatment==0,outcome], na.rm=TRUE)
  results$std.error3<-results$std.error/sd(dataIn[dataIn$treatment==0,outcome], na.rm=TRUE)
  results$N <- Model3$nobs
  return(results[c("estimate3","std.error3","p.value", "N","outcome")])
}  


interactions <- function(outcome="", covIn=NULL, dataIn=NULL){
  Model3 <- lm_lin(as.formula(paste0(outcome,"~","treatment")), covariates=covIn, data=dataIn)
  results <- tidy(Model3)
  results <- results[results[,"term"]=="treatment",]
  results$estimate3<-results$estimate/sd(dataIn[dataIn$treatment==0,outcome], na.rm=TRUE)
  results$std.error3<-results$std.error/sd(dataIn[dataIn$treatment==0,outcome], na.rm=TRUE)
  results$N <- Model3$nobs
  return(results[c("estimate3","std.error3","p.value", "N","outcome")])}


