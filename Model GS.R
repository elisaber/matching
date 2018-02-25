#load packages
library(matchingR)
library(ineq)
library(rmarkdown)
?rmarkdown
#library(ggplot2)

#set seed
set.seed(123)

#Set Assumptions
nmen <- 400
nwomen <- 600
mn <- 1000
sd <- 200
nAgents <- nmen + nwomen
repetitions <- 1
wI <- 1
wC <- 1
wH <- 1

#Create Preference Generation Function
generatePreferences <- function(nmen, nwomen, mn, sd, wI, wC, wH) {
  #generate individual cardinal preference matrices
  iUm <<- matrix(rnorm(nmen*nwomen, mn, sd), nrow=nwomen, ncol=nmen) 
  iUw <<- matrix(rnorm(nwomen*nmen, mn, sd), nrow=nmen, ncol=nwomen) 
  
  #Calculate attractiveness score of each agent
  Aw <<- rowMeans(iUm)
  Am <<- rowMeans(iUw)
  
  #generate homolog preferences
  hUm <<- matrix(rnorm(nmen*nwomen, mn, sd), nrow=nwomen, ncol=nmen)
  hUw <<- t(hUm)
  
  #Calculate preferences stated by each agent
  uM2 <<- (wI*iUm + wC*Aw + wH*hUm)/(wI+wC+wH)
  uW2 <<- (wI*iUw + wC*Am + wH*hUw)/(wI+wC+wH)
  
  #sender
  senderMen = t(t(rep(1:nmen, each = nwomen)))
  senderWomen = t(t(rep(1:nwomen, each = nmen)))
  senderList = c(senderMen, senderWomen)
  
  #receiver
  receiverWomen = rep(1:nmen, times = nwomen)
  receiverMen = rep((nmen+1):(nmen+nwomen), times = nmen)
  receiverList = c(receiverWomen, receiverMen)
  
  #preferences
  uM3 <- as.list(uM2)
  uW3 <- as.list(uW2)
  preferenceList <- c(uM3, uW3)
  
  preferences <<- data.frame(cbind(senderList,receiverList,preferenceList))
  colnames(preferences) <<- c("swiperID", "targetID", "utility")
}

#Invoke function, 200x repeated
matching <- function(nmen, nwomen) {
  #Generate Profiles
  generatePreferences(nmen, nwomen, mn, sd, wI, wC, wH)
  
  if(!exists("uMMav")){
    tuWWav <<- matrix(0,1,0)
    tuMWav <<- matrix(0,1,0)
    tuWMav <<- matrix(0,1,0)
    tuMMav <<- matrix(0,1,0)
  }
  
  #female-optimal matching
  resultsW <<- galeShapley.marriageMarket(uW2, uM2) 
  galeShapley.checkStability(uW2, uM2, resultsW$proposals, resultsW$engagements)
  #calculate average utility of each matched agent
  resultsWW <<- diag(uW2[resultsW$proposals,1:nwomen])
  uWWav <<- mean(resultsWW, na.rm=TRUE)
  tuWWav <<- cbind(tuWWav, uWWav)
  resultsMW <<- diag(uM2[resultsW$engagements,1:nmen])
  uMWav <<- mean(resultsMW, na.rm=TRUE)
  tuMWav <<- cbind(tuMWav, uMWav)
  
  #male-optimal matching
  resultsM <- galeShapley.marriageMarket(uM2, uW2) 
  check <- galeShapley.checkStability(uM2, uW2, resultsM$proposals, resultsM$engagements)
  #calculate average utility of each matched agent
  resultsWM <<- diag(uW2[resultsM$engagements,1:nwomen])
  uWMav <<- mean(resultsWM, na.rm=TRUE)
  tuWMav <<- cbind(tuWMav, uWMav)
  resultsMM <<- diag(uM2[resultsM$proposals,1:nmen])
  uMMav <<- mean(resultsMM, na.rm=TRUE)
  tuMMav <<- cbind(tuMMav, uMMav)
}
replicate(repetitions, matching(nmen, nwomen))

#Create results
generateResults <- function() {
  #Gini Coefficient female-optimal
  wealthW <<- c(pnorm(resultsWW,mean=mn,sd=sd), pnorm(resultsMW,mean=mn,sd=sd))
  wealthW[is.na(wealthW)] <- 0
  #Gini Coefficient male-optimal
  wealthM <<- c(pnorm(resultsMM,mean=mn,sd=sd), pnorm(resultsWM,mean=mn,sd=sd))
  wealthM[is.na(wealthM)] <- 0
  
  results <<- matrix(c(
    # FEMALE
    #Minimum Value in Female-Optimal
    round(100*pnorm(min(resultsWW),mean=mn,sd=sd),digits=0), 
    #Average Value in Female-Optimal
    round(100*pnorm(tuWWav,mean=mn,sd=sd),digits=0), 
    #Maximum Value in Female-Optimal
    round(100*pnorm(max(resultsWW),mean=mn,sd=sd),digits=0), 
    #Minimum Value in Male-Optimal
    round(100*pnorm(min(resultsWM),mean=mn,sd=sd),digits=0), 
    #Average Value in Male-Optimal
    round(100*pnorm(tuWMav,mean=mn,sd=sd),digits=0), 
    #Maximum Value in Male-Optimal
    round(100*pnorm(max(resultsWM),mean=mn,sd=sd),digits=0), 
    # MALE
    #Minimum Value in Female-Optimal
    round(100*pnorm(min(resultsMW, na.rm = TRUE),mean=mn,sd=sd),digits=0), 
    #Average Value in Female-Optimal
    round(100*pnorm(tuMWav,mean=mn,sd=sd),digits=0), 
    #Maximum Value in Female-Optimal
    round(100*pnorm(max(resultsMW, na.rm = TRUE),mean=mn,sd=sd),digits=0), 
    #Minimum Value in Male-Optimal
    round(100*pnorm(min(resultsMM, na.rm = TRUE),mean=mn,sd=sd),digits=0), 
    #Average Value in Male-Optimal
    round(100*pnorm(tuMMav,mean=mn,sd=sd),digits=0), 
    #Maximum Value in Male-Optimal
    round(100*pnorm(max(resultsMM, na.rm = TRUE),mean=mn,sd=sd),digits=0), 
    #Gini Coefficient
    rep(round(100*ineq(wealthW,type="Gini"),digits = 0),3), 
    rep(round(100*ineq(wealthM,type="Gini"),digits = 0),3)
    ), nrow = 6, ncol = 3)
  rownames(results) <<- c("fo min", "fo avg", "fo max", "mo min", "mo avg", "mo max")
  colnames(results) <<- c("Match Percentil for Women", "Match Percentil for Men", "Gini Coefficient")
  results
}
generateResults()

#Lorenz curves
plot(Lc(wealthW),col="darkred",lwd=2)
plot(Lc(wealthM),col="darkred",lwd=2)

#generate histograms
#hist(resultsMM)
#hist(resultsMW)
#hist(resultsWM)
#hist(resultsWW)

#Export code to Word
render("Model GS.R", word_document())
