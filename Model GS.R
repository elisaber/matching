###
#load packages (assumed they are already installed)
library(matchingR)
library(ineq)
library(rmarkdown)

###Assumptions, to be set by researcher
#Network size
nmen <- 600
nwomen <- 400
#Preferences
wI <- 1
wC <- 1
wH <- 1
#Nr of experiments
repetitions <- 200

#Automatically set parameters
seed = 1
seedMin=seed
seedMax=seed+repetitions
R=matrix(0,6,3)
mn <- 1000
sd <- 200

###Define preferences
generatePreferences <- function(nmen, nwomen, mn, sd, wI, wC, wH) {
  #generate individual cardinal preference matrices
  iUm <- matrix(rnorm(nmen*nwomen, mn, sd), nrow=nwomen, ncol=nmen) #independent pref. by men for women
  iUw <- matrix(rnorm(nwomen*nmen, mn, sd), nrow=nmen, ncol=nwomen) #independent pref. by women for men
  
  #Calculate attractiveness score of each agent
  Aw <- rowMeans(iUm) #average rating for each woman
  Am <- rowMeans(iUw) #average rating for each man
  
  #generate homolog preferences
  hUm <- matrix(rnorm(nmen*nwomen, mn, sd), nrow=nwomen, ncol=nmen) #homolog pref. by men for women
  hUw <- t(hUm) #homolog pref. by women for men (like each other exactly the same)
  
  #Calculate preferences stated by each agent
  uM <<- (wI*iUm + wC*Aw + wH*hUm)/(wI+wC+wH) #weighted pref. by men for women
  uW <<- (wI*iUw + wC*Am + wH*hUw)/(wI+wC+wH) #weighted pref. by women for men
  
  #sender
  senderMen = t(t(rep(1:nmen, each = nwomen)))
  senderWomen = t(t(rep(1:nwomen, each = nmen)))
  senderList = c(senderMen, senderWomen)
  
  #receiver
  receiverWomen = rep(1:nmen, times = nwomen)
  receiverMen = rep((nmen+1):(nmen+nwomen), times = nmen)
  receiverList = c(receiverWomen, receiverMen)
  
  #preferences
  preferenceList <- c(as.list(uM), as.list(uW))
  preferences = data.frame(cbind(senderList,receiverList,preferenceList))
  names(preferences) = c("swiperID", "targetID", "utility")
  return(preferences)
}

### 1x Experiment = "create two-sided network with preferences, match up in pairs, examine results"
### Run and repeat experiment x times, each time with new random preferences
for (i in seedMin:seedMax){
  #set seed
  set.seed(i)

  #Generate Profiles
  preferences=generatePreferences(nmen, nwomen, mn, sd, wI, wC, wH)
  
  #female-optimal matching
  resultsW <<- galeShapley.marriageMarket(uW, uM) 
  galeShapley.checkStability(uW, uM, resultsW$proposals, resultsW$engagements)
  #calculate average utility of each matched agent
  resultsWW <<- diag(uW[resultsW$proposals,1:nwomen])
  uWWav <<- mean(resultsWW, na.rm=TRUE)
  resultsMW <<- diag(uM[resultsW$engagements,1:nmen])
  uMWav <<- mean(resultsMW, na.rm=TRUE)
  
  #male-optimal matching
  resultsM <- galeShapley.marriageMarket(uM, uW) 
  check <- galeShapley.checkStability(uM, uW, resultsM$proposals, resultsM$engagements)
  #calculate average utility of each matched agent
  resultsWM <<- diag(uW[resultsM$engagements,1:nwomen])
  uWMav <<- mean(resultsWM, na.rm=TRUE)
  resultsMM <<- diag(uM[resultsM$proposals,1:nmen])
  uMMav <<- mean(resultsMM, na.rm=TRUE)
  
  #Create results
  #Gini Coefficient female-optimal
  wealthW = c(pnorm(resultsWW,mean=mn,sd=sd), pnorm(resultsMW,mean=mn,sd=sd))
  wealthW[is.na(wealthW)] <- 0
  #Gini Coefficient male-optimal
  wealthM = c(pnorm(resultsMM,mean=mn,sd=sd), pnorm(resultsWM,mean=mn,sd=sd))
  wealthM[is.na(wealthM)] <- 0
  
  #Define result matrix
  #resultsUtility = matrix(c(min(resultsWW, na.rm = TRUE), uWWav, max(resultsWW, na.rm = TRUE), min(resultsWM, na.rm = TRUE), uWMav, max(resultsWM, na.rm = TRUE), min(resultsMW, na.rm = TRUE), uMWav, max(resultsMW, na.rm = TRUE), min(resultsMM, na.rm = TRUE), uMMav, max(resultsMM, na.rm = TRUE), rep(round(100*ineq(wealthW,type="Gini"),digits=0),3), rep(round(100*ineq(wealthM,type="Gini"), digits = 0),3)), nrow = 6, ncol = 3)
  resultsPercentile = matrix(c(pnorm(min(resultsWW, na.rm = TRUE),mean=mn,sd=sd), pnorm(uWWav,mean=mn,sd=sd), pnorm(max(resultsWW, na.rm = TRUE),mean=mn,sd=sd), pnorm(min(resultsWM, na.rm = TRUE),mean=mn,sd=sd), pnorm(uWMav,mean=mn,sd=sd), pnorm(max(resultsWM, na.rm = TRUE),mean=mn,sd=sd), pnorm(min(resultsMW, na.rm = TRUE),mean=mn,sd=sd), pnorm(uMWav,mean=mn,sd=sd), pnorm(max(resultsMW, na.rm = TRUE),mean=mn,sd=sd), pnorm(min(resultsMM, na.rm = TRUE),mean=mn,sd=sd), pnorm(uMMav,mean=mn,sd=sd), pnorm(max(resultsMM, na.rm = TRUE),mean=mn,sd=sd), rep(ineq(wealthW,type="Gini"),3), rep(ineq(wealthM,type="Gini"),3)), nrow = 6, ncol = 3)
  rownames(resultsPercentile) = c("fo min", "fo avg", "fo max", "mo min", "mo avg", "mo max")
  colnames(resultsPercentile) = c("Match Percentil for Women", "Match Percentil for Men", "Gini Coefficient")

  #Print results
  print(i)
  print(resultsPercentile)
  R=R+resultsPercentile
}

#Average Value after x repititions
AVERAGE=R/(repetitions+1)
AVERAGE

### Visualization (Lorenz curves)
# plot(Lc(wealthW),col="darkred",lwd=2)
# plot(Lc(wealthM),col="darkred",lwd=2)

### Export to MS Word
# render("Model GS.R", word_document())
