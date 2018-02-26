#########################################################################################
#load packages
library(matchingR)
library(ineq)
library(rmarkdown)
#?rmarkdown
#library(ggplot2)

#########################################################################################
#Set Seed for reproducability
set.seed(123)

#########################################################################################
#Set Assumptions
nmen <- 4
nwomen <- 6
mn <- 10
sd <- 2
nAgents <- nmen + nwomen
repetitions <- 1
wI <- 1
wC <- 1
wH <- 1


#########################################################################################
#Generate Preferences Helper Function
generatePreferences_aux<- function(NR, NC, mn, sd, wI, wC, wH,hU) {
  # M---NR=nwomen, NC=nmen
  # W---NR=nmen, NC=nwomen
  #generate individual cardinal preference matrices
  iU=matrix(rnorm(NR*NC, mn, sd), nrow=NR, ncol=NC) 
  #Calculate attractiveness score of each agent
  A=rowMeans(iU)
  #Calculate preferences stated by each agent
  output=list(
    u2=(wI*iU + wC*A + wH*hU)/(wI+wC+wH),
    sender=matrix(rep(1:NC, each = NR)))
  return(output)  
}

#########################################################################################
#Generate Preference Function
generatePreferences <- function(nmen, nwomen, mn, sd, wI, wC, wH) {
  #generate homolog preferences
  hUm = matrix(rnorm(nmen*nwomen, mn, sd), nrow=nwomen, ncol=nmen)
  hUw = t(hUm)

  MEN=generatePreferences_aux(nwomen,nmen , mn, sd, wI, wC, wH,hUm)
  WOMEN=generatePreferences_aux(nmen,nwomen , mn, sd, wI, wC, wH,hUw)
  
  #sender
  senderList = c(MEN$sender, WOMEN$sender)
  
  #receiver
  receiverWomen = rep(1:nmen, times = nwomen)
  receiverMen = rep((nmen+1):(nmen+nwomen), times = nmen)
  receiverList = c(receiverWomen, receiverMen)
  
  #preferences
  preferenceList <- c(MEN$u2, WOMEN$u2)
  preferences=data.frame(swiperID=senderList,targetID=receiverList,utility=preferenceList)
  R=list(uW2=WOMEN$u2,uM2=MEN$u2,Preferences=preferences)
  return(R)
}

#########################################################################################
diag_mean=function(A,B,n,m){
    results= galeShapley.marriageMarket(A, B) 
    check <- galeShapley.checkStability(A,B, results$proposals, results$engagements)
    result_n = diag(A[results$proposals,1:n])
    result_m= diag(B[results$engagements,1:m])
    uwm=list(u_n= mean(result_n, na.rm=TRUE),u_m= mean(result_m, na.rm=TRUE))
    return(uwm)
  }

#########################################################################################
#Matching Function
matching <- function(nmen, nwomen) {
  #Generate Profiles
  GP=generatePreferences(nmen, nwomen, mn, sd, wI, wC, wH)
  
  uW2=GP$uW2
  uM2=GP$uM2
  #female-optimal matching
  uwm=diag_mean(uW2,uM2,nwomen,nmen)[[1]]
  resultsW=diag_mean(uW2,uM2,nwomen,nmen)[[2]]
  
  tuWWav =uwm$u_n
  tuMWav= uwm$u_m
  
  #male-optimal matching
  umw=diag_mean(uM2,uW2,nmen,nwomen)[[1]]
  resultsM=diag_mean(uW2,uM2,nwomen,nmen)[[2]]
  tuMMav = umw$u_n
  tuWMav = umw$u_m
 
   output=list(tuWWav =tuWWav,
            tuMWav= tuMWav, 
            tuMMav = tuMMav, 
            tuWMav = tuWMav,
            resultsW=resultsW,
            resultsM=resultsM)
 return(output)
  }

tuWWav =c()#<<- matrix(0,1,0)
tuMWav =c()
tuWMav =c()
tuMMav =c()

#########################################################################################
#Repeat x times

for (i in 1:repetitions)
{
  results=matching(nmen, nwomen)
  averages=results[1:4]
  resultsWW=results[[5]][[1]]
  resultsMW=results[[5]][[2]]
  
  resultsWM=results[[6]][[1]]
  resultsMM=results[[6]][[2]]
  
  tuWWav =c(tuWWav,averages$tuWWav)
  tuMWav= c(tuMWav, averages$tuMWav)
  tuMMav = c(tuMMav, averages$tuMMav)
  tuWMav = c(tuWMav, averages$tuWMav)
            
}  

#########################################################################################
#Generate results
generateResults <- function() {
  #Gini Coefficient female-optimal
  wealthW <<- c(pnorm(tuWWav,mean=mn,sd=sd), pnorm(tuWMav,mean=mn,sd=sd))
  wealthW[is.na(wealthW)] <- 0
  #Gini Coefficient male-optimal
  wealthM <<- c(pnorm(tuMMav,mean=mn,sd=sd), pnorm(tuWMav,mean=mn,sd=sd))
  wealthM[is.na(wealthM)] <- 0
  
  results <<- matrix(c(
    # FEMALE
    #Minimum Value in Female-Optimal
    round(100*pnorm(min(tuWWav),mean=mn,sd=sd),digits=0), 
    #Average Value in Female-Optimal
    round(100*pnorm(tuWWav,mean=mn,sd=sd),digits=0), 
    #Maximum Value in Female-Optimal
    round(100*pnorm(max(tuWWav),mean=mn,sd=sd),digits=0), 
    #Minimum Value in Male-Optimal
    round(100*pnorm(min(tuWMav),mean=mn,sd=sd),digits=0), 
    #Average Value in Male-Optimal
    round(100*pnorm(tuWMav,mean=mn,sd=sd),digits=0), 
    #Maximum Value in Male-Optimal
    round(100*pnorm(max(tuWMav),mean=mn,sd=sd),digits=0), 

    # MALE
    #Minimum Value in Female-Optimal
    round(100*pnorm(min(tuMWav, na.rm = TRUE),mean=mn,sd=sd),digits=0), 
    #Average Value in Female-Optimal
    round(100*pnorm(tuMWav,mean=mn,sd=sd),digits=0), 
    #Maximum Value in Female-Optimal
    round(100*pnorm(max(tuMWav, na.rm = TRUE),mean=mn,sd=sd),digits=0), 
    #Minimum Value in Male-Optimal
    round(100*pnorm(min(tuMMav, na.rm = TRUE),mean=mn,sd=sd),digits=0), 
    #Average Value in Male-Optimal
    round(100*pnorm(tuMMav,mean=mn,sd=sd),digits=0), 
    #Maximum Value in Male-Optimal
    round(100*pnorm(max(tuMMav, na.rm = TRUE),mean=mn,sd=sd),digits=0), 
   
    #Gini Coefficient
    rep(round(100*ineq(wealthW,type="Gini"),digits = 0),3), 
    rep(round(100*ineq(wealthM,type="Gini"),digits = 0),3)), nrow = 6, ncol = 3)

  rownames(results) <<- c("fo min", "fo avg", "fo max", "mo min", "mo avg", "mo max")
  colnames(results) <<- c("Match Percentil for Women", "Match Percentil for Men", "Gini Coefficient")
  results
}
generateResults()

#########################################################################################
# Display Graphs

#########################################################################################
#Output Code to Word
#render("Model GS.R", word_document())
