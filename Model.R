##### Smaldino & Schank (2011) #####
# ##Purpose##
# Determine whether assortative mating can emerge from individual mating motivations. Agents will select one another as mates based on a physical attractiveness variable.
# Each agent will pursue mates probabilistically and in proportion to either maximizing mates' physical attractiveness or maximizing mates’ similarity. The model will then calculate the extent to which agents mate assortatively for attractiveness.
# Determine whether assortative mating is constrained by an individual’s localization/walk strategy and individual decision rules. Agents will move around a two-dimensional space.
# The population will be dynamic such that there will always be an influx of new, single agents who are available to date. 
# When a pair forms, they are replaced in the population by two new simulated agents.
#
# ##Agents##
# Each agent will have four randomly assigned features: sex, physical attractiveness, move strategy, and decision rule. All agents have a position in a 2-D space.
# Half of all agents will be male and half will be female.
# Agent physical attractiveness values will be drawn from random uniform distributions constrained between 0 and 10.
#Decision Rule 1: choose the most attractive partner. Decision Rule 2: choose the most similar partner. Half of the agents will be assigned Rule 1 and half will be assigned rule 2.
# Agent move strategy will be drawn equally from two different strategies, Brownian walk or Zigzag walk. The Brownian walk consists of the agent choosing randomly between its eight nearest neighboring sites. Each site has an equal probability of being selected. The Zigzag walk consists of the agent choosing between moving forward-left or forward-right depending on its current orientation. Each site has an equal probability of being chosen. 
# Agents will also possess a maximum number of dates they will tolerate going on before settling for any mate.
# Agent selectivity will gradually decline as they approach their maximum number of dates.
# 
# ##Life Cycle##
# 1. Assess
# 2. Date
# 3. Offer
# 4. Pair
# 
# #Assess#
# Each agent will first calculate how far they are from each other agent in the 2D space. The agent will then identify all agents who reside within a set radius around them. These agents constitute the focal agent's neighborhood.
# The focal agent will then determine the sex of all agents in their neighborhood.
#
# #Date#
# At the start of the life cycle, a random agent will be paired with a random opposite-sex partner within their neighbrhood. Dating agents' number of dates will be incremented by one. 
# 
# #Select mates#
# Next, each dating agent will decide whether to make a long-term commitment offer to their dating partner. For Rule 1, the probability of making an offer will be proportional to their date's attractiveness, adjusted by the number of dates the agent has gone on. For Rule 2, the probability of making an offer will be proportional to their date’s similarity, adjusted by the number of dates the agent has gone on. 
# If an agent has reached their maximum number of dates, they will make an offer to their next date regardless of attractiveness.
# 
# #Pair#
# Finally, dating agents will pair if both agents mutually make long-term commitment offers to one another.
# When a pair forms, they are replaced in the population by two new simulated agents.
# 
# The life cycle will repeat for a specified number of time steps. 





#####Packages#####
library(ggplot2)




#####Parameters#####

#Population Size#
#The number of agents to be generated
popsize<-100

#Maximum Number of Dates#
#Maximum number of dates agents will go on
maxdates<-50

#Neighborhood Radius#
#The size of each agent's neighborhood
radius<-10


#####Functions#####

#Agent Generation#
agentgenerate<-function(n,sex){
  
  #Assign agents random physical attractiveness
  physatt<-runif(n,0,10)
  
  #Assign agents a decision rule
  rule<-sample(c("rule 1","rule 2"),n,replace=T)
  
  #Create a vector to store the agents' date memories
  numdates<-0
  
  #Generate an ID number for each agent
  ID<-1:n
  
  #Assign each agent a random x-position
  xpos<-sample(0:100,1)
  
  #Assign each agent a random y-position
  ypos<-sample(0:100,1)
  
  #Put the agents together
  agents<-data.frame(ID,sex,physatt,numdates,xpos,ypos,rule)
  
  #Output the agents
  return(agents)
  
}



#Assessment Function#
assess<-function(focal,target){
  
  #Determine which agents are in an agent's neighborhood
  date<-apply(focal,1,function(x)
    dist(rbind(x[5:6],target[,5:6]))<radius
    
    )
   
}



#Move#
#A function to have agents move using the Brownian strategy
move<-function(agents){
  
  #Create a matrix of movement options
  posDiffs<-expand.grid(-1:1,-1:1)[-5,]
  
  #Loops through agents one-by-one...
  for(a in 1:nrow(agents)){
    
    #Give them new X and Y coordinates to move to one of eight neighboring sites
    agents$xpos[a]<-agents$xpos[a]+posDiffs[sample(1:8,1),]
    agents$ypos[a]<-agents$ypos[a]+posDiffs[sample(1:8,1),]
    
  }
  
  #Output the agents with their new positions
  return(agents)
  
}



#Better Sampling#
#A sample function that returns a single value
bettersample<-function(x,size,replace=FALSE,prob=NULL){
  
  #If the sample vector has more than 1 element...
  if(length(x)>1){
    #Use sample just like normal
    x<-sample(x,size,replace,prob)
  }
  
  #Output the sample result
  return(x)
}



#####Model Start#####

#Generate agents
females<-agentgenerate(popsize/2,"female")
males<-agentgenerate(popsize/2,"male")

#Create empty vectors to store paired agents
pairedfemales<-c()
pairedmales<-c()

#Save the agents' starting positions
femalestartdata<-females
malestartdata<-males

###Life Cycle###

#Set the maximum number of steps the while loop can run
maxsteps<-100

#Set the current number of steps that have run
steps<-0

while(steps<maxsteps){
  #Assess#
  fdatedecision<-assess(females,males)
  mdatedecision<-assess(males,females)
  
  
  #Date#
  
  #Select a random single male and single female in the same neighborhood to date
  fdate<-bettersample(females$ID[!(females$ID %in% pairedfemales)]&fdatedecision==TRUE,1)
  mdate<-bettersample(males$ID[!(males$ID %in% pairedmales)]&mdatedecision==TRUE,1)
  
  #Increment the number of date agents have been on by 1
  females$numdates[fdate]<-females$numdates[fdate]+1
  males$numdates[mdate]<-males$numdates[mdate]+1
  
  
  #Offer#
  
  #Determine how selective agents will be based on number of dates completed so far 
  fchoosy<-((maxdates+1)-females$numdates[fdate])/maxdates
  mchoosy<-((maxdates+1)-males$numdates[mdate])/maxdates
  
  #Prevent choosiness values less than 0
  fchoosy<-ifelse(fchoosy>0,fchoosy,0)
  mchoosy<-ifelse(mchoosy>0,mchoosy,0)
  
  #If the agent abides by Rule 1...
  if(females$rule[fdate]=="rule 1"|males$rule[mdate]=="rule 1"){
    
    #Set the probability of a commitment offer based on date attractiveness and
    #agent selectiveness
    fp<-((males$physatt[mdate]^3)/1000)^fchoosy
    mp<-((females$physatt[fdate]^3)/1000)^mchoosy
    
    #Have each agent probabilistically make an offer of commitment
    foffer<-rbinom(1,1,fp)
    moffer<-rbinom(1,1,mp)
    
  } else {
    #Otherwise, if the agent abides by rule 2...
    
    #Set the probability of a commitment offer based on date similarity and
    #agent selectiveness
    fp<-(10-abs(males$physatt[mdate]-females$physatt[fdate])/10)^fchoosy
    mp<-(10-abs(females$physatt[fdate]-males$physatt[mdate])/10)^mchoosy
    
    #Have each agent probabilistically make an offer of commitment
    foffer<-rbinom(1,1,fp)
    moffer<-rbinom(1,1,mp)
    
  }
  
  
  #Pair#
  
  #If both agents make a mutual offer of commitment...
  if(foffer & moffer){
    
    #...add their ID numbers to the paired agent vectors
    pairedfemales<-c(pairedfemales,fdate)
    pairedmales<-c(pairedmales,mdate)
    
    #Generate two new agents
    
  }

}

#Save the agents' ending positions
femalesenddata<-females
malesenddata<-males
  



#####Analysis#####





