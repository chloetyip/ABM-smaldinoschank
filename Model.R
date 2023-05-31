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
  xpos<-sample(0:100,n,replace=T)
  
  #Assign each agent a random y-position
  ypos<-sample(0:100,n,replace=T)
  
  #Put the agents together
  agents<-data.frame(ID,sex,physatt,numdates,xpos,ypos,rule)
  
  #Output the agents
  return(agents)
  
}



#Assessment Function#
assess<-function(focal,target){
  
  #Determine which agents are in an agent's neighborhood
  date<-t(apply(focal,1,function(x)
    apply(target,1,function(y)
          dist(rbind(x[5:6],y[5:6]))<radius)
    ))
  
  rownames(date)<-focal$ID
  colnames(date)<-target$ID
  
  return(date)
   
}



#Move#
#A function to have agents move using the Brownian strategy
move<-function(agents){
  
  #Create a matrix of movement options
  posDiffs<-expand.grid(-1:1,-1:1)
  
  #Remove the central position
  posDiffs<-posDiffs[-5,]
  
  #Loops through agents one-by-one...
  for(a in 1:nrow(agents)){
    
    #Give them new X and Y coordinates to move to one of eight neighboring sites
    newPos<-sample(1:8,1)
    newX<-agents$xpos[a]+posDiffs[newPos,1]
    newY<-agents$ypos[a]+posDiffs[newPos,2]
    
    #Update the agents' positions
    agents$xpos[a]<-newX
    agents$ypos[a]<-newY
    
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
femalesstartdata<-females
malesstartdata<-males

###Life Cycle###

#Set the maximum number of steps the while loop can run
maxsteps<-100

#Set the current number of steps that have run
steps<-0

while(steps<maxsteps){
  
  #Assess#
  
  #Have agents assess whether they are neighbors
  assessment<-assess(females,males)

  #Determine which agents actually are neighbors
  neighbors<-which(assessment==T,arr.ind=T)
  
  #Eliminate paired females
  neighbors<-neighbors[!(neighbors[,1] %in% pairedfemales),]
  
  #Eliminate paired males
  neighbors<-neighbors[!(neighbors[,2] %in% pairedmales),]
  
  #Pick a random pair of neighbors to date
  date<-sample(1:nrow(neighbors),1)
  
  #Date#
  
  #Select a random single male and single female in the same neighborhood to date
  fdate<-neighbors[date,1]
  mdate<-neighbors[date,2]
  
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
  
  #Set the probability of a commitment offer based on date attractiveness and
  #agent selectiveness
  fp<-((males$physatt[mdate]^3)/1000)^fchoosy
  mp<-((females$physatt[fdate]^3)/1000)^mchoosy
  
  #Have each agent probabilistically make an offer of commitment
  foffer<-rbinom(1,1,fp)
  moffer<-rbinom(1,1,mp)
  
  
  #Pair#
  
  #If both agents make a mutual offer of commitment...
  if(foffer & moffer){
    
    #...add their ID numbers to the paired agent vectors
    pairedfemales<-c(pairedfemales,fdate)
    pairedmales<-c(pairedmales,mdate)
    
    #Generate two new agents
    fnew<-agentgenerate(1,"female")
    mnew<-agentgenerate(1,"male")
    
    #Assign random positions to the new agents
    fnew$xpos<-sample(0:100,1)
    fnew$ypos<-sample(0:100,1)
    mnew$xpos<-sample(0:100,1)
    mnew$ypos<-sample(0:100,1)
    
    #Update the ID number of the new agents
    fnew$ID<-max(females$ID)+1
    mnew$ID<-max(males$ID)+1
    
    #Add the new agents to the population
    females<-rbind(females,fnew)
    males<-rbind(males,mnew)
    
    #Save the new agents' starting positions
    femalesstartdata<- rbind(femalesstartdata,fnew)
    malesstartdata <- rbind(malesstartdata,mnew)
    
  }
  
  #Move#
  
  #Move agents
  females<-move(females)
  males<-move(males)

  
  #Increment steps by 1
  steps<-steps+1
  
}

#Save the agents' ending positions
femalesenddata<-females
malesenddata<-males
  



#####Analysis#####





