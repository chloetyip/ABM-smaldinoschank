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
# Agent move strategy will be drawn equally from three different strategies, Brownian walk, Zigzag walk, and the nonspatial condition. The Brownian walk consists of the agent choosing randomly between its eight nearest neighboring sites. Each site has an equal probability of being selected. The Zigzag walk consists of the agent choosing between moving forward-left or forward-right depending on its current orientation. Each site has an equal probability of being chosen. The nonspatial condition entails no particular walk strategy. Encounters are completely random and are uncorrelated in space and time. 
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
# At the start of the life cycle, a random agent will be paired with a random opposite-sex partner. Dating agents' number of dates will be incremented by one. 
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
