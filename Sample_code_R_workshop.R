library(geiger)
library(phytools)
library(nlme)

rate <- 0.05

#lets say this phylogeny represents a group of 25 species of birds
#and we are interested in testing if two traits are correlated using pgls
#make a tree, b is birthrate, d is deathrate, stop when have 25 taxa in tree
tree <- sim.bdtree(b=0.1, d=0, stop ="taxa", n=25)
plot(tree)

#simulate character evolution along tree for trait 1 - average time birds spend in trees
sim_time<- sim.char(tree, rate, 1, model = "BM", root=30)

#simulated character evolution for trait 2 - fitness score of offspring  
sim_offspring<- sim.char(tree, rate, 1, model = "BM", root=1)

#extract trait data from each simulation
time <- sim_time[1:25]
time <- setNames(time, tree$tip.label)

offspring <- sim_offspring[1:25]
offspring <- setNames(offspring, tree$tip.label)


#plot change in character states across phylogeny 
obj1 <- contMap(tree, time, plot=FALSE)
obj1 <- setMap(obj1,invert=TRUE)
plot(obj1,fsize=c(0.4,1), outline=FALSE,lwd=c(3,7),leg.txt="Average time in tree")

obj2 <- contMap(tree, offspring, plot=FALSE)
obj2 <- setMap(obj1,invert=TRUE)
plot(obj2,fsize=c(0.4,1), outline=FALSE,lwd=c(3,7),leg.txt="Fitness of offspring")

#making data frame where row names are tips of the tree 
bird_data <- data.frame(time,offspring, row.names=tree$tip.label)

#pgls anaylsis
pglsModel <- gls(time ~ offspring, correlation = corBrownian(phy = tree),
                 data = bird_data, method = "ML")
#view output
summary(pglsModel)

#plot correlation and add best line of fit from "pglsModel"
plot(time~offspring, data = bird_data)
abline(pglsModel)
