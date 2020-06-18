#This code is used for calculating the indicators proposed in the paper and to plot the results, as it is presented in the Figures 3 to 8
#in the paper.

#clear your global environment
rm(list=ls())
#set working directory:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load libraries we will use:
library(ggplot2)
library(stringr)
library(tidyverse)
library(plyr)
library("readxl")

#1.Raw data analysis ----
#On the first part of the analysis, we will load our data

#We start by loading all patents we collected:
maindata2 <- read.csv("data/Info_Full dataset.csv", sep = ";", header = TRUE)

#and then we load the data from priorities. this data is used as reference only for identifying the patent office where
#each priority was filled:
priorfildata <- read.csv("data/Info_Priorities.csv", sep = ";", header = TRUE)

#Now we separate the patents by kind, keeping only Patents of Invention (PI) and excluding Utility Models (UM) 
#and Design Patents (DP)
maindata <- maindata2[which(maindata2$ipr_type == 'PI'), ]

#save the information of the Patent Office where the earliest filing was applied
maindata$countryoforigin2 <- priorfildata$appln_auth[match(maindata$earliest_filing_id, priorfildata$appln_id)]

# create a dataset with the priorities of the maindata
priorities <- maindata[which(maindata$earliest_filing_id == maindata$appln_id), ]
# create a dataset with the non priorities of the maindata
nonpriorities <- maindata[which(maindata$earliest_filing_id != maindata$appln_id), ]

#create priorities based on appln kinds A (patent) and W (PCT application):
NoPCTPriorities <- priorities[which(priorities$appln_kind == 'A '), ]
YesPCTPriorities <- priorities[which(priorities$appln_kind == 'W '), ]

#collect number count of priorities, non priorities and where they go (being the where they go indicator what we refer
#in the paper as "AI patents going abroad_Country")
sumpriorities <- as.data.frame(table(priorities$appln_auth, priorities$appln_filing_year))
sumnonpriorities <- as.data.frame(table(nonpriorities$appln_auth, nonpriorities$appln_filing_year))
wheretheygo <- as.data.frame(table(nonpriorities$countryoforigin2, nonpriorities$appln_filing_year))
NoPCTPatents <- as.data.frame(table(NoPCTPriorities$countryoforigin2, NoPCTPriorities$appln_filing_year))
PCTPatents <- as.data.frame(table(YesPCTPriorities$countryoforigin2, YesPCTPriorities$appln_filing_year))

#rename columns
names(sumpriorities) <- c("Country_code", "Year", "FreqP")
names(sumnonpriorities) <- c("Country_code", "Year", "FreqN")
names(wheretheygo) <- c("Country_code", "Year", "FreqW")
names(NoPCTPatents) <- c("Country_code", "Year", "NoPCTPatents")
names(PCTPatents) <- c("Country_code", "Year", "PCTPatents")

#Merge datasets
tabledata2 <- merge(sumpriorities, wheretheygo, all=TRUE, by=c("Country_code", "Year"))
tabledata2 <- merge(tabledata2, sumnonpriorities, all=TRUE, by=c("Country_code", "Year"))
tabledata2 <- merge(tabledata2, NoPCTPatents, all=TRUE, by=c("Country_code", "Year"))
tabledata2 <- merge(tabledata2, PCTPatents, all=TRUE, by=c("Country_code", "Year"))

#replace NA values by 0
tabledata2[is.na(tabledata2)] <- 0

#Now we include number of inventions per country independently of appln kind. We have already collected and summarized
#the data we use here by collecting all priority patents from patstat that each country filled in each year. This
#indicator is the indicator named "Total Patents_Country", presented in the equation 2 of the paper.
infopatentspercountry <-read.csv("data/InfopartialTSummbycountry.csv", sep = ";", header = TRUE)
infopatentspercountry <- infopatentspercountry[ , c((-1),(-4),(-5),(-6),(-7))]
names(infopatentspercountry) <- c("Country_code", "Year", "TotalPatentsCountry")


tabledata2 <- merge(tabledata2, infopatentspercountry, all=FALSE, by=c("Country_code", "Year"))
tabledata2[is.na(tabledata2)] <- 0

#include total number of inventions of all countries per year independently of appln kind. Again, we have already 
#collected and summarized the data we use here by collecting all priority patents from patstat per year (that is, regardless
#of which country filled them) per year. This indicator is the indicator named "Global Number of Patents", presented in the 
#equation 2 of the paper.
infopatentstotalcountry <-read.csv("data/InfototalT1SummAllYears.csv", sep = ";", header = TRUE)
infopatentstotalcountry <- infopatentstotalcountry[ , c((-1),(-3),(-4),(-5),(-6))]
names(infopatentstotalcountry) <- c("Year", "TotalNumberofPatentsYear")
tabledata2 <- merge(tabledata2, infopatentstotalcountry, by = "Year")

#include info about all patents generated in AI per year regarding the distinct appln_kinds. We already have this in our
#priorities dataset.
AIpatents <- as.data.frame(table(priorities$appln_filing_year))
names(AIpatents) <- c("Year", "TotalNumberofAIPatentsYear")
tabledata2 <- merge(tabledata2, AIpatents, by = "Year")

#let's save our dataset before applying the calculations
write.csv2((tabledata2), file = "AllinformationB4calculations.csv", row.names = F)

#2.Calculations ----
#Let's clear again our global environment
rm(list=ls())
#and then we read the file we just saved
tabledata3 <- read.csv("AllinformationB4calculations.csv", sep = ";", header = TRUE)

#select the time period we want in a new dataset
tabledata3 <-tabledata3[tabledata3$Year > 1980,]

#cut in 3 periods of 12 years each
tabledata3$Period <- cut(tabledata3$Year, breaks= c(1,1991, 2003, 2016), 
                         labels=c('1979-1991', '1991-2003', '2003-2015'))

#and aggregate it by the mean of each period
newtable <- aggregate(tabledata3[, 2:11], list(tabledata3$Country_code, tabledata3$Period), mean)

#for calculating the indexes:
newtable$SpecialisationIndice <- (newtable$FreqP/newtable$TotalPatentsCountry)/(newtable$TotalNumberofAIPatentsYear/newtable$TotalNumberofPatentsYear)
newtable$NoPCTPatents <- as.numeric(newtable$NoPCTPatents)
newtable$PCTPatents <- as.numeric(newtable$PCTPatents)
newtable$WeightedPatents <- newtable$NoPCTPatents + (newtable$PCTPatents)*3
newtable$Sources <- newtable$SpecialisationIndice*newtable$FreqP
newtable$Sources2 <- newtable$WeightedPatents*newtable$SpecialisationIndice
newtable$BreedingGroundInt <- (newtable$FreqW*newtable$FreqN)/newtable$FreqP
newtable$BreedingGroundInt2 <- (newtable$FreqW*newtable$FreqN)/newtable$WeightedPatents

names(newtable)[names(newtable) == 'Group.1'] <- 'Country'
names(newtable)[names(newtable) == 'Group.2'] <- 'Period'

write.csv2((newtable), file = "InformationByPeriodwithcalculations.csv", row.names = F)

#3.Visualization ----
#3.1. Fig 3 to 7 ----
#let's clear our environment again
rm(list=ls())
#and again we read the table we just created:
newtable <- read.csv("InformationByPeriodwithcalculations.csv", sep = ";", header = TRUE, dec = ",")

#now we create the multiple plot function (from  http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2))
#which allows us to plot multiple graphs in 1 figure
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# replace NAs
newtable[is.na(newtable)] <- 0
newtable$BreedingGroundInt[newtable$BreedingGroundInt==Inf] <- 0
newtable$BreedingGroundInt2[newtable$BreedingGroundInt2==Inf] <- 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

newtable[is.nan(newtable)] <- 0

#use countries names instead of codes:
countries <- read.csv("data/countries.csv", sep = ";", header = TRUE)
newtable$Country <- countries$Country[match(newtable$Country, countries$Symb1)]

newtableshort <- newtable
newtableshort <- newtableshort[ , (-2)]
testsads2 <- aggregate(newtableshort, list(newtableshort$Country), mean)
names(testsads2)[names(testsads2) == 'Group.1'] <- 'Country'
topRTAs <- testsads2$Country[order(testsads2$SpecialisationIndice, decreasing = TRUE)]

cdat <- ddply(subset(newtable, Country %in% topRTAs [1 : 20]), "Period", summarise, Value.mean=mean(SpecialisationIndice))

#Figure 3: Top 20 Patent Offices according to (and sorted by) the sum of their RTAs over three periods.
RTAs <- ggplot(subset(newtable, Country %in% topRTAs [1 : 20]),aes(x = SpecialisationIndice, y=reorder(Country, SpecialisationIndice), group=Period)) + geom_point(aes(shape=Period, color=Period, size = Period)) + 
  ggtitle(NULL) +
  geom_vline(data=cdat, aes(xintercept=Value.mean,  color=Period),
             linetype=c(4,2,3), size=1.2) + 
  xlab("Revealed Technology Advantage (RTA) index") +
  ylab(NULL)+
  scale_shape_manual(values=c(15,16,17)) + scale_size_manual(values=c(4,4.5,4)) + theme(legend.position = "right")

#Plot Figure:
RTAs

#Figures 4 (r1) and 5 (r2): Top 15 Patent offices which are considered National AI Breeding Grounds, according to 
#the Nat Breeding Grounds_Countryp and the Nat Breeding Grounds_Weighted_Countryp indicators, respectively
newtable$WeightedPatents <- (newtable$NoPCTPatents)/5 + (newtable$PCTPatents)*5
newtable$Sources2 <- newtable$WeightedPatents*newtable$SpecialisationIndice

#Graphics comparison Sources and Sources2
newtableshort2 <- newtable
newtableshort2 <- newtableshort2[ , (-2)]
testsads3 <- aggregate(newtableshort2, list(newtableshort2$Country), mean)
names(testsads3)[names(testsads3) == 'Group.1'] <- 'Country'
BreedingGroundst <- testsads3
#delete WIPO and EPO (they can't be sources, according to the definition)
testsads3 <- testsads3[which(testsads3$Country != 'European Patent Office (EPO)'), ]
testsads3 <- testsads3[which(testsads3$Country != 'International Bureau of the WIPO'), ]
testsads3 <- testsads3[which(testsads3$Country != 'Eurasian Patent Organization (EAPO)'), ]
topSources <- testsads3$Country[order(testsads3$Sources, decreasing = TRUE)]
topSources2 <- testsads3$Country[order(testsads3$Sources2, decreasing = TRUE)]

cdat2 <- ddply(subset(newtable, Country %in% topSources [1 : 15]), "Period", summarise, Value.mean=mean(Sources))

r1 <- ggplot(subset(newtable, Country %in% topSources [1 : 15]),aes(x = log10(Sources), y=reorder(Country, Sources), group=Period)) + geom_point(aes(shape=Period, color=Period, size = Period)) + 
  ggtitle(NULL) + geom_vline(data=cdat2, aes(xintercept=log10(Value.mean),  color=Period), linetype=c(4,2,3), size=1.2) + 
  xlab("LOG National Breeding Grounds Indices") +
  ylab(NULL)+
  scale_shape_manual(values=c(15,16,17)) + scale_size_manual(values=c(4,4.5,4)) + theme(legend.position = "right")

cdat3 <- ddply(subset(newtable, Country %in% topSources2 [1 : 15]), "Period", summarise, Value.mean=mean(Sources2))
r2 <- ggplot(subset(newtable, Country %in% topSources2 [1 : 15]),aes(x = log10(Sources2), y=reorder(Country, Sources2), group=Period)) + geom_point(aes(shape=Period, color=Period, size = Period)) + 
  ggtitle(NULL) + geom_vline(data=cdat3, aes(xintercept=log10(Value.mean),  color=Period), linetype=c(4,2,3), size=1.2) + 
  xlab("LOG National Breeding Grounds_Weighted Indices") +
  ylab(NULL)+
  scale_shape_manual(values=c(15,16,17)) + scale_size_manual(values=c(4,4.5,4)) + theme(legend.position = "right")

#plot figures:
multiplot(r1, r2, cols=1)

#Figures 6 (f1) and 7 (t2): Top 15 Patent Offices which are considered International Breeding Grounds, according to 
#the Int BreedingGround_Countryp and the Int BreedingGround_Weighted_Countryp indicators, respectively. 
newtable$WeightedPatents <- (newtable$NoPCTPatents)/5 + (newtable$PCTPatents)*5
newtable$BreedingGroundInt2 <- (newtable$FreqW*newtable$FreqN)/newtable$WeightedPatents
newtable$BreedingGroundInt2[newtable$BreedingGroundInt2==Inf] <- 0

#Graphics comparison BreedingGrounds and BreedingGrounds2
topBreedingGrounds <- BreedingGroundst$Country[order(BreedingGroundst$BreedingGroundInt, decreasing = TRUE)]
topBreedingGrounds2 <- BreedingGroundst$Country[order(BreedingGroundst$BreedingGroundInt2, decreasing = TRUE)]
newtable$BreedingGroundInt2[newtable$BreedingGroundInt2==Inf] <- 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
newtable[is.nan(newtable)] <- 0

newtable$Country <- as.vector(newtable$Country)
topBreedingGrounds3 <- newtable$Country[order(newtable$BreedingGroundInt2, decreasing = TRUE)]
cdat5 <- ddply(subset(newtable, Country %in% topBreedingGrounds3 [1 : 20]), "Period", summarise, Value.mean=mean(BreedingGroundInt2))
cdat4 <- ddply(subset(newtable, Country %in% topBreedingGrounds [1 : 15]), "Period", summarise, Value.mean=mean(BreedingGroundInt))

f1 <- ggplot(subset(newtable, Country %in% topBreedingGrounds [1 : 15]),aes(x = log10(BreedingGroundInt), y=reorder(Country, BreedingGroundInt), group=Period)) + geom_point(aes(shape=Period, color=Period, size = Period)) + 
  ggtitle(NULL) + geom_vline(data=cdat4, aes(xintercept=log10(Value.mean),  color=Period), linetype=c(4,2,3), size=1.2) + 
  xlab("LOG International Breeding Grounds Indices") +
  ylab(NULL)+
  scale_shape_manual(values=c(15,16,17)) + scale_size_manual(values=c(4,4.5,4)) + theme(legend.position = "right")

newtable$BreedingGroundInt2[newtable$BreedingGroundInt2==Inf] <- 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

newtable[is.nan(newtable)] <- 0
cdat5 <- ddply(subset(newtable, Country %in% topBreedingGrounds3 [1 : 15]), "Period", summarise, Value.mean=mean(BreedingGroundInt2))

t2 <- ggplot(subset(newtable, Country %in% topBreedingGrounds3 [1 : 27]),aes(x = log10(BreedingGroundInt2), y=reorder(Country, BreedingGroundInt2), group=Period)) + geom_point(aes(shape=Period, color=Period, size = Period)) + 
  ggtitle(NULL) + geom_vline(data=cdat5, aes(xintercept=log10(Value.mean),  color=Period), linetype=c(4,2,3), size=1.2) + 
  xlab("LOG International Breeding Grounds_Weighted Indices") +
  ylab(NULL)+
  scale_shape_manual(values=c(15,16,17)) + scale_size_manual(values=c(4,4.5,4)) + theme(legend.position = "right")

#plot figures:
multiplot(f1, t2, cols=1)

#3.2. Fig 8 ----
#For the techniques figure (Fig 8), we have to reload the data and make some analysis based on keywords:
#As we already did, last start with an empty environment
rm(list=ls())

#Let's read again the title and abstract data:
titledata <-read.csv("data/Info_Titles.csv", sep = ";", header = TRUE)
abstractdata <-read_excel("data/Info_Abstracts.xlsx")

#and transform it into lower case:
abstractdata$appln_abstract <- tolower(abstractdata$appln_abstract)
titledata$appln_title <- tolower(titledata$appln_title)

#now we read the main data
maindata2 <- read.csv("data/Info_Full dataset.csv", sep = ";", header = TRUE)

maindata2$appln_title <- titledata$appln_title[match(maindata2$appln_id, titledata$appln_id)]
maindata2$appln_abstract <- abstractdata$appln_abstract[match(maindata2$appln_id, abstractdata$appln_id)]

#filter for inventions:
maindata <- maindata2[which(maindata2$ipr_type == 'PI'), ]

#select priorities
priorities <- maindata[which(maindata$earliest_filing_id == maindata$appln_id), ]
#select only the columns we'll need:
priorities2 <- priorities[ , c((1),(15),(28),(29))]
#exclude patents which are not in the period we are analyzing:
priorities2 <-priorities2[priorities2$earliest_filing_year > 1978,]
priorities2 <-priorities2[priorities2$earliest_filing_year < 2017,]

#create a new column with the periods we want:
priorities2$Period <- cut(priorities2$earliest_filing_year, breaks= c(1,1991, 2003, 2016), 
                          labels=c('1979-1991', '1991-2003', '2003-2015'))

#put a number 1 for each patent, so it's easier to count them
priorities2$number <- 1

#Now we start picking the patents which have the keywords we want.

#1 - Neural networks - 11,932 priorities
Neuralnetworks <- unique(c(grep("neural network", priorities2$appln_abstract),
                           grep("neural network", priorities2$appln_title)))
NeuralnetworksSet <- priorities2[Neuralnetworks, ]
#Now we sum how many patents we have per period
newtable <- aggregate(NeuralnetworksSet[, 6], list(NeuralnetworksSet$Period), sum)
#and rename the dataset with the name of the technique for which we've collected the data:
newtable$Technique <- 'Neural Networks'

#2 - Machine learning - 2,391 priorities
MachineLearning <- unique(c(grep("machine learn", priorities2$appln_abstract), 
                            grep("machine learn", priorities2$appln_title)))
MachineLearningSet <- priorities2[MachineLearning, ]
Temporarytable <- aggregate(MachineLearningSet[, 6], list(MachineLearningSet$Period), sum)
Temporarytable$Technique <- 'Machine Learning'
newtable <- rbind(newtable, Temporarytable)

#3 - Expert systems - 2,048 priorities
ExpertSystems <- unique(c(grep("expert system", priorities2$appln_abstract),
                          grep("expert system", priorities2$appln_title)))
ExpertSystemsSet <- priorities2[ExpertSystems, ]
Temporarytable <- aggregate(ExpertSystemsSet[, 6], list(ExpertSystemsSet$Period), sum)
Temporarytable$Technique <- 'Expert Systems'
newtable <- rbind(newtable, Temporarytable)

#4 - Support vector machines - 2,855 priorities
SupportVectorMachine <- unique(c(grep("support vector machin|support vector network", priorities2$appln_abstract),
                                 grep("support vector machin|support vector network", priorities2$appln_title)))
SupportVectorMachineSet <- priorities2[SupportVectorMachine, ]
Temporarytable <- aggregate(SupportVectorMachineSet[, 6], list(SupportVectorMachineSet$Period), sum)
Temporarytable$Technique <- 'Support Vector Machine'
newtable <- rbind(newtable, Temporarytable)

#5 - Fuzzy logic - 1,274 priorities
Fuzzylogic <- unique(c(grep("fuzzy logic", priorities2$appln_abstract),
                       grep("fuzzy logic", priorities2$appln_title)))
FuzzylogicSet <- priorities2[Fuzzylogic, ]
Temporarytable <- aggregate(FuzzylogicSet[, 6], list(FuzzylogicSet$Period), sum)
Temporarytable$Technique <- 'Fuzzy Logic'
newtable <- rbind(newtable, Temporarytable)

#6 - Graphical Model - 335 priorities
ProbabilisticGraphicalModel <- unique(c(grep("probabilistic graphical model|graphical model|structured probabilistic model", priorities2$appln_abstract),
                                        grep("probabilistic graphical model|graphical model|structured probabilistic model", priorities2$appln_title)))
ProbabilisticGraphicalModelSet <- priorities2[ProbabilisticGraphicalModel, ]
Temporarytable <- aggregate(ProbabilisticGraphicalModelSet[, 6], list(ProbabilisticGraphicalModelSet$Period), sum)
Temporarytable$Technique <- 'Graphical Model'
newtable <- rbind(newtable, Temporarytable)

#7 - Supervised and unsupervised learning - 372 priorities
Supervised_and_unsuper_learn <- unique(c(grep("pervised learn", priorities2$appln_abstract),
                                         grep("pervised learn", priorities2$appln_title)))
Supervised_and_unsuper_learnSet <- priorities2[Supervised_and_unsuper_learn, ]
Temporarytable <- aggregate(Supervised_and_unsuper_learnSet[, 6], list(Supervised_and_unsuper_learnSet$Period), sum)
Temporarytable$Technique <- 'Sup. and Uns. Learning'
newtable <- rbind(newtable, Temporarytable)

#8 - Deeplearning - 534 priorities
Deeplearning <- unique(c(grep("deep learn|deep structured learn|hierarchical learn", priorities2$appln_abstract),
                         grep("deep learn|deep structured learn|hierarchical learn", priorities2$appln_title)))
DeeplearningSet <- priorities2[Deeplearning, ]
Temporarytable <- aggregate(DeeplearningSet[, 6], list(DeeplearningSet$Period), sum)
Temporarytable$Technique <- 'Deep Learning'
newtable <- rbind(newtable, Temporarytable)

#9 - Classification and Regression Trees - 222 priorities
ClassificationandRegressionTrees <- unique(c(grep("classification tree|regression tree|decision tree learn", priorities2$appln_abstract),
                                             grep("classification tree|regression tree|decision tree learn", priorities2$appln_title)))
ClassificationandRegressionTreesSet <- priorities2[ClassificationandRegressionTrees, ]
Temporarytable <- aggregate(ClassificationandRegressionTreesSet[, 6], list(ClassificationandRegressionTreesSet$Period), sum)
Temporarytable$Technique <- 'Class. and Reg. Trees'
newtable <- rbind(newtable, Temporarytable)

#10 - Reinforced learning - 214 priorities
Reinforcedlearning <- unique(c(grep("reinforced learn|reinforcement learn", priorities2$appln_abstract),
                               grep("reinforced learn|reinforcement learn", priorities2$appln_title)))
ReinforcedlearningSet <- priorities2[Reinforcedlearning, ]
Temporarytable <- aggregate(ReinforcedlearningSet[, 6], list(ReinforcedlearningSet$Period), sum)
Temporarytable$Technique <- 'Reinforced Learning'
newtable <- rbind(newtable, Temporarytable)

#11 - Logic Programming - 69 priorities
LogicProgramming <- unique(c(grep("logic programming", priorities2$appln_abstract),
                             grep("logic programming", priorities2$appln_title)))
LogicProgrammingSet <- priorities2[LogicProgramming, ]
Temporarytable <- aggregate(LogicProgrammingSet[, 6], list(LogicProgrammingSet$Period), sum)
Temporarytable$Technique <- 'Logic Programming'
newtable <- rbind(newtable, Temporarytable)

#12 - Rule learning - 71 priorities
RuleLearn <- unique(c(grep("rule learn|rule induction", priorities2$appln_abstract),
                      grep("rule learn|rule induction", priorities2$appln_title)))
RuleLearnSet <- priorities2[RuleLearn, ]
Temporarytable <- aggregate(RuleLearnSet[, 6], list(RuleLearnSet$Period), sum)
Temporarytable$Technique <- 'Rule Learning'
newtable <- rbind(newtable, Temporarytable)

newtable$AveragePerYear <- (newtable$x)/12

TechniquesEvolution <- newtable$Technique[order(newtable$AveragePerYear, decreasing = TRUE)]
#let's put the right names on the table:
names(newtable)[names(newtable) == 'Group.1'] <- 'Period'
names(newtable)[names(newtable) == 'x'] <- 'Number of related patents'

cdat <- ddply(newtable, "Period", summarise, AveragePerYear.mean=mean(AveragePerYear))

techniques <- ggplot(subset(newtable, Technique %in% TechniquesEvolution),aes(x = log10(AveragePerYear), y=reorder(Technique, AveragePerYear,sum), group=Period)) + geom_point(aes(shape=Period, color=Period, size = Period)) + 
  ggtitle(NULL) +
  geom_vline(data=cdat, aes(xintercept=log10(AveragePerYear.mean),  color=Period),
             linetype=c(4,2,3), size=1.2) + 
  xlab("Log of the mean number of patents of the technique for the period") +
  ylab("Name of the technique")+
  scale_shape_manual(values=c(15,16,17)) + scale_size_manual(values=c(4,4.5,4)) + theme(legend.position = "right")

#Finally, we plot figure our Figure 8:
techniques
