#This code is used for comparing the result from our Keyword-based search with the results from two IPC-based searches, proposed by 
#H. Fujii, S. Managi (2018) and C.-Y. Tseng, P.-H. Ting (2013).


#clear your global environment
rm(list=ls())
#set working directory:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load library to read the xlsx abstract file
library("readxl")

#1.Raw data analysis ----
#In the first part, we show how we separated the data from the three queries (ours and the results from the mentioned authors), which we 
#analyze in the second part of this code.

#First we read the title data of our results:
titledata <-read.csv("data_main_analysis/Info_Titles.csv", sep = ";", header = TRUE)

#Then we read the abstract data of our results:
abstractdata <-read_excel("data_main_analysis/Info_Abstracts.xlsx")

#Then we read all the appln_ids from our results:
maindata2 <- read.csv("data_main_analysis/Info_Full dataset.csv", sep = ";", header = TRUE)

#And finally we read all the priorities from our results. We use this data to separate priorities from non-priorities.
priorfildata <- read.csv("data_main_analysis/Info_Priorities.csv", sep = ";", header = TRUE)

#We also put the text from titles and abstracts in lower-case
abstractdata$appln_abstract <- tolower(abstractdata$appln_abstract)
titledata$appln_title <- tolower(titledata$appln_title)

#And then we match this titles and abstract data with their corresponding appln_ids:
maindata2$appln_title <- titledata$appln_title[match(maindata2$appln_id, titledata$appln_id)]
maindata2$appln_abstract <- abstractdata$appln_abstract[match(maindata2$appln_id, abstractdata$appln_id)]

#Now weep only Patents of Invention (PI), thus excluding Utility Models (UM) and Design Patents (DP)
maindata <- maindata2[which(maindata2$ipr_type == 'PI'), ]

#and now, finally, we separate the priorities from the rest (non-priorities). It's this dataset we will analyze.
priorities <- maindata[which(maindata$earliest_filing_id == maindata$appln_id), ]
#therefore, we have 23,416 priorities;

#Let's exclude columns and datasets we won't use:
priorities <- priorities[,c((1), (28), (29))]
rm(abstractdata)
rm(maindata)
rm(maindata2)
rm(priorfildata)
rm(titledata)

#and check how many unique appln_ids we have:
length(unique(priorities$appln_id)) #23,416

#if you want, you can write our query to check its abstracts and titles:
write.csv2(priorities, file = "MyQuery1.csv", row.names = TRUE)

#Query 2 - Query from H. Fujii, S. Managi (2018)
#We already collected only priorities for the two additional queries. 
#First we read the data containing appln_ids and the type of patent.
Query2 <- read.csv("data_comparison/Query 2.csv", sep = ";", header = F)
names(Query2) <- c("appln_id", "ipr_type")

#Now we separate Inventions from other types of patents (utility models and design patents)
Query2 <- Query2[which(Query2$ipr_type == 'PI'), ] #30,082 priorities
#there is a small typo on the first register, related to a problematic conversion from PATSTAT to a csv file. We adjust that by doing:
Query2$appln_id <- gsub("ï»¿", "", str_trim(Query2$appln_id))

#and now we check how many priorities we have on this data
length(unique(Query2$appln_id)) #23,599 priorities

#create function for selecting patents that in one dataset (Query2) and not in another (priorities):
'%notin%' <- Negate('%in%')

#apply newly created function:
NotInQuery2 <- Query2[Query2$appln_id %notin% priorities$appln_id,]

#load library used for excluding duplicates
library(tidyverse)
#apply the exclude duplicates function, so we have only non repeated appln_ids:
NotInQuery2_unique <- NotInQuery2[!duplicated(NotInQuery2$appln_id), ] #16,984

#if you want, you can write the appln_ids and ipc_type of Query 2, and later use the appln_id information to check via PATSTAT the abstract
#or title information:
write.csv2(NotInQuery2_unique, file = "NotInQuery2_unique2.csv", row.names = TRUE)

#Query 3 - Query from C.-Y. Tseng, P.-H. Ting (2013)
#We do the same as we did for Query 2:
Query3 <- read.csv("data_comparison/Query 3.csv", sep = ";", header = F)
names(Query3) <- c("appln_id", "ipr_type")
Query3 <- Query3[which(Query3$ipr_type == 'PI'), ] #167,307 priorities

Query3$appln_id <- gsub("ï»¿", "", str_trim(Query3$appln_id))
length(unique(Query3$appln_id)) #146,049 priorities

NotInQuery3 <- Query3[Query3$appln_id %notin% priorities$appln_id,]
NotInQuery3_unique <- NotInQuery3[!duplicated(NotInQuery3$appln_id), ] #138,294 unique priorities

#if you want, you can write the appln_ids and ipc_type of Query 2, and later use the appln_id information to check via PATSTAT the abstract
#or title information:
write.csv2(NotInQuery3_unique, file = "NotInQuery3_unique2.csv", row.names = TRUE)

#2.Comparison of Queries ----
#The data loaded here is a result of the selection of the previous steps. We explain how we select the analyzed patents in the paper.
#we start by cleaning the working environment:
rm(list=ls())

#load merged data, which contains our classification for 100 patents of each dataset (ours and the 2 others based in IPC codes);
merged <-read.csv("data_comparison/merged.csv", sep = ";", header = TRUE)

#filter out repeated appln_ids;
merged <- merged[!duplicated(merged$appln_id), ]

#load unique of each dataset (3 uniques) and add a column to each (saying if it ai or not), matching by appln_id
#Our Query:
Myquery <- read.csv("data_comparison/MyQuery1_100.csv", sep = ";", header = TRUE)
Myquery$IsAI <- merged$AI.patent.[match(Myquery$appln_id, merged$appln_id)]
table(Myquery$IsAI)

#Thus, we have 90 AI patents on this dataset, 4 patents which are not related to AI, and 6 which are unclear. Excluding the unclear ones,
#we can calculate the accuracy by doing:

#Accuracy Myquery:
1-(4/(4+90))

#Query 2:
Query2 <- read.csv("data_comparison/NotIn1_Query2_unique_100.csv", sep = ";", header = TRUE)
Query2$IsAI <- merged$AI.patent.[match(Query2$appln_id, merged$appln_id)]
table(Query2$IsAI)

#Accuracy Query 2:
1-(16/(16+78))


#Query 3:
Query3 <- read.csv("data_comparison/NotIn1_Query3_unique_100.csv", sep = ";", header = TRUE)
Query3$IsAI <- merged$AI.patent.[match(Query3$appln_id, merged$appln_id)]
table(Query3$IsAI)

#Accuracy Query 3:
1-(62/(62+37))
