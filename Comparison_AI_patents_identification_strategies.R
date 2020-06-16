#This code is used for comparing the result from our Keyword-based search with the results from two IPC-based searches, proposed by 
#H. Fujii, S. Managi (2018) and C.-Y. Tseng, P.-H. Ting (2013)
#clear your global environment
rm(list=ls())
#set working directory:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load library to read the xlsx abstract file
library("readxl")

#1.Raw data analysis ----
titledata <-read.csv("data/Info_Titles.csv", sep = ";", header = TRUE)
abstractdata <-read_excel("data/Info_Abstracts.xlsx")
maindata2 <- read.csv("data/Info_Full dataset.csv", sep = ";", header = TRUE)
priorfildata <- read.csv("data/Info_Priorities.csv", sep = ";", header = TRUE)

abstractdata$appln_abstract <- tolower(abstractdata$appln_abstract)
titledata$appln_title <- tolower(titledata$appln_title)

maindata2$appln_title <- titledata$appln_title[match(maindata2$appln_id, titledata$appln_id)]
maindata2$appln_abstract <- abstractdata$appln_abstract[match(maindata2$appln_id, abstractdata$appln_id)]

#Keep only Patents of Invention (PI), thus excluding Utility Models (UM) and Design Patents (DP)
maindata <- maindata2[which(maindata2$ipr_type == 'PI'), ]

priorities <- maindata[which(maindata$earliest_filing_id == maindata$appln_id), ]
#therefore, we have 23,416 priorities;
priorities <- priorities[,c((1), (28), (29))]
rm(abstractdata)
rm(maindata)
rm(maindata2)
rm(priorfildata)
rm(titledata)

length(unique(priorities$appln_id)) #23,416
#if you want, you can write our query to check its abstracts and titles:
write.csv2(priorities, file = "MyQuery1.csv", row.names = TRUE)

#Query 2 - Query from H. Fujii, S. Managi (2018)
Query2 <- read.csv("data/Data_comparison/Query 2.csv", sep = ";", header = F)
names(Query2) <- c("appln_id", "ipr_type")
Query2 <- Query2[which(Query2$ipr_type == 'PI'), ] #30,082 priorities
Query2$appln_id <- gsub("ï»¿", "", str_trim(Query2$appln_id))
length(unique(Query2$appln_id)) #23,599 priorities

#create function for selecting patents that in one dataset and not in another:
'%notin%' <- Negate('%in%')
NotInQuery2 <- Query2[Query2$appln_id %notin% priorities$appln_id,]

#load library used for excluding duplicates
library(tidyverse) 
NotInQuery2_unique <- NotInQuery2[!duplicated(NotInQuery2$appln_id), ] #16,984

#if you want, you can write the appln_ids and ipc_type of Query 2, and later use the appln_id information to check via PATSTAT the abstract
#or title information:
write.csv2(NotInQuery2_unique, file = "NotInQuery2_unique2.csv", row.names = TRUE)

#Query 3 - Query from C.-Y. Tseng, P.-H. Ting (2013)
Query3 <- read.csv("data/Data_comparison/Query 3.csv", sep = ";", header = F)
names(Query3) <- c("appln_id", "ipr_type")
Query3 <- Query3[which(Query3$ipr_type == 'PI'), ] #167,307 priorities

Query3$appln_id <- gsub("ï»¿", "", str_trim(Query3$appln_id))
length(unique(Query3$appln_id)) #146,049 priorities

NotInQuery3 <- Query3[Query3$appln_id %notin% priorities$appln_id,]
NotInQuery3_unique <- NotInQuery3[!duplicated(NotInQuery3$appln_id), ] #138,294 unique priorities

#if you want, you can write the appln_ids and ipc_type of Query 2, and later use the appln_id information to check via PATSTAT the abstract
#or title information:
write.csv2(NotInQuery3_unique, file = "NotInQuery3_unique2.csv", row.names = TRUE)

#2.Comparison Queries ----
rm(list=ls())

#load merged data, which contains our classification for 100 patents of each dataset (ours and the 2 others based in IPC codes);
merged <-read.csv("data/Data_comparison/merged.csv", sep = ";", header = TRUE)

#filter out repeated appln_ids;
merged <- merged[!duplicated(merged$appln_id), ]

#load unique of each dataset (3 uniques) and add a column to each (saying if it ai or not), matching by appln_id
#Our Query:
Myquery <- read.csv("data/Data_comparison/MyQuery1_100.csv", sep = ";", header = TRUE)
Myquery$IsAI <- merged$AI.patent.[match(Myquery$appln_id, merged$appln_id)]
table(Myquery$IsAI)

#Accuracy Myquery:
1-(4/(4+90))

#Query 2:
Query2 <- read.csv("data/Data_comparison/NotIn1_Query2_unique_100.csv", sep = ";", header = TRUE)
Query2$IsAI <- merged$AI.patent.[match(Query2$appln_id, merged$appln_id)]
table(Query2$IsAI)

#Accuracy Query 2:
1-(16/(16+78))


#Query 3:
Query3 <- read.csv("data/Data_comparison/NotIn1_Query3_unique_100.csv", sep = ";", header = TRUE)
Query3$IsAI <- merged$AI.patent.[match(Query3$appln_id, merged$appln_id)]
table(Query3$IsAI)

#Accuracy Query 3:
1-(62/(62+37))
