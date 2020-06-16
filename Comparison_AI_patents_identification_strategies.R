#clear your global environment
rm(list=ls())
#set working directory:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load library to read the xlsx abstract file
library("readxl")

#1.Raw data analysis ----
titledata <-read.csv("data/Info_Titles.csv", sep = ";", header = TRUE)
abstractdata <-read_excel("data/Info_Abstracts.xlsx")
maindata2 <- read.csv("data/Info__Full dataset.csv", sep = ";", header = TRUE)
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

write.csv2(priorities, file = "MyQuery1.csv", row.names = TRUE)

#Query 2
Query3 <- read.csv("Query 3.csv", sep = ";", header = F)
names(Query3) <- c("appln_id", "ipc_class_symbol", "appln_title", "appln_abstract", "ipr_type ")
Query3 <- Query3[which(Query3$ipr_type == 'PI'), ] #30,082 priorities
Query3$appln_id <- gsub("ï»¿", "", str_trim(Query3$appln_id))
length(unique(Query3$appln_id)) #23,599 priorities

NotInQuery3 <- Query3[Query3$appln_id %notin% priorities$appln_id,]

write.csv2(NotInQuery3, file = "NotInQuery3.csv", row.names = TRUE)

#load library used for excluding duplicates
library(tidyverse) 

NotInQuery3_unique <- NotInQuery3[!duplicated(NotInQuery3$appln_id), ]
NotInQuery3_unique$appln_abstract <- tolower(NotInQuery3_unique$appln_abstract)
NotInQuery3_unique$appln_title <- tolower(NotInQuery3_unique$appln_title)
NotInQuery3_unique <- NotInQuery3_unique[,(-2)] #16,984

write.csv2(NotInQuery3_unique, file = "NotInQuery3_unique2.csv", row.names = TRUE)
write.csv2(table(Query3$ipc_class_symbol), file = "TableQuery3ClassSymbol.csv", row.names = TRUE)

#Query 4
Query4 <- read.csv("Query 4.csv", sep = ";", header = F)
names(Query4) <- c("appln_id", "ipc_class_symbol", "appln_title", "appln_abstract", "ipr_type ")
Query4 <- Query4[which(Query4$ipr_type == 'PI'), ] #167,307 priorities

Query4$appln_id <- gsub("ï»¿", "", str_trim(Query4$appln_id))
length(unique(Query4$appln_id)) #146,049 priorities

NotInQuery4 <- Query4[Query4$appln_id %notin% priorities$appln_id,]

write.csv2(NotInQuery4, file = "NotInQuery4.csv", row.names = TRUE)
NotInQuery4_unique <- NotInQuery4[!duplicated(NotInQuery4$appln_id), ]
NotInQuery4_unique$appln_abstract <- tolower(NotInQuery4_unique$appln_abstract)
NotInQuery4_unique$appln_title <- tolower(NotInQuery4_unique$appln_title)
NotInQuery4_unique <- NotInQuery4_unique[,(-2)] #138,294

write.csv2(NotInQuery4_unique, file = "NotInQuery4_unique2.csv", row.names = TRUE)
write.csv2(table(Query4$ipc_class_symbol), file = "TableQuery4ClassSymbol2.csv", row.names = TRUE)

#2.Comparison Queries ----
rm(list=ls())

#load merged400 data;
merged <-read.csv("merged.csv", sep = ";", header = TRUE)
#filter out repeated appln_ids;
merged <- merged[!duplicated(merged$appln_id), ]
write.csv2(merged, file = "merged_final.csv", row.names = F)

#load unique of each dataset (3 uniques) and add a column to each (saying if it ai or not), matching by appln_id
#Our Query:
Myquery <- read.csv("MyQuery1_100.csv", sep = ";", header = TRUE)
Myquery$IsAI <- merged$AI.patent.[match(Myquery$appln_id, merged$appln_id)]
table(Myquery$IsAI)

#Accuracy Myquery:
1-(4/(4+90))

#Query 2:
Query2 <- read.csv("NotIn1_Query3_unique_100.csv", sep = ";", header = TRUE)
Query2$IsAI <- merged$AI.patent.[match(Query2$appln_id, merged$appln_id)]
table(Query2$IsAI)

#Accuracy Query 2:
1-(16/(16+78))


#Query 3:
Query3 <- read.csv("NotIn1_Query4_unique_100.csv", sep = ";", header = TRUE)
Query3$IsAI <- merged$AI.patent.[match(Query3$appln_id, merged$appln_id)]
table(Query3$IsAI)

#Accuracy Query 3:
1-(62/(62+37))
