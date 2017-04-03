library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(RCurl)
library(XML)
library(EnQuireR)


setwd("L:/MSO/data/")
data <- read_excel("Cancellation_Survey_03222017.xlsx", sheet=1, col_names=TRUE)

data <- read.csv("Cancellation_Survey_03222017.csv", header=TRUE)


colnames(data)[colnames(data) == 'Question.ID'] <- 'Quest_ID'
colnames(data)[colnames(data) == "Respondent.s.Answer"] <- 'Resp_ans'
colnames(data)[colnames(data) == "Respondent.ID"] <- 'Resp_ID'
colnames(data)[colnames(data) == "Answer.ID"] <- 'Ans_ID'

#create data lookup table for questions

quest <- data %>% distinct(Quest_ID,Question)

remove <- !names(data)  %in% c("Question")

data <- data[,remove]

#removes the hypertext tags from the questions.

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

#overwrite text questions
quest[,2]<- apply(quest[,2, drop=F], 2, cleanFun)
quest$Question <- as.factor(quest$Question)

df_merged <- merge(x = data, y = quest, by = 'Quest_ID', all= TRUE)

df_merged <- df_merged[,c("Date.Submitted", "Question.Format", "Quest_ID",  "Question", "Resp_ID", "Ans_ID", "Resp_ans")]

#create a space for merged id fields
df_merged$id <- df_merged$Quest_ID

#create a new question identifier for all multi select check boxes
mult <- df_merged$Question.Format=='Multi-Select (i.e. checkboxes)' 

#subset all questions with Multi-Select
subset <- df_merged[mult,]

df_merged[mult, "id"] <- paste(subset$Quest_ID,subset$Ans_ID, sep=".")

col_head <- df_merged %>% distinct(id,Question)

df_long <- dcast(df_merged, Resp_ID~id, value.var="Resp_ans")

#create column headings for the updated spreadsheet
col_head <- df_merged %>% distinct(id,Question)
col_head <- paste(col_head$id,col_head$Question, sep="-")

col_head <- c("Resp_ID", col_head)

colnames(df_long) <- col_head 

write.csv(df_long, "canceled_long.csv")



