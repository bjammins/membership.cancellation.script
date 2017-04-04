library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
#library(RCurl)
library(XML)

#set working director
setwd("L:/MSO/data/")
#data <- read_excel("Cancellation_Survey_03222017.xlsx", sheet=1, col_names=TRUE)

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

#graph how long have you been an AWHONN Member



pl <- ggplot(df_long,aes(factor(`788884-How long have you been an AWHONN member?`)))+geom_bar()+
  xlab("Membership Duration Category (n=65)")

# select the question Please indicate the primary reasons you are discontinuing your membership

df_788885 <- df_long[ , grepl( "788885" , names( df_long ) )  ]

df_788885 <- cbind(df_long$Resp_ID,df_788885[,])

colnames(df_788885)[colnames(df_788885) == 'df_long$Resp_ID'] <- 'Resp_ID'

melt_788885 <- melt(df_788885, id.vars="Resp_ID")

melt_788885 <- na.omit(melt_788885,cols="value")



pl <- ggplot(melt_788885,aes(factor(value)))+geom_bar()+
  xlab("Reason") + theme(axis.text.x=element_blank(),
                         axis.ticks.x=element_blank())+ scale_y_continuous( limits = c(0,30) )
pl <- pl + annotate(geom="text", x=1, y=3, label="No networking opportunities")
pl <- pl + annotate(geom="text", x=2, y=7, label="Obtain same benefits from other resources")
pl <- pl + annotate(geom="text", x=3, y=2, label="Not satisfied w memb experience")
pl <- pl + annotate(geom="text", x=4, y=19, label="Dues too high compared to benefits")
pl <- pl + annotate(geom="text", x=5, y=17, label="Employer will not reimburse dues")
pl <- pl + annotate(geom="text", x=6, y=3, label="Discounts don't offset the costs")
pl <- pl + annotate(geom="text", x=7, y=27, label="No longer in the field")
pl <- pl + annotate(geom="text", x=8, y=19, label="Retiring")


# to rejoin AWHONN I would primarily need


df_788892 <- df_long[ , grepl( "788892" , names( df_long ) )  ]

df_788892 <- cbind(df_long$Resp_ID,df_788892[,])

colnames(df_788892)[colnames(df_788892) == 'df_long$Resp_ID'] <- 'Resp_ID'

melt_788892 <- melt(df_788892, id.vars="Resp_ID")

melt_788892 <- na.omit(melt_788892,cols="value")

pl <- ggplot(melt_788892,aes(factor(value)))+geom_bar()+
  xlab("Reason")+ theme(axis.text.x=element_blank(),
                        axis.ticks.x=element_blank()) 

pl <- pl + annotate(geom="text", x=1, y=6, label="Leadership/Volunteer opportunities")
pl <- pl + annotate(geom="text", x=2, y=3, label="More online resources")
pl <- pl + annotate(geom="text", x=3, y=7, label="More online education programs")
pl <- pl + annotate(geom="text", x=4, y=6, label="More networking events")
pl <- pl + annotate(geom="text", x=5, y=8, label="Certification/Credential")
pl <- pl + annotate(geom="text", x=6, y=11, label="Larger member course discounts")
pl <- pl + annotate(geom="text", x=7, y=4, label="Larger memb. conference discounts")
pl <- pl + annotate(geom="text", x=8, y=42, label="Nothing")



#Overall how happy were you with AWHONN membership?

pl <- ggplot(df_long,aes(factor(`788894-Overall how happy were you with your AWHONN membership?`)))+geom_bar()+
  xlab("")+scale_y_continuous( limits = c(0,40) ) +
  theme(axis.text.x = element_text(colour="grey20",size=14), axis.text.y = element_text(colour="grey20",size=12))


# How likely are you to recommend membership and encourage your colleagues or industry partners
pl <- ggplot(df_long,aes(factor(`788897-How likely are you to recommend membership and encourage your colleagues or industry partners`)))+geom_bar()+
  xlab("")+scale_y_continuous( limits = c(0,30) ) +
  theme(axis.text.x = element_text(colour="grey20",size=14), axis.text.y = element_text(colour="grey20",size=12))



# Are you a member of another professional nursing organization?

pl <- ggplot(df_long,aes(factor(`788898-Are you a member of another professional nursing organization?`)))+geom_bar()+
  xlab("")+scale_y_continuous( limits = c(0,50) ) +
  theme(axis.text.x = element_text(colour="grey20",size=14), axis.text.y = element_text(colour="grey20",size=12))



#Subest people who responded where dues are too high compared to resources

df_dues_high <- df_long[df_long$`788885.2728611-Please indicate the primary reason(s) you are discontinuing your membership in AWHONN?`=='E. Dues are too high for the benefits gained through membership',]

#remove NA Resp_ID from the data frame
df_dues_high <- df_dues_high[!is.na(df_dues_high$Resp_ID),]

#graph how long have you been an AWHONN Member for people where dues are too high

pl <- ggplot(df_dues_high,aes(factor(`788884-How long have you been an AWHONN member?`)))+geom_bar()+
  xlab("Membership Duration Category (n=18)")+ggtitle("How long have you been an AWHONN member?")+ theme(plot.title = element_text(size=22, hjust = 0.5))



# select the question Please indicate the primary reasons you are discontinuing your membership

df_788885 <- df_dues_high[ , grepl( "788885" , names( df_long ) )  ]

df_788885 <- cbind(df_dues_high$Resp_ID,df_788885[,])

colnames(df_788885)[colnames(df_788885) == 'df_dues_high$Resp_ID'] <- 'Resp_ID'

melt_788885 <- melt(df_788885, id.vars="Resp_ID")

melt_788885 <- na.omit(melt_788885,cols="value")



pl <- ggplot(melt_788885,aes(factor(value)))+geom_bar()+
  xlab("Reason") + scale_y_continuous( limits = c(0,20) ) + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(size=22, hjust = 0.5)) +
  ggtitle("Please indicate the primary reason(s) you are discontinuing your membership in AWHONN?")


pl <- pl + annotate(geom="text", x=1, y=2, label="No networking opportunities")
pl <- pl + annotate(geom="text", x=2, y=5, label="Obtain same benefits from other resources")
pl <- pl + annotate(geom="text", x=3, y=2, label="Not satisfied w memb experience")
pl <- pl + annotate(geom="text", x=4, y=19, label="Dues too high compared to benefits")
pl <- pl + annotate(geom="text", x=5, y=9, label="Employer will not reimburse dues")
pl <- pl + annotate(geom="text", x=6, y=3, label="Discounts don't offset the costs")
pl <- pl + annotate(geom="text", x=7, y=2, label="No longer in the field")
pl <- pl + annotate(geom="text", x=8, y=2, label="Retiring")
pl




#for people whose dues are too high compared to the benefits gained through membership
# to rejoin AWHONN I would primarily need


df_788892 <- df_dues_high[ , grepl( "788892" , names( df_dues_high ) )  ]

df_788892 <- cbind(df_dues_high$Resp_ID,df_788892[,])

colnames(df_788892)[colnames(df_788892) == 'df_dues_high$Resp_ID'] <- 'Resp_ID'

melt_788892 <- melt(df_788892, id.vars="Resp_ID")

melt_788892 <- na.omit(melt_788892,cols="value")

pl <- ggplot(melt_788892,aes(factor(value)))+geom_bar()+
  xlab("Reason")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(size=22, hjust = 0.5)) +
  ggtitle("To rejoin AWHONN, I would primarily need...")

pl <- pl + annotate(geom="text", x=1, y=3, label="Leadership/Volunteer opportunities")
pl <- pl + annotate(geom="text", x=2, y=2, label="More online resources")
pl <- pl + annotate(geom="text", x=3, y=5, label="More online education programs")
pl <- pl + annotate(geom="text", x=4, y=3, label="More networking events")
pl <- pl + annotate(geom="text", x=5, y=5, label="Certification/Credential")
pl <- pl + annotate(geom="text", x=6, y=8, label="Larger member course discounts")
pl <- pl + annotate(geom="text", x=7, y=3, label="Larger memb. conference discounts")
pl <- pl + annotate(geom="text", x=8, y=7, label="Nothing")
pl

#for people whose dues are too high compared to the benefits gained through membership
#Overall how happy were you with AWHONN membership?

pl <- ggplot(df_dues_high,aes(factor(`788894-Overall how happy were you with your AWHONN membership?`)))+geom_bar()+
  xlab("")+scale_y_continuous( limits = c(0,15) ) +
  theme(axis.text.x = element_text(colour="grey20",size=14), axis.text.y = element_text(colour="grey20",size=12), plot.title = element_text(size=22, hjust = 0.5)) +
  ggtitle("Overall, how happy were you with AWHONN Membership?")
pl


#for people whose dues are too high compared to the benefits gained through membership
# How likely are you to recommend membership and encourage your colleagues or industry partners
pl <- ggplot(df_dues_high,aes(factor(`788897-How likely are you to recommend membership and encourage your colleagues or industry partners`)))+geom_bar()+
  xlab("")+scale_y_continuous( limits = c(0,20) ) +
  theme(axis.text.x = element_text(colour="grey20",size=14), axis.text.y = element_text(colour="grey20",size=12), plot.title = element_text(size=22, hjust = 0.5)) + ggtitle("How likely are you to recommend AWHONN membership to your colleagues or industry partners?")
pl


# Are you a member of another professional nursing organization?

pl <- ggplot(df_dues_high,aes(factor(`788898-Are you a member of another professional nursing organization?`)))+geom_bar()+
  xlab("")+scale_y_continuous( limits = c(0,20) ) +
  theme(axis.text.x = element_text(colour="grey20",size=14), axis.text.y = element_text(colour="grey20",size=12), plot.title = element_text(size=22, hjust = 0.5)) +
  ggtitle("Are you a member of another professional nursing organization?")
pl

#faceting


pl <- ggplot(df_long,aes(factor(`788894-Overall how happy were you with your AWHONN membership?`))) + geom_bar()
pl + facet_grid(.~`788898-Are you a member of another professional nursing organization?`)


