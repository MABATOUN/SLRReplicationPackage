#Packages to install
install.packages("stringr")
install.packages("tidyverse")
install.packages("janitor")
install.packages("factoextra")
install.packages("lubridate")
install.packages("devtools")
install.packages("tidyr")
install.packages("tibble")
install.packages("plotly")
install.packages("data.table")
install.packages("readr")
install.packages("ggpubr")
install.packages("effsize")
install.packages("hash")

#Libraries to call
library(stringr)
library(tidyverse)
library(janitor)
library(factoextra)
library(lubridate)
library(devtools)
library(tidyr)
library(tibble)
library(plotly)
library(data.table)
library(readr)
library(ggpubr)
library(effsize)
library(hash)


# Query1Results.csv to ignore
# Query2Results.csv for metrics
# Query3Results.csv for upvotes/downvotes
# Query4Results.csv for time of accepted answer
# combinedData_composition31.csv for the mapping between topics and questions

composition_path <- paste(getwd(), "/topicsComposition/LOGGINGCOMPOSITION25.txt", sep = "")
composition <- read.csv(composition_path, header=FALSE, sep="\t")

questions_per_topic <- hash()

for (t in 0:24){ #topic0==>topic25
  qId_topicPercentage <- composition[,c(2,3+t)]
  colnames(qId_topicPercentage) <- c("id","percentage")
  length(qId_topicPercentage[[1]])
  qId_topicPercentage <- qId_topicPercentage[qId_topicPercentage$percentage>=0.1,]
  length(qId_topicPercentage[[1]])
  questions_per_topic[[as.character(t)]] <- qId_topicPercentage$id
}

number_of_questions_per_topic <- c()
for (key in keys(questions_per_topic)){
  print(paste(key," : ",length(questions_per_topic[[key]])))
  number_of_questions_per_topic <- append(number_of_questions_per_topic, length(questions_per_topic[[key]]))
}

median(number_of_questions_per_topic)
min(number_of_questions_per_topic)
max(number_of_questions_per_topic)



# topic_id_to_name <- hash()
# topic_id_to_name[["0"]] <- "Handling Log Files"
# topic_id_to_name[["1"]] <- "Interference Between Logging Libraries"
# topic_id_to_name[["2"]] <- "Usage of Mobile Infrastructure Logs"
# topic_id_to_name[["3"]] <- "Unrelated"
# topic_id_to_name[["4"]] <- "Usage of Server Infrastructure Logs"
# topic_id_to_name[["5"]] <- "Misusage of Logging API"
# topic_id_to_name[["6"]] <- "Misconfiguration of Maven Dependencies"
# topic_id_to_name[["7"]] <- "Understanding Webserver Logs"
# topic_id_to_name[["8"]] <- "Configuring Multiple Log Locations"
# topic_id_to_name[["9"]] <- "Parsing Complex Log Data"
# topic_id_to_name[["10"]] <- "Log Configuration Errors"
# topic_id_to_name[["11"]] <- "Usage of Logs in the Code"
# topic_id_to_name[["12"]] <- "Configuration & Dependency Issues - Winston"
# topic_id_to_name[["13"]] <- "Log Configuration in Laravel"
# topic_id_to_name[["14"]] <- "Distributed Logging"
# topic_id_to_name[["15"]] <- "Unrelated"
# topic_id_to_name[["16"]] <- "Log Configuration for External Libraries"
# topic_id_to_name[["17"]] <- "Context-dependent Usage of Logs"
# topic_id_to_name[["18"]] <- "Logging challenges in .NET projects"
# topic_id_to_name[["19"]] <- "Logging challenges in Java projects"
# topic_id_to_name[["20"]] <- "Unrelated"
# topic_id_to_name[["21"]] <- "Log Managment for Subprocesses and Bash Scripts"
# topic_id_to_name[["22"]] <- "Logging databases"
# topic_id_to_name[["23"]] <- "Disabling Application Logging"
# topic_id_to_name[["24"]] <- "Transfering Log Data"

topic_id_to_name <- hash()
topic_id_to_name[["0"]] <- "Handling Log Files"
topic_id_to_name[["1"]] <- "Dependency Configuration"
topic_id_to_name[["2"]] <- "Infrastructure Configuration"
topic_id_to_name[["3"]] <- "Unrelated"
topic_id_to_name[["4"]] <- "Infrastructure Configuration"
topic_id_to_name[["5"]] <- "Logging Libraries Code"
topic_id_to_name[["6"]] <- "Dependency Configuration"
topic_id_to_name[["7"]] <- "Infrastructure Configuration"
topic_id_to_name[["8"]] <- "Scattered Logging"
topic_id_to_name[["9"]] <- "Parsing Complex Log Data"
topic_id_to_name[["10"]] <- "Dependency Configuration"
topic_id_to_name[["11"]] <- "Logging Libraries Code"
topic_id_to_name[["12"]] <- "Dependency Configuration"
topic_id_to_name[["13"]] <- "Infrastructure Configuration"
topic_id_to_name[["14"]] <- "Scattered Logging"
topic_id_to_name[["15"]] <- "Unrelated"
topic_id_to_name[["16"]] <- "Infrastructure Configuration"
topic_id_to_name[["17"]] <- "Context-dependent Logging"
topic_id_to_name[["18"]] <- "Logging Libraries Code"
topic_id_to_name[["19"]] <- "Logging Libraries Code"
topic_id_to_name[["20"]] <- "Unrelated"
topic_id_to_name[["21"]] <- "Infrastructure Configuration"
topic_id_to_name[["22"]] <- "Scattered Logging"
topic_id_to_name[["23"]] <- "Infrastructure Configuration"
topic_id_to_name[["24"]] <- "Scattered Logging"


########################### Part I ###########################

metrics_path <- paste(getwd(), "/Query2Results.csv", sep = "")
metrics <- read.csv(metrics_path, header=TRUE, sep=",")


#csv file with: topic-id,question-id,time to get response,number of views

L=0
for (t in 0:length(questions_per_topic)-1){
  L = L+ length(questions_per_topic[[as.character(t)]])
}

output_aaTime <- data.frame(topicID = rep(0,L), questionID = rep(0,L), aaTime = rep(0,L), aaScore = rep(0,L))
output_views <- data.frame(topicID = rep(0,L), questionID = rep(0,L), views = rep(0,L), score = rep(0,L))

i=1

for (t in 0:length(questions_per_topic)-1){
  for (r in 1:length(questions_per_topic[[as.character(t)]])){
    topicId = t
    questionId = as.character(questions_per_topic[[as.character(t)]][r])
    score = metrics[metrics$Id==questionId,]$Score
    if (length(metrics[metrics$Id==questionId,][[1]])>0){
      views = metrics[metrics$Id==questionId,]$ViewCount
      output_views$topicID[i] = topicId
      output_views$questionID[i] = questionId
      output_views$views[i] = views
      output_views$score[i] = score
      i = i+1
    }
  }
}

output_views_without_zeros = output_views[as.numeric(output_views$questionID)>0,]
write.csv(output_views_without_zeros, "views.csv")


######################## Views Counts ########################
median_counts <- c()
min_counts <- c()
max_counts <- c()

median_counts <- append(median_counts, median(metrics[metrics$Id %in% questions_per_topic[["0"]],]$ViewCount))
min_counts <- append(min_counts, min(metrics[metrics$Id %in% questions_per_topic[["0"]],]$ViewCount))
max_counts <- append(max_counts, max(metrics[metrics$Id %in% questions_per_topic[["0"]],]$ViewCount))

Views <- read.csv("views_clusters.csv")
for (r in 1:length(Views[[1]])){
  Views$topic[r] <- topic_id_to_name[[as.character(Views$topicID[r])]]
}
Views <- Views[Views$topic != "Unrelated",]

fig <- plot_ly(data = Views, y = ~views, x=~topic, type = "box")
fig <- fig %>% layout(yaxis = list(type = 'log', dtick=1, exponentformat='e', title = "Views count"),
                      xaxis = list(dtick=1, title= "", tickangle = -90),
                      font=list(size=17),
                      autosize=F,
                      width=700,
                      height=600)
fig

# questions about configuration
config_questions <- Views[grepl("configuration", Views$topic, ignore.case = TRUE), ]
length(unique(config_questions$questionID))


wilcoxon_values <- matrix(0, 25, 25)

for (t in 0:24){
  for (tt in 0:24){
    wilcoxon_values[t+1,tt+1] <- wilcox.test(metrics[metrics$Id %in% questions_per_topic[[as.character(t)]],]$ViewCount,metrics[metrics$Id %in% questions_per_topic[[as.character(tt)]],]$ViewCount)$p.value
  }
}

for (t in 0:24){
  data <- metrics[metrics$Id %in% questions_per_topic[[as.character(t)]],]$ViewCount
  median_counts <- append(median_counts, median(na.omit(data)))
  min_counts <- append(min_counts, min(na.omit(data)))
  max_counts <- append(max_counts, max(na.omit(data)))
}

median(median_counts)
min(median_counts)
max(median_counts)
print('==========')
median(min_counts)
min(min_counts)
max(min_counts)
print('==========')
median(max_counts)
min(max_counts)
max(max_counts)


######################## Answers Counts ########################
percentage_at_least_one <- c()
median_counts <- c()
min_counts <- c()
max_counts <- c()

median_counts <- append(median_counts, median(metrics[metrics$Id %in% questions_per_topic[["0"]],]$AnswerCount))
min_counts <- append(min_counts, min(metrics[metrics$Id %in% questions_per_topic[["0"]],]$AnswerCount))
max_counts <- append(max_counts, max(metrics[metrics$Id %in% questions_per_topic[["0"]],]$AnswerCount))

for (t in 1:25){
  data <- metrics[metrics$Id %in% questions_per_topic[[as.character(t)]],]$AnswerCount
  median_counts <- append(median_counts, median(na.omit(data)))
  min_counts <- append(min_counts, min(na.omit(data)))
  max_counts <- append(max_counts, max(na.omit(data)))

  subdata <- metrics[metrics$Id %in% questions_per_topic[[as.character(t)]],]

  percentage_at_least_one <- append(percentage_at_least_one, length(subdata[subdata$AnswerCount>0,][[1]])/length(subdata[[1]]))
}

median(median_counts)
min(median_counts)
max(median_counts)
print('==========')
median(min_counts)
min(min_counts)
max(min_counts)
print('==========')
median(max_counts)
min(max_counts)
max(max_counts)
print('==========')
median(percentage_at_least_one)
min(percentage_at_least_one)
max(percentage_at_least_one)

wilcoxon_values <- matrix(0, 25, 25)

for (t in 0:24){
  for (tt in 0:24){
    wilcoxon_values[t+1,tt+1] <- wilcox.test(metrics[metrics$Id %in% questions_per_topic[[as.character(t)]],]$AnswerCount,metrics[metrics$Id %in% questions_per_topic[[as.character(tt)]],]$AnswerCount)$p.value
  }
}
wilcoxon_values

######################## Comments Counts ########################
median_counts <- c()
min_counts <- c()
max_counts <- c()

median_counts <- append(median_counts, median(metrics[metrics$Id %in% questions_per_topic[["0"]],]$CommentCount))
min_counts <- append(min_counts, min(metrics[metrics$Id %in% questions_per_topic[["0"]],]$CommentCount))
max_counts <- append(max_counts, max(metrics[metrics$Id %in% questions_per_topic[["0"]],]$CommentCount))

fig <- plot_ly(y = ~metrics[metrics$Id %in% questions_per_topic[["0"]],]$CommentCount, type = "box",name="0")

for (t in 1:25){
  data <- metrics[metrics$Id %in% questions_per_topic[[as.character(t)]],]$CommentCount
  fig <- add_trace(fig, y=unlist(data), name = as.character(t), evaluate = TRUE)
  median_counts <- append(median_counts, median(na.omit(data)))
  min_counts <- append(min_counts, min(na.omit(data)))
  max_counts <- append(max_counts, max(na.omit(data)))
}

fig <- fig %>% layout(yaxis = list(type = 'log', dtick=1, exponentformat='e', title = "Comments count"),
                      xaxis = list(dtick=1),
                      font=list(size=20),
                      autosize=F,
                      width=1200,
                      height=400)
fig

median(median_counts)
min(median_counts)
max(median_counts)
print('==========')
median(min_counts)
min(min_counts)
max(min_counts)
print('==========')
median(max_counts)
min(max_counts)
max(max_counts)

wilcoxon_values <- matrix(0, 31, 31)

for (t in 0:25){
  for (tt in 0:25){
    wilcoxon_values[t+1,tt+1] <- wilcox.test(metrics[metrics$Id %in% questions_per_topic[[as.character(t)]],]$CommentCount,metrics[metrics$Id %in% questions_per_topic[[as.character(tt)]],]$CommentCount)$p.value
  }
}

######################## Part II ########################

updown_path <- paste(getwd(), "/Query3Results.csv", sep = "")
updown_votes <- read.csv(updown_path, header=TRUE, sep=",")
updown <- merge(metrics, updown_votes, by = c('Id'))

######################## Upvotes ########################
median_counts <- c()
min_counts <- c()
max_counts <- c()

median_counts <- append(median_counts, median(updown[updown$Id %in% questions_per_topic[["0"]],]$upvotes))
min_counts <- append(min_counts, min(updown[updown$Id %in% questions_per_topic[["0"]],]$upvotes))
max_counts <- append(max_counts, max(updown[updown$Id %in% questions_per_topic[["0"]],]$upvotes))

fig <- plot_ly(y = ~updown[updown$Id %in% questions_per_topic[["0"]],]$upvotes, type = "box",name="0")

for (t in 1:25){
  data <- updown[updown$Id %in% questions_per_topic[[as.character(t)]],]$upvotes
  fig <- add_trace(fig, y=unlist(data), name = as.character(t), evaluate = TRUE)
  median_counts <- append(median_counts, median(na.omit(data)))
  min_counts <- append(min_counts, min(na.omit(data)))
  max_counts <- append(max_counts, max(na.omit(data)))
}

fig <- fig %>% layout(yaxis = list(type = 'log', dtick=1, exponentformat='e', title = "Upvotes count"),
                      xaxis = list(dtick=1),
                      font=list(size=20),
                      autosize=F,
                      width=1200,
                      height=400)
fig

median(median_counts)
min(median_counts)
max(median_counts)
print('==========')
median(min_counts)
min(min_counts)
max(min_counts)
print('==========')
median(max_counts)
min(max_counts)
max(max_counts)

wilcoxon_values <- matrix(0, 31, 31)

for (t in 0:25){
  for (tt in 0:25){
    wilcoxon_values[t+1,tt+1] <- wilcox.test(updown[updown$Id %in% questions_per_topic[[as.character(t)]],]$upvotes,updown[updown$Id %in% questions_per_topic[[as.character(tt)]],]$upvotes)$p.value
  }
}

######################## Downvotes ########################
median_counts <- c()
min_counts <- c()
max_counts <- c()

median_counts <- append(median_counts, median(updown[updown$Id %in% questions_per_topic[["0"]],]$downvotes))
min_counts <- append(min_counts, min(updown[updown$Id %in% questions_per_topic[["0"]],]$downvotes))
max_counts <- append(max_counts, max(updown[updown$Id %in% questions_per_topic[["0"]],]$downvotes))

fig <- plot_ly(y = ~updown[updown$Id %in% questions_per_topic[["0"]],]$downvotes, type = "box",name="0")

for (t in 1:25){
  data <- updown[updown$Id %in% questions_per_topic[[as.character(t)]],]$downvotes
  fig <- add_trace(fig, y=unlist(data), name = as.character(t), evaluate = TRUE)
  median_counts <- append(median_counts, median(na.omit(data)))
  min_counts <- append(min_counts, min(na.omit(data)))
  max_counts <- append(max_counts, max(na.omit(data)))
}

fig <- fig %>% layout(yaxis = list(type = 'log', dtick=1, exponentformat='e', title = "Downvotes count"),
                      xaxis = list(dtick=1),
                      font=list(size=20),
                      autosize=F,
                      width=1200,
                      height=400)
fig

median(median_counts)
min(median_counts)
max(median_counts)
print('==========')
median(min_counts)
min(min_counts)
max(min_counts)
print('==========')
median(max_counts)
min(max_counts)
max(max_counts)

wilcoxon_values <- matrix(0, 31, 31)

for (t in 0:25){
  for (tt in 0:25){
    wilcoxon_values[t+1,tt+1] <- wilcox.test(updown[updown$Id %in% questions_per_topic[[as.character(t)]],]$downvotes,updown[updown$Id %in% questions_per_topic[[as.character(tt)]],]$downvotes)$p.value
  }
}

######################## Score ########################
percentages_positive_score <- c()
median_counts <- c()
min_counts <- c()
max_counts <- c()

median_counts <- append(median_counts, median(updown[updown$Id %in% questions_per_topic[["0"]],]$Score))
min_counts <- append(min_counts, min(updown[updown$Id %in% questions_per_topic[["0"]],]$Score))
max_counts <- append(max_counts, max(updown[updown$Id %in% questions_per_topic[["0"]],]$Score))


Scores <- read.csv("views_clusters.csv")
for (r in 1:length(Scores[[1]])){
  Scores$topic[r] <- topic_id_to_name[[as.character(Scores$topicID[r])]]
}
Scores <- Scores[Scores$topic != "Unrelated",]

fig <- plot_ly(data = Scores, y = ~score+1, x=~topic, type = "box")
fig <- fig %>% layout(yaxis = list(type = 'log', dtick=1, exponentformat='e', title = "Scores"),
                      xaxis = list(dtick=1, title= "", tickangle = -90),
                      font=list(size=17),
                      autosize=F,
                      width=700,
                      height=600)
fig

for (t in 1:25){
  data <- updown[updown$Id %in% questions_per_topic[[as.character(t)]],]$Score
  median_counts <- append(median_counts, median(na.omit(data)))
  min_counts <- append(min_counts, min(na.omit(data)))
  max_counts <- append(max_counts, max(na.omit(data)))
  subdata<- updown[updown$Id %in% questions_per_topic[[as.character(t)]],]

  percentages_positive_score <- append(percentages_positive_score, length(subdata[subdata$Score>=0,][[1]])/length(subdata[[1]]))

}


median(median_counts)
min(median_counts)
max(median_counts)
print('==========')
median(min_counts)
min(min_counts)
max(min_counts)
print('==========')
median(max_counts)
min(max_counts)
max(max_counts)
print('==========')
median(percentages_positive_score)
min(percentages_positive_score)
max(percentages_positive_score)


wilcoxon_values <- matrix(0, 31, 31)

for (t in 0:25){
  for (tt in 0:25){
    wilcoxon_values[t+1,tt+1] <- wilcox.test(updown[updown$Id %in% questions_per_topic[[as.character(t)]],]$Score,updown[updown$Id %in% questions_per_topic[[as.character(tt)]],]$Score)$p.value
  }
}

######################## Time for accepted answer ########################

answers_path <- paste(getwd(), "/Query4Results.csv", sep = "")
answers <- read.csv(answers_path, header=TRUE, sep=",")
for (r in 1:length(answers[[1]])){
  creationDate <- as.POSIXct(answers$q_creationDate[r], format="%Y-%m-%d %H:%M:%S", tz="UTC")
  acceptedAnswerDate <- as.POSIXct(answers$aa_creationDate[r], format="%Y-%m-%d %H:%M:%S", tz="UTC")
  TimeForAcceptedAnswer <- as.numeric(difftime(acceptedAnswerDate,creationDate,units = "hour"))
  answers$timeForAcceptedAnswer[r] <- TimeForAcceptedAnswer
}

i=1

for (t in 0:length(questions_per_topic)-1){
  for (r in 1:length(questions_per_topic[[as.character(t)]])){
    topicId = t
    questionId = as.character(questions_per_topic[[as.character(t)]][r])
    aa_score = answers[answers$q_id==questionId,]$aa_score
    if (length(answers[answers$q_id==questionId,][[1]])>0){
      aaTime = answers[answers$q_id==questionId,]$timeForAcceptedAnswer
      output_aaTime$topicID[i] = topicId
      output_aaTime$questionID[i] = questionId
      output_aaTime$aaTime[i] = aaTime
      output_aaTime$aaScore[i] = aa_score
      i = i+1
    }
  }
}

output_aaTime_without_zeros = output_aaTime[as.numeric(output_aaTime$questionID)>0,]
write.csv(output_aaTime_without_zeros, "acceptedAnswerTime.csv")

acceptedAnswerTime <- read.csv("aaTime_clusters.csv", header=TRUE, sep=",")
for (r in 1:length(acceptedAnswerTime[[1]])){
  acceptedAnswerTime$topic[r] <- topic_id_to_name[[as.character(acceptedAnswerTime$topicID[r])]]
}
acceptedAnswerTime <- acceptedAnswerTime[acceptedAnswerTime$topic != "Unclear",]

fig <- plot_ly(data = acceptedAnswerTime, y = ~aaTime, x=~topic, type = "box")
fig <- fig %>% layout(yaxis = list(type = 'log', dtick=1, exponentformat='e', title = "Time for the accepted answer"),
                      xaxis = list(dtick=1, title = "", tickangle = -90),
                      font=list(size=17),
                      autosize=F,
                      width=700,
                      height=600)
fig



percentages_before_one_day <- c()

median_counts <- c()
min_counts <- c()
max_counts <- c()

median_counts <- append(median_counts, median(answers[answers$q_id %in% questions_per_topic[["0"]],]$timeForAcceptedAnswer))
min_counts <- append(min_counts, min(answers[answers$q_id %in% questions_per_topic[["0"]],]$timeForAcceptedAnswer))
max_counts <- append(max_counts, max(answers[answers$q_id %in% questions_per_topic[["0"]],]$timeForAcceptedAnswer))

# fig <- plot_ly(y = ~answers[answers$q_id %in% questions_per_topic[["0"]],]$timeForAcceptedAnswer, type = "box",name="0")

for (t in 0:24){
  data <- answers[answers$q_id %in% questions_per_topic[[as.character(t)]],]$timeForAcceptedAnswer
  # fig <- add_trace(fig, y=unlist(data), name = as.character(t), evaluate = TRUE)
  median_counts <- append(median_counts, median(na.omit(data)))
  min_counts <- append(min_counts, min(na.omit(data)))
  max_counts <- append(max_counts, max(na.omit(data)))

  subdata<- answers[answers$q_id %in% questions_per_topic[[as.character(t)]],]

  percentages_before_one_day <- append(percentages_before_one_day, length(subdata[subdata$timeForAcceptedAnswer <= 24,][[1]])/length(subdata[[1]]))
}

# fig <- fig %>% layout(yaxis = list(type = 'log', dtick=1, exponentformat='e', title = "Time for the accepted answer (hours)"),
#                       xaxis = list(dtick=1),
#                       font=list(size=20),
#                       autosize=F,
#                       width=1200,
#                       height=500)
# fig

median(median_counts)
min(median_counts)
max(median_counts)
print('==========')
median(min_counts)
min(min_counts)
max(min_counts)
print('==========')
median(max_counts)
min(max_counts)
max(max_counts)
print('==========')
median(percentages_before_one_day)
min(percentages_before_one_day)
max(percentages_before_one_day)



wilcoxon_values <- matrix(0, 25, 25)

for (t in 0:24){
  for (tt in 0:24){
    wilcoxon_values[t+1,tt+1] <- wilcox.test(answers[answers$q_id %in% questions_per_topic[[as.character(t)]],]$timeForAcceptedAnswer,answers[answers$q_id %in% questions_per_topic[[as.character(tt)]],]$timeForAcceptedAnswer)$p.value
  }
}



######################## Accepted answers Scores ########################
percentages_positive_score <- c()
median_counts <- c()
min_counts <- c()
max_counts <- c()

median_counts <- append(median_counts, median(answers[answers$q_id %in% questions_per_topic[["0"]],]$aa_score))
min_counts <- append(min_counts, min(answers[answers$q_id %in% questions_per_topic[["0"]],]$aa_score))
max_counts <- append(max_counts, max(answers[answers$q_id %in% questions_per_topic[["0"]],]$aa_score))

aanswers <- read.csv("acceptedAnswerTime.csv", header=TRUE, sep=",")
for (r in 1:length(aanswers[[1]])){
  aanswers$topic[r] <- topic_id_to_name[[as.character(aanswers$topicID[r])]]
}
aanswers <- aanswers[aanswers$topic != "Unclear",]

fig <- plot_ly(data = aanswers, y = ~aaScore, x=~topic, type = "box")
fig <- fig %>% layout(yaxis = list(type = 'log', dtick=1, exponentformat='e', title = "Score of the accepted answer"),
                      xaxis = list(dtick=1, title= "", tickangle = -90),
                      font=list(size=17),
                      autosize=F,
                      width=700,
                      height=600)
fig

config_questions_aa <- aanswers[grepl("configuration", aanswers$topic, ignore.case = TRUE), ]
length(unique(config_questions_aa$questionID))
mean(config_questions_aa$aaTime)

for (t in 0:24){
  data <- answers[answers$q_id %in% questions_per_topic[[as.character(t)]],]$aa_score
  # fig <- add_trace(fig, y=unlist(data), name = as.character(t), evaluate = TRUE)
  median_counts <- append(median_counts, median(na.omit(data)))
  min_counts <- append(min_counts, min(na.omit(data)))
  max_counts <- append(max_counts, max(na.omit(data)))
  subdata<- answers[answers$q_id %in% questions_per_topic[[as.character(t)]],]

  percentages_positive_score <- append(percentages_positive_score, length(subdata[subdata$aa_score>0,][[1]])/length(subdata[[1]]))
}

# fig <- fig %>% layout(yaxis = list(type = 'log', dtick=1, exponentformat='e', title = "Score of the accepted answer"),
#                       xaxis = list(dtick=1),
#                       font=list(size=20),
#                       autosize=F,
#                       width=1200,
#                       height=400)
# fig

median(median_counts)
min(median_counts)
max(median_counts)
print('==========')
median(min_counts)
min(min_counts)
max(min_counts)
print('==========')
median(max_counts)
min(max_counts)
max(max_counts)
print('==========')
median(percentages_positive_score)
min(percentages_positive_score)
max(percentages_positive_score)


wilcoxon_values <- matrix(0, 31, 31)

for (t in 0:25){
  for (tt in 0:25){
    wilcoxon_values[t+1,tt+1] <- wilcox.test(answers[answers$q_id %in% questions_per_topic[[as.character(t)]],]$aa_score,answers[answers$q_id %in% questions_per_topic[[as.character(tt)]],]$aa_score)$p.value
  }
}

######################## Self answered questions ########################
answers_path <- paste(getwd(), "/Query4Results.csv", sep = "")
answers <- read.csv(answers_path, header=TRUE, sep=",")
for (r in 1:length(answers[[1]])){
  if(!is.na(answers$q_oid[r]) & !is.na(answers$aa_oid[r])){
      if(answers$q_oid[r]==answers$aa_oid[r]){
        answers$self_answered[r]=1
      } else {
        answers$self_answered[r]=0
      }
  } else if (!is.na(answers$q_author[r]) & !is.na(answers$aa_author[r])){
      if(answers$q_author[r]==answers$aa_author[r]){
          answers$self_answered[r]=1
      } else {
          answers$self_answered[r]=0
      }
  } else {
    #print('NOT ANSWERED')
    answers$self_answered[r]=-1
  }
}

Tags <- seq(0,25)
Self_Answered <- c(rep(0,26))
Not_Self_Answered <- c(rep(0,26))
data <- data.frame(Tags, Self_Answered, Not_Self_Answered)

for (t in 0:25){
  data$Self_Answered[t+1] <- length(answers[answers$q_id %in% questions_per_topic[[as.character(t)]] & answers$self_answered==1,][[1]])/length(answers[answers$q_id %in% questions_per_topic[[as.character(t)]],][[1]])*100
  data$Not_Self_Answered[t+1] <- 100 - data$Self_Answered[t+1]

}

fig <- plot_ly(data, x = ~Tags, y = ~Self_Answered, type = 'bar', name = 'S.A')
fig <- fig %>% add_trace(y = ~Not_Self_Answered, name = 'Not S.A')
fig <- fig %>% layout(yaxis = list(ticksuffix = "%", title = '% of S.A questions'), barmode = 'stack',
                      xaxis = list(dtick=1, title = 'Topics'),
                      font=list(size=18),
                      autosize=F,
                      width=1200,
                      height=400)
fig

