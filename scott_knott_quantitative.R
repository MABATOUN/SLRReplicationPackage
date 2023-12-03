install.packages("ScottKnott")
library(ScottKnott)

views<-read.csv("views.csv")
time<-read.csv("acceptedAnswerTime.csv")

views<-views[which(!is.na(views$views)),]
views$topicID<-as.factor(views$topicID)
sk1<-SK(views~topicID, data=views, which='topicID')
results<-sk1$out$Result
results$topic<-rownames(results)
results$group<-paste0(results$G1, results$G2, results$G3)
results$G1<-NULL
results$G2<-NULL
results$G3<-NULL
results$Means<-NULL

views_clustered<-merge(views, results, by.x="topicID", by.y="topic")
write.csv(views_clustered, "views_clusters.csv")




time$topicID<-as.factor(time$topicID)
sk1<-SK(aaTime~topicID, data=time, which='topicID')
results<-sk1$out$Result
results$topic<-rownames(results)
results$group<-paste0(results$G1, results$G2)
results$G1<-NULL
results$G2<-NULL
results$Means<-NULL

time_clustered<-merge(time, results, by.x="topicID", by.y="topic")
write.csv(time_clustered, "aaTime_clusters.csv")
