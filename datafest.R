### Setup
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(ggfortify)
library(Hmisc)
library(class)
library(plotly)

setwd("/Users/shinsukeadachi/Desktop/Datafest/data files/")
df <- read_csv("logs.csv",guess_max=2106600)

player1 <- read_csv("player-6427031.csv", guess_max = 7332)
player2 <- read_csv("player-6486029.csv")
survey <- read_csv("S5_scores_cleaned.csv")

df_subs  <- subset(df, select =c(player_id,event_id,event_description,event_time, event_time_dbl,stack_id) )
newdf <- df_subs %>% 
  mutate (timedif = event_time_dbl - lag(event_time_dbl))


### How much time each player spends on minigames
minigame_time <- newdf %>%
  filter((event_id >= 400 & event_id <=420) |
         (event_id >= 500 & event_id <=515) |
         (event_id >= 800 & event_id <=818) |
         (event_id >= 900) & (event_id <=91)) %>%
  group_by(player_id) %>%
  summarise(total_time = sum(timedif)/3600) 
  
knowledge_time <- newdf %>%
  filter((event_id >= 400 & event_id <=420)) %>%
  group_by(player_id) %>%
  summarise(total_time = sum(timedif)/3600) 

refuse_time <- newdf %>%
  filter((event_id >= 500 & event_id <=515)) %>%
  group_by(player_id) %>%
  summarise(total_time = sum(timedif)/3600) 

priority_time <- newdf %>%
  filter((event_id >= 800 & event_id <=818)) %>%
  group_by(player_id) %>%
  summarise(total_time = sum(timedif)/3600) 

people_time <- newdf %>%
  filter((event_id >= 900 & event_id <=912)) %>%
  group_by(player_id) %>%
  summarise(total_time = sum(timedif)/3600) 

minigame_time <- merge(x=knowledge_time,y=refuse_time,by="player_id",all=TRUE)
minigame_time <- merge(x=minigame_time, y=priority_time,by="player_id",all=TRUE)
minigame_time <- merge(x=minigame_time, y=people_time,by="player_id",all=TRUE)
colnames(minigame_time) <- c('player_id','knowledge_time', 'refuse_time','priority_time','people_time')

minigame_time[is.na(minigame_time)] = 0

minigame_time$total_time <- minigame_time$knowledge_time + 
  minigame_time$refuse_time + minigame_time$priority_time + minigame_time$people_time

minigame_time %>%
  ggplot(aes(total_time)) + geom_histogram(bins=10)

p1 <- minigame_time %>%
  ggplot(aes(knowledge_time)) + geom_histogram(bins=10)
p2 <- minigame_time %>%
  ggplot(aes(refuse_time)) + geom_histogram(bins=10)
p3 <- minigame_time %>%
  ggplot(aes(priority_time)) + geom_histogram(bins=10)
p4 <- minigame_time %>%
  ggplot(aes(people_time)) + geom_histogram(bins=10)

grid.arrange(p1, p2,p3,p4, nrow=2)


### Survey data 
survey_df <- survey %>%
  group_by(player_id) %>%
  summarise(max = max(S5_mean), min = min(S5_mean), mean = mean(S5_mean))

### Join survey data and minigame time data
time_survey <- merge(minigame_time_2, survey_df, by = 'player_id', all = FALSE)

### Run regression max/min survey score on total time 
lm_max <- lm(max ~ total_time, data=time_survey)
summary(lm_max)
time_survey %>%
  ggplot(aes(x=total_time, y=max)) + geom_smooth(method = 'lm')

lm_min <- lm(min ~ total_time, data=time_survey)
summary(lm_min)
time_survey %>%
  ggplot(aes(x=total_time, y=min)) + geom_smooth(method = 'lm')

lm_avg <- lm(mean ~ total_time, data=time_survey)
summary(lm_avg)
time_survey %>%
  ggplot(aes(x=total_time, y=mean)) + geom_smooth(method = 'lm')


### Join Latifa's data. Cleaning
minigame_time_merged <- log_merge
minigame_time_merged$total_fail <- minigame_time_merged$n3starsPeople + minigame_time_merged$nFailRefuse

minigame_time_merged2 <- na.omit(minigame_time_merged)

### Select players that finish at least 9 chapters
finished <- df_subs %>%
  group_by(player_id) %>%
  filter(!is.na(stack_id)) %>%
  summarise(stack_number = length(unique(stack_id))) %>%
  filter(stack_number > 10) 

minigame_time_merged_finished <- minigame_time_merged2 %>%
  filter(player_id %in% finished$player_id)

### K-means Clustering
kmean_model <- stats::kmeans(minigame_time_merged_finished[c(5:8,10)], 2)
autoplot(kmean_model, minigame_time_merged_finished[c(5:8,10)], frame = TRUE)

kmean_model <- stats::kmeans(minigame_time_merged_finished[c(5:8,10)], 3)
autoplot(kmean_model, minigame_time_merged_finished[c(5:8,10)], frame = TRUE)

library(cluster)
x <- minigame_time_merged_finished[c(9:10)]
withinss <- 0

for (i in 1:10){
  model=kmeans(x,i)
  autoplot(model, x, frame = TRUE)
  withinss[i] <- model$tot.withinss 
}

plot(1:10, withinss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares",
     main = "Scree Plot")

model <- list()
model[[1]]=kmeans(x,2)
clusplot(x,model[[1]]$cluster)
clusplot(x,model[[1]]$cluster,color=T,shade=T)
autoplot(model[[1]], x, frame = TRUE)

model[[2]]=kmeans(x,3)
clusplot(x,model[[2]]$cluster)
clusplot(x,model[[2]]$cluster,color=T,shade=T)
autoplot(model[[2]], x, frame = TRUE)

x2 <- minigame_time_merged_finished %>%
  mutate(clust2cl = as.factor(model[[1]]$cluster), clust3cl = as.factor(model[[2]]$cluster))

### EDA of Clustering

## k=2
x2 %>%
  group_by(clust2cl) %>%
  summarise(n = n(), 
            total_time = mean(total_time),
            total_fail = mean(total_fail),
            knowledge = mean(knowledge_time),
            refuse = mean(refuse_time),
            priority = mean(priority_time),
            people = mean(people_time)) 

boxplot(x2$knowledge_time ~  as.factor(x2$clust2cl), main = 'knowledge')
boxplot(x2$refuse_time ~  as.factor(x2$clust2cl), main = 'refuse')
boxplot(x2$priority_time ~  as.factor(x2$clust2cl), main = 'priority')
boxplot(x2$people_time ~  as.factor(x2$clust2cl), main = 'people')
boxplot(x2$total_time ~  as.factor(x2$clust2cl), main = 'total time')
boxplot(x2$total_fail ~  as.factor(x2$clust2cl), main = 'total failures')

# k=3

k3_summary <- x2 %>%
  group_by(clust3cl) %>%
  summarise(n = n(), 
            total_time = mean(total_time),
            total_fail = mean(total_fail),
            knowledge = mean(knowledge_time),
            refuse = mean(refuse_time),
            priority = mean(priority_time),
            people = mean(people_time)) %>%
  as.data.frame()

boxplot(x2$knowledge_time ~  as.factor(x2$clust3cl), main = 'knowledge')
boxplot(x2$refuse_time ~  as.factor(x2$clust3cl), main = 'refuse')
boxplot(x2$priority_time ~  as.factor(x2$clust3cl), main = 'priority')
boxplot(x2$people_time ~  as.factor(x2$clust3cl), main = 'people')
boxplot(x2$total_time ~  as.factor(x2$clust3cl), main = 'total time')
boxplot(x2$total_fail ~  as.factor(x2$clust3cl), main = 'total failures')


### Plots to include in the slide
k3_summary %>%
  ggplot(aes(x=clust3cl, y=n, fill = clust3cl)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  ggtitle('Number of Players')
p1 <- k3_summary %>%
  ggplot(aes(x=clust3cl, y=total_time,fill = clust3cl)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  ggtitle('Total Time')
p2 <- k3_summary %>%
  ggplot(aes(x=clust3cl, y=total_fail,fill = clust3cl)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  ggtitle('Number of Failures')

p3 <- x2 %>%
  ggplot(aes(x=clust3cl,y=total_time, fill=clust3cl))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle('Total Time') + theme_minimal() +
  labs(y="Total Time", x="Cluster Group")

p4 <- x2 %>%
  ggplot(aes(x=clust3cl,y=total_fail, fill=clust3cl))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle('Number of Failures') + theme_minimal() +labs(y="Number of Failures", x="Cluster Group")

grid.arrange(p1,p2,nrow=1)
grid.arrange(p3,p4,nrow=1)

x2 %>%
  ggplot(aes(x=total_time,y=total_fail, color=clust3cl)) + 
  geom_point(size = 5) + 
  theme_minimal() +
  labs(title="Total Time vs Number of Failures", y="Number of Failures", x="Total Time")

### interactive graph
p <- x2 %>%
  ggplot(aes(total_time, total_fail,color=clust3cl)) +
  geom_point() +
  theme_minimal()

ggplotly(p)

## Visualization
dat1 <- x2 %>%
  group_by(clust3cl) %>%
  summarise(total_time = mean(total_time), knowledge_time = mean(knowledge_time), refuse_time = mean(refuse_time), priority_time = mean(priority_time),people_time = mean(people_time))  
  
  
small_df <- dat1
 
small_df$number_player <- c('1','2','3')
small_df 

df5<- small_df %>% 
  select(-total_time) %>%
  pivot_longer(ends_with("time"), names_to = "minigames", values_to = "time") 

df5%>% 
  ggplot(aes(x = number_player, y = time))+
  geom_col(position ='dodge', aes(fill=minigames), palette = 'saturation')+
  labs(title="Time vs. Minigame", y="Time", x="Player")+
  scale_fill_discrete(name = 'Minigames', labels = c('Knowledge game','Refuse game', 'Priority game', 'People game'))+
  theme_minimal()+
  coord_flip()  
  
dat2 <- x2 %>%
  group_by(clust3cl) %>%
  summarise(failure = mean(total_fail))  

