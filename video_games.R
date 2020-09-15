####Which is better: paid game or free game?

attach(steam)
as.numeric(average_playtime)
as.numeric(median_playtime)
as.numeric(price)
as.numeric(positive_ratings)
as.numeric(negative_ratings)
as.numeric(achievements)
release_date = as.Date(steam$release_date, "%Y-%m-%d")

##########################################clean data
clean_steam<-steam[(steam$average_playtime!=0),]
clean_steam$categories_dummy<-ifelse(grepl("Multi-player",clean_steam$categories)|grepl("Online Multi-Player",clean_steam$categories),1,0)
clean_steam$genre_action<-ifelse(grepl("Action",clean_steam$genres),1,0)
clean_steam$genre_adven<-ifelse(grepl("Adventure",clean_steam$genres),1,0)
clean_steam$genre_casual<-ifelse(grepl("Casual",clean_steam$genres),1,0)
clean_steam$genre_indie<-ifelse(grepl("Indie",clean_steam$genres),1,0)
clean_steam$genre_mm<-ifelse(grepl("Massively Multiplayer",clean_steam$genres),1,0)
clean_steam$genre_rpg<-ifelse(grepl("RPG",clean_steam$genres),1,0)
clean_steam$genre_racing<-ifelse(grepl("Racing",clean_steam$genres),1,0)
clean_steam$genre_sim<-ifelse(grepl("Simulation",clean_steam$genres),1,0)
clean_steam$genre_sports<-ifelse(grepl("Sports",clean_steam$genres),1,0)
clean_steam$genre_stra<-ifelse(grepl("Strategy",clean_steam$genres),1,0)
clean_steam$release_date = as.Date(clean_steam$release_date, "%Y-%m-%d")
clean_steam$release_year<-as.numeric(format(clean_steam$release_date, "%Y"))
clean_steam$free<-as.factor(ifelse(clean_steam$price==0,1,0))
clean_steam$Category<-ifelse(clean_steam$price==0,"Free","Paid")


hist(clean_steam$owners_size)

summary(clean_steam$average_playtime)
as.numeric(clean_steam$average_playtime)
as.numeric(clean_steam$median_playtime)
as.numeric(clean_steam$price)
as.numeric(clean_steam$positive_ratings)
as.numeric(clean_steam$negative_ratings)
as.numeric(clean_steam$achievements)
clean_steam$owners_size=as.clean_steam$owners
clean_steam$owners_size<-ifelse(clean_steam$owners_size=='0-20000',1,
                                (ifelse(clean_steam$owners_size == "20000-50000",2,
                                        (ifelse(clean_steam$owners_size == "50000-100000",3,
                                                ifelse(clean_steam$owners_size == "100000-200000",4,
                                                       ifelse(clean_steam$owners_size == "200000-500000",5,
                                                              ifelse(clean_steam$owners_size == "500000-1000000",6,
                                                                     ifelse(clean_steam$owners_size == "1000000-2000000",7,
                                                                            ifelse(clean_steam$owners_size == "2000000-5000000",8,
                                                                                   ifelse(clean_steam$owners_size == "5000000-10000000",9,
                                                                                          ifelse(clean_steam$owners_size == "10000000-20000000",10,
                                                                                                 ifelse(clean_steam$owners_size == "20000000-50000000",11,
                                                                                                        ifelse(clean_steam$owners_size == "50000000-100000000",12,
                                                                                                               ifelse(clean_steam$owners_size == "100000000-200000000",13,0)))))))))))))))

###Calculate rating score based on SteamDB method.

total_reviews=clean_steam$positive_ratings+clean_steam$negative_ratings
average=clean_steam$positive_ratings/total_reviews
clean_steam$score=(average-(average*0.5)*2**(-log10(total_reviews+1)))*100


#free?
table(clean_steam$free) #paid:5348 free:822

free_games<-clean_steam[(clean_steam$price==0),]
summary(free_games$score)

paid_games<-clean_steam[(clean_steam$price!=0),]
summary(paid_games$score)


########################################free games analysis
library(ggplot2)
library(ggfortify)
free_games$high_score<-ifelse(free_games$score>80.61,1,0)
free_vars=free_games[,c(12,15:16,19:30,33:34,36,37)]
free_labels=free_games[,c(2,35)]
pca_free=prcomp(free_vars, scale=TRUE)
pca_free
p_free<-autoplot(pca_free, data =free_vars, loadings = TRUE, col=ifelse(free_labels$high_score==1,'blue','grey'), loadings.label = TRUE)
p_free+ggtitle("PCA of Free Games Before Removing Any Collinear Variables")+theme(plot.title = element_text(hjust=0.5))

free_vars_remove_score=free_games[,c(12,15:16,19:30,34,36:37)]
free_labels_remove_score=free_games[,c(2,35)]
pca_free_remove_score=prcomp(free_vars_remove_score, scale=TRUE)
pca_free_remove_score

library(gbm)
set.seed (1)
boosted_free=gbm(score~achievements+average_playtime+categories_dummy+genre_action+genre_adven+genre_casual+
                   genre_indie+genre_mm+genre_rpg+genre_sim+genre_stra+release_year+owners_size,
            data=free_games,distribution='gaussian',n.trees=10000, interaction.depth=6)
summary(boosted_free)
predicted_free=predict(boosted_free, newdata=free_games, n.trees=10000)
mean((predicted_free-free_games$score)^2)

free_vars_model=free_games[,c(12,15,19:25,27:30,33:34)]
free_labels_model=free_games[,c(2,35)]
pca_free_model=prcomp(free_vars_model, scale=TRUE)
pca_free_model
p_free_model<-autoplot(pca_free_model, data =free_vars_model, loadings = TRUE, col=ifelse(free_labels_model$high_score==1,'blue','grey'), loadings.label = TRUE)
p_free_model+ggtitle("PCA of Free Games Model")+theme(plot.title = element_text(hjust=0.5))

library(tree)
library(rpart)
library(rpart.plot)

###regression tree
free_tree=rpart(free_games$score~free_games$average_playtime+free_games$achievements+free_games$owners_size+free_games$release_year,control=rpart.control(cp= 0.008))
rpart.plot(free_tree)
free_tree$cptable[which.min(free_tree$cptable[,"xerror"]),"CP"]
summary(free_tree)

#english
table(free_games$english)
hist(free_games$english)

#score
summary(free_games$score)
boxplot(free_games$score,col='red',main='Free Games: Score')
p_fscore<-ggplot(free_games,aes(x=score))
p_fscore+geom_histogram(binwidth = 1,fill="red")+xlab('Game Score')+ylab('Number of Games')+ggtitle("Game Score: Free Games")+theme(plot.title = element_text(hjust=0.5),legend.position="none")+scale_x_continuous(breaks = seq(0, 100, by = 10))

#age
table(free_games$required_age)
summary(free_games$required_age)
plot(free_games$required_age,free_games$score)
hist(free_games$required_age)

#owner_size
summary(free_games$owners_size)
plot(free_games$owners_size,free_games$score)
hist(free_games$owners_size)
p1<-ggplot(free_games,aes(x=owners_size))
p1+geom_histogram(binwidth = 0.5,fill="red")+xlab('Onwer size of Games')+ylab('Count')+ggtitle("Owner Size: Free Games")+theme(plot.title = element_text(hjust=0.5),legend.position="none")+scale_x_continuous(breaks = seq(0, 13, by = 1))

#achievements
summary(free_games$achievements)
plot(free_games$achievements,free_games$score)
hist(free_games$achievements)

#cate
table(free_games$categories_dummy)
hist(free_games$categories_dummy)

#average_palytime
summary(free_games$average_playtime)
plot(free_games$average_playtime,free_games$score)
hist(free_games$average_playtime)

#median_playtime
summary(free_games$median_playtime)
plot(free_games$median_playtime,free_games$score)
hist(free_games$median_playtime)

#year
summary(free_games$release_year)
plot(free_games$release_year,free_games$score)
hist(free_games$release_year)

library(tidyverse)
p2<-ggplot(free_games,aes(x=release_year))
p2+geom_histogram(binwidth = 0.5,fill="red")+xlab('Release Year of Games')+ylab('Number of Games')+ggtitle("Release Year: Free Games")+theme(plot.title = element_text(hjust=0.5),legend.position="none")+scale_x_continuous(breaks = seq(2001, 2019, by = 1))


names(sort(summary(as.factor(free_games$developer)), decreasing=T)[1:21])
#top developers:[1]NEKO WORKs  [2]Valve  [3]DONTNOD Entertainment  [4]Scott Cawthon 
free_games$top_developers<-ifelse(grepl("NEKO WORKs",free_games$developer)|grepl("Valve",free_games$developer)|grepl("DONTNOD Entertainment",free_games$developer)|
                                    grepl("Scott Cawthon",free_games$developer),1,0)

names(sort(summary(as.factor(free_games$publisher)), decreasing=T)[1:26])
#top publishers:[1]Valve  [2]SEGA  [3]Square Enix [4]Sekai Project  [5]AGM PLAYISM  [6]Devolver Digital
free_games$top_publishers<-ifelse(grepl("Valve",free_games$publisher)|grepl("SEGA",free_games$publisher)|grepl("Square Enix",free_games$publisher)|
                                    grepl("AGM PLAYISM",free_games$publisher)|grepl("Devolver Digital",free_games$publisher)|grepl("Sekai Project",free_games$publisher) ,1,0)


#action
table(free_games$genre_action)
hist(free_games$genre_action)

#adven
table(free_games$genre_adven)
hist(free_games$genre_adven)

#casual
table(free_games$genre_casual)
hist(free_games$genre_casual)

#indie
table(free_games$genre_indie)
hist(free_games$genre_indie)

#mm
table(free_games$genre_mm)
hist(free_games$genre_mm)

#sim
table(free_games$genre_sim)
hist(free_games$genre_sim)

#rpg
table(free_games$genre_rpg)
hist(free_games$genre_rpg)

#stra
table(free_games$genre_stra)
hist(free_games$genre_stra)

#racing
table(free_games$genre_racing)
hist(free_games$genre_racing)

#sport
table(free_games$genre_sports)
hist(free_games$genre_sports)


########################################paid games analysis
library(ggplot2)
library(ggfortify)
paid_games$high_score<-ifelse(paid_games$score>82.01,1,0)
paid_vars=paid_games[,c(12,15:16,18:30,33:34,36,37)]
paid_labels=paid_games[,c(2,35)]
pca_paid=prcomp(paid_vars, scale=TRUE)
p_paid<-autoplot(pca_paid, data =paid_vars, loadings = TRUE, col=ifelse(paid_labels$high_score==1,'blue','grey'), loadings.label = TRUE)
p_paid+ggtitle("PCA of Paid Games Before Removing Collinear Variables")+theme(plot.title = element_text(hjust=0.5))

library(gbm)
set.seed (1)
boosted_paid=gbm(score~achievements+average_playtime+price+categories_dummy+genre_action+genre_adven+genre_casual+genre_indie+
                   genre_rpg+genre_sim+genre_stra+release_year+owners_size,
                 data=paid_games,distribution='gaussian',n.trees=28000, interaction.depth=8)
summary(boosted_paid)
predicted_paid=predict(boosted_paid, newdata=paid_games, n.trees=28000)
mean((predicted_paid-paid_games$score)^2)

paid_vars_modele=paid_games[,c(12,15,18:23,25,27,29,30,33,34)]
paid_labels_model=paid_games[,c(2,35)]
pca_paid_model=prcomp(paid_vars_modele, scale=TRUE)
pca_paid_model
p_paid_model<-autoplot(pca_paid_model, data =paid_vars_modele, loadings = TRUE, col=ifelse(paid_labels_model$high_score==1,'blue','grey'), loadings.label = TRUE)
p_paid_model+ggtitle("PCA of Paid Games Model")+theme(plot.title = element_text(hjust=0.5))

library(tree)
library(rpart)
library(rpart.plot)

###regression tree
paid_tree=rpart(paid_games$score~paid_games$average_playtime+paid_games$price+paid_games$achievements+paid_games$release_year+paid_games$owners_size,control=rpart.control(cp= 0.006))
rpart.plot(paid_tree)
paid_tree$cptable[which.min(paid_tree$cptable[,"xerror"]),"CP"]
summary(paid_tree)

#socre
summary(paid_games$score)
hist(paid_games$score)

p<-ggplot(clean_steam, aes(x=Category,y=score,fill=Category))+geom_boxplot()+xlab('Game Category')+ylab('Game Score')+ggtitle("Game Score: Free vs. Paid")+theme(plot.title = element_text(hjust=0.5),legend.position="none")+coord_flip()
p

#english
table(paid_games$english)
hist(paid_games$english)

#age
table(paid_games$required_age)
summary(paid_games$required_age)
plot(paid_games$required_age,paid_games$score)
hist(paid_games$required_age)

#price
summary(paid_games$price)
plot(paid_games$price,paid_games$score)
hist(paid_games$price)
boxplot(paid_games$price)
p5<-ggplot(paid_games,aes(x=price))
p5+geom_histogram(binwidth = 0.5,fill="blue")+xlab('Price of Games (£)')+ylab('Number of Games')+ggtitle("Price: Paid Games")+theme(plot.title = element_text(hjust=0.5,size=18),legend.position="none",axis.text=element_text(size=15),axis.title=element_text(size=16))+scale_x_continuous(breaks = seq(0, 120, by = 5))

#owner_size
summary(paid_games$owners_size)
plot(paid_games$owners_size,paid_games$score)
hist(paid_games$owners_size)
p4<-ggplot(paid_games,aes(x=owners_size))
p4+geom_histogram(binwidth = 0.5,fill="blue")+xlab('Onwer size of Games')+ylab('Count')+ggtitle("Owner Size: Paid Games")+theme(plot.title = element_text(hjust=0.5),legend.position="none")+scale_x_continuous(breaks = seq(0, 13, by = 1))

#achievements
summary(paid_games$achievements)
plot(paid_games$achievements,paid_games$score)
boxplot(paid_games$achievements)
hist(paid_games$achievements)

#cate
table(paid_games$categories_dummy)
hist(paid_games$categories_dummy)

#palytime
summary(paid_games$average_playtime)
plot(paid_games$average_playtime,paid_games$score)
hist(paid_games$average_playtime)

#median_playtime
summary(paid_games$median_playtime)
plot(paid_games$median_playtime,paid_games$score)
hist(paid_games$median_playtime)

#year
summary(paid_games$release_year)
plot(paid_games$release_year,paid_games$score)
hist(paid_games$release_year)
boxplot(paid_games$release_year)
library(tidyverse)
p3<-ggplot(paid_games,aes(x=release_year))
p3+geom_histogram(binwidth = 0.5,fill="blue")+xlab('Release Year of Games')+ylab('Number of Games')+ggtitle("Release Year: Paid Games")+theme(plot.title = element_text(hjust=0.5))+theme(legend.position="none")+scale_x_continuous(breaks = seq(1997, 2019, by = 1))

#top 10 developers:[1]NEKO WORKs  [2]Valve  [3]The Behemoth [4]Free Lives  [5]Klei Entertainment [6]Hopoo Games [7]Scott Cawthon [8]Supergiant Games
#[9]Frictional Games  [10]Hidden Path Entertainment
#https://steam250.com/developer
paid_games$top_developers<-ifelse(grepl("NEKO WORKs",paid_games$developer)|grepl("Valve",paid_games$developer)|grepl("The Behemoth",paid_games$developer)|
                                    grepl("Klei Entertainment",paid_games$developer)|grepl("Hopoo Games",paid_games$developer)|grepl("Scott Cawthon",paid_games$developer)|
                                    grepl("Supergiant Games",paid_games$developer)|grepl("Frictional Games",paid_games$developer)|grepl("Hidden Path Entertainment",paid_games$developer)|
                                    grepl("Free Lives",paid_games$developer)
                                  ,1,0)

#top 10 publishers:[1]Valve  [2]Devolver Digital  [3]Quiet River  [4]XSEED Games  [5]Marvelous USA, Inc. [6]HIKARI FIELD [7]Sekai Project [8]Capcom
#[9]Klei Entertainment [10]MangaGamer
paid_games$top_publishers<-ifelse(grepl("Devolver Digital",paid_games$publisher)|grepl("Valve",paid_games$publisher)|grepl("Quiet River",paid_games$publisher)|
                                    grepl("XSEED Games",paid_games$publisher)|grepl("Marvelous USA, Inc.",paid_games$publisher)|grepl("HIKARI FIELD",paid_games$publisher)|
                                    grepl("Sekai Project",paid_games$publisher)|grepl("Capcom",paid_games$publisher)|grepl("Klei Entertainment",paid_games$publisher)|
                                    grepl("MangaGamer",paid_games$publisher)
                                  ,1,0)

#action
table(paid_games$genre_action)
hist(paid_games$genre_action)

#adven
table(paid_games$genre_adven)
hist(paid_games$genre_adven)

#casual
table(paid_games$genre_casual)
hist(paid_games$genre_casual)

#indie
table(paid_games$genre_indie)
hist(paid_games$genre_indie)

#mm
table(paid_games$genre_mm)
hist(paid_games$genre_mm)

#sim
table(paid_games$genre_sim)
hist(paid_games$genre_sim)

#rpg
table(paid_games$genre_rpg)
hist(paid_games$genre_rpg)

#stra
table(paid_games$genre_stra)
hist(paid_games$genre_stra)

#racing
table(paid_games$genre_racing)
hist(paid_games$genre_racing)

#sport
table(paid_games$genre_sports)
hist(paid_games$genre_sports)
