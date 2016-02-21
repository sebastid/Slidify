library(dplyr)
lichield_league <- read.table("LichfieldLeagueResults.csv",
                              header=TRUE, sep=";", na.strings = "-", stringsAsFactors = FALSE, strip.white = TRUE)
names(lichield_league) <- c("Date","HomeTeam","HomeGoals","AwayTeam","AwayGoals")
is_na_idx <- which(is.na(lichield_league$HomeGoals))

training <- lichield_league[-is_na_idx,]
testing <- lichield_league[is_na_idx,]


training$HomeGoals <- as.numeric(training$HomeGoals)
training$AwayGoals <- as.numeric(training$AwayGoals)

training$HomeResult <- as.factor(with(training, {ifelse( HomeGoals > AwayGoals, "W", ifelse( HomeGoals == AwayGoals,"D","L"))}))
training$AwayResult <- as.factor(with(training, {ifelse( HomeGoals > AwayGoals, "L", ifelse( HomeGoals == AwayGoals,"D","W"))}))


training.indep <- data.frame(Team=as.factor(c(as.character(training$HomeTeam),
                                              as.character(training$AwayTeam))),
                             Opponent=as.factor(c(as.character(training$AwayTeam),
                                                  as.character(training$HomeTeam))),
                             Goals=c(training$HomeGoals, training$AwayGoals),
                             Home=c(as.integer(rep(1, dim(training)[1])), as.integer(rep(0, dim(training)[1]))),
                             Result=factor(c(training$HomeResult,training$AwayResult), labels=c("D","L","W")))

table_results <- tbl_df(training.indep)  %>% group_by(Team) %>% summarise(Played=n())
table_results <- merge(table_results,tbl_df(training.indep) %>% group_by(Team) %>% summarise(Won=sum(Result=="W")))
table_results <- merge(table_results,tbl_df(training.indep) %>% group_by(Team) %>% summarise(Drawn=sum(Result=="D")))
table_results <- merge(table_results,tbl_df(training.indep) %>% group_by(Team) %>% summarise(Lost=sum(Result=="L")))
table_results <- merge(table_results,tbl_df(training.indep) %>% group_by(Team) %>% summarise(For=sum(Goals)))
table_results <- merge(table_results, tbl_df(training.indep) %>% group_by(Opponent) %>% summarise(Against=sum(Goals)), by.x="Team", by.y="Opponent")
table_results <- table_results %>% mutate(Diff=For-Against, Points=Won*3+Drawn) %>% arrange(desc(Points))

###

library(dplyr)
tbl_training <- tbl_df(training)
tbl_scored_H <- tbl_training %>% group_by(HomeTeam) %>% summarise(HomeGoals=mean(HomeGoals))
tbl_scored_A <- tbl_training %>% group_by(AwayTeam) %>% summarise(AwayGoals=mean(AwayGoals))
tbl_scored_Merged <- merge(tbl_scored_H,tbl_scored_A,by.x="HomeTeam",by.y = "AwayTeam")
names(tbl_scored_Merged) <- c("Team","HomeScoredGoalAvg","AwayScoredGoalAvg")
plot_tbl_scored <- tbl_scored_Merged %>% mutate(AvgGoalScored=(HomeScoredGoalAvg+AwayScoredGoalAvg)/2) %>% arrange(AvgGoalScored)
plot_tbl_scored$Team <- factor(plot_tbl_scored$Team,levels=plot_tbl_scored$Team)


tbl_conceded_H <- tbl_training %>% group_by(HomeTeam) %>% summarise(HomeGoals=mean(AwayGoals))
tbl_conceded_A <- tbl_training %>% group_by(AwayTeam) %>% summarise(AwayGoals=mean(HomeGoals))
tbl_conceded_Merged <- merge(tbl_conceded_H, tbl_conceded_A, by.x="HomeTeam", by.y = "AwayTeam")
names(tbl_conceded_Merged) <- c("Team","HomeConcededGoalAvg","AwayConcededGoalAvg")
plot_tbl_conceded <- tbl_conceded_Merged %>% mutate(AvgGoalConceded=(HomeConcededGoalAvg+AwayConcededGoalAvg)/2) %>% arrange(desc(AvgGoalConceded))
plot_tbl_conceded$Team <- factor(plot_tbl_conceded$Team,levels=plot_tbl_conceded$Team)


library(ggplot2)
library(gridExtra)
plot1 <- ggplot(plot_tbl_scored, aes(x = Team, y = AvgGoalScored)) 
plot1 <- plot1 + xlab("") + ylab("Average Scored Goals") + geom_bar(stat = "identity", fill="steelblue") + coord_flip() 
plot1 <- plot1 + theme_bw(base_family = "Avenir", base_size = 15)
plot1 <- plot1 + geom_hline(yintercept=median(plot_tbl_scored$AvgGoalScored),linetype=3)
plot2 <- ggplot(plot_tbl_conceded, aes(x = Team, y = AvgGoalConceded)) 
plot2 <- plot2 + xlab("") + ylab("Average Conceded Goals") + geom_bar(stat = "identity",  fill="firebrick2") + coord_flip()
plot2 <- plot2 + theme_bw(base_family = "Avenir", base_size = 15)
plot2 <- plot2 + geom_hline(yintercept=median(plot_tbl_conceded$AvgGoalConceded),linetype=3)
grid.arrange(plot1, plot2, ncol=2)
#options(repr.plot.width=8, repr.plot.height=3)

###

library(reshape2)
home_scored_goals <-  mean(unlist(tbl_training %>% group_by(HomeTeam) %>% summarise(AvgHomeGoals = mean(HomeGoals)) %>% select(AvgHomeGoals)))
away_scored_goals <- mean(unlist(tbl_training %>% group_by(AwayTeam) %>% summarise(AvgAwayGoals = mean(AwayGoals))%>% select(AvgAwayGoals)))
home_conceded_goals <- mean(unlist(tbl_training %>% group_by(HomeTeam) %>% summarise(AvgHomeGoals = mean(AwayGoals))%>% select(AvgHomeGoals)))
away_conceded_goals <- mean(unlist(tbl_training %>% group_by(AwayTeam) %>% summarise(AvgAwayGoals = mean(HomeGoals))%>% select(AvgAwayGoals)))

goals_df1 <- melt(data.frame(home_goals_avg=home_scored_goals,away_goals_avg=away_scored_goals),measure.vars = c("home_goals_avg","away_goals_avg"))
goals_df1$variable <- factor(goals_df1$variable,levels=c("away_goals_avg","home_goals_avg"),labels=c("Away Scored Goals","Home Scored Goals"))

goals_df2 <- melt(data.frame(home_goals_avg=home_conceded_goals,away_goals_avg=away_conceded_goals),measure.vars = c("home_goals_avg","away_goals_avg"))
goals_df2$variable <- factor(goals_df2$variable,levels=c("away_goals_avg","home_goals_avg"),labels=c("Away Conceded Goals","Home Conceded Goals"))


plot1 <- ggplot(goals_df1, aes(x = variable, y = value, fill=variable)) + geom_bar(stat = "identity") + coord_flip()
plot1 <- plot1 + theme_bw(base_family = "Avenir", base_size = 15) 
plot1 <- plot1 + scale_fill_manual(values = alpha(c("firebrick2", "steelblue"), .9))
plot1 <- plot1 + xlab("") + ylab("Average Goal Scored Per Match") + theme(legend.position="none")

plot2 <- ggplot(goals_df2, aes(x = variable, y = value, fill=variable)) + geom_bar(stat = "identity") + coord_flip()
plot2 <- plot2 + theme_bw(base_family = "Avenir", base_size = 15) 
plot2 <- plot2 + scale_fill_manual(values = alpha(c("firebrick2", "steelblue"), .9))
plot2 <- plot2 + xlab("") + ylab("Average Goal Conceded Per Match")  + theme(legend.position="none")

grid.arrange(plot1, plot2, ncol=2)

###

home_goals <- tbl_training %>% group_by(HomeTeam) %>% summarise(AvgHomeGoals = mean(HomeGoals)/2)
away_goals <- tbl_training %>% group_by(AwayTeam) %>% summarise(AvgAwayGoals = mean(AwayGoals)/2)
home_away_goals <- merge(home_goals,away_goals,by.x = "HomeTeam", by.y = "AwayTeam") 

n1<-round(home_away_goals$AvgHomeGoals/(home_away_goals$AvgHomeGoals+home_away_goals$AvgAwayGoals)*100,1)
n2<-round(home_away_goals$AvgAwayGoals/(home_away_goals$AvgHomeGoals+home_away_goals$AvgAwayGoals)*100,1)
home_away_goals$AvgHomeGoals <- n1
home_away_goals$AvgAwayGoals <- n2
home_away_goals <- home_away_goals %>% arrange(as.integer(home_away_goals$AvgHomeGoals))


home_adv_plt <- melt(home_away_goals,measure.vars = c("AvgHomeGoals","AvgAwayGoals"))

names(home_adv_plt) <- c("Team","AvgGoalType","AvgGoals")
home_adv_plt$Team <- factor(home_adv_plt$Team, levels=home_away_goals$HomeTeam)
levels(home_adv_plt$AvgGoalType) <- c("Home Scored Goals","Away Scored Goals")

plot2 <- ggplot(home_adv_plt, aes(x = Team, y = AvgGoals, fill=AvgGoalType)) 
plot2 <- plot2 + geom_bar(stat = "identity") + coord_flip() 
plot2 <- plot2 + theme_bw(base_family = "Avenir", base_size = 15)
plot2 <- plot2 + scale_fill_manual(values = alpha(c("steelblue", "firebrick2"), .9))
plot2 <- plot2 + geom_hline(yintercept=50,linetype=3)
plot2 <- plot2 + xlab("") +ylab("Scored Goal %") + guides(fill = guide_legend(reverse = F, keywidth = 3, keyheight = 1, title=NULL))
plot2

home_goals <- tbl_training %>% group_by(HomeTeam) %>% summarise(AvgHomeGoals = mean(AwayGoals)/2)
away_goals <- tbl_training %>% group_by(AwayTeam) %>% summarise(AvgAwayGoals = mean(HomeGoals)/2)
home_away_goals <- merge(home_goals,away_goals,by.x = "HomeTeam", by.y = "AwayTeam") 

n1<-round(home_away_goals$AvgHomeGoals/(home_away_goals$AvgHomeGoals+home_away_goals$AvgAwayGoals)*100,1)
n2<-round(home_away_goals$AvgAwayGoals/(home_away_goals$AvgHomeGoals+home_away_goals$AvgAwayGoals)*100,1)
home_away_goals$AvgHomeGoals <- n1
home_away_goals$AvgAwayGoals <- n2
home_away_goals <- home_away_goals %>% arrange(as.integer(home_away_goals$AvgHomeGoals))


home_adv_plt <- melt(home_away_goals,measure.vars = c("AvgHomeGoals","AvgAwayGoals"))

names(home_adv_plt) <- c("Team","AvgGoalType","AvgGoals")
home_adv_plt$Team <- factor(home_adv_plt$Team, levels=home_away_goals$HomeTeam)
levels(home_adv_plt$AvgGoalType) <- c("Home Conceded Goals","Away Conceded Goals")

plot2 <- ggplot(home_adv_plt, aes(x = Team, y = AvgGoals, fill=AvgGoalType)) 
plot2 <- plot2 + geom_bar(stat = "identity") + coord_flip() 
plot2 <- plot2 + theme_bw(base_family = "Avenir", base_size = 15)
plot2 <- plot2 + scale_fill_manual(values = alpha(c("steelblue", "firebrick2"), .9))
plot2 <- plot2 + geom_hline(yintercept=50,linetype=3)
plot2 <- plot2 + xlab("") + ylab("Conceded Goal %") + guides(fill = guide_legend(reverse = F, keywidth = 3, keyheight = 1, title=NULL))
plot2

###

library(scales)
h_gplt <- data.frame(Goals=training$HomeGoals)
a_gplt <- data.frame(Goals=training$AwayGoals)
lambda_ <- mean(h_gplt$Goals)
mu <- mean(a_gplt$Goals)
h_poisson_df <- data.frame(x=0:12,p=dpois(0:12,lambda_))
a_poisson_df <- data.frame(x=0:12,p=dpois(0:12,mu))
label1 <- sprintf("Mean = %.2f Home Goals/Match",round(lambda_,2))
label2 <- sprintf("Mean = %.2f Away Goals/Match",round(mu,2))

g1 <- ggplot(h_gplt, aes(x = Goals))
g1 <- g1 + geom_histogram(aes(y=..count../sum(..count..)), origin=0,  binwidth = 1, color="black", fill="steelblue")
g1 <- g1 + geom_vline(xintercept = mean(h_gplt$Goals),color="red")
g1 <- g1 + stat_function(fun=dpois, args=list(lambda=lambda_), col="red", n = 14, size=1, geom="line")
g1 <- g1 + geom_text(data = data.frame(), aes(5, 0.25, hjust=0, label = label1, col="red", size = 6, parse = TRUE))  + theme(legend.position="none")
g1 <- g1 + scale_x_continuous(breaks=0:13,limits = c(0,13)) 
g1 <- g1 + theme_bw(base_family = "Avenir", base_size = 15)
g1 <- g1 + xlab("Home Goals per Match")  + ylab("Density") + theme(legend.position="none")

g2 <- ggplot(a_gplt, aes(x = Goals))
g2 <- g2 + geom_histogram(aes(y=..count../sum(..count..)), origin=0,  binwidth = 1, color="black", fill="steelblue")
g2 <- g2 + geom_vline(xintercept = mean(a_gplt$Goals),color="red")
g2 <- g2 + stat_function(fun=dpois, args=list(lambda=mu), col="red", n = 14, size=1, geom="line")
g2 <- g2 + geom_text(data = data.frame(), aes(4, 0.25, hjust=0, label = label2, col="red", size = 6, parse = TRUE))  + theme(legend.position="none")
g2 <- g2 + scale_x_continuous(breaks=0:13,limits = c(0,13)) 
g2 <- g2 + theme_bw(base_family = "Avenir", base_size = 15)
g2 <- g2 + xlab("Away Goals per Match")  + ylab("Density") + theme(legend.position="none")

grid.arrange( g1, g2, nrow=2)

###

fixtures<-testing
fixtures$HomeGoals <- NULL
fixtures$AwayGoals <- NULL

###

model <- glm(Goals ~ Home + Team + Opponent, data=training.indep, family=poisson(link="log"))
expected <- fitted(model)
home.expected <- expected[1:nrow(training)]
away.expected <- expected[(nrow(training)+1):(nrow(training)*2)]


mytau <- Vectorize(function(xx, yy, lambda, mu,  rho) {
  if(xx==0 & yy==0) {return(1-(lambda*mu*rho))
  } else if(xx==0 & yy==1) {return(1+(lambda*rho))
  } else if(xx==1 & yy==0) {return(1+(mu*rho))
  } else if(xx==1 & yy==1) {return(1-rho) 
  } else {return(1)}
})

DCLogLik <- function(y1, y2, lambda, mu, rho=0) {
  #rho=0, independence
  #y1: home goals
  #y2: aways goals
  
  #print(paste(y1,y2,lambda,mu))
  #print(log(mytau(y1,y2,lambda, mu, rho)) + log(dpois(y1, lambda)) + log(dpois(y2, mu)))
  #print(paste(mytau(y1,y2,lambda, mu, rho),rho))
  sum(log(mytau(y1,y2,lambda, mu, rho)) + log(dpois(y1, lambda)) + log(dpois(y2, mu)))
}

DCoptimRhoFn <- function(par) {
  rho <- par[1]
  r=DCLogLik(training$HomeGoals, training$AwayGoals, home.expected, away.expected, rho)
  print(r)
  r
}
res <- optim(par=c(0.00001), fn=DCoptimRhoFn, control=list(fnscale=-1), method='BFGS')

###

maxgoal <- 12
prob_matrix <- matrix(0,nrow=maxgoal+1,ncol=maxgoal+1)
lambda <- 0.0
mu <- 0.0
predict_match <- function(date_, team, opponent) {
  #Expected Hone Goals
  lambda <<- predict(model, data.frame(Home=1, Team=team, Opponent=opponent), type="response")
  #expected Away Goals
  mu <<- predict(model, data.frame(Home=0, Team=opponent, Opponent=team), type="response")
  prob_matrix <<- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
  HomeWinPr <- sum(prob_matrix[lower.tri(prob_matrix)])
  DrawPr <- sum(diag(prob_matrix))
  AwayWinPr <- sum(prob_matrix[upper.tri(prob_matrix)])
  #return(data.frame(Date=date_,Team=team,Opponent=opponent,Win=HomeWinPr,Draw=DrawPr,Lose=AwayWinPr))
  return(c(HomeWinPr,DrawPr,AwayWinPr))
}

match_results <- round(apply(fixtures, 1, function(x) predict_match(x['Date'],x['HomeTeam'],x['AwayTeam'])),3)
fixtures["Win"] <- match_results[1,]
fixtures["Draw"] <- match_results[2,]
fixtures["Lose"] <- match_results[3,]

library(reshape2); library(ggplot2)
mplt <- melt(fixtures,id.vars = c("Date","HomeTeam","AwayTeam"),measure.vars = c("Win","Draw","Lose"), variable.name = "Home Match Result")
mplt["match"] <- with(mplt,{paste(Date,HomeTeam,AwayTeam,sep=" | ")})

g <- ggplot(data=mplt, aes(x=match, y= value, fill=`Home Match Result`))
g <- g + geom_bar(stat = "identity")+coord_flip()
g <- g + theme_bw(base_family = "Avenir", base_size = 15)
g <- g + xlab("") + ylab("Probability") + ggtitle("Match Prediction")
g

###


scaling_matrix <- matrix(mytau(c(0,1,0,1),c(0,0,1,1), lambda, mu, res$par), nrow=2)
prob_matrix[1:2,1:2] <- prob_matrix[1:2,1:2] * scaling_matrix

predict_match_adj <- function(date_, team, opponent) {
  #Expected Hone Goals
  lambda <<- predict(model, data.frame(Home=1, Team=team, Opponent=opponent), type="response")
  #expected Away Goals
  mu <<- predict(model, data.frame(Home=0, Team=opponent, Opponent=team), type="response")
  prob_matrix <<- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
  prob_matrix[1:2,1:2] <- prob_matrix[1:2,1:2] * scaling_matrix
  print(prob_matrix)
  HomeWinPr <- sum(prob_matrix[lower.tri(prob_matrix)])
  DrawPr <- sum(diag(prob_matrix))
  AwayWinPr <- sum(prob_matrix[upper.tri(prob_matrix)])
  #return(data.frame(Date=date_,Team=team,Opponent=opponent,Win=HomeWinPr,Draw=DrawPr,Lose=AwayWinPr))
  return(c(HomeWinPr,DrawPr,AwayWinPr))
}

match_results <- round(apply(fixtures, 1, function(x) predict_match_adj(x['Date'],x['HomeTeam'],x['AwayTeam'])),3)
fixtures["Win"] <- match_results[1,]
fixtures["Draw"] <- match_results[2,]
fixtures["Lose"] <- match_results[3,]

library(reshape2); library(ggplot2)
mplt <- melt(fixtures,id.vars = c("Date","HomeTeam","AwayTeam"),measure.vars = c("Win","Draw","Lose"), variable.name = "Home Match Result")
mplt["match"] <- with(mplt,{paste(Date,HomeTeam,AwayTeam,sep=" | ")})

g <- ggplot(data=mplt, aes(x=match, y= value, fill=`Home Match Result`))
g <- g + geom_bar(stat = "identity")+coord_flip()
g <- g + theme_bw(base_family = "Avenir", base_size = 15)
g <- g + xlab("") + ylab("Probability") + ggtitle("Match Prediction")
g
