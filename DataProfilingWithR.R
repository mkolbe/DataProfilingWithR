install.packages("ggplot2")
library(ggplot2)
#Starting NFL data profiling now
#Read in data
nfl <- read.csv("https://drive.google.com/uc?export=download&id=0BwNeFVkFjEYSSlQ0N1M1SFlHd3c")
#View it
View(nfl)
#Generate summary statistics
summary(nfl)
summary(subset(nfl, Year==2012))
#Filter the dataset
subset(nfl, Yds==5177)
nfl2012 <- subset(nfl, Year==2012)
subset(nfl2012, Yds==5177)
#Grouping
by(nfl2012[,c("Age","Yds", "Height..inches.", "Weight")], nfl2012$FantPos, colMeans)
qplot(Age, data=nfl2012, geom="bar", main="NFL Ages")
#Histogram
#Need to get rid of factors in data frame
nfl2012.matrix <- data.matrix(nfl2012)
nfl2012.mod <- data.frame(nfl2012.matrix)
ggplot(nfl2012.mod, aes(x=X40.Yard)) + geom_histogram(binwidth=1)
ggplot(nfl2012.mod, aes(x=X40.Yard)) + geom_histogram(binwidth=3)
no0in40 <- subset(nfl2012.mod, X40.Yard > 0)
ggplot(no0in40, aes(x=X40.Yard)) + geom_histogram(binwidth=3)
no0in40 <- subset(nfl2012.mod, X40.Yard > 1)
ggplot(no0in40, aes(x=X40.Yard)) + geom_histogram(binwidth=3)
#Distribution
ggplot(no0in40, aes(x=X40.Yard)) + geom_density()
ggplot(nfl2012.mod, aes(x=X40.Yard)) + geom_density()
rm(no0in40)
#Group the Chart
ggplot(nfl2012, aes(x=X40.Yard)) + geom_density() + facet_wrap(~ FantPos)
rm(nfl2012.mod)
rm(nfl2012.matrix)
#Distribution of Categorical Data
xtabs(~ FantPos, data=nfl2012)
xtabs(~ Draft.Year, data=nfl2012)
xtabs(~ FantPos + Draft.Year, data=nfl2012)
#BoxPlot
ggplot(nfl2012, aes(x=FantPos, y=Height..inches.)) + geom_boxplot()
#Scatterplot
qbs <- subset(nfl, FantPos=="QB")
#qplot(qbs$TD, qbs$Int, main="Touchdowns & Interceptions", xlab="TDs", ylab="Ints")
ggplot(qbs, aes(x=TD, y=Int, color=Year)) + geom_point(shape=1)
#Add regression line
ggplot(qbs, aes(x=TD, y=Int, color=Year)) + geom_point(shape=2) + geom_smooth(method=lm)
rm(qbs)
#Line Chart
myQBs <- subset(nfl, Player %in% c("Aaron Rodgers", "Peyton Manning", "Tom Brady"))
ggplot(data=myQBs, aes(x=Year, y=Cmp, group=Player, color=Player)) + geom_line()
ggplot(data=myQBs, aes(x=Year, y=TD, fill=Player)) + geom_bar(stat="identity") + geom_line(data=myQBs, aes(x=Year, y=Cmp, color=Player, group=Player)) + ylab("TD & Completions")
ggplot(data=myQBs, aes(x=Year, y=TD, fill=Player)) + geom_bar(stat="identity", position="dodge") + geom_line(data=myQBs, aes(x=Year, y=Cmp, color=Player, group=Player)) + ylab("TD & Completions")
rm(myQBs)
#Wordcloud
install.packages("tm")
install.packages("wordcloud")
library(tm)
library(wordcloud)
wordcloud(nfl$College)
#MissingData
subset(nfl, Player=="Jeff Garcia")
sapply(subset(nfl, Player=="Jeff Garcia"), function(y) sum(is.na(y)))
sum(is.na(nfl$College))
sum(is.null(nfl$College))
na_count <-sapply(nfl, function(y) sum(is.na(y)))
na_count$name<-rownames(na_count)
View(na_count)
null_count <-sapply(nfl, function(y) sum(is.null(y)))
null_count$name<-rownames(null_count)
View(null_count)
#Clean up #NA data
nfl[nfl=="#N/A"] = NA
subset(nfl, Player=="Jeff Garcia")
sapply(subset(nfl, Player=="Jeff Garcia"), function(y) sum(is.na(y)))
sapply(nfl, function(y) sum(is.na(y)))
#Using Data Quality Package
install.packages("dataQualityR")
library("dataQualityR")
checkDataQuality(data=nfl, out.file.num= "/users/michellekolbe/Desktop/dq_nfl_num.csv", out.file.cat= "/users/michellekolbe/Desktop/dq_nfl_cat.csv")
