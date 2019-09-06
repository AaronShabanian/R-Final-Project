data <- read.csv(file.choose())
summary(data)
library(ggplot2) 
#Barplot of comparison of income
counts <- table(data$Income)
barplot(counts, main="Income", 
        xlab="<=50k vs >50k")
library(ggthemes)
#Income compared to number of hours worked and education
gg <- ggplot(data,aes(x = data$Hours.Worked.Week, y = data$Education,color=data$Income))
gg
gg + geom_point()
chisq.test(data$Hours.Worked.Week)

#Hours worked vs Age
gg4 <- ggplot(data,aes(y = data$Hours.Worked.Week, x = data$Age))
gg4+geom_smooth(method='lm')
t.test(data$Age,data$Hours.Worked.Week)

#Hours worked vs Age and income
gg3 <- ggplot(data,aes(y = data$Hours.Worked.Week, x = data$Age, color=data$Income))
gg3+geom_smooth(method='lm')
t.test(data$Age~data$Income)
#To demonstrate the lopsided difference in genders in this surver
counts <- table(data$Gender)
barplot(counts, main="Gender Distribution", 
        xlab="Gender")
#I multiplied the female population in each education level by 2 to make the 2 sides even
counts <- table(data$Gender, data$Education)
counts
counts[2,]<-counts[2,]/2
counts
barplot(counts, main="Gender distribution by Education Level",
        xlab="Gender", col=c("pink","blue"),
        legend = rownames(counts), beside=TRUE)

t.test(data$Age~data$Gender)
#Comparing Gender and income
counts <- table(data$Gender, data$Income)
counts[2,]<-counts[2,]/2.02
counts
barplot(counts, main="Gender distribution by Income Level",
        xlab="Gender", col=c("pink","blue"),
        legend = rownames(counts), beside=TRUE)
t.test(data$Hours.Worked.Week~data$Income)
