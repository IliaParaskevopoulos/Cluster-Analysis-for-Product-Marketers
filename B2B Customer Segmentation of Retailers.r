
#Import Data

library(foreign) 

mydata = read.spss("C:\\Users\\ilias\\Desktop\\cluster.sav",to.data.frame=TRUE)
write.table(mydata,"cluster.csv") 
mydata = read.spss("C:\\Users\\ilias\\Desktop\\cluster_assignment.sav",to.data.frame=TRUE)
write.table(mydata,"cluster_assignment.csv") 
mydata = read.spss("C:\\Users\\ilias\\Desktop\\cluster_initial_centers.sav",to.data.frame=TRUE)
write.table(mydata,"cluster_initial_centers.csv") 

getwd()

setwd("C:/Users/Ilias/Documents")

#Transform Data into analyzable dataframes
customers <- read.table('cluster.csv', sep = ' ', encoding = 'UTF-8', fill = T, header=T,na.strings = c('NA','None'), quote='')
cluster_assignment <- read.table('cluster_assignment.csv', sep = '', encoding = 'UTF-8', fill = T, header=T,na.strings = c('NA','None'), quote='')
cluster_initial_centers <- read.table('cluster_initial_centers.csv', sep = '', encoding = 'UTF-8', fill = T, header=T,na.strings = c('NA','None'), quote='')

# Import Libraries and omit missing values

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms

customers <- na.omit(customers)
cluster_assignment <- na.omit(cluster_assignment)
cluster_initial_centers <- na.omit(cluster_initial_centers)

#summary(customers)
#summary(cluster_assignment)
#summary(cluster_initial_centers)
#View(customers)
#View(cluster_assignment)
#View(cluster_initial_centers)

names(cluster_assignment)[1] <- "Store Number"
names(cluster_assignment)[2] <- "Area"
names(cluster_assignment)[3] <- "Surface"
names(cluster_assignment)[4] <- "Size"
names(cluster_assignment)[5] <- "Revenue"
names(cluster_assignment)[6] <- "Age"
names(cluster_assignment)[7] <- "TwoWorks"
names(cluster_assignment)[8] <- "Homeowners"
names(cluster_assignment)[9] <- "Household Size"
names(cluster_assignment)[10] <- "High Education"
names(cluster_assignment)[11] <- "Low Education"
names(cluster_assignment)[12] <- "Urban"
names(cluster_assignment)[13] <- "Income"

#Normalize 
z <- cluster_assignment[,-c(1,1)]
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale=sds)

#Calculate distance matrix  
distance = dist(nor)

#Hierarchical agglomerative clustering  
C = hclust(distance)
plot(C)
plot(C,labels=C$X.StoreNumber.,main='Default from hclust')
plot(C,hang=-1)

#Scree Plot
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-means clustering
kc<-kmeans(nor,4)
kc$cluster


#2D Represantation 
library(cluster)
clusplot(cluster_assignment, kc$cluster, main="2D representation of Clusters", shade=TRUE, label=2, lines=0)
# Cluster Analysis Code primarily taken from https://www.youtube.com/watch?v=tkAJT8gWBSY 




# ____________ Below are Draft Notes ___________________

library(ggplot2);
library(ggally);
library(MASS)
library(gganimate) 

install.packages("radarchart")
library(radarchart)


#Parallel Chart
parcoord(cluster_assignment, lty = 1, var.label = FALSE, col = kc$cluster, lwd = 3)

#Radar Chart https://www.rdocumentation.org/packages/fmsb/versions/0.7.0/topics/radarchart
labs = c("Store Number", "Area", "Surface", "Size",  "Revenue", "Age", "TwoWorks", "Homeowners",  "High Education",
         "Low Education", "Urban", "Income")
chartJSRadar(z, labs, col = kc$cluster, lwd = 1)
radarchart(cluster_assignment, lty = 1, var.label = FALSE, col = kc$cluster, lwd = 1) 

#Simple Plot
plot(cluster_assignment$X.area.~cluster_assignment$X.surface., pch = 15, col = kc$cluster) 

#Testing more plots
DF=cluster_assignment(rbind(set1,set2,set3,set4,set5),cluster=as.factor(c(rep(1:5,each=300))))
ggplot(cluster_assignment,aes(x=cluster_assignment$Size,y=cluster_assignment$Urban,lwd = 3, color=kc$cluster))+geom_point()






