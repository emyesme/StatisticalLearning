####Introduction to R####

x<- c(1,3,5,7)
x

y<- c(1,2,3,4)
y

z<- c(12,34,56); z

y[2] #gives second value of vector
y

terzo_el_z<-z[3]

length(x)
length(y)
x+y
sum<-x+y; sum

ls() #lists everything

rm(x) #removes x

rm(list=ls())
ls()

x=matrix(data=c(1,2,3,4),nrow=2,ncol=2)
x=matrix(c(1,2,3,4),2,2); x
Y=matrix(c(1,2,3,4),2,2,byrow=TRUE);Y
dim(Y)

A<- matrix(c(1,2,-4,18,23,15,5,12,67,32,11,34),4,3);A;dim(A)
A[2,3] # 2nd row and 3rd column element
A[1,] #all elements in first row
A[,2]
A
A[,c(1,3)] #elements in first and third column are taken

sqrt(A) ##ERROR
A^2

#indexing

A<-matrix(1:16,4,4); A
A[3,2]
A[2,3]

#diagonal matrix

B<- diag(c(1,5,-2,-8));B

diagonale<-diag(B);diagonale

I2<-diag(c(1,1));I2
I4<-diag(1,4,4);I4

B<-c(7,5,3,1);B
C<-c(6,8,9,9);C
D<-cbind(B,C);D
E<-rbind(B,C);E

A<-matrix(c(1,2,3,4,5,6),3,2);A
B<-matrix(c(9,5,7,8,10,11),3,2);B
C<-A+B;C
D<-A-B;D

D<-matrix(c(7,8,9,10,11,12),2,3)

A%*%D #product of 2 matrices
A*B #element-wise multiplication

####Exercise 8 (chap 2)####

college <- read.csv("/home/emily/Desktop/StatisticalLearning/College.csv")
rownames(college)=college[,1]
fix(college)

college=college[,-1]
fix(college)

summary(college)

###
str(college)
college[,1]=as.numeric(factor(college[,1])) #factor transforms a categorical variable such as a string into a quantity
pairs(college[,1:10]) #pairs of data are taken

plot(Outstate ~ as.factor(Private),data=college, xlab="Private University",ylab="Tuition in $")
#'Yes' and 'No' for Private converted into quantitative values that differ

Elite=rep("No",nrow(college));Elite
Elite[college$Top10perc >50] ="Yes" #takes top 10% greater than 50
Elite=as.factor(Elite)
college=data.frame(college,Elite) #appends the Elite variable to the college

college$Elite<-as.factor(ifelse(college$Top10perc>50,"Yes","No"))

summary(Elite)

plot(Outstate ~ Elite,data=college,xlab="Elite university",ylab="Tuition in $")

par(mfrow=c(2,2))
hist(college$Apps,xlab="Applications Received",main="")
hist(college$perc.alumni,col=2,xlab="% of Alumni who donated",main="")
hist(college$S.F.Ratio,col=3,breaks=10,xlab="Student:Faculty ratio",main="")
hist(college$Expend,breaks=100,xlab="Instructional expenditure per student",main="")

#university with most students in the top 10 of the class
row.names(college)[which.max(college$Top10perc)]

acceptance_rate<-college$Accept/college$Apps
acceptance_rate

#lowest acceptance rate
row.names(college)[which.min(acceptance_rate)]

row.names(college)[which.max(acceptance_rate)]

plot(Grad.Rate~Outstate,data=college)

summary(college$Apps)
row.names(college[college$Apps>25000, ])

####Exercise 10 (chap 2)####

library(MASS)
head(Boston)

#Part a

dim(Boston)

#Part b

str(Boston)
Boston$chas<-as.numeric(Boston$chas)
Boston$rad<-as.numeric(Boston$rad)

pairs(Boston)

cor(Boston) #coefficient of correlation

summary(Boston$crim)

library(ggplot2)

qplot(Boston$crim,binwidth=5,xlab="Crime rate",ylab="Number of Suburbs")
summary(Boston$tax)
qplot(Boston$tax,binwidth=50,xlab="Full-value property-tax rate per $10,000",ylab="Number of Suburbs")
summary(Boston$ptratio)
qplot(Boston$ptratio,binwidth=50,xlab="Pupil-teacher ratio by town",ylab="Number of Suburbs")

hist(Boston$crim[Boston$crim > 1],breaks = 25)

hist(Boston$tax,breaks = 25)

hist(Boston$ptratio,breaks = 25)

#Part e

nrow(subset(Boston,chas==1)) #number of rows that have the Charles river
#There are 35 such suburbs

#Part f

summary(Boston$ptratio)

#Part g

selection<-Boston[order(Boston$medv),] #order according to the variable medv
selection[1,]
selection[2,] #2 have the same values so they are randomly ordered as 1st and 2nd

summary(selection)

#Part h

rm_over_7<-subset(Boston,rm>7)
nrow(rm_over_7)

#There are 64 suburbs that have more than 7 rooms per dwelling

rm_over_8<-subset(Boston,rm>8)
nrow(rm_over_8)

#There are 13 suburbs that have more than 8 rooms per dwelling

summary(rm_over_8)

summary(Boston)

