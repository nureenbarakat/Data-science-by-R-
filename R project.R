install.packages("dplyr")
library(dplyr)
products <- read.csv("C:/Users/LENOVO/OneDrive/grc.csv")
class(products)

transactions=strsplit(as.vector(products$city),',')
transactions
unique_city<-unique(unlist(transactions))
unique_city

transactions=strsplit(as.vector(products$customer),',')
transactions
unique_customer<-unique(unlist(transactions))
unique_customer



products<- arrange(city, desc(total))
products


write.csv(products,"C:/Users/LENOVO/OneDrive/grc.csv")



table(products$paymentType)
barplot(
  height = table(products$paymentType),
  col= "pink",
  main = "Compare Payment Type",
  xlab = "Payment Type",
  ylab = "Numbers",
)


totalPerage<-group_by(products,age)
totalPerage<-summarise(totalPerage,totaltotal=sum(total))
totalPerage
pie(
  x=totalPerage$totaltotal,
  labels=totalPerage$age,
  main="compare Total by Age"
)


totalPercity<-group_by(products,city)
totalPercity<-summarise(totalPercity,total=sum(total))
totalPercity
totalPercity<- arrange(totalPercity, desc(total))
totalPercity
barplot(
  height=totalPercity$total,
  name=totalPercity$city,
  col="black",
  main="Compare total spending by each city",
  xlab="Total",
  ylab="City",
  horiz = TRUE,
  las=1,
 )




boxplot(
  x =products$total,
  main="Distribution of Total Spending",
  xlab="Total"
)
 


par(mfrow=c(2,2))
barplot(
  height = table(products$paymentType),
  col= "pink",
  main = "Compare Payment Type",
  xlab = "Payment Type",
  ylab = "Numbers",
)
pie(
  x=totalPerage$totaltotal,
  labels=totalPerage$age,
  main="compare Total by Age"
)
barplot(
  height=totalPercity$total,
  name=totalPercity$city,
  col="black",
  main="Compare total spending by each city",
  xlab="Total",
  ylab="City",
  horiz = TRUE,
  las=1,
)
boxplot(
  x =products$total,
  main="Distribution of Total Spending",
  xlab="Total"
)




totalPercustomer<-group_by(products,customer)
totalPercustomer<-summarise(totalPercustomer,total=sum(total))
totalPercustomer


library("stats")
age <- c(50,29,37,39,30,35,60,36,55,22,55,22,25,37,23)
total<-c(824064, 829587, 772871, 794570, 819231, 825147, 901010,  831272, 932250,  893789,  869668, 841167, 820900, 857901,  900797)
dataPoints<-cbind(age,total)
colnames(dataPoints)<-c("Age(x)","Total(y)")
rownames(dataPoints)<-c("Adel","Walaa","Rania","Huda","Ahmed","Sameh","Maged","Magdy",
                        "Shimaa","Hanan","Samy","Farida","Mohamed","Sayed","Eman")

n <-as.numeric(readline("Enter the number of clusters"))
result <- kmeans(dataPoints,centers = n)
result


install.packages("gtools")
library(gtools)
n <- as.numeric(readline("Enter the min support"))
m <- as.numeric(readline("Enter the min confidence"))
install.packages("arules")
library(arules)
tdata<-read.transactions("C:/Users/LENOVO/OneDrive/grc.csv", sep=",")
class(tdata)
inspect(tdata)
apriori_rules<-apriori(tdata,
                       parameter=list(supp=min_support, conf=min_confidence ,minlen=2))
inspect(apriori_rules)