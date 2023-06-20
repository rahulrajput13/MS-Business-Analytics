## HOMEWORK 1

install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("gmodels")
install.packages("tidyr")
install.packages("corrplot")
install.packages("ggcorrplot")

## Answer 1
#parta
dow = read.csv("Dow.csv")
dow

#removing comma and converting character series into numeric series and finally into time series

dow$Closing.Values<-gsub(",","",as.character(dow$Closing.Values))
dow$Closing.Values = as.numeric(dow$Closing.Values)
dowts = ts(dow$Closing.Values, frequency=1, start=1)
dowts
class(dow$Closing.Values)

dowtslag <- stats::lag(dowts, -1)
dowtslag

#then finding growth factors

growthfactor <- dowts/dowtslag
growthfactor

#finding geometric mean and calculating avg returns from that
library(psych)
avgreturn = 100*(geometric.mean(growthfactor)-1)

sprintf("The average Dow returns over the period have been %.2f%%.", avgreturn)

#partb

histdata = (growthfactor-1)*100
hist(histdata)

sprintf("The returns are not normally distributed but left skewed")

#partc
stddev = sd(histdata)
meandata = mean(histdata)
TotalSampleSize = length(histdata)

One_SD_Range_Lower = meandata - 1*stddev
One_SD_Range_Upper = meandata + 1*stddev

Num_Within_One_SD = length(which(histdata >= One_SD_Range_Lower & histdata <= One_SD_Range_Upper))
Percent_Within_One_SD = 100*(Num_Within_One_SD/TotalSampleSize)
Percent_Within_One_SD

Two_SD_Range_Lower = meandata - 2*stddev
Two_SD_Range_Upper = meandata + 2*stddev

Num_Within_Two_SD = length(which(histdata >= Two_SD_Range_Lower & histdata <= Two_SD_Range_Upper))
Percent_Within_Two_SD = 100*(Num_Within_Two_SD/TotalSampleSize)
Percent_Within_Two_SD

sprintf("Percentage of data within the first standard deviation is %.2f%%",Percent_Within_One_SD)
sprintf("Percentage of data within the second standard deviation is %.2f%%",Percent_Within_Two_SD)

sprintf("The data can be approximated as a normal distribution but it is left skewed. The percentage of data under one and two SD's respectively follow the empirical rule of 68%% and 95%% closely.")

#partd
boxplot(histdata,horizontal=TRUE)
fivenum(histdata, na.rm=TRUE)

coeffofvar = stddev/meandata
coeffofvar

sprintf("50%% of the values of returns are quite close to the mean and only vary by roughly 2.3%%. The data has a lot of outliers.")

#parte

dow["returns"] = 0
dow["returns"] = dow["returns"] + lag(as.numeric(histdata),1)
dow

histdata

Q1 <- quantile(histdata, 0.25) ## First quartile
Q2 <- quantile(histdata, 0.50) ## Second quartile
Q3 <- quantile(histdata, 0.75) ## Third quartile
Q1
Q2
Q3
IQR1 <- IQR(histdata, na.rm = TRUE) ## IQR
IQR1
returns = data.frame(histdata)
#making a function to identify outliers
outlier = with(returns,
  ifelse(((returns>Q3+1.5*IQR)|(returns<Q1-1.5*IQR))&((returns<Q3+3*IQR)|(returns>Q1-3*IQR)),"Mild Outlier",
         ifelse(((returns>Q3+3*IQR)|(returns<Q1-3*IQR)),"Extreme",
                "Normal")))

returns["outliers"] = outlier
returns

table(returns$outliers)

sprintf("There are only 23 months where the Dow returns can be classified as being outliers out of a total of 859 returns")


## Answer 2

#Data collected for 100 in-store credit card transactions while promotion running
#discount coupon used - promotional customers

#parta

melvyl = read.csv("Melvyl.csv")
melvyl

melvyl_df = as.data.frame(melvyl)
melvyl_df

hist(melvyl$Items)
hist(melvyl$Net.Sales)

library(dplyr)
table(melvyl['Method.of.Payment'])
barplot(table(melvyl['Method.of.Payment']))

table(melvyl['Gender'])
barplot(table(melvyl['Gender']))

table(melvyl['Marital.Status'])
barplot(table(melvyl['Marital.Status']))

table(melvyl['Age'])
barplot(table(melvyl['Age']))

library(dplyr)
library(tidyr)

melvyl_df

#partb

library(dplyr)
crosscustomersales = melvyl_df %>%
  group_by(Type.of.Customer) %>%
  summarise(sum=sum(Net.Sales))
crosscustomersales

sprintf("At a macro level it seems as if the offer was hugely successful as the promotional customers bought items worth 3 times more than regular customers")

#partc

descriptive = function(x) {
  c(Count = length(x[!is.na(x)]),
    Count_NA = length(x[is.na(x)]),
    Mean = round(mean(x, na.rm = TRUE), digits = 2),
    Median = round(median(x, na.rm = TRUE), digits = 2),
    Min = round(min(x, na.rm = TRUE), digits = 2),
    Max = round(max(x, na.rm = TRUE), digits = 2),
    Range = round((max(x, na.rm = TRUE) - min(x, na.rm = TRUE)), digits = 2),
    Variance = round(var(x, na.rm = TRUE), digits = 2),
    StdDev = round(sd(x, na.rm = TRUE), digits = 2),
    CV_in_Percent = round(((sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))*100), digits = 2))
}

netsales = descriptive(melvyl$Net.Sales)
netsales

#boxplot for net sales

boxplot(melvyl$Net.Sales,horizontal=TRUE)

sprintf("the outliers are mostly for large purchases rather than very small ones")

netsalespromo = descriptive(melvyl$Net.Sales[which(melvyl$Type.of.Customer=="Promotional")])
netsalespromo

netsalesregular = descriptive(melvyl$Net.Sales[which(melvyl$Type.of.Customer=="Regular")])
netsalesregular

boxlabels = c("netsales","netsalespromo","netsalesregular")
boxplot(melvyl$Net.Sales,melvyl$Net.Sales[which(melvyl$Type.of.Customer=="Promotional")],melvyl$Net.Sales[which(melvyl$Type.of.Customer=="Regular")],horizontal=TRUE,names=boxlabels)

sprintf("It is clear from the boxplot that the customers attracted by the promotional offers made several large purchases as compared to the regular customers, although the means are not too different")

netsalesbypayment = melvyl %>%
  group_by(Method.of.Payment) %>%
  summarise(sum=sum(Net.Sales))
netsalesbypayment

barplot(netsalesbypayment$sum,names.arg = c("American Express","Discover","Mastercard","Proprietary","Visa"))

netsalesbygender = melvyl %>%
  group_by(Gender) %>%
  summarise(sum=sum(Net.Sales))
netsalesbygender

slices = c(7365,395)
lbls = c("Female","Male")
pie(slices,labels=lbls)

netsalesfemale = descriptive(melvyl$Net.Sales[which(melvyl$Gender=="Female")])
netsalesfemale

netsalesmale = descriptive(melvyl$Net.Sales[which(melvyl$Gender=="Male")])
netsalesmale

sprintf("93% of purchases were done by women and the average puchase by a woman was 23 dollars higher than men")

typeofcustomerbygender = xtabs(~Gender+Type.of.Customer, data=melvyl)
typeofcustomerbygender

sprintf("66 out of the 70 promotional cutomers were women")

# partd - Relation between age and sales
partd = data.frame(melvyl$Age,melvyl$Net.Sales)
partd

summaryagesales1 = partd%>%
  group_by(melvyl.Age)%>%
  summarise(avg=mean(melvyl.Net.Sales))
summaryagesales1

summaryagesales2 = partd%>%
  group_by(melvyl.Age)%>%
  summarise(med=median(melvyl.Net.Sales))
summaryagesales2

summaryagesales3 = partd%>%
  group_by(melvyl.Age)%>%
  summarise(sdev=sd(melvyl.Net.Sales))
summaryagesales3

summaryagesales4 <- merge(summaryagesales1,summaryagesales2,by='melvyl.Age')
summaryagesales4

summaryagefinal = merge(summaryagesales4,summaryagesales3,by='melvyl.Age')
summaryagefinal

## ANSWER 3

supermarket = read.csv("SuperMarketTransactions.csv")
supermarket

#part1

supermarket$Revenue<-gsub("\\$","",as.character(supermarket$Revenue))
supermarket$Revenue = as.numeric(supermarket$Revenue)

boxplot(supermarket$Revenue~supermarket$State.or.Province, data = supermarket, horizontal=TRUE)

sprintf("The distributions are mostly symmetric with the exception of one state")

#part2

part2 = data_frame(supermarket) %>%
  filter(supermarket$Country == "USA")

boxplot(part2$Revenue~part2$State.or.Province, data=part2, horizontal=TRUE)

#part3
library(dplyr)

part3df = data_frame(supermarket$State.or.Province,supermarket$Revenue)
part3df

part3 = part3df %>%
  group_by(`supermarket$State.or.Province`)%>%
  summarise(totalrev=sum(`supermarket$Revenue`))
part3

#part4
part4df = data_frame(supermarket$Product.Category, supermarket$Revenue)
part4df

part4 = part4df %>%
  group_by(`supermarket$Product.Category`) %>%
  summarise(totalrev=sum(`supermarket$Revenue`))
part4

#part5
part5 = supermarket %>%
  group_by(State.or.Province) %>%
  summarise(Transaction = n()) %>%
  arrange(desc(Transaction))
part5

#part6
part6 = supermarket %>%
  group_by(Children) %>%
  summarise(Transaction=n())
part6

morethanone = ((2839+2893+2826+1439)/(1344+2718+2839+2893+2826+1439))*100
sprintf("proportion of shoppers with more than one child is %.f%%",morethanone)

#part7
class(supermarket$Purchase.Date)

supermarket$Purchase.Date = as.Date(supermarket$Purchase.Date,"%m/%d/%Y")
supermarket["month"] = format(supermarket$Purchase.Date,"%m")
supermarket["year"] = format(supermarket$Purchase.Date,"%Y")

supermarket

part7 = supermarket %>%
  filter(supermarket$year=="2017")

part71 = part7 %>%
  filter(part7$month=="01")

part7jan = part71%>%
  summarize(total=sum(part71$Revenue))
part7jan

part72 = part7 %>%
  filter(part7$month=="02")

part7feb = part72%>%
  summarize(total=sum(part72$Revenue))
part7feb

finalrevenue = rbind(part7jan,part7feb)

rownames(finalrevenue) = c("Jan", "Feb","Total")
colnames(finalrevenue) = c("Revenue")
finalrevenue

total = c(7673.05)
finalrevenue = rbind(finalrevenue,total)

finalrevenue

#part8
crossgenderproduct = xtabs(~Gender+Product.Family, data=supermarket)
crossgenderproduct

#part9
part9 = supermarket %>%
  filter(Marital.Status=="S") %>%
  filter(Homeowner=="Y")
part9

proportion_of_single_homeowners = 100*(length(part9$Transaction)/length(supermarket$Transaction))
proportion_of_single_homeowners*100

#part10
part10 = supermarket %>%
  group_by(Gender) %>%
  summarise(Transaction=n())

part10["percentage"] = c(7170*100/(7170+6889),6889*100/(7170+6889))
part10


##ANSWER 4

cellphone = read.csv("CellphoneMarket.csv")
cellphone

summary(cellphone)

cellphone["churnnumeric"] <- with(cellphone, 
                                  cellphone$Churn. <- ifelse(cellphone$Churn. == "Yes", 
                                                             1, 0))
cellphone["intplannumeric"] <- with(cellphone,
                                    cellphone$International.Plan <- ifelse(cellphone$International.Plan == "yes",
                                                                           1, 0))
cellphone["voicemailplannumeric"]<- with(cellphone,
                                         cellphone$Voice.Mail.Plan <- ifelse(cellphone$Voice.Mail.Plan == "yes",
                                                                             1, 0))

cellphone

table(cellphone$International.Plan)
table(cellphone$Voice.Mail.Plan)
table(cellphone$Churn)
table(cellphone$Account.Length)

boxplot(cellphone$Account.Length, horizontal = TRUE)
hist(cellphone$intplannumeric)
hist(cellphone$churnnumeric)
hist(cellphone$Customer.Service.Calls)
hist(cellphone$Account.Length)

sprintf("The account lengths of customers are distributed normally")
sprint("Roughly 14% of the customers shifted from the cellphone company")
class(cellphone)

cellphone1=cellphone
finaldf = cellphone1 %>% 
  select(-"International.Plan")%>% 
  select(-"Voice.Mail.Plan")%>% 
  select(-"Churn.")
finaldf

cor(cellphone$Account.Length,cellphone$churnnumeric)
cor(cellphone$Customer.Service.Calls,cellphone$churnnumeric)
cor(cellphone$Evening.Charge,cellphone$Evening.Minutes)
cor(cellphone$Day.Charge,cellphone$Day.Minutes)
cor(cellphone$International.Minutes,cellphone$International.Charge)
cor(cellphone$intplannumeric,cellphone$churnnumeric)

cor(finaldf)

data_cor <- cor(finaldf[ , colnames(finaldf) != "churnnumeric"], 
                finaldf$churnnumeric)
data_cor 

library(ggcorrplot)
ggcorrplot(data_cor)

data_cor_custservice = cor(finaldf[ , colnames(finaldf) != "Customer.Service.Calls"], 
                           finaldf$Customer.Service.Calls)
ggcorrplot(data_cor_custservice)

data_cor_intplannumeric = cor(finaldf[ , colnames(finaldf) != "intplannumeric"], 
                              finaldf$intplannumeric)
ggcorrplot(data_cor_intplannumeric)


##Answer 5

#parta

probsexpand = c(0.5,0.25,0.25)
npvexpand = c(40,15,-20)

expectedexpand = (sum(npvexpand*probsexpand))
expectedexpand

#partb

probshome = c(0.20,0.50,0.30)
npvhome = c(140,15,-35)

expectedhome = sum(npvhome*probshome)

varexpand = (npvexpand-expectedexpand)^2
varhome = (npvhome-expectedhome)^2

varianceexpand = sum(varexpand*probsexpand)
stddevexpand = sqrt(varianceexpand)
variancehome = sum(varhome*probshome)
stddevhome = sqrt(variancehome)

varianceexpand
stddevexpand
variancehome
stddevhome

#parte
expectedhome
expectedexpand

sprintf("The Expected Value of Project B has a higher value")

#partf&g
coeffexpand = stddevexpand/expectedexpand
coeffhome = stddevhome/expectedhome

coeffexpand
coeffhome

sprintf("Project B, despite having a higher Expected value is the more riskier option as it has a very high standard deviation and a highe coefficient of variance than Project A implying that the value would be more volatile")


##ANSWER 6

#binomial distr, 4 trials, prob of success = 0.27, prob of failure = 0.73

#parta
#function to use - dbinom

probsuccess = c(0.27,0.73*0.27,0.73*0.73*0.27,0.73*0.73*0.73*0.27)
moneyspent = c(500,1000,1500,2000)

probmodel = cbind(moneyspent,probsuccess)
probmodel

#partb
expectedvalue = 4*0.27
expectedvalue

#partc

situationprobs = c(0.27,0.73*0.27,0.73*0.73*0.27,0.73^3*0.27,0.73^4)
moneyspent = c(500,1000,1500,2000,9500)

expectedexpense = sum(situationprobs*moneyspent)
expectedexpense

##ANSWER 7

#parta
dbinom(10,20,0.4)
#partb
pbinom(10,20,0.4)
pbinom(10,20,0.4,lower.tail = TRUE)
#partc 
1-pbinom(14,20,0.4)
pbinom(14,20,0.4,lower.tail=FALSE)


##ANSWER 8
#Poisson distribution with a mean of 400calls/day over 16 hrs or 400/16 calls/hr

#parta
meanhour = 400/16
meanhalf = 400/32
meanquarter = 400/64

#partb
dpois(6,meanquarter)

#partc
dpois(0,meanquarter)

#partd
1-ppois(2,meanquarter)
ppois(1,meanquarter,lower.tail=FALSE)


##ANSWER9
#50 trials, 0.05*1000 = 50, therefore we have to use Hypergeometric distribution
#
actualfraction = c(0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18)

#calculating probabilities of batch passing

calcprob = function(x){
  phyper(4,x*1000,(1-x)*1000,50,lower.tail = TRUE)
}
probs = calcprob(actualfraction)
probs

barplot(probs,names.arg=actualfraction)

calcprob5 = function(x){
  phyper(5,x*1000,(1-x)*1000,50,lower.tail = TRUE)
}

probsnew = calcprob5(actualfraction)
barplot(probsnew,names.arg=actualfraction)

combined = cbind(probs,probsnew)
barplot(combined,beside=T)

sprintf("the barplot for the second case, 5 acceptable failures, is less right skewed compared to the first case. Overall the probabilities of the batch passing the random check go up for each case")

probs
probsnew

##ANSWER10

#Poisson distribution

demand = c(0,1,2,3,4,5,6,7,8,9)
probdemand = c(0.05,0.05,0.08,0.16,0.30,0.16,0.10,0.05,0.05,0)
# 5 window acs are currently stocked

X = c(5,4,3,2,1,0,-1,-2,-3,-4)
probX = c(0.05,0.05,0.08,0.16,0.30,0.16,0.10,0.05,0.05,0.00)
barplot(probX,names.arg=X)

Y = c(0,0,0,0,0,0,1,2,3,4)
probY = c(0.05,0.05,0.08,0.16,0.30,0.16,0.10,0.05,0.05,0)
barplot(probY,names.arg=Y)

EX = sum(X*probX)
EY = sum(Y*probY)
EX
EY

profit = 60*X
loss = -20*Y

earning = c(0,60,120,180,240,300,340,380,420,460)

expectedprofit = sum(earning*probdemand)
expectedprofit













