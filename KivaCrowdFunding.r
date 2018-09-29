loans <- read.csv("K:/Spring 2018/Alogrithmic Learning Theory/Project/kiva_loans.csv")
regions<-read.csv("K:/Spring 2018/Alogrithmic Learning Theory/Project/kiva_mpi_region_locations.csv")
theme<-read.csv("K:/Spring 2018/Alogrithmic Learning Theory/Project/loan_theme_ids.csv")
theme_region<-read.csv("K:/Spring 2018/Alogrithmic Learning Theory/Project/loan_themes_by_region.csv")
#Short summary of loans,regions(few of the datasets)
summary(loans)
summary(regions)
loans %>%
  mutate(use = trimws(use)) %>%
  filter(!is.na(use)) %>%
  group_by(use) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(use = reorder(use,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = use,y = Count)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = use, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Use of Loans', 
       y = 'Count', 
       title = 'Use of Loans and Count') +
  coord_flip() +
  theme_bw() 
#Making columns consistent
loans=cbind(loans,Gender1=ifelse(loans$borrower_genders=="female",1,0))
##Finding the loan mean,the funding mean and the max loan that can be given
USD<-c(round(mean(loans$loan_amount),digits=2), round(mean(loans$funded_amount),digits = 2), round(max(loans$loan_amount)))
USD<-data.frame(USD, row.names = c("Loan Mean", "Funded Mean", "Max Loan"))
kable(USD, "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 16)
loans$repayment_interval  =as.numeric(loans$repayment_interval)
#Now We measure the number of loans vs the secotrs.
options(repr.plot.width=6, repr.plot.height=4)
loan_by_sector <- loans %>%
  group_by(sector) %>% 
  summarise(loan_count =length(loan_amount))
ggplot(loan_by_sector, aes(x = reorder(sector, loan_count), y = loan_count, fill=loan_count )) + xlab("Sectors")+ylab("Number of Loans")+
  geom_bar(stat = 'identity', fill = "aquamarine2", aes(color = I('black')), size = 0.1)+coord_flip()

#Now we compare the various activities with the loan amount
loan_by_activity <- loans %>%
  group_by(activity) %>% 
  summarise(Loan_Count = length(loan_amount)) %>%
  top_n(20, wt = Loan_Count)

ggplot(loan_by_activity, aes(x = reorder(activity, Loan_Count), y = Loan_Count, fill=Loan_Count)) + 
  geom_bar(stat = 'identity',fill = "coral", aes(color = I('black')), size = 0.1)+coord_flip()+xlab("Activity")


#Now we visualize KIVA locations
world_map <- map_data("world")
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

world <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                          colour="light blue", fill="light blue")

map_data <- 
  world +
  geom_point(data=theme_region, 
             aes(x=lon, y=lat), colour="Pink", 
             fill="Yellow",pch=21, size=2, alpha=I(0.7)) + ggtitle("KIVA Locations")

map_data

#Now we analyze the Country vs the Loan_count
x<-table(loans$country)
x<-data.frame(head(sort(x, decreasing = TRUE),10))
x<-data.frame(x)
names(x)[1]<-"Countries"
names(x)[2]<-"Loans"
x %>%
  mutate(
    Loans = color_bar("lightgreen")(Loans)
  ) %>%

  kable("html", escape = F) %>%
  kable_styling("hover", full_width = F, font_size = 16)%>%
  column_spec(2,width = "50cm")
#Now we analyze the repayement interval of loans
ggplot(loans, aes(x = repayment_interval, fill = repayment_interval)) + 
  geom_bar()+
  xlab("Repayment Interval")

#Decision Trees 
dt<-loans[loans$activity,c(3,7,15,4)]
x<- 60/100
rid<-sample(1:nrow(dt),x*nrow(dt))
train<-dt[rid,]
test<-dt[-rid,]

start.time <- Sys.time()

dt_model<-rpart(activity~.,train,method = "class")
prp(dt_model)
test <- test[complete.cases(test),]
prediccion <- predict(dt_model, newdata = test[1:3],na.action = na.pass)
hpre<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediction<-hpre

end.time <- Sys.time()
dtime <- end.time - start.time
kable(head(test,10), "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 16)

test$prediction <- factor(test$prediction, levels=levels(test$activity))
#table(factor(test$activity, levels=min(test):max(test)), factor(test$prediction, levels=min(test):max(test)))
cfm<-confusionMatrix(test$prediction,test$activity)
dacc<-cfm$overall[1]
dacc

#Coorelation matrix
loans1<-loans[, sapply(loans, is.numeric)]
M <- cor(loans1)
corrplot(M, method = "number")

#Countries vs Loans
x<-table(loans$country)
x<-data.frame(head(sort(x, decreasing = TRUE),10))
x<-data.frame(x)
names(x)[1]<-"Countries"
names(x)[2]<-"Loans"
x %>%
  mutate(
    Loans = color_bar("lightgreen")(Loans)
  ) %>%
  kable("html", escape = F) %>%
  kable_styling("hover", full_width = F, font_size = 16)%>%
  column_spec(2,width = "50cm")


#K means Clustering
clusts<-loans[,c(2,16)]
clusts$activity<-as.numeric(loans$activity)
clusts$country_code<-as.numeric(loans$country_code)

x <- (nrow(clusts[,1:4])-1)*sum(apply(clusts[,1:4],2,var))
for (i in 1:8) 
  x[i] <- sum(kmeans(clusts[,1:4], centers=i)$withinss)
plot(1:8, x, type="b", xlab="Number of Groups",  ylab="Data within groups")
km<-kmeans(clusts,4)
clusts$grupoK<-km$cluster
loans$grupoK<-km$cluster
kgrupos<-data.frame(sort(table(clusts$grupoK)))
names(kgrupos)[1]<-"Cluster"
names(kgrupos)[2]<-"Count"

g1<- loans[loans$grupoK==1,c(2,3,4,5,8,15,16,19)]
g2<- loans[loans$grupoK==2,c(2,3,4,5,8,15,16,19)]
g3<- loans[loans$grupoK==3,c(2,3,4,5,8,15,16,19)]
g4<- loans[loans$grupoK==4,c(2,3,4,5,8,15,16,19)]
sm1<-summary(g1)
sm2<-summary(g2)
sm3<-summary(g3)
sm4<-summary(g4)

gs1<-data.frame()
gs1<-rbind(gs1,data.frame(Cluster="1", Mean=sm1[4,2], Max=sm1[6,2], Activity=sm1[1,3], Country=sm1[1,5], Lender_Count=sm1[4,7]))
gs1<-rbind(gs1,data.frame(Cluster="2", Mean=sm2[4,2], Max=sm2[6,2], Activity=sm2[1,3], Country=sm2[1,5], Lender_Count=sm2[4,7]))
gs1<-rbind(gs1,data.frame(Cluster="3", Mean=sm3[4,2], Max=sm3[6,2], Activity=sm3[1,3], Country=sm3[1,5], Lender_Count=sm3[4,7]))
gs1<-rbind(gs1,data.frame(Cluster="4", Mean=sm4[4,2], Max=sm4[6,2], Activity=sm4[1,3], Country=sm4[1,5], Lender_Count=sm4[4,7]))

kable(gs1, "html") %>%
  kable_styling(bootstrap_options = "striped", font_size = 12)  %>%
  add_header_above(header=c(" "=1, "Loans"=2, " "=3))
kable(kgrupos, "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 16)
plotcluster(clusts[,1:4],clusts$grupoK,  xlab="",  ylab="")

#Naive Bayes
dt<-loans[loans$activity,c(3,7,15,4)]
x<- 60/100

corte <- sample(nrow(dt),nrow(dt)*x)
train<-dt[corte,]
test<-dt[-corte,]

start.time <- Sys.time()

modelo<-naiveBayes(activity~.,data=train)
predBayes<-predict(modelo, newdata = test)
test$prediction<-predBayes

end.time <- Sys.time()
btime <- end.time - start.time

kable(head(test,10), "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 16)


cfm<-confusionMatrix(predBayes,test$activity)
bacc<-cfm$overall[1]
kable(cfm$overall, "html", col.names = "") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 16, colnames(""))
library(e1071)


#Linear Regression
dt<-loans[,c(3,7,15,4)]
dt$actnum<- as.numeric(dt$activity)
dt$ctnum<- as.numeric(dt$country_code)
data<-dt[,c(1,3,5,6)]
names(data)[3]<-"activity"
names(data)[4]<-"country_code"

plot(x=data$term_in_months,y=data$loan_amount,xlab="Term in Months",ylab="Loan Amount", main = "Term in Months vs. Loan Amount")
abline(lm(data$term_in_months ~ data$loan_amount), col="red")


x<- 60/100
rid<-sample(1:nrow(data),x*nrow(data))
train<-data[rid,]
test<-data[-rid,]

start.time <- Sys.time()

fit<-lm(loan_amount ~ ., data = train)
summary(fit)


pred<-predict(fit, newdata = test)
test$predicted<-round(pred,0)

for(i in 1:length(test$predicted)){
  x <- test$predicted[i]
  y <- test$loan_amount[i]
  if((x>=y-25) & (x<=y+25)){
    test$hit[i]<-TRUE
  } else {
    test$hit[i]<-FALSE
  }
}

end.time <- Sys.time()
rtime <- end.time - start.time


racc<-(table(test$hit)["TRUE"]/table(test$hit)["FALSE"])*100
names(racc)[1]<-"Accuracy %"
kable(racc, "html", col.names = "") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 16)

##Few more Exploratory Data Analysis

loans %>% group_by(borrower_genders, repayment_interval) %>% summarise(nr = length(borrower_genders)) %>% ungroup() %>%
  ggplot(aes(x = reorder(borrower_genders,nr), y = nr/1000, fill=repayment_interval)) +
  geom_bar(stat="identity", aes(fill=repayment_interval), colour="black") +
  coord_flip() + theme_bw(base_size = 12)  +
  labs(title="", x ="Borrower gender", y = "Number of loans (thousands)", fill="Repayment interval",
       main="Repayment interval")




loan_by_country = loans %>%
  group_by(country) %>%
  summarise(n = n(),
            mean_funded = mean(funded_amount),
            median_funded = median(funded_amount),
            mean_loan = mean(loan_amount),
            median_loan = median(loan_amount),
            mean_lender = mean(lender_count),
            median_lender = median(lender_count)) %>%
  arrange(desc(n)) %>%
  #I only want those use with top 10 amount
  head(5)

options(repr.plot.width = 8, repr.plot.height = 4)
ggplot(aes((reorder(country,n)),n),data = loan_by_country) + geom_bar(stat='identity') +
  geom_text(aes(x = country, y = 1, label = paste0("(",n,")",sep="")),hjust=0,colour = 'black',size=0.5) +
  coord_flip() + labs(x = 'Country',y = 'Count', title = 'Country Count')  +
  theme(axis.text.x=element_text(size=rel(0.5), angle=90),axis.text.y=element_text(size=rel(0.7)),aspect.ratio = 0.5)


options(repr.plot.width = 8, repr.plot.height = 4)
loan$term_in_months = as.factor(loan$term_in_months)


se<-data.frame(bacc*100, btime, row.names = "Naive Bayes", fix.empty.names =FALSE)
x<-data.frame(dacc*100, dtime, row.names = "Decision Tree", fix.empty.names =FALSE)
se<-rbind(se,x)
x<-data.frame(racc, rtime, row.names = "Lineal Regression", fix.empty.names =FALSE)
se<-rbind(se,x)


kable(se, "html", col.names =c("Accuracy", "Proccesing Time")) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 16)

#Neural Network(Just for trial/Poor output)
dt<-loans[loans$activity,c(3,7,15,4)]
x<- 60/100
rid<-sample(1:nrow(dt),x*nrow(dt))
train<-dt[rid,]
test<-dt[-rid,]

start.time <- Sys.time()

targetTrain<-as.data.frame(dummy(train$activity))
targetTest<-as.data.frame(dummy(test$activity))

modelnn <- nnet(activity~.,data = dt,subset = rid, size=2, rang=0.1, decay=5e-4, maxit=200)
pred <- as.data.frame(predict(modelnn, newdata = test))
columnaMasAlta<-apply(pred, 1, function(x) colnames(pred)[which.max(x)])
test$prednn<-columnaMasAlta

end.time <- Sys.time()
nntime <- end.time - start.time

plotnet(modelnn)

cfm<-confusionMatrix(as.factor(test$prednn),test$activity)
nnacc<-cfm$overall[1]
kable(cfm$overall, "html", col.names = "") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 16, colnames(""))




