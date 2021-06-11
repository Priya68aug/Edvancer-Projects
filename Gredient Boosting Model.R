library(dplyr)
library(kknn)
library(gbm)
library(randomForest)
library(ggplot2)

setwd("D:/")

#train and test data
hs_train=read.csv("housing_train.csv",stringsAsFactors = F)
hs_test= read.csv("housing_test.csv",stringsAsFactors = F)
hs_test$Price=NA
hs_train$data='train'
hs_test$data='test'
hs_all=rbind(hs_train,hs_test)
library(dplyr)
glimpse(hs_all)

#create dummies for method, type
#createdummies function

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
table(hs_all$Method)
hs_all = CreateDummies(hs_all,"Method")
glimpse(hs_all)
#quizquestion    
mean_h=mean(hs_all$Price[hs_all$Type=='h'],na.rm=T)
mean_h
mean_t=mean(hs_all$Price[hs_all$Type=='t'],na.rm=T)
mean_t
mean_h-mean_t
mean_u=mean(hs_all$Price[hs_all$Type=='u'],na.rm =T)
mean_u
table(hs_all$Type)
hs_all = CreateDummies(hs_all,"Type")
glimpse(hs_all)

#removing that unwanted values 
hs_all = hs_all %>% 
  select(-Suburb,-Address,-SellerG,-CouncilArea)
glimpse(h_all)

#find the NAs and remove it

lapply(hs_all,function(x) sum(is.na(x)))
for(col in names(hs_all)){
  
  if(sum(is.na(hs_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    hs_all[is.na(hs_all[,col]),col]=mean(hs_all[hs_all$data=='train',col],na.rm=T)
  }
}
glimpse(hs_all)
## separate train and test

hs_train=hs_all %>% filter(data=='train') %>% select(-data)
hs_test=hs_all %>% filter(data=='test') %>% select(-data,-Price)

#taking 80%train data
set.seed(2)
s=sample(1:nrow(hs_train),0.8*nrow(hs_train))
hs_train1=hs_train[s,]
hs_train2=hs_train[-s,]


glimpse(hs_train1)

#using simple linear regression
fit=lm(Price~.,data =hs_train1)
summary(fit)
library(car)

sort(vif(fit),decreasing = T)[1:3]

fit=lm(Price~.-Method_S,data=hs_train1)

hs_train2.pred=predict(fit,newdata =hs_train2)
(hs_train2.pred-hs_train1$Price)**2%>%mean()%>%sort()
sort(vif(fit),decreasing = T)

fit=step(fit)

fit=lm(Price ~(Rooms + Distance + Postcode  + Landsize + BuildingArea + YearBuilt + Method_VB + Method_SP + 
                 Method_PI + Method_S + Type_u + Type_h) )








#using GBM model 
library(gbm)
install.packages(lattice)
library(lattice)
library(cvTools)

param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))

## ------------------------------------------------------------------------
subset_paras=functio8y5tthjm0n(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=10
my_params=subset_paras(param,num_trials)
myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(gbm,Price~.,
             data =hs_train1,
             tuning =params,
             args = list(distribution="gaussian"),
             folds = cvFolds(nrow(h_train1), K=10, type = "random"),
             seed =2,
             predictArgs = list(n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    myerror=score.this
    print(myerror)
    best_params=params
  }
  
  print('DONE')
  
}

best_params=data.frame(interaction.depth=7,
                       n.trees=500,
                       shrinkage=0.1,
                       n.minobsnode=1)

myerror
best_params
hs_train1.gbm.final=gbm(Price~.,data=hs_train1,
                        n.trees = best_params$n.trees,
                        n.minobsinnode = best_params$n.minobsnode,
                        shrinkage = best_params$shrinkage,
                        interaction.depth = best_params$interaction.depth,
                        distribution = "gaussian")

test.pred=predict(hs_train1.gbm.final,newdata=hs_test,n.trees = best_params$n.trees)
write.csv(test.pred,"Priya_Dangle_P1_part2.csv.csv",row.names = F)
