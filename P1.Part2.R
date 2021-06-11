
setwd("D:/")

ld_train=read.csv("D:/Data Science/R Module/R.csv/housing_train.csv",stringsAsFactors = F)

ld_test=read.csv("D:/Data Science/R Module/R.csv/housing_test.csv",stringsAsFactors = F)

ld_test$Price=NA

ld_train$data='train'
ld_test$data='test'

ld_all=rbind(ld_train,ld_test)

library(dplyr)

glimpse(ld_all)

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

glimpse(ld_all)

ld_all=CreateDummies(ld_all ,"Method")
ld_all=CreateDummies(ld_all,"Type")
glimpse(ld_all)

#### Removing unwanted Variables...

ld_all = ld_all %>%
  select(-Suburb,-Address,-CouncilArea,-SellerG)
glimpse(ld_all)


### Finding the NA's Value..and removing it....

lapply(ld_all,function(x) sum(is.na(x)))

#lapply(ld_all,function(x) length(unique(x)))


for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    ld_all[is.na(ld_all[,col]),col] = mean(ld_all[ld_all$data=='train',col],na.rm=T)
  }
  
}
lapply(ld_all,function(x) sum(is.na(x)))
glimpse(ld_all)
glimpse(ld_train)



## separate train and test

ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Price)

##Breaking the train data into 80-20%....

set.seed(2)
s=sample(1:nrow(ld_train),0.8*nrow(ld_train))
ld_train1=ld_train[s,]
ld_train2=ld_train[-s,]
library(car)

fit=lm(Price~., data= ld_train1)
summary(fit)


sort(vif(fit),decreasing = T)[1:3]


# we'll take vif cutoff as 5



fit=lm(Price~.-Method_S,data=ld_train1)
sort(vif(fit),decreasing = T)[1:3]

ld_train2.pred=predict(fit,newdata = ld_train2)
(ld_train2.pred-ld_train1$Price)**2%>%mean()%>%sort()
sort(vif(fit),decreasing = T)

fit=step(fit)

## AIC score 

formula(fit)

fit=lm(Price ~(Rooms + Distance + Postcode  + Landsize + BuildingArea + YearBuilt + Method_VB + Method_SP + 
                 Method_PI + Method_S + Type_u + Type_h) )


##Using GBM MOdel
library(gbm)
library(lattice)       
library(cvTools)

param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))
-----------------------------------------------------
  subset_paras=functio8y5tthjm0n(full_list_para,n=10){
    all_comb=expand.grid(full_list_para)
    s=sample(1:nrow(all_comb),n)
    subset_para=all_comb[s,]
    return(subset_para)
  }
num_trails=10
my_params=subset_paras(param,num_trails)
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




