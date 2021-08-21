

hd_train=read.csv("C:/Users/sr/Downloads/housing_train.csv")
hd_test=read.csv("C:/Users/sr/Downloads/housing_test.csv")
View(hd_train)
View(hd_test)
hd_test$Price=NA
hd_train$data="train"
hd_test$data="test"
hd_all=rbind(hd_train,hd_test)
View(hd_all)
library(dplyr)

hd_all=hd_all %>% select(-Postcode,-Address)

#data prep
library(dplyr)
library(tidyr)
glimpse(hd_all)



CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    2
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}
char_cols=names(hd_all)[sapply(hd_all, function(x) is.character(x))]
char_cols=c("Suburb","Type","Method","SellerG","CouncilArea","data" )
char_cols=char_cols[!(char_cols %in% c("data","Price"))]
char_cols=c("Suburb","Type","Method","SellerG","CouncilArea")

for(cat in char_cols){
  hd_all=CreateDummies(hd_all,cat,50)
}
hd_all$YearBuilt_NA_FLAG=as.numeric(is.na(hd_all$YearBuilt))

hd_all=hd_all[!((is.na(hd_all$Price)) & hd_all$data=='train'), ]

for(col in names(hd_all)){
  if(sum(is.na(hd_all[,col]))>0 & !(col %in% c("data","Price"))){
    hd_all[is.na(hd_all[,col]),col]=mean(hd_all[hd_all$data=='train',col],na.rm=T)
  }
}
glimpse(hd_all)


#-------------------------------
hd_train=hd_all %>% filter(data=="train") %>% select(-data)
hd_test=hd_all %>% filter(data=="test") %>% select(-data,-Price)

sum(is.na(hd_test))




s=sample(1:nrow(hd_all),0.7*nrow(hd_all))
hd_train1=hd_train[s,]
hd_train2=hd_train[-s,] 

#----------------------------------Linear Regression---------------------
#	The aim is to building regression models which will predict the continuous 
#price for each property 

library(car)
fit=lm(Price~.-CouncilArea_-Distance,,data=hd_train1)
vif(fit)
sort(vif(fit),decreasing=TRUE)[1:3]

fit=step(fit)


summary(fit)
formula(fit)
fit=lm(formula(fit),data=hd_train1)
train_predictions=predict(fit,newdata = hd_train2)
train_predictions
rmse=mean(hd_train2$Price-train_predictions)^2 %>% sqrt()
rmse
library(ggplot2)
fit.final=lm(Price~.,data = hd_train)
fit_final=step(fit.final)
test_pred=predict(fit.final,newdata = hd_test)
test_pred
#-------------------------Decision Trees-----------------------------------------------------------
library(tree)
hd.tree=tree(Price~.,data = hd_train1)
hd.tree
plot(hd.tree)
text(hd.tree)
val.price=predict(hd.tree,newdata = hd_train2)
val.price

rmse=((val.price)-(hd_train2$Price))^2 %>% mean() %>% sqrt()
rmse_val

hd.tree.final=tree(Price~.,data=hd_train)
test_price=predict(hd.tree.final,newdata=hd_test)
test_price
#-------------------------------Random Forest----------------------------------
param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}


num_trails=20
my_params=subset_paras(param,num_trails)
my_params

library(randomForest)
library(cvTools)

myerror=9999999

for(i in 1:num_trails){
  print(paste0('startingiteration:',i))
  params=my_params[i,]
  
  K=cvTuning(randomForest,Price~.,data=hd_train,
             tuning=params,
             folds = cvFolds(nrow(hd_train),K=10,type='random'),seed=2)
  
  score.this=K$cv[,2]
  
  if(score.this<myerror){
    print(params)
    myerror=score.this
    print(myerror)
    best_params=params
  }
  print("done")
}
myerror
best_params

hd.rf.final=randomForest(Price~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,data=hd_train)
test.pred=predict(hd.rf.final,newdata = hd_test)
test.pred

#oob error
randomForest(formula=Price~.,data=hd_train,mtry=best_params$mtry,
             ntree=best_params$ntree,
             maxnodes=best_params$maxnodes,
             nodesize=best_params$nodesize)



#variable importence
d=importance(hd.rf.final)
d=as.data.frame(d)
d
d$variableName=rownames(d)
d %>% arrange(desc(IncNodePurity))
varImpPlot(hd.rf.final)

#partial dependent plot
var='Distance'
pred.resp=predict(hd.rf.final,newdata = hd_train)

myvar=hd_train[,var]
trend.data=data.frame(response=pred.resp,myvar=myvar)
library(ggplot2)
trend.data %>% ggplot(aes(y=response,x=myvar))+geom_smooth()

