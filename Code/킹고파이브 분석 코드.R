# Frequently used manual function -------------------------------------------------------
datacleanser <- function(inputdata,glmlasso_select=FALSE,compound_add=FALSE){
  cleaned <- inputdata
  cleaned_lassocopy <- inputdata
  # money scaling
  scaling_column <- function(input_data,column_num,datasource){
    if(datasource=='sci') multiplier <- 1000
    if(datasource=='hanhwaincome') multiplier <- 10000
    handlee <- input_data[,column_num]
    handlee[handlee==0] <- 2
    handlee[handlee%%10==1] <- handlee[handlee%%10==1]-1
    handlee[handlee==0] <- 1
    handlee[handlee==2] <- 0
    return(as.integer(handlee*multiplier))
  }
  
  for(i in c(7:10,16,18,19,24)){
    if(i<18){
      cleaned[,i] <- scaling_column(cleaned,i,'sci')
    } else {
      cleaned[,i] <- scaling_column(cleaned,i,'hanhwaincome')
    }
  }
  for(i in c(7:10,16,18,19,24)){
    if(i<18){
      cleaned_lassocopy[,i] <- scaling_column(cleaned_lassocopy,i,'sci')
    } else {
      cleaned_lassocopy[,i] <- scaling_column(cleaned_lassocopy,i,'hanhwaincome')
    }
  }
  
  # total loan amount&total loan count
  index3456 <- (inputdata[,3]==0)+(inputdata[,4]==0)+(inputdata[,5]==0)+(inputdata[,6]==0)
  cleaned[index3456==4,7] <- 0
  cleaned[index3456==4,8] <- 0
  
  # max_payment_insuarance
  index44 <- inputdata[,44]==0
  cleaned[index44,34] <- 0
  cleaned[index44,35] <- 0
  cleaned[index44,40] <- 0
  cleaned[index44,41] <- 0
  
  # insurance request&received
  index51 <- inputdata[,51]==0
  cleaned[index51,50] <- 0
  
  # latest child
  index22 <- as.character(inputdata[,22])
  cleaner22 <- rep(0,nrow(inputdata))
  cleaner22[index22!='0'&index22!='NULL'] <- 1
  cleaned[,22] <- as.factor(cleaner22)
  
  # last 1year payment delay ratio
  character35 <- as.character(inputdata[,35])
  space <- rep(0,nrow(cleaned))
  space[character35=='10미만'] <- 5
  space[character35=='20미만'] <- 15
  space[character35=='30미만'] <- 25
  space[character35=='40미만'] <- 35
  space[character35=='50미만'] <- 45
  space[character35=='60미만'] <- 55
  space[character35=='90미만'] <- 75
  space[character35=='90이상'] <- 95
  cleaned[,35] <- space

  character53 <- as.character(inputdata[,53])
  character53[character53=='*'] <- 0
  cleaned[,53] <- as.integer(character53)
  
  # date cleanser
  TEL_CNTT_QTR_clean = function(data1) {
    index=(data1$TEL_CNTT_QTR !=0)
    data1$TEL_CNTT_QTR  = as.character(data1$TEL_CNTT_QTR )
    sav=data1$TEL_CNTT_QTR [index]
    year=substr(sav,1,4)
    month=substr(sav,5,5)
    yy=2016-as.integer(year)
    mm=6-(as.integer(month)*3)
    dist=12*yy+(mm)
    data1$TEL_CNTT_QTR [index]=dist
    data1$TEL_CNTT_QTR  = as.integer(data1$TEL_CNTT_QTR )
    return (data1)
  }
  cleaned <- TEL_CNTT_QTR_clean(cleaned)
  
  min_cntt_date_clean = function(data1){
    index=(data1$MIN_CNTT_DATE!=0)
    data1$MIN_CNTT_DATE = as.character(data1$MIN_CNTT_DATE)
    sav=data1$MIN_CNTT_DATE[index]
    year=substr(sav,1,4)
    month=substr(sav,5,6)
    yy=2016-as.integer(year)
    mm=6-as.integer(month)
    dist=12*yy+mm
    data1$MIN_CNTT_DATE[index]=dist
    data1$MIN_CNTT_DATE = as.integer(data1$MIN_CNTT_DATE)
    return (data1)
  }
  cleaned <- min_cntt_date_clean(cleaned)
  
  # glmlasso_select ---------------------------------------------------------
  if(glmlasso_select){
    selected <- c('CPT_LNIF_CNT','SPART_LNIF_CNT','TOT_LNIF_AMT','BNK_LNIF_AMT','CRDT_OCCR_MDIF',
                  'CRDT_CARD_CNT','CTCD_OCCR_MDIF','CB_GUIF_CNT','CRLN_OVDU_RATE','MIN_CNTT_DATE',
                  'LT1Y_PEOD_RATE','LT1Y_CTLT_CNT','MOBL_FATY_PRC','TLFE_UNPD_CNT','AGE',
                  'LT1Y_MXOD_AMT','PAYM_METD','LINE_STUS','MOBL_PRIN')
    
    index_saver <- c()
    for(i in 1:length(selected)){
      if(selected[i] %in% names(cleaned)){
        index_saver <- c(index_saver,which(names(cleaned)==selected[i]))
      }
    }
    cleaned <- cleaned[,c(1,2,index_saver)]
  }
  
  # compound_add ------------------------------------------------------------
  if(compound_add){
    
    revalue_column_conttable <- function(mydata,target,ngroupA,label.A='A',label.B='B'){
      target_index <- ifelse(is.numeric(target),target,which(names(mydata)==target))
      groupA <- levels(factor(mydata[,target_index]))[1:ngroupA]
      storage <- mydata[,target_index]
      storage[storage %in% groupA] <- label.A
      storage[storage!='A'] <- label.B
      # direct indexing gurantees best performance in such ifelse setting.
      return(storage)
    }
    compound_2var <- function(data,index1,index2,ngroupA1,ngroupA2){
      if(class(ngroupA1)=='character'){
        ngroupA1 <- which(as.numeric(as.character(levels(factor(data[,index1]))))==as.numeric(ngroupA1))
      }
      if(class(ngroupA2)=='character'){
        ngroupA2 <- which(as.numeric(as.character(levels(factor(data[,index2]))))==as.numeric(ngroupA2))
      }
      revalue1 <- revalue_column_conttable(data,index1,ngroupA1)
      revalue2 <- revalue_column_conttable(data,index2,ngroupA2)
      correlator <- rep(0,length(revalue1))
      correlator[revalue1=='B'] <- 2
      correlator[revalue2=='B'] <- correlator[revalue2=='B']+1
      return(correlator)
    }
    compound3456 <- function(original_data){
      
      subsetted_factored <- as.data.frame(
        cbind(revalue_column_conttable(original_data,3,1),
              revalue_column_conttable(original_data,4,1),
              revalue_column_conttable(original_data,5,2),
              revalue_column_conttable(original_data,6,2))
      )
      # 사용할 데이터를 만드는 과정
      
      category_3456 <- rep(0,nrow(subsetted_factored))
      category_3456[subsetted_factored[,1]=='B'] <- category_3456[subsetted_factored[,1]=='B']+1
      category_3456[subsetted_factored[,2]=='B'] <- category_3456[subsetted_factored[,2]=='B']+2
      category_3456[subsetted_factored[,3]=='B'] <- category_3456[subsetted_factored[,3]=='B']+4
      category_3456[subsetted_factored[,4]=='B'] <- category_3456[subsetted_factored[,4]=='B']+8
      # 이 과정을 통해 2*2*2*2=16가지의 경우의 수가 겹치지 않고 0부터 15까지의 수를 배정받게 된다.
      # 이렇게 하면 16줄을 4줄만에 코딩할 수가 있어서 좋다.
      
      category_3456[category_3456==0] <- 'A'
      category_3456[category_3456==1] <- 'A'
      category_3456[category_3456==2] <- 'B'
      category_3456[category_3456==3] <- 'A'
      category_3456[category_3456==4] <- 'B'
      category_3456[category_3456==5] <- 'B'
      category_3456[category_3456==6] <- 'B'
      category_3456[category_3456==7] <- 'B'
      category_3456[category_3456==8] <- 'C'
      category_3456[category_3456==9] <- 'B'
      category_3456[category_3456==10] <- 'B'
      category_3456[category_3456==11] <- 'B'
      category_3456[category_3456==12] <- 'C'
      category_3456[category_3456==13] <- 'C'
      category_3456[category_3456==14] <- 'C'
      category_3456[category_3456==15] <- 'C'
      
      return(category_3456)
    }
    
    # 생성 ----------------------------------------------------------------------
    comp3456 <- compound3456(inputdata)
    
    interact0709 <- compound_2var(inputdata,7,9,8,5)
    interact0709[interact0709==0] <- 'A'
    interact0709[interact0709==1] <- 'B'
    interact0709[interact0709==2] <- 'C'
    interact0709[interact0709==3] <- 'D'
    
    critical1112 <- compound_2var(inputdata,11,12,2,2)
    critical1112[critical1112==0] <- 'A'
    critical1112[critical1112==1] <- 'B'
    critical1112[critical1112==2] <- 'C'
    critical1112[critical1112==3] <- 'D'
    
    fail49 <- revalue_column_conttable(inputdata,49,1)
    full52 <- revalue_column_conttable(inputdata,52,1)
    interact4952 <- rep(0,nrow(inputdata))
    interact4952[fail49=='B'] <- 2
    interact4952[full52=='A'] <- interact4952[full52=='A']+1
    interact4952[interact4952==0] <- 'A'
    interact4952[interact4952==1] <- 'B'
    interact4952[interact4952==2] <- 'C'
    interact4952[interact4952==3] <- 'D'
    
    
    interact6466 <- rep(1,nrow(inputdata))
    interact6466[inputdata[,64]==0&inputdata[,66]==0] <- 0
    interact6466[inputdata[,64]!=0&inputdata[,66]!=0] <- 2
    interact6466[interact6466==0] <- 'A'
    interact6466[interact6466==1] <- 'B'
    interact6466[interact6466==2] <- 'C'
    
    # middle_loan_ratio
    nan_handler1 <- inputdata$TOT_LNIF_AMT==0
    middle_loan_ratio <- cleaned_lassocopy$CPT_LNIF_AMT/cleaned_lassocopy$TOT_LNIF_AMT
    middle_loan_ratio[nan_handler1] <- 0
    rm('nan_handler1')
    
    # amount_per_gurantee
    nan_handler2 <- inputdata$CB_GUIF_CNT==0
    amount_per_gurantee <- cleaned_lassocopy$CB_GUIF_AMT/cleaned_lassocopy$CB_GUIF_CNT
    amount_per_gurantee[nan_handler2] <- 0
    rm('nan_handler2')
    
    # amount_per_bankloan
    nan_handler3 <- inputdata$BNK_LNIF_CNT==0
    amount_per_bankloan <- cleaned_lassocopy$BNK_LNIF_AMT / cleaned_lassocopy$BNK_LNIF_CNT
    amount_per_bankloan[nan_handler3] <- 0
    rm('nan_handler3')
    
    # lately_delayed
    lately_delayed <- ifelse(inputdata$CRMM_OVDU_AMT!=0|inputdata$CRLN_30OVDU_RATE!=0,1,0)
    
    # 삽입 ----------------------------------------------------------------------
    
    cleaned$loaner_effect1 <- as.factor(comp3456)
    cleaned$credit_effect <- as.factor(interact0709)
    cleaned$loaner_effect2 <- as.factor(critical1112)
    cleaned$periodic_failure <- as.factor(interact4952)
    cleaned$phonefee_delaytrend <- as.factor(interact6466)
    cleaned$middle_loan_ratio=middle_loan_ratio
    cleaned$amount_per_gurantee=amount_per_gurantee
    cleaned$amount_per_bankloan=amount_per_bankloan
    cleaned$lately_delayed=as.factor(lately_delayed)
  }
  return(cleaned)  
} # 피피티, 보고서 방향대로 데이터를 전처리하고 파생변수를 추가하는 함수입니다.

resampler <- function(inputdata,tr_rate){
  kingodata <- inputdata
  index_zero <- sample(which(kingodata$TARGET==0),round(length(which(kingodata$TARGET==0))*tr_rate,0),replace=F)
  index_one <- sample(which(kingodata$TARGET==1),round(length(which(kingodata$TARGET==1))*tr_rate,0),replace=F)
  kingotrain <<- rbind(kingodata[index_zero,],kingodata[index_one,])
  kingotest <<- kingodata[-as.numeric(rownames(kingotrain)),]
} # inputdata의 0과 1 비율을 96:4로 맞춰서 tr_rate비율만큼의 트레이닝 데이터와 나머지 비율만큼의 테스트 데이터를 생성해주는 함수

critical_table <- function(data,index,result='distribution',scale=1,ppplot_ratio=0.9,quantile=seq(0.95,0.99,0.01)){
  if(result=='distribution'){return(table(data$TARGET,data[,index]))} else {
    if(result=='probability'){
      return(table(data$TARGET,data[,index])[2,]/apply(table(data$TARGET,data[,index]),2,sum))
    } else {
      if(result=='population'){
        return(apply(table(data$TARGET,data[,index]),2,sum))
      } else {
        if(result=='pplot'){
          if(('ggplot2' %in% rownames(installed.packages()))==0) install.packages('ggplot2')
          if(('package:ggplot2' %in% search())==0) library(ggplot2)
          critical <- table(data$TARGET,data[,index])[2,]/apply(table(data$TARGET,data[,index]),2,sum)
          pplot_dframe <- data.frame(x=as.numeric(names(critical))/scale,y=critical)
          ggplot(pplot_dframe,aes(x=x,y=y))+geom_line()+xlab(paste(names(data)[index],"(",index,")",sep=""))+
            ylab('Cumulated Ratio of TARGET=1')
        } else {
          if(result=='ppplot'){
            if(('ggplot2' %in% rownames(installed.packages()))==0) install.packages('ggplot2')
            if(('package:ggplot2' %in% search())==0) library(ggplot2)
            critical <- table(data$TARGET,data[,index])[2,]/apply(table(data$TARGET,data[,index]),2,sum)
            population_ratio <- cumsum(apply(table(data$TARGET,data[,index]),2,sum)/nrow(data))
            pplot_dframe <- data.frame(x=as.numeric(names(critical))/scale,y=critical,population=population_ratio)
            ggplot(pplot_dframe)+geom_line(aes(x=x,y=y),size=1.5)+geom_line(aes(x=x,y=population))+
              annotate('segment',x=quantile(data[,index],ppplot_ratio),
                       xend=quantile(data[,index],ppplot_ratio),
                       y=ppplot_ratio-0.0001,yend=0,size=1.5,color='red')+
              annotate('text',x=pplot_dframe$x[which(cumsum(population_ratio>ppplot_ratio)==1)],
                       y=0.5,label=pplot_dframe$x[which(cumsum(population_ratio>ppplot_ratio)==1)])+
              geom_hline(yintercept = 0.04277035,color='red',size=0.7)+
              xlab(paste(names(data)[index],"(",index,")",sep=""))+ylab('Cumulated Ratio of TARGET=1')
          } else {
            if(result=='quantile'){
              return(quantile(data[,index],quantile))
            }
          }
        }
      }
    }
  }
} # 시각화나 데이터 탐색을 위해 사용한 함수

revalue_column_conttable <- function(mydata,target,ngroupA,label.A='A',label.B='B'){
  target_index <- ifelse(is.numeric(target),target,which(names(mydata)==target))
  groupA <- levels(factor(mydata[,target_index]))[1:ngroupA]
  storage <- mydata[,target_index]
  storage[storage %in% groupA] <- label.A
  storage[storage!='A'] <- label.B
  # direct indexing gurantees best performance in such ifelse setting.
  return(storage)
} # 찾아낸 분할지점을 기준으로 그 전을 A, 그 후를 B로 코딩해주는 함수

revalue_critical_table <- function(inputdata,colnum,seperator,inputresult='distribution'){
  criticaldata <- inputdata
  ngroupA <- which(as.numeric(levels(factor(inputdata[,colnum])))==seperator)
  criticaldata[,colnum] <- revalue_column_conttable(inputdata,colnum,ngroupA)
  return(critical_table(criticaldata,colnum,result=inputresult))
} # 시각화나 데이터 탐색을 위해 사용한 함수2

compound_2var <- function(data,index1,index2,ngroupA1,ngroupA2,return_what='vector'){
  if(class(ngroupA1)=='character'){
    ngroupA1 <- which(as.numeric(as.character(levels(factor(data[,index1]))))==as.numeric(ngroupA1))
  }
  if(class(ngroupA2)=='character'){
    ngroupA2 <- which(as.numeric(as.character(levels(factor(data[,index2]))))==as.numeric(ngroupA2))
  }
  revalue1 <- revalue_column_conttable(data,index1,ngroupA1)
  revalue2 <- revalue_column_conttable(data,index2,ngroupA2)
  correlator <- rep(0,length(revalue1))
  correlator[revalue1=='B'] <- 2
  correlator[revalue2=='B'] <- correlator[revalue2=='B']+1
  critical_table <- table(data$TARGET,correlator)
  cat('0:AA  1:AB  2:BA  3:BB\n\n')
  ifelse(return_what=='vector',return(correlator),return(critical_table))
} # A,B로 분할된 이변량 변수 두 개를 합쳐주는 함수입니다.
  # 둘 다 A면 0, 둘 다 B면 3, 하나만 A면 그 순서에 따라 1,2로 코딩됩니다.


# Searching interactions & compound variable --------------------------------


# Finding best cutoff using contingency table -------------------------------------------

ordinal_signif_conttable <- function(inputdata,index){
  revalue_column_conttable <- function(mydata,target,ngroupA,label.A='A',label.B='B'){
    target_index <- ifelse(is.numeric(target),target,which(names(mydata)==target))
    groupA <- levels(factor(mydata[,target_index]))[1:ngroupA]
    storage <- mydata[,target_index]
    storage[storage %in% groupA] <- label.A
    storage[storage!='A'] <- label.B
    # direct indexing gurantees best performance in such ifelse setting.
    return(storage)
  }
  index_level <- as.numeric(levels(factor(inputdata[,index])))
  seperator <- c()
  chisq <- c()
  
  for(i in 1:(length(index_level)-1)){
    grouped <- revalue_column_conttable(inputdata,index,i)
    critical_table <- table(inputdata$TARGET,grouped)
    a <- as.numeric(critical_table[1,1])
    b <- as.numeric(critical_table[1,2])
    c <- as.numeric(critical_table[2,1])
    d <- as.numeric(critical_table[2,2])
    n <- sum(critical_table)
    
    seperator <- c(seperator,index_level[i+1])
    chisq <- c(chisq,n*((a*d-b*c)^2)/((a+b)*(c+d)*(a+c)*(b+d)))
  }
  return(data.frame(sep=seperator,chisquare=chisq))
} # 최적의 분할지점을 찾아주는 함수입니다.

partition_finder <- function(colnum,nreplicate=5){
  seperator <- rep(0,nreplicate)
  chisquare <- rep(0,nreplicate)
  for(i in 1:nreplicate){
    resampler()
    result <- ordinal_signif_conttable(kingotrain,colnum)
    seperator[i] <- result[which(result$chisquare==max(result$chisquare)),1]
    chisquare[i] <- max(result$chisquare)
  }
  return(data.frame(sep=seperator,maxchisq=chisquare))
} # 샘플링을 nreplicate번 만큼 반복 실행해 최적의 분할지점을 찾아주는 함수입니다.
  # 이 함수를 모든 변수에 반복실행해 가장 안정적으로 발견되는 분할지점을 사용했습니다.

# Interpretation model: lasso & boosting -----------------------------------------------

performance_tester <- function(training,test,coef_return=TRUE){
  if(("glmnet" %in% rownames(installed.packages()))==0) install.packages('glmnet')
  if(("package:glmnet" %in% search())==0) library(glmnet)
  kingo_cv_auc <- cv.glmnet(model.matrix(CUST_ID+TARGET~.,data=training),
                            training$TARGET,alpha=1,family='binomial',type.measure='auc')
  
  find_best_cutoff <- function(testdata,cutoff){
    lambda <- kingo_cv_auc$lambda.1se
    predicted <- predict(kingo_cv_auc,model.matrix(CUST_ID+TARGET~.,data=testdata),
                         s=lambda,type='response')
    result_matrix <- matrix(0,nrow=2,ncol=length(cutoff))
    for(i in 1:length(cutoff)){
      result_matrix[1,i] <- cutoff[i]
      lasso_result <- rep(0,nrow(testdata))
      lasso_result[predicted>=cutoff[i]] <- 1
      critical_table <- table(true=testdata$TARGET,pred=lasso_result)
      precision <- critical_table[2,2]/(critical_table[1,2]+critical_table[2,2])
      recall <- critical_table[2,2]/(critical_table[2,1]+critical_table[2,2])
      result_matrix[2,i] <- 2*precision*recall/(precision+recall)
    }
    return(result_matrix)
  }
  cutoff <- seq(0.1,0.3,by=0.01)
  if(coef_return){
    return(list(fmeasure=find_best_cutoff(test,cutoff),coef=data.frame(names=rownames(coef(kingo_cv_auc,s=kingo_cv_auc$lambda.1se)),
                                                                       values=as.numeric(coef(kingo_cv_auc,s=kingo_cv_auc$lambda.1se)))))
  } else{
    return(find_best_cutoff(test,cutoff))
  }
} # lasso를 통한 변수선택에서 cutoff별 F-measure를 출력하고 옵션에 따라 선택된 변수도 출력해주는 함수입니다.
  # 이 함수를 10번 실행해 변수별 생존 횟수를 작성했습니다.

init_data <- datacleanser(start_data,F,F)
for(i in 1:10){
  resampler(init_data,0.7)
  result <- performance_tester(kingotrain,kingotest)
  print(result$fmeasure)
} # 데이터 전처리만 거친 데이터로 lasso cv를 실행하는 코드

init_data <- datacleanser(start_data,F,T)
for(i in 1:10){
  resampler(init_data,0.7)
  result <- performance_tester(kingotrain,kingotest)
  print(result$fmeasure)
} # 전처리를 거친 데이터에 파생변수를 추가해 lasso cv를 실행하는 코드

library(mboost)
init_data <- datacleanser(start_data,F,T)
resampler(init_data,0.7)
kingo_gb <- glmboost(TARGET~.,data=kingotrain,family=Binomial(type='glm',link='logit'),control=boost_control(mstop=10000))
  # glm componentwise gradient boost를 실행해 변수를 선택하는 코드
fmeasure_gb <- function(data,gbobject,cutoff){
  result <- matrix(0,nrow=2,ncol=length(cutoff))
  for(i in 1:length(cutoff)){
    gbtable <- table(data$TARGET,ifelse(predict(gbobject,newdata=data,type='response')>cutoff[i],1,0))
    a=gbtable[2,2]/(gbtable[1,2]+gbtable[2,2])
    b=gbtable[2,2]/(gbtable[2,1]+gbtable[2,2])
    result[1,i] <- cutoff[i]
    result[2,i] <- 2*a*b/(a+b)
  }
  return(result)
}
fmeasure_gb(kingotest,kingo_gb,seq(0.155,0.175,0.001))
  # 선택된 변수와 상응하는 계수를 가지고 cutoff별 F-measure를 출력하는 함수


# Prediction Models (RandomForest, SVM, XGBoost ---------------------------

#install.packages('randomForest')
library(randomForest)
#install.packages('xgboost')
library(xgboost)
#install.packages('e1071')
library(e1071)
#install.packages('caret')
library(caret)

rawdata=read.csv(file.choose()) # Data_set.csv

# Preprocessing -----------------------------------------------------------

svm.cleansing=datacleanser(rawdata,glmlasso_select = T,compound_add = T)
rf.cleansing=svm.cleansing

xg.cleansing=datacleanser(rawdata,compound_add = T)


# Support Vector Machine --------------------------------------------------

resampler(svm.cleansing,0.7)
  # 트레이닝 데이터:테스트 데이터 비율을 7:3으로 해서 데이터를 분할한다

svm.model=svm(factor(TARGET)~.-CUST_ID,data=kingotrain,kernel='radial',gamma=1/32,cost=3,class.weights=c('0'=0.25),scale = T)
svm.pred=predict(svm.model,newdata=kingotest)

a=confusionMatrix(svm.pred,kingotest$TARGET)
  # Confusion Matrix를 생성한다

precision <- a$table[2,2]/(a$table[1,2]+a$table[2,2])
recall <- a$table[2,2]/(a$table[2,1]+a$table[2,2])
f=2*precision*recall/(precision+recall)
f # F값을 얻는다


# Random Forest -----------------------------------------------------------

resampler(rf.cleansing,0.7) 
  # 트레이닝 데이터:테스트 데이터 비율을 7:3으로 해서 데이터를 분할한다
rf.model=randomForest(factor(TARGET)~.-CUST_ID,data=kingotrain,family='binomial',ntree=500,mtry=7)
rf.pred=predict(rf.model,newdata=kingotest,type='class')

rf.table=confusionMatrix(rf.pred,kingotest$TARGET)

precision <- rf.table$table[2,2]/(rf.table$table[1,2]+rf.table$table[2,2])
recall <- rf.table$table[2,2]/(rf.table$table[2,1]+rf.table$table[2,2])
f=2*precision*recall/(precision+recall) # F값을 얻는다
f


# XGBoost -----------------------------------------------------------------

XGdata=XGclean(xg.cleansing)
str(XGdata,list.len=200)

## 트레이닝/테스트 분할
n=nrow(XGdata)
index=sample(1:n,0.7*n)
XGtrain=XGdata[index,]
XGtest=XGdata[-index,]
XGtrain=data.matrix(XGtrain)
XGtest=data.matrix(XGtest)
dtrain = xgb.DMatrix(XGtrain[,-(1:2)], label = XGtrain[,2])
dtest = xgb.DMatrix(XGtest[,-(1:2)], label = XGtest[,2])

watchlist = list(test = dtest, train = dtrain)


param = list(booster = 'gbtree',
             eta = 0.025,
             gamma = 0,
             max_depth = 120,
             lambda = 1,
             alpha = 0,
             objective = 'binary:logistic',
             eval_metric = f1value)

XGmodel = xgb.train(params = param, data = dtrain,
                    nrounds = 100, watchlist=watchlist)


xg.pred=predict(XGmodel,XGtest[,-(1:2)])

F1score(kingotest,xg.pred,0.2) 

### Stacking Ensenble
install.packages('mgcv')
install.packages('cvTools')
install.packages('glmnet')
install.packages('xgboost')
install.packages('e1071')
install.packages('dplyr')
library(mgcv)
library(glmnet)
library(xgboost)
library(e1071)
library(dplyr)

## train / test 생성된 이후에 쓰면 96:4로 비율 조정!
resampler <- function(tr.rate){
  kingodata <- data1
  index_zero <- sample(which(kingodata$TARGET==0),round(length(which(kingodata$TARGET==0))*tr.rate,0),replace=F)
  index_one <- sample(which(kingodata$TARGET==1),round(length(which(kingodata$TARGET==1))*tr.rate,0),replace=F)
  train <<- rbind(kingodata[index_zero,],kingodata[index_one,])
  test <<- kingodata[-c(index_zero,index_one),]
  rm('kingodata')
}



dummy_maker <- function(column){   #by.sunsic
  level <- levels(factor(column))
  nlevel <- length(levels(factor(column)))
  storage <- matrix(rep(0,length(column)*nlevel),ncol=nlevel)
  for(i in 1:nlevel){
    storage[column==level[i],i] <- 1
  }
  result <- as.data.frame(storage)
  names(result) <- paste0('_',level)
  return(result)
}

dummy_maker2 = function(data,col){
  colnum=which(names(data)==col)
  if(colnum == length(data)){
    A=data[,1:(colnum-1)]
    New=dummy_maker(data[,colnum])
    names(New)=paste0(names(data)[colnum],names(New))
    NewData=cbind(A,New)
  } else {
    original = names(data)[length(data)]
    A=data[,1:(colnum-1)]
    B=data[,(colnum+1):length(data)]
    New=dummy_maker(data[,colnum])
    names(New)=paste0(names(data)[colnum],names(New))
    NewData=cbind(A,New,B)
    names(NewData)[length(NewData)] = original
  }
  return(NewData)
}

dummy_maker3 = function(data){
  FacCol=c()
  for (i in 1:length(data)){
    if(class(data[,i]) == 'factor'){
      FacCol=c(FacCol,names(data)[i])
    }
  }
  for (i in 1:length(FacCol)){
    data=dummy_maker2(data,FacCol[i])
  }
  return(data)
}

XGclean=function(data){
  data=dummy_maker3(data)
  return(data)
}


### f1score 구하는 함수 for XGboost
f1value = function(preds, dtrain){
  labels = getinfo(dtrain, "label")
  preds = ifelse(preds<0.06,0,1)
  precision = sum(preds==1 & labels==1)/sum(labels==1)
  recall = sum(preds==1 & labels==1)/sum(preds==1)
  F1=(2*precision*recall)/(precision+recall)
  return(list(metric = 'F1', value = F1))
}



### f1score 구하는 함수
F1score = function(test,pred,rate){
  preds = as.numeric(pred > rate)
  labels = test$TARGET
  precision = sum(preds==1 & labels==1)/sum(labels==1)
  recall = sum(preds==1 & labels==1)/sum(preds==1)
  F1=(2*precision*recall)/(precision+recall)
  return(F1)
}



### f1 cutoff
f1cut = function(test,pred) {
  f1=c()
  for (i in (1:99/100)){
    new=data.frame(cbind(i,F1score(test,pred,i)))
    f1=rbind(f1,new)
  }
  names(f1)=c('cutoff','F1')
  return (f1)
}


### best f1
selectcut = function(F1) {
  index = F1$F1==max(F1$F1,na.rm=T)
  bst=na.omit(F1[index,])
  return(bst)
}


### f1 for binary response
binaryf1 = function(test,pred) {
  labels = as.integer(test$TARGET) - 1
  precision = sum(pred==1 & labels==1)/sum(labels==1)
  recall = sum(pred==1 & labels==1)/sum(pred==1)
  F1=(2*precision*recall)/(precision+recall)
  return (F1)
}




### 데이터 불러오기
data = read.csv(file.choose())
data1 = datacleanser(data,glmlasso_select = FALSE, compound_add = TRUE)
str(data1)
index = sample(1:nrow(data1),nrow(data1)*0.9)
train = data1[index,]
test = data1[-index,]
resampler(.9)
answer.train = train
answer.test = test
nrow(answer.train)
nrow(answer.test)
sum(answer.test$CUST_ID %in% answer.train$CUST_ID)


## cv
library(cvTools)
k=5
folds=cvFolds(NROW(answer.train),K=k)
acc=rep(0,10)

meta=data.frame()  ##NULL data frame 만들기 

for(i in 1:k){
  train1=answer.train[folds$subsets[folds$which!=i],]    #set the training set
  validation1=answer.train[folds$subset[folds$which==i],]    #set the validation set
  
  ##XG BOOST ---------------------------------------------------------------
  
  XGtrain=as.matrix(XGclean(train1))
  XGtest=as.matrix(XGclean(validation1))
  dtrain = xgb.DMatrix(XGtrain[,-(1:2)], label = XGtrain[,2])
  dtest = xgb.DMatrix(XGtest[,-(1:2)], label = XGtest[,2])
  
  param = list(
    booster = 'gbtree',   #dart 가능.
    silent = 1,
    eta = 0.025,
    gamma = 0,
    max_depth = 120,
    lambda = 1,
    alpha = 0,
    objective = 'binary:logistic',
    eval_metric = f1value)
  
  XGmodel = xgb.train(params = param, data = dtrain,
                      nrounds = 1200)
  xg.pred=predict(XGmodel,XGtest[,-(1:2)])
  plot(xg.pred)
  xg.margin=predict(XGmodel,XGtest[,-(1:2)],outputmargin = T)
  #xg.pred=f1r(XGtest,pred,0.04)
  
  
  print(paste0(i,'/',k,' xgboost is finished',' : ',Sys.time()))
  
  ##GLM ----------------------------------------------------------------------------
  glm.model = glm(factor(TARGET) ~ . -CUST_ID, data = train1, family = 'binomial')
  glm.pred = predict(glm.model, newdata=validation1, type="response")
  plot(glm.pred)
  
  print(paste0(i,'/',k,' glm is finished',' : ',Sys.time()))
  
  ##LASSO --------------------------------------------------------------------------
  lasso.model = cv.glmnet(model.matrix(CUST_ID+TARGET~.,data=train1),
                          train1$TARGET,alpha=1,family='binomial',type.measure='deviance')
  lasso.pred = predict(lasso.model,model.matrix(CUST_ID+TARGET~.,data=validation1),
                       s=lasso.model$lambda.1se,type='response')
  
  
  print(paste0(i,'/',k,' lasso is finished',' : ',Sys.time()))
  
  ##svm-----------------------------------------------------------------------------
  svm.model = svm(factor(TARGET)~CPT_LNIF_CNT + SPART_LNIF_CNT + TOT_LNIF_AMT +  
                    BNK_LNIF_AMT + CRDT_OCCR_MDIF + CRDT_CARD_CNT + CTCD_OCCR_MDIF + CB_GUIF_CNT +        
                    CRLN_OVDU_RATE + MIN_CNTT_DATE + LT1Y_PEOD_RATE + LT1Y_CTLT_CNT + MOBL_FATY_PRC +       
                    TLFE_UNPD_CNT + AGE + LT1Y_MXOD_AMT + PAYM_METD + LINE_STUS +          
                    MOBL_PRIN + loaner_effect1 + credit_effect + loaner_effect2 + periodic_failure +    
                    phonefee_delaytrend + middle_loan_ratio + amount_per_gurantee + amount_per_bankloan + lately_delayed,
                  data=train1,kernel='radial',
                  gamma = 1/32, cost = 3, class.weights=c('0'=0.25), scale = T)
  
  # compute decision values and probabilities:
  svm.pred = predict(svm.model, newdata = validation1, decision.values = TRUE)
  svm.pred.resp = as.integer(svm.pred) - 1
  svm.decisionV = as.numeric(attr(svm.pred, "decision.values"))       #0보다 작으면 1, 크면 0
  
  
  print(paste0(i,'/',k,' svm is finished',' : ',Sys.time()))
  
  ##Combining -----------------------------------------------------------
  meta.fold = cbind(validation1$CUST_ID,
                    svm.pred.resp , svm.decisionV , glm.pred ,lasso.pred, xg.pred, xg.margin,
                    validation1$TARGET)
  meta = rbind(meta,meta.fold)
}
names(meta) = c('CUST_ID','SVM','SVM.D','GLM','LASSO','XG','XG.M','TARGET')
meta$TARGET=as.factor(meta$TARGET)
meta$TARGET=as.integer(meta$TARGET) - 1
meta$TARGET=as.factor(meta$TARGET)
answer.train.meta=meta
str(answer.train.meta)



############################# test set의 변수값(각 모델의 확률값) 만들기 ################################

##1. xgboost ---------------------------------------------------------------------
XGtrain=as.matrix(XGclean(answer.train)) ################## train 전체데이터를 넣어서 xgboost model을 만든다.
XGtest=as.matrix(XGclean(answer.test)) ############# test 데이터V 넣기
dtrain = xgb.DMatrix(XGtrain[,-(1:2)], label = XGtrain[,2])
dtest = xgb.DMatrix(XGtest[,-(1:2)], label = XGtest[,2])

watchlist = list(test = dtest, train = dtrain)

param = list(
  booster = 'gbtree',   #dart 가능.
  eta = 0.025,
  gamma = 0,
  max_depth = 120,
  lambda = 1,
  alpha = 0,
  objective = 'binary:logistic',
  eval_metric = f1value)

XGmodel = xgb.train(params = param, data = dtrain,    ## xgmodel을 만들때에는 train의 전체데이터,  
                    nrounds = 1200)    
xg.pred.test = predict(XGmodel,XGtest[,-(1:2)])
xg.margin.test=predict(XGmodel,XGtest[,-(1:2)],outputmargin = T)


print(paste0('test xgb is finished',' : ',Sys.time()))


##2. GLM-------------------------------------------------------------------

test.glm=glm(factor(TARGET)~. -CUST_ID,data=answer.train,family='binomial') 
##마찬가지로 trainset 전체를 이용하여 glm 모델을 만들기

glm.pred.test=predict(test.glm, newdata=answer.test, type="response") ##위에서 만들어진 glm으로 testV 넣어서 test 확률값 만들기 


print(paste0('test glm is finished',' : ',Sys.time()))

##3. LASSO-------------------------------------------------------------------
test.lasso = cv.glmnet(model.matrix(CUST_ID+TARGET~.,data=answer.train),
                       answer.train$TARGET,alpha=1,family='binomial',type.measure='deviance')
lasso.pred.test = predict(test.lasso,model.matrix(CUST_ID+TARGET~.,data=answer.test),
                          s=lasso.model$lambda.1se,type='response')


print(paste0('test lasso is finished',' : ',Sys.time()))

##4. svm--------------------------------------------------------

svm.model = svm(factor(TARGET)~CPT_LNIF_CNT + SPART_LNIF_CNT + TOT_LNIF_AMT +  
                  BNK_LNIF_AMT + CRDT_OCCR_MDIF + CRDT_CARD_CNT + CTCD_OCCR_MDIF + CB_GUIF_CNT +        
                  CRLN_OVDU_RATE + MIN_CNTT_DATE + LT1Y_PEOD_RATE + LT1Y_CTLT_CNT + MOBL_FATY_PRC +       
                  TLFE_UNPD_CNT + AGE + LT1Y_MXOD_AMT + PAYM_METD + LINE_STUS +          
                  MOBL_PRIN + loaner_effect1 + credit_effect + loaner_effect2 + periodic_failure +    
                  phonefee_delaytrend + middle_loan_ratio + amount_per_gurantee + amount_per_bankloan + lately_delayed,
                data=answer.train,kernel='radial',
                gamma = 1/32, cost = 3, class.weights=c('0'=0.25), scale = T)

# compute decision values and probabilities:
svm.pred = predict(svm.model, newdata = answer.test, decision.values = TRUE)
svm.pred.resp.test = as.integer(svm.pred) - 1
svm.decisionV.test = as.numeric(attr(svm.pred, "decision.values"))       #0보다 작으면 1, 크면 0


print(paste0('test svm is finished',' : ',Sys.time()))




##4. 위의 3개의 test set 확률값들 붙이기 ----------------------------------


test.meta=cbind(answer.test$CUST_ID,
                svm.pred.resp.test,svm.decisionV.test,
                glm.pred.test,lasso.pred.test,
                xg.pred.test,xg.margin.test,
                answer.test$TARGET)
test.meta = data.frame(test.meta)
names(test.meta) = c('CUST_ID','SVM','SVM.D','GLM','LASSO','XG','XG.M','TARGET')
test.meta$TARGET=as.factor(test.meta$TARGET)
answer.test.meta = test.meta
str(test.meta)

############################################## 메타모델 만들기(GLM) #################################################
## 1. 모델 적합

answer.meta.glm=glm(TARGET~ SVM.D + XG.M + SVM.D*XG.M,data=answer.train.meta,family='binomial')
summary(answer.meta.glm)
plot(answer.meta.glm)

## 2. 모델 prediction & F1구하기
answer.metaglm.pred = predict(answer.meta.glm, newdata=answer.test.meta, type="response")
plot(answer.metaglm.pred)

GLM.final.pred = data.frame(cbind(answer.test.meta$CUST_ID,answer.metaglm.pred))
selectcut(f1cut(answer.test.meta,GLM.final.pred[,2]))

############################################## 메타모델 만들기(GAM) #################################################
## 1. 모델 적합
answer.meta.gam = gam(TARGET ~ s(SVM.D) + XG.M, data = answer.train.meta, family = 'binomial')
summary(answer.meta.gam)
plot(answer.meta.gam)
## 2. 모델 prediction & F1구하기
answer.metagam.pred = predict.gam(answer.meta.gam,newdata=answer.test.meta,type='response')
plot(answer.metagam.pred)

GAM.final.pred = data.frame(cbind(test.meta$CUST_ID,answer.metagam.pred))
selectcut(f1cut(answer.test.meta,GAM.final.pred[,2]))

############################################## 모델별 F1벨류 ###############################################
## 1. SVM F1
binaryf1(answer.test.meta,answer.test.meta$SVM)

## 2. GLM cutoff point 찾기
selectcut(f1cut(answer.test.meta,answer.test.meta$GLM))

## 3. LASSO cutoff point 찾기
selectcut(f1cut(answer.test.meta,answer.test.meta$LASSO))

## 4. XGB cutoff point 찾기
selectcut(f1cut(answer.test.meta,answer.test.meta$XG))
