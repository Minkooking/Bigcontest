
# glm selected ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
null_scoreboard <- read.csv('NULL GLM.csv')
  # NULL GLM은 파생변수가 부착되지 않은 원시데이터를 가지고 10번 lasso glm을 fitting했을 때의 계수와 f-measure을 기록한 데이터였습니다.
ggplot(null_scoreboard,aes(x=count,y=reorder(names,count)))+geom_point()+geom_segment(xend=0,aes(yend=names))+
  scale_x_discrete(limits=1:10)+theme(panel.grid.major.y=element_blank(),
                                      panel.grid.minor.y=element_blank(),
                                      plot.background = element_rect(fill='grey95'),
                                      axis.title.x=element_text(size=12),
                                      axis.title.y=element_text(size=12),
                                      panel.background = element_rect(fill='grey95'))+
  xlab('survival count')+ylab('variable name')

# lasso selected ----------------------------------------------------------
  # LASSO COEF COUNT는 파생변수를 부착한원시데이터를 가지고 10번 lasso glm을 fitting했을 때의 계수와 f-measure을 기록한 데이터였습니다.
interact_scoreboard <- read.csv('LASSO COEF COUNT.csv')
ggplot(interact_scoreboard,aes(x=count,y=reorder(names,count),color=vartype,size=vartype))+geom_point()+
  geom_segment(xend=0,aes(yend=names,color=vartype))+
  scale_x_discrete(limits=1:10)+theme(panel.grid.major.y=element_blank(),
                                      panel.grid.minor.y=element_blank(),
                                      plot.background = element_rect(fill='grey95'),
                                      axis.title.x=element_text(size=12),
                                      axis.title.y=element_text(size=12),
                                      panel.background = element_rect(fill='grey95'))+
  xlab('survival count')+ylab('variable name')+scale_color_manual(values=c('deepskyblue','black'))+
  scale_size_manual(values=c(1.1,0.5))+theme(axis.ticks.y = element_blank(),legend.background = element_rect(fill='grey90'),
                                             legend.box.background = element_rect(fill='grey90'),
                                             legend.position=c(0.75,0.15),legend.key=element_rect(fill='grey90',color='grey90'),
                                             legend.text = element_text(size=15),legend.key.width = unit(5,'line'),
                                             legend.title = element_text(size=13))

# glmboost selected -------------------------------------------------------
  # GLMBOOST COEF COUNT는 파생변수를 부착한 원시데이터를 가지고 10번 boosted glm을 fitting했을 때의 계수와 f-measure을 기록한 데이터였습니다.
interact_scoreboard <- read.csv('GLMBOOST COEF COUNT.csv')
ggplot(interact_scoreboard,aes(x=count,y=reorder(names,count),color=vartype,size=vartype))+geom_point()+
  geom_segment(xend=0,aes(yend=names,color=vartype))+
  scale_x_discrete(limits=1:10)+theme(panel.grid.major.y=element_blank(),
                                      panel.grid.minor.y=element_blank(),
                                      plot.background = element_rect(fill='grey95'),
                                      axis.title.x=element_text(size=12),
                                      axis.title.y=element_text(size=12),
                                      panel.background = element_rect(fill='grey95'))+
  xlab('survival count')+ylab('variable name')+scale_color_manual(values=c('deepskyblue','black'))+
  scale_size_manual(values=c(1.1,0.5))+theme(axis.ticks.y = element_blank(),legend.position = 'none')

# total scoreboard --------------------------------------------------------
  
total_scoreboard <- read.csv('ULTIMATE SCOREBOARD.csv')
summarize(group_by(total_scoreboard[1:20,],interaction),average=mean(f.measure))
ggplot(total_scoreboard[1:20,],aes(x=replicate,y=f.measure,group=interaction,color=interaction))+geom_line(size=2)+
  scale_color_manual(values=c('deepskyblue','grey80'))+
  annotate('segment',x=0.5,xend=10.5,y=0.4253827,yend=0.4253827,color='deepskyblue')+
  annotate('text',x=1.25,y=0.427,size=10,label='0.4254')+
  annotate('segment',x=0.5,xend=10.5,y=0.3934974,yend=0.3934974,color='grey60')+
  annotate('text',x=1.25,y=0.395,size=10,label='0.3948')+
  theme(plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key=element_rect(color='grey95'))
  scale_x_discrete(limits=1:10)

lassoboost_comparison <- gather(total_scoreboard[11:30,],measure,value,c(cutoff,f.measure,count))
ggplot(lassoboost_comparison,aes(x=replicate,y=value,group=selector,color=selector))+geom_line(size=2)+
  facet_grid(measure~.,scales = 'free_y')+
  scale_color_manual(values=c('deepskyblue','grey80'))+
  theme(plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key=element_rect(color='grey95'),axis.title.y = element_blank(),
        strip.text.y=element_text(size=18))+
  scale_x_discrete(limits=1:10)
# motivation: variable partition ------------------------------------------
  # ppt 11장에 사용된 인포매틱스를 형성하는 코드입니다.
ggplot(data.frame(categories=c('BANK','CAPITAL','SPART','ECT'),credit=c(0.8,0.5,0.2,0.1)),aes(x=categories,y=credit,fill=categories))+
  geom_bar(stat='identity',width=0.5)+ylab('Credit Required')+scale_x_discrete(limits=c('BANK','CAPITAL','SPART','ECT'))+
  scale_fill_manual(values=c('lightskyblue','lightskyblue1','lightskyblue2','lightskyblue3'))+guides(fill=FALSE)+
  theme(axis.text.y=element_blank())
  

# building comp3456 -------------------------------------------------------
resampler(0.7)

subsetted_factored <- as.data.frame(
  cbind(revalue_column_conttable(kingotrain,3,1),
        revalue_column_conttable(kingotrain,4,1),
        revalue_column_conttable(kingotrain,5,2),
        revalue_column_conttable(kingotrain,6,2))
)
category_3456 <- rep(0,nrow(subsetted_factored))
category_3456[subsetted_factored[,1]=='B'] <- category_3456[subsetted_factored[,1]=='B']+1
category_3456[subsetted_factored[,2]=='B'] <- category_3456[subsetted_factored[,2]=='B']+2
category_3456[subsetted_factored[,3]=='B'] <- category_3456[subsetted_factored[,3]=='B']+4
category_3456[subsetted_factored[,4]=='B'] <- category_3456[subsetted_factored[,4]=='B']+8

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
visual3456 <- data.frame(names=dimnames(table(kingotrain$TARGET,category_3456))$category_3456,
                         ratio_TARGET1=table(kingotrain$TARGET,category_3456)[2,]/apply(table(kingotrain$TARGET,category_3456),2,sum),
                         population=apply(table(kingotrain$TARGET,category_3456),2,sum)/nrow(kingotrain))
visual3456 <- gather(visual3456,type,values,ratio_TARGET1:population)
ggplot(visual3456,aes(x=names,y=values,fill=type))+geom_bar(stat='identity',position='dodge')+xlab('Groups')+ylab('Ratio')+
  geom_hline(yintercept = 0.0427,size=1.5,color='grey75')+scale_fill_manual(values=c('grey80','deepskyblue'))+
  theme(plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'))
  

# building critical1112 ---------------------------------------------------

visual1112 <- data.frame(names=factor(0:3),ratio_TARGET1=critical_table(kingotrain,72,'probability'),
                         population=critical_table(kingotrain,72,'population')/nrow(kingotrain))
visual1112 <- gather(visual1112,type,values,ratio_TARGET1:population)
visual1112$names <- as.character(visual1112$names)
visual1112[visual1112$names==0,1] <- 'A'
visual1112[visual1112$names==1,1] <- 'B'
visual1112[visual1112$names==2,1] <- 'C'
visual1112[visual1112$names==3,1] <- 'D'
visual1112$names <- as.factor(visual1112$names)
ggplot(visual1112,aes(x=names,y=values,fill=type))+geom_bar(stat='identity',position='dodge')+xlab('Groups')+ylab('Ratio')+
  geom_hline(yintercept = 0.0427,size=1.5,color='grey75')+scale_fill_manual(values=c('grey80','deepskyblue'))+
  theme(plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'))+
  scale_x_discrete(limits=c('A','B','C','D'))

# building 0709 -----------------------------------------------------------

visual0709 <- data.frame(names=factor(0:3),ratio_TARGET1=critical_table(kingotrain,71,'probability'),
                         population=critical_table(kingotrain,71,'population')/nrow(kingotrain))
visual0709 <- gather(visual0709,type,values,ratio_TARGET1:population)
visual0709$names <- as.character(visual0709$names)
visual0709[visual0709$names==0,1] <- 'A'
visual0709[visual0709$names==1,1] <- 'B'
visual0709[visual0709$names==2,1] <- 'C'
visual0709[visual0709$names==3,1] <- 'D'
visual0709$names <- as.factor(visual0709$names)
ggplot(visual0709,aes(x=names,y=values,fill=type))+geom_bar(stat='identity',position='dodge')+xlab('Groups')+ylab('Ratio')+
  geom_hline(yintercept = 0.0427,size=1.5,color='grey75')+scale_fill_manual(values=c('grey80','deepskyblue'))+
  theme(plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'))

# building 4952 -----------------------------------------------------------

visual4952 <- data.frame(names=factor(0:3),ratio_TARGET1=critical_table(kingotrain,73,'probability'),
                         population=critical_table(kingotrain,73,'population')/nrow(kingotrain))
visual4952 <- gather(visual4952,type,values,ratio_TARGET1:population)
visual4952$names <- as.character(visual4952$names)
visual4952[visual4952$names==0,1] <- 'A'
visual4952[visual4952$names==1,1] <- 'B'
visual4952[visual4952$names==2,1] <- 'C'
visual4952[visual4952$names==3,1] <- 'D'
visual4952$names <- as.factor(visual4952$names)
ggplot(visual4952,aes(x=names,y=values,fill=type))+geom_bar(stat='identity',position='dodge')+xlab('Groups')+ylab('Ratio')+
  geom_hline(yintercept = 0.0427,size=1.5,color='grey75')+scale_fill_manual(values=c('grey80','deepskyblue'))+
  theme(plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'))


# building 6466 -----------------------------------------------------------
revalue_column_conttable <- function(mydata,target,ngroupA,label.A='A',label.B='B'){
  target_index <- ifelse(is.numeric(target),target,which(names(mydata)==target))
  groupA <- levels(factor(mydata[,target_index]))[1:ngroupA]
  storage <- mydata[,target_index]
  storage[storage %in% groupA] <- label.A
  storage[storage!='A'] <- label.B
  # direct indexing gurantees best performance in such ifelse setting.
  return(storage)
}
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
  ifelse(return_what=='vector',return(correlator),return(critical_table))
}
kingotrain$interact6466 <- compound_2var(kingotrain,64,66,1,1)

visual6466 <- data.frame(names=factor(0:3),ratio_TARGET1=critical_table(kingotrain,74,'probability'),
                         population=critical_table(kingotrain,74,'population')/nrow(kingotrain))
visual6466 <- gather(visual6466,type,values,ratio_TARGET1:population)
visual6466$names <- as.character(visual6466$names)
visual6466[visual6466$names==0,1] <- 'A'
visual6466[visual6466$names==1,1] <- 'B'
visual6466[visual6466$names==2,1] <- 'C'
visual6466[visual6466$names==3,1] <- 'D'
visual6466$names <- as.factor(visual6466$names)
ggplot(visual6466,aes(x=names,y=values,fill=type))+geom_bar(stat='identity',position='dodge')+xlab('Groups')+ylab('Ratio')+
  geom_hline(yintercept = 0.0427,size=1.5,color='grey75')+scale_fill_manual(values=c('grey80','deepskyblue'))+
  theme(plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'))

init_data <- datacleanser(start_data,F,T)
resampler(0.7)
visual6466 <- data.frame(names=factor(0:2),ratio_TARGET1=critical_table(kingotrain,74,'probability'),
                         population=critical_table(kingotrain,74,'population')/nrow(kingotrain))
visual6466 <- gather(visual6466,type,values,ratio_TARGET1:population)
visual6466$names <- as.character(visual6466$names)
visual6466[visual6466$names==0,1] <- 'A'
visual6466[visual6466$names==1,1] <- 'B'
visual6466[visual6466$names==2,1] <- 'C'
visual6466$names <- as.factor(visual6466$names)

ggplot(visual6466,aes(x=names,y=values,fill=type))+geom_bar(stat='identity',position='dodge')+xlab('Groups')+ylab('Ratio')+
  geom_hline(yintercept = 0.0427,size=1.5,color='grey75')+scale_fill_manual(values=c('grey80','deepskyblue'))+
  theme(plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'))

# need to select! ---------------------------------------------------------

howmanyzero <- function(data,index){sum(data[,index]==0)}
name <- c()
zero <- c()
for(i in 1:69){
  name <- c(name,names(start_data)[i])
  zero <- c(zero,howmanyzero(start_data,i))
}
manyzeros <- data.frame(names=name,zeros=zero)
manyzeros <- arrange(manyzeros,desc(zeros))[1:13,]
manyzeros <- manyzeros[-6,]
ggplot(manyzeros,aes(x=reorder(names,zeros),y=zeros))+geom_histogram(stat='identity',fill='deepskyblue')+
  coord_flip()+xlab('number of zeros')+ylab('variable names')+
  theme(panel.background=element_rect(fill='grey95'),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.background = element_rect(fill='grey95'))
  

probability <- c(critical_table(input_data,71,'probability'),critical_table(input_data,72,'probability'),
                 critical_table(input_data,73,'probability'))
ggprob <- data.frame(levels=names(probability),ratio_TARGET1=probability,
                     varname=c(rep(names(input_data)[71],4),rep(names(input_data)[72],4),rep(names(input_data)[73],4)))
ggplot(ggprob,aes(x=levels,y=ratio_TARGET1,color=varname,group=varname))+geom_line(size=1.5)+
  theme(legend.position = 'bottom',legend.title = element_blank(),panel.background = element_rect(fill='grey95'),
        plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key = element_rect(color='grey95'))+
  scale_color_manual(values=c('deepskyblue4','lightsteelblue3','lightskyblue1'))


# numeric coefficient --------------------------------------------------

continuous <- read.csv('BOOST CONTINUOUS.csv')
continuous$class <- factor(continuous$class,levels=c('count','amount','money'))
continuous$effect <- factor(continuous$effect,levels=c('positive','negative'))
continuous$coefficient <- abs(continuous$coefficient)
ggplot(continuous,aes(x=coefficient,y=reorder(varname,coefficient),color=effect))+geom_point(size=4)+
  facet_grid(class~.,space='free',scales='free')+geom_segment(aes(yend=varname,color=effect),xend=0)+
  theme(plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key=element_rect(color='grey95'),axis.title.y = element_blank(),
        strip.text.y=element_text(size=18),legend.position='bottom',legend.key.width = unit(2,'cm'))+
  scale_color_manual(values=c('deepskyblue','grey60'))

# categorical coefficient -------------------------------------------------

categorical <- read.csv('BOOST CATEGORY.csv')
loaner1 <- categorical[1:3,]
ggplot(loaner1,aes(x=factor(level),y=odds_increase,color=significance,group=varname))+geom_line(color='grey70',size=1.3)+
  ggtitle('Loaner Effect 1')+geom_point(size=4)+ylim(0.9,1.2)+xlab('level')+ylab('odds increase')+scale_color_manual(values=c('grey60','deepskyblue'))+
  theme(plot.title=element_text(size=20),plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key = element_rect(color='grey95'),legend.position = 'bottom',legend.text = element_text(size=14),
        legend.title = element_blank())

loaner2 <- categorical[8:11,]
ggplot(loaner2,aes(x=factor(level),y=odds_increase,color=significance,group=varname))+geom_line(color='grey70',size=1.3)+
  ggtitle('Loaner Effect 2')+geom_point(size=4)+xlab('level')+ylab('odds increase')+scale_color_manual(values=c('grey60','deepskyblue'))+
  theme(plot.title=element_text(size=20),plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key = element_rect(color='grey95'),legend.position = 'bottom',legend.text = element_text(size=14),
        legend.title = element_blank())

credit <- categorical[4:7,]
ggplot(credit,aes(x=factor(level),y=odds_increase,color=significance,group=varname))+geom_line(color='grey70',size=1.3)+
  ggtitle('Credit Effect')+geom_point(size=4)+xlab('level')+ylab('odds increase')+scale_color_manual(values=c('grey60','deepskyblue'))+
  theme(plot.title=element_text(size=20),plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key = element_rect(color='grey95'),legend.position = 'bottom',legend.text = element_text(size=14),
        legend.title = element_blank())

periodic <- categorical[12:15,]
ggplot(periodic,aes(x=factor(level),y=odds_increase,color=significance,group=varname))+geom_line(color='grey70',size=1.3)+
  ggtitle('Periodic Payment Failure')+geom_point(size=4)+xlab('level')+ylab('odds increase')+scale_color_manual(values=c('grey60','deepskyblue'))+
  theme(plot.title=element_text(size=20),plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key = element_rect(color='grey95'),legend.position = 'bottom',legend.text = element_text(size=14),
        legend.title = element_blank())

phone <- categorical[16:18,]
ggplot(phone,aes(x=factor(level),y=odds_increase,color=significance,group=varname))+geom_line(color='grey70',size=1.3)+
  ggtitle('Phonefee Delay Trend')+geom_point(size=4)+xlab('level')+ylab('odds increase')+scale_color_manual(values=c('grey60','deepskyblue'))+
  theme(plot.title=element_text(size=20),plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key = element_rect(color='grey95'),legend.position = 'bottom',legend.text = element_text(size=14),
        legend.title = element_blank())+ylim(0.75,2.25)

lately <- categorical[19:20,]
lately$level <- factor(lately$level,levels=c('No','Yes'))
ggplot(lately,aes(x=factor(level),y=odds_increase,color=significance,group=varname))+geom_line(color='grey70',size=1.3)+
  ggtitle('Experienced Phonefee Delay in Recent 1 Month')+geom_point(size=4)+xlab('level')+ylab('odds increase')+scale_color_manual(values=c('grey60','deepskyblue'))+
  theme(plot.title=element_text(size=20),plot.background = element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key = element_rect(color='grey95'),legend.position = 'bottom',legend.text = element_text(size=14),
        legend.title = element_blank())+ylim(0.8,1.4)
# classification fmeasure -------------------------------------------------

fmeasure <- data.frame(classifier=c('GLM','Random\nForest','SVM','XGBoost'),
                       f.measure=c(0.417,0.435,0.451,0.485))
ggplot(fmeasure,aes(x=classifier,y=f.measure,fill=classifier))+geom_bar(stat='identity',width=0.7)+
  scale_fill_manual(values=c('lightskyblue1','lightskyblue2','lightskyblue3','deepskyblue'),
                    limits=c('GLM','Random\nForest','SVM','XGBoost'),labels=c('GLM','Random Forest','SVM','XGBoost'))+
  theme(plot.background=element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key=element_rect(color='grey95'),legend.position = 'bottom',
        legend.title = element_blank())

fmeasure <- data.frame(classifier=c('GLM','Random\nForest','SVM','XGBoost','Ensemble'),
                       f.measure=c(0.417,0.435,0.451,0.485,0.494))
fmeasure$classifier <- factor(fmeasure$classifier,levels=c('GLM','Random\nForest','SVM','XGBoost','Ensemble'))
ggplot(fmeasure,aes(x=classifier,y=f.measure,fill=classifier))+geom_bar(stat='identity',width=0.7)+
  scale_fill_manual(values=c('lightskyblue1','lightskyblue2','lightskyblue3','lightskyblue4','deepskyblue'),
                    limits=c('GLM','Random\nForest','SVM','XGBoost','Ensemble'),
                    labels=c('GLM','Random Forest','SVM','XGBoost','Ensemble'))+
  theme(plot.background=element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key=element_rect(color='grey95'),legend.position = 'bottom',
        legend.title = element_blank())

XGBoost <- data.frame(selection=1:4,f.measure=c(0.4713,0.469,0.4718,0.4781))
XGBoost$type <- c('Ensemble','Ensemble','Ensemble','single XGBoost')
ggplot(XGBoost,aes(x=selection,y=f.measure,color=type))+geom_line(color='grey70',size=1.3)+
  geom_point(size=4)+scale_color_manual(values=c('grey70','deepskyblue'))+
  theme(plot.background=element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key=element_rect(color='grey95'),legend.position = 'bottom',
        legend.title = element_blank())+
  scale_x_continuous(breaks=1:4,labels=c('A','B','C','D'))+ylim(0.4675,0.479)

XGBoost <- data.frame(selection=1:4,f.measure=c(0.476,0.475,0.4804,0.4812))
XGBoost$selected <- c('N','N','N','Y')
ggplot(XGBoost,aes(x=selection,y=f.measure,color=selected))+geom_line(color='grey70',size=1.3)+
  geom_point(size=4)+scale_color_manual(values=c('grey70','deepskyblue'))+
  theme(plot.background=element_rect(fill='grey95'),legend.background = element_rect(fill='grey95'),
        legend.key=element_rect(color='grey95'),legend.position = 'bottom',
        legend.title = element_text(size=12))+
  scale_x_continuous(breaks=1:4,labels=c('A','B','C','D'))+ylim(0.4725,0.483)

