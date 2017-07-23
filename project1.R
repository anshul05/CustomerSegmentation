
#Clear environment
rm(list = ls())

#Set working directory
setwd("/home/anshul/Documents/BA/project")

#Import Dataset
cc <- read.csv("CC GENERAL.csv", header = T)


#Create function for descriptive analysis
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  nmiss_per <- (nmiss/n)*100
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, nmiss_per=nmiss_per, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

var <- sapply(cc, is.numeric)

stats_cc <-t(data.frame(apply(cc[var], 2, mystats)))
View(stats_cc)
str(cc)


#Create funciton for outlier treatment
M1_fun <- function(x){
  quantiles <- quantile( x, c(.01, .99 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

###Treating missing values
require(Hmisc)
data<-data.frame(apply(cc[var],2, function(x) impute(x, mean)))
str(data)

stats_data <- t(data.frame(apply(data, 2, mystats)))

data1 <- data.frame(sapply(data, M1_fun))

stats_data1 <- t(data.frame(apply(data1, 2, mystats)))

###Building KPI's

data1$MON_PURCHASES <- data1$PURCHASES*data1$PURCHASES_FREQUENCY
data1$MON_CASH_ADVANCE <- data1$CASH_ADVANCE*data1$CASH_ADVANCE_FREQUENCY
data1$MON_ONEOFF_PURCHASES <- data1$ONEOFF_PURCHASES*data1$ONEOFF_PURCHASES_FREQUENCY
data1$MON_INSTALLMENTS_PURCHASES <- data1$INSTALLMENTS_PURCHASES*data1$PURCHASES_INSTALLMENTS_FREQUENCY
data1$PURCHASES_PER_TRX <- data1$PURCHASES/data1$PURCHASES_TRX
data1$CASH_ADVANCE_PER_TRX <- data1$CASH_ADVANCE/data1$CASH_ADVANCE_TRX
data1$LIMIT_USAGE <- data1$BALANCE/data1$CREDIT_LIMIT
data1$PAYMENT_TO_MIN_PAYMENT <- data1$PAYMENTS/data1$MINIMUM_PAYMENTS

hist(data1$PURCHASES_TRX)
data2 <- replace(data1, is.na(data1), 0)


#Factor Analysis
corrm <- cor(data2)    ###correlation atrix

require(psych)
require(GPArotation)

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT
eigen(corrm)$values                                                     ### EIGEN VALUES

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 



write.csv(eigen_values, file = "p1_eigen.csv")  ### EXPORTING EIGEN VALUE SUMMARY


FA<-fa(r=corrm, 8, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT
FA_SORT$loadings
Loadings<-data.frame(FA_SORT$loadings[1:ncol(data2),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME



#standardizing the data
inputdata_final = scale(data2)
#View(inputdata_final)
#building clusters using k-means clustering 
cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)

cluster_four
cluster_five
cluster_six

data_new<-cbind(data2,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster )
View(data_new)

#Graph based on k-means - Optional
require(cluster)

clusplot(inputdata_final, #dataframe
         cluster_six$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

vars <- sapply(data,is.numeric)

###Profiling
require(tables)
tt<-cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)
                  ~ Heading()*length*All(data_new["BALANCE"]), data=data_new),
          tabular( 1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)
                   ~ Heading()*mean*All(data2[]), data=data_new))
tt1<-as.data.frame.matrix(tt)

View(tt1)
names(data_new)
rownames(tt1)<-c("ALL", "KM3_1" ,"KM3_2" ,"KM3_3", "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,"KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6")
names(data_new)
colnames(tt1)<-c("SEGMENT_SIZE",  "BALANCE" ,"BALANCE_FREQUENCY" ,              
                 "PURCHASES"   ,                     "ONEOFF_PURCHASES",                
                 "INSTALLMENTS_PURCHASES",           "CASH_ADVANCE",                    
                 "PURCHASES_FREQUENCY",              "ONEOFF_PURCHASES_FREQUENCY",      
                 "PURCHASES_INSTALLMENTS_FREQUENCY", "CASH_ADVANCE_FREQUENCY",          
                 "CASH_ADVANCE_TRX",                 "PURCHASES_TRX",                   
                 "CREDIT_LIMIT",                     "PAYMENTS",                        
                 "MINIMUM_PAYMENTS",                 "PRC_FULL_PAYMENT",                
                 "TENURE",                           "MON_PURCHASES",                   
                 "MON_CASH_ADVANCE",                 "MON_ONEOFF_PURCHASES",            
                 "MON_INSTALLMENTS_PURCHASES",       "PURCHASES_PER_TRX",               
                 "CASH_ADVANCE_PER_TRX",             "LIMIT_USAGE",                     
                 "PAYMENT_TO_MIN_PAYMENT")


cluster_profiling<-data.frame(t(tt1))

write.csv(cluster_profiling, "p1_cluster_profiling.csv") 


