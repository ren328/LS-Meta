## Use Proper function to load bmi values
bmi="path"
## Use Proper funciton to load environmental factor
data="path"

count<-c()
data<-data.frame(data)
for(i in 1:ncol(data)){
  count[i]<-sum(is.na(data[,i]))
}
index<-which(count<10000)
data1<-data[,index]


beta<-c()
beta_sd<-c()
n_snp<-c()
p_value<-c()
for(i in 1:ncol(data1)){
  print(i)
  n_snp[i]<-sum(is.na(data1[,i]))
  if(n_snp[i]==0){
    m1<-lm(bmi~data1[,i])
    beta[i]<-m1$coef[2]
    beta_sd[i]<-summary(m1)$coef[2,2]
    p_value[i]<-summary(m1)$coef[2,4]
  }else{
    na_id<-which(is.na(data1[,i]==T))
    m1<-lm(bmi[-na_id]~data1[-na_id,i])
    beta[i]<-m1$coef[2]
    beta_sd[i]<-summary(m1)$coef[2,2]
    p_value[i]<-summary(m1)$coef[2,4]
  }
}
##use proper function to save the result
save(n_snp,beta,beta_sd,p_value,file=paste0(""path"))



