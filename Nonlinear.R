##Use proper function to load imputation trait values
bmi="path"

##Use proper function to load Metabolites Matrix
meta="path"


n_snp<-c()
p_value<-c()
for(i in 1:ncol(meta)){
  print(i)
  n_snp[i]<-sum(is.na(meta[,i]))
  if(n_snp[i]==0){
    m1<-lm(bmi~meta[,i]+I(meta[,i]^2))
    p_value[i]<-summary(m1)$coef[3,4]
  }else{
    na_id<-which(is.na(meta[,i]==T))
    m1<-lm(bmi[-na_id]~meta[-na_id,i]+I(meta[-na_id,i]^2))
    p_value[i]<-summary(m1)$coef[3,4]
  }
}
save(p_value,file"path")
