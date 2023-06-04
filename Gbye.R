##Use proper function to load bmi values
bmi="path"
##Use proper function to load Environmental factor matrix(include GS_BMI and other covariates), the corresponding order
## is bmi, 15PCs, age, sex, batch, GS, 27 environmental factors
data="path"

##id is all the environmental factors we consider
id<-c("Bread intake","Cereal intake","Cooked vegetable intake","Dried fruit intake","Fresh fruit intake","Salad/Raw vegetable intake",
"Tea intake","Water intake","Duration of walks","Number of days/week of moderate physical activity 10+ minutes",
"Number of days/week of vigorous physical activity 10+ minutes","Number of days/week walked 10+ minutes",
"Time spent driving","Time spent using computer","Time spent watching television","Age first had sexual intercourse","Lifetime number of sexual partners",
"Sleep duration","Exposure to tobacco smoke at home","Exposure to tobacco smoke outside home","Childhood sunburn occasions",
"Frequency of solarium/sunlamp use","Time spend outdoors in summer","Time spent outdoors in winter","Townsend deprivation index at recruitment","Length of time at current address",
"Number in household")

##The following chunk is used to change certain answers to "NA"
id1<-c(1,2,3,4,5,6,7,8,13,14,15,17,18,19,20,21,22,23,24,26,27)
id2<-id1+20

data1<-data
for(i in id2){
ind1<-which(data[,i]==-1)
ind2<-which(data[,i]==-3)
data1[ind1,i]=NA
data1[ind2,i]=NA
}

id12<-c(9,10,11,12)
for(i in id12){
ind1<-which(data[,i]==-1)
ind2<-which(data[,i]==-3)
data1[ind1,i]=NA
data1[ind2,i]=NA
}

result<-matrix(0,27,3)
for(i in 21:47){
print(i)
input<-cbind(data1[1:20],data1[,i])
input<-data.frame(input)
input[,21]= replace(input[,21], input[,21] >= quantile(input[,21],0.99,na.rm=T), NA)
ind<-ncol(input)
colnames(input)[ind]<-"env"
n<-40432-sum(is.na(input$env))
m1<-lm(bmi~GS+env+GS*env+age+I(age^2)+sex+GS*age+GS*I(age^2)+GS*sex+env*age+env*I(age^2)+env*sex+batch+pc1
+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9+pc10+pc11+pc12+pc13+pc14+pc15,data=input)
beta_gbye<-summary(m1)$coef[23,c(1,4)]
result[i-20,]<-c(n,beta_gbye)
}
result<-data.frame(result)
result1<-cbind(id,result)
colnames(result1)<-c("Variable","N","beta","p-value")

##save the result (result1) with proper function
"path"

