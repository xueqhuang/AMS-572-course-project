data<-read.table('~/Google/Project/total.txt',header=T) 
attach(data)
Sex<-factor(Sex,labels=c('Male','Female'))
Survive<-factor(Survive,labels=c('Survived','Died'))
Type<-factor(Type,labels=c('Shock','Non_shock'))
Record<-factor(Record,labels=c('Pre','Post'))
group<-split(data,Record)
MAP.Post<-group$Post$MAP
MAP.Pre<-group$Pre$AT
#Test 1 Paird T Test, Wilcox Test, Variance Test.
boxplot(MAP.Pre,MAP.Post,col=(c("gold","darkgreen")))
t.test(MAP.Post,MAP.Pre,paired=T)
wilcox.test(MAP.Post,MAP.Pre,paired=T)
var.test(MAP.Post,MAP.Pre)

length(data)
names(data)
z<-data[2:21]
dim(z)
p_value<-matrix(rep(NA,400),nrow=20,ncol=20)
for(i in 1:20)
{
for(j in 1:20)
{if (j>i)
{
cor<-cor.test(z[,i],z[,j])
p_value[i,j]<-round(cor$p.value,digits=4)
}
else t<-NA
}
} 
rownames(p_value)<-names(z)
colnames(p_value)<-names(z)
#Test 2, Correaltion Test
p_value 

#From this matrix, we find some variable has autocorrelation, then we decide to use stepwise regression. 
l1<-lm(MAP~AGE+HT+Sex+Survive+Type+SBP+AT+HR+DBP+MCVP+BSI+CI+MCT+UO+PVI+RCI+HG+HCT+Record)
library(MASS)
boxcox(l1)
#Use boxcox transform data, but our data is good.
l2<-lm(MAP~AGE+HT+Sex+Survive+Type+SBP+AT+HR+DBP+MCVP+BSI+CI+MCT+UO+PVI+RCI+HG+HCT+Record)
step(l2,direction=c('backward'))
l3<-lm (MAP ~ AGE + Sex + Survive + SBP + HR + DBP + MCVP+ CI + MCT + Record)
plot(l3)
shapiro.test(l3$residuals)
# Residual do not follow normal distribution, drop some outliners.
data1<-data[c(-51,-52),]
l4<-lm(MAP~AGE+HT+Sex+Survive+Type+SBP+AT+HR+DBP+MCVP+BSI+CI+MCT+UO+PVI+RCI+HG+HCT+Record,data=data1)
step(l4,diection=c('backward'))
l5<-lm(MAP~AGE+Sex+Survive+SBP+HR+DBP+MCVP+BSI+CI+MCT+Record,data = data1)
summary(l5)
#We find record is not signaficiant at alpha=0.1,the drop record.
l6<-update(l5,.~.-Record,data=data1)
anova(l5,l6)
#Test 3, test effectiveness of coefficient 
summary(l6)
#Test 4, anova test.
anova(l6)
#Test 5, test normality of residual. This part is very important for the effectiveness of our model.
plot(l6)
shapiro.test(l6$residuals)
#Luckily, residuals follow normal distribution.