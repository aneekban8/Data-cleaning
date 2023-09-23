library(xlsx)
library(tidyverse)
library(moments)
s_data<-read.xlsx("D:\\Upwork\\SPIndex.xlsx","Sheet1",rowIndex=1:399,colIndex=1:7)
mp_data<-function(df,n1,n2){
UP_Down<-c()
prev_days_change<-c()
for (i in n1:398)
{
 if(((df$Adj_Close[i]-df$Adj_Close[i-1])/df$Adj_Close[i-1])<0)
{
  UP_Down[i]<-c("Down")
} else
{ 
  UP_Down[i]<-c("Up")
 }
prev_days_change[i]<-((df$Adj_Close[i-n2]-df$Adj_Close[i-(n2+1)])/df$Adj_Close[i-(n2+1)])*100
}
b3<-cbind(df,UP_Down,prev_days_change)
return(b3)
}
bx_plot<-function(df1,tt,xvar="UP_Down",yvar="prev_days_change",fillvar="UP_Down")
{
p<-ggplot(na.omit(df1),aes_string(x=xvar,y=yvar,fill=fillvar))
p+geom_boxplot()+scale_fill_brewer(palette="YlGn")+ylab("Change %")+ggtitle(tt)+theme(plot.title = element_text(hjust = 0.5))
}
hi_plot<-function(df1,tt,xvar="prev_days_change")
{
p<-ggplot(na.omit(df1),aes_string(x=xvar))
p+geom_histogram(fill="blue",colour="black")+facet_grid(UP_Down~.)+xlab(tt)
}
rm_out<-function(a1)
{
LB<-quantile(a1,probs=0.25)-1.5*(quantile(a1,probs=0.75)-quantile(a1,probs=0.25))
UB<-quantile(a1,probs=0.75)+1.5*(quantile(a1,probs=0.75)-quantile(a1,probs=0.25))
f_v<-subset(a1,a1>=LB & a1<=UB)
print("Median")
print(quantile(f_v,probs=0.5))
print("Std. deviation")
print(sd(f_v))
print("Skewness")
print(skewness(f_v))
print("Kurtosis")
print(kurtosis(f_v))
}
b4<-mp_data(s_data,3,1)
View(b4)
#print(class(b4))
b4$UP_Down<-as.factor(b4$UP_Down)
#dev.new()
bx_plot(b4,"Previous Days Change")
dev.new()
hi_plot(b4,"Previous days change")
tapply(b4$prev_days_change,b4$UP_Down,skewness)
tapply(b4$prev_days_change,b4$UP_Down,kurtosis)
tapply(b4$prev_days_change,b4$UP_Down,summary)
tapply(b4$prev_days_change,b4$UP_Down,sd)
b5<-mp_data(s_data,5,3)
View(b5)
b5$UP_Down<-as.factor(b5$UP_Down)
dev.new()
bx_plot(b5,"3 day previous change")
dev.new()
hi_plot(b5,"3 day previous change")
tapply(b5$prev_days_change,b5$UP_Down,skewness)
tapply(b5$prev_days_change,b5$UP_Down,kurtosis)
tapply(b5$prev_days_change,b5$UP_Down,summary)
tapply(b5$prev_days_change,b5$UP_Down,sd)
x1_UP<-subset(b4,UP_Down=="Up",select=prev_days_change)
rm_out(x1_UP$prev_days_change)
x1_Down<-subset(b4,UP_Down=="Down",select=prev_days_change)
rm_out(x1_Down$prev_days_change)
x1_UP<-subset(b5,UP_Down=="Up",select=prev_days_change)
rm_out(x1_UP$prev_days_change)
x1_Down<-subset(b5,UP_Down=="Down",select=prev_days_change)
rm_out(x1_Down$prev_days_change)
