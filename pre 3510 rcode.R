x<-c(1,2,3,4,5,6,7,8)
y<-c(1.98,0.75,2.67,3.65,3.34,4.82,6.64,5.97)
data<-data.frame(
  value=c(x,y),
  group=factor(rep(c("X","Y"),each=length(x)))
)
model<-aov(value~group,data=data)
anova_table<-summary(model)
print(anova_table)


####chap2part3_example7
x<-c(46,20,52,30,57,25,28,36,22,43,57,33,22,63,40,48,28,49,52,59,29,34,24,50)
y<-c(3.5,1.9,4.0,2.6,4.5,3.0,2.9,3.8,2.1,3.8,4.1,3.0,2.5,4.6,3.2,4.2,2.3,4.0,4.3,3.9,3.3,3.2,2.5,3.3)
slr<-lm(y~x)
anova(slr)
qf(0.05,1,22,lower.tail = FALSE)
