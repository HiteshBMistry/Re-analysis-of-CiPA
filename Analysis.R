# This script loads up the CiPA data-set with a 
# Bnet score derived using dynamic IC50 value and
# qNet score based on average of 1-4 times Cmax value

require(ggplot2)

dat<-read.csv("dynamic_qnet_versus_dynamic_bnet.csv",header=T)

ggplot(dat, aes(x = Bnet, y = qNet)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+theme_bw(base_size=14)+
  xlab("Bnet at 1xFreeCmax")+ylab("Average qNet at 1 to 4xFreeCmax")

summary(lm(qNet~Bnet,data=dat))

# This clearly shows that Bnet at the free Cmax is similar to the 
# average of qNet across 1 to 4 times the free Cmax

# Let's analyse how well each metric describes the test-set

require(rms)
test<-dat[13:28,]
lrm(CiPA~Bnet,data=test)
lrm(CiPA~qNet,data=test)

require(ROCR)
p1<-prediction(test$Bnet, test$CiPA>0)
p2<-prediction(test$Bnet, test$CiPA>1)
performance(p1,"auc")# 0.98
performance(p2,"auc")# 0.98

p3<-prediction(test$qNet, test$CiPA>0)
p4<-prediction(test$qNet, test$CiPA>1)
performance(p3,"auc")# 0.88
performance(p4,"auc")# 1.00
