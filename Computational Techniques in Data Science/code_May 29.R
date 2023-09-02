libra
dose<-c(1.6907,1.7242,1.7552,1.7842,1.8113,1.8369,1.861,1.8839)
num_bettles<-c(59,60,62,56,63,59,62,60)
dead<-c(6,13,18,28,52,53,61,60)


data<-tibble(dose=dose,num_bettles=num_bettles,dead=dead)
data
data$prop_dead<-data$dead/data$num_bettles
attach(data)
plot(dose,prop_dead)

fit<-lm(prop_dead~dose,data=data)
summary(fit)

data$odds<-data$prop_dead/(1-data$prop_dead)


# The logistic regression model in R

# Binomial data

fit1<-glm(cbind(dead,num_bettles-dead)~dose,data=data,family = binomial(link = logit))
summary(fit1)

# Bernoulli

dose1<-rep(c(1.6907,1.7242,1.7552,1.7842,1.8113,1.8369,1.861,1.8839),c(59,60,62,56,63,59,62,60))
dead1<-c(rep(c(0,1),c(data$num_bettles[1]-dead[1],dead[1])),
        rep(c(0,1),c(data$num_bettles[2]-dead[2],dead[2])),
        rep(c(0,1),c(data$num_bettles[3]-dead[3],dead[3])),
        rep(c(0,1),c(data$num_bettles[4]-dead[4],dead[4])),
        rep(c(0,1),c(data$num_bettles[5]-dead[5],dead[5])),
        rep(c(0,1),c(data$num_bettles[6]-dead[6],dead[6])),
        rep(c(0,1),c(data$num_bettles[7]-dead[7],dead[7])),
        rep(c(0,1),c(data$num_bettles[8]-dead[8],dead[8])))
data1<-tibble(dose1=dose1,dead1=dead1)
data1
ftable(data1$dose1,data1$dead1)


fit2<-glm(dead1~dose1,data=data1,family=binomial(link=logit))
summary(fit2)



fit21<-lm(dead1~dose1,data=data1)
summary(fit21)

data1$prob<-predict(fit2, type = "response")

data1$pred[data1$prob<0.5]<-0
data1$pred[data1$prob>=0.5]<-1

ftable(data1$pred,data1$dead1)




amhc<-read_csv("amhc.csv")
amhc%>%names()


# Unadjusted model

fit331<-glm(Adc~peduc,data=amhc,family = binomial(link = logit))
summary(fit331)
fit332<-glm(Adc~educ,data=amhc,family = binomial(link = logit))
summary(fit332)


# Adjusted model


fit33<-glm(Adc~peduc+educ+wealth+insurance+mediaexpo+mage+ethinicdiv+NpropHig+comhospdel+desirepreg+pregcomplication,
           data=amhc,family = binomial(link = logit))

summary(fit33)

