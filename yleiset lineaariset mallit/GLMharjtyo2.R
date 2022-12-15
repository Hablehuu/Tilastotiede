pisadata <- read.table("http://users.jyu.fi/~slahola/files/ylm1_datoja/pisadata.txt", header = TRUE)

set.seed(19260378)
n <- nrow(pisadata)
ind <- sample(1:n, 470)
pisadata2 <- pisadata[ind, ]
#The research question is to find out what background variables
#can explain exelent reading skill in Finnish teenagers.
summary(pisadata2)
pisadata2$lukem <- factor(pisadata2$lukem)
pisadata2$sukup <- factor(pisadata2$sukup)
pisadata2$koulsij <- factor(pisadata2$koulsij)
sum(pisadata2)
#the languages other than Finnish had too small sampe sizes
#(some were size of 1), 
#so I decided to combine then to a factor of whether someone
#speaks Finnish or not
pisadata2$Finnish <- ifelse(pisadata2$kieli == "Finnish",1,0)
model1 <- glm(data = pisadata2,lukut ~ koulsij+sukup+SES+lukem+ICT
              ,family = binomial)
sum(pisadata2$lukem == 1)
sum(pisadata2$lukem == 2)
sum(pisadata2$lukem == 3)
sum(pisadata2$lukem == 4)
sum(pisadata2$lukem == 5)
sum(pisadata2$Finnish == 1)

summary(model1)
#new variable seems significant
#now we test if the model is significantly different from the
#model that has languages as seperate
model2 <- glm(data = pisadata2,lukut ~ koulsij+Finnish+sukup+SES+lukem+ICT
              ,family = binomial)
summary(model2)
model3 <- glm(data = pisadata2,lukut ~ Finnish+sukup+SES+lukem
              ,family = binomial)
summary(model3)
anova(model1,model2)
1 - pchisq(7.4378, 1)
AIC(model1,model2)
#analysis of deviances tells us that the model significantly different
#can we make the model better by dropping nt significant variables
anova(model3,model2)
1 - pchisq(2.5191, 3)
AIC(model3,model2)
#new model is not better so we stick with model 2

b <- coef(model3)
#write descriptions for odds and odds ratios
exp(b[1]) #the odds of the reference group, that is 
#a girl from a small town who speaks Finnish, dosen't read for fun and
# average SES and ICT scores(in this case zero) of having exelent
#reading skills versus not having exelent reading skills
exp(b[2]) # if all else is the same the odds are 1.4  times higher when comparing 
#students in cities to students in towns
exp(b[3])# if all else is the same the odds are 2.585354 times higher when comparing 
#students in metropolies to students in towns

exp(b[4])# the odds of having exelent reading skills are 4.499285 times higher if 
#student speaks Finnish at home compared to other lanquages

exp(b[5]) #the odds of having exelent reading skills is 0.4293674 times higher if 
#student is a boy compared to girls

exp(b[6]) #When comparing two individuals whose SES differs by one and all other
#things beign equal the odds

exp(b[7])#the odds of having exelent reading skills is 5.813965 times higher if the student reads
#less than 30 min per day compared to those who don't read
exp(b[8])#the odds of having exelent reading skills is 5.116952 times higher if the student reads
#31 - 60 min per day compared to those who don't read
exp(b[9])#the odds of having exelent reading skills is 10.2419 times higher if the student reads
#1 - 2 hours per day compared to those who don't read
exp(b[10])#the odds of having exelent reading skills is 9.24596 times higher if the student reads
#more than 2 hours per day compared to those who don't read
exp(b[11])

exp(confint(model3))
#logit(PI) = b0 + b1koulsij+b2Finnish+b3sukup+b4SES+b5lukem+b6ICT
library(statmod)
#anayzing randomized quantile residuals we can see that the residuals don't
#have any patterns or have big deviances from zero
plot(qresiduals(model3) ~ fitted(model3), ylab = "quantile residuals", xlab = "fitted values")

#and Q-Q plot seems very good as well, the values seem linear
qqnorm(qresiduals(model3))
#riippumattomuus, lineaari suhde
#vaste on binäärinen o1 muuttuja logistinen regressio

