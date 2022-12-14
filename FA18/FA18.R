# Roshan Bhandari, Md Ariful Haque Miah, Saad Mirza
## Model Equation :
# y_ijk = mu + a_i + b_j(i) + e_ijk where i = 1,2,3  j= 1,2,3,4 and k= 1,2,3


# Null : a_i = 0
# ALternative: a_i != 0

#Null: b_j(i)=0
#Alternative: b_j(i) !=0

library(GAD)
process<-c(rep(1,12),rep(2,12),rep(3,12))
batch<-rep(c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)),3)

obs<-c(25,30,26,19,28,20,15,17,14,15,16,13,19,17,14,23,24,21,18,21,17,35,27,25,14,15,20,35,21,24,38,54,50,25,29,33)

Table<-data.frame(process,batch,obs)

process<-as.fixed(process)
batch<-as.random(batch)

model<-lm(obs~process+batch%in%process)
gad(model)

# The process is not sifnificant(so we fail to reject the null hypothesis ) because it has a P-value (0.2815) which is greater than alpha=0.05
# Batch nested with process is significant (successfully reject the null hypothesis) because it has a P-value (5.477e-07) which is smaller than alpha = 0.05