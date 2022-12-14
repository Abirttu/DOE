Dof_total<-15
Dof_error<-8
Dof_interaction<-3
Dof_A<-1

Dof_B<-Dof_total-Dof_error-Dof_interaction-Dof_A
print(Dof_B)

#SSA
MSA<-0.0002
SSA<-MSA*Dof_A
print(SSA)

#MSB,MSAB,MSE
SSB<-180.378
SSAB<-8.479
SSE<-158.797

MSB<-SSB/Dof_B
MSAB<-SSAB/Dof_interaction
MSE<-SSE/Dof_error

print(MSB)

print(MSAB)
print(MSE)

#Finding F-Statistic Values:
Fa<-MSA/MSE
Fb<-MSB/MSE
Fab<-MSAB/MSE

print(Fa)
print(Fb)
print(Fab)

#Finding P-Values:
pf(0.00001007576,1,8, lower.tail = FALSE)
pf(3.0290,3,8, lower.tail = FALSE)



#PART B: (Levels of Source B)
LevelsofB<-Dof_B+1
b<-LevelsofB
print(b)

#part c
LevelsofA<-Dof_A+1
a<-LevelsofA

#Dof_Total: 15 = a*b*n - 1
#a*b*n = 16
#n=16/a*b

n<-16/(a*b)
print(n)

#part d
#interaction effect
Fcritical_interaction=qf(0.9317,3,8)
print(Fcritical_interaction)
#fail to reject

#main effect A
Fcritical_A=qf(0.998,1,8)
print(Fcritical_A)
#fail to reject

#main effect B
Fcritical_B=qf(0.093,3,8)
print(Fcritical_B)
#reject null
