library(pwr)

##INCREMENTAL POWER PREDICTION IN MULTIPLE REGRESSION 
R2=.2
SR2=.1
power = .85

#my.f2 = sr2 / (1 - r2)

my.f2 <- .01 / (1 - .20)

print (my.f2)

#f2 = .0125

#u = 1 since we are only looking at one predictor right now 
pwr.f2.test(u=1, f2=.0125, power=.85)

N= 1 + 719 +1 

print(N)
