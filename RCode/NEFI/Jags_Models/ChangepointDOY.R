model{
for(i in 1:N){
y[i] ~ dpois(m[i])

temp[i] ~ dnorm(15,0.16) #lo mean, var from actual data summary
m[i] <- exp(x[i]) 
x[i]~dnorm(mu[i],tau_add) #mu's and x's here are on log scale
}
#### Process Model
for(i in 2:N){
#temp2[i] =temp[i] * temp[i]
#mu[i]~dnorm(mu[i-1],tau_add) #mu's here are on log scale
muLO[i]=b0[1]+r*x[i-1] + beta[1]*temp[i] 
muHI[i]=b0[2] + r*x[i-1] + beta[2]*temp[i]
mu[i]=ifelse(doy[i]>k,muHI[i],muLO[i])
}

#### Priors
#k~dnorm(224,0.003) #prior from HE, based on 2008-16 lakes in ME, last max obs
k=240
for(j in 1:2){
beta[j]~dnorm(0,1E-6)
b0[j]~dnorm(0,1E-6)
}
r~dnorm(0,1E-6)

mu[1] ~ dnorm(x_ic,tau_ic) 

tau_add ~ dgamma(a_add,r_add)

}