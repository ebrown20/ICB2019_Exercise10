# Modeling mutant vs. non-mutant cancer cells treated with drugs

# time points to simulate over (simulate over 1000 timesteps)
# a place to store population sizes for mutant and non-mutant (value of state variables through time)
# values for parameters 
# before treatment: (rN = rM = 0.1; K = 1000000)
# after treatment: (rN = -0.1; rM = 0.05, K = 1000000)
# a loop for simulating
# initial starting places; N0 = 99; M0 = 1

# set normal tumor cell inital values
N0=99
rN=0.1

# set mutant tumor cell initial values
M0=1
rM=0.1

# set remaining initial values
K=1000000
timesteps=1000

# create a vector to store N's and set inital N
Ns=numeric(length=timesteps)
Ns[1]=N0

# create a vector to store M's and set initial M
Ms=numeric(length=timesteps)
Ms[1]=M0

# simulate after first mutation until "equilibrium" (t=300) is reached
# after 300 timesteps, cells are treated with drug
for(t in 1:(timesteps-1)) {
  if(t < 300) {
    Ns[t + 1] <- Ns[t] + (rN*Ns[t]*(1 - ((Ns[t] + Ms[t])/K)))
    Ms[t + 1] <- Ms[t] + (rM*Ms[t]*(1 - ((Ns[t] + Ms[t])/K)))
  }
  else {
    rN=-0.1
    rM=0.05
    Ns[t + 1] <- Ns[t] + (rN*Ns[t]*(1 - ((Ns[t] + Ms[t])/K)))
    Ms[t + 1] <- Ms[t] + (rM*Ms[t]*(1 - ((Ns[t] + Ms[t])/K)))
  }
}

# plot simulation
library(ggplot2)
sim <- data.frame(time=1:length(Ns),N=Ns,M=Ms)
ggplot(data=sim) + geom_line(aes(x=time,y=N,colour="Non-mutant")) +
  geom_line(aes(x=time,y=M,colour="Mutant")) +
  theme_classic() + theme(legend.position="top") +
  scale_colour_manual(values=c("Non-mutant"="red","Mutant"="blue")) +
  xlab("Time") + ylab("Number of cells")
