# Modeling mutant vs. non-mutant cancer cells treated with drugs

# time points to simulate over (simulate over 100 years)
# a place to store population sizes for mutant and non-mutant (value of state variables through time)
# values for parameters (rN = -0.1; rM = 0.05, K = 1000000)
# a loop for simulating
# initial starting places; N0 = 99; M0 = 1

# set normal tumor cell inital values (in the presence of drug treatment)
N0=99
rN=-0.1

# set mutant tumor cell initial values (in the presence of drug treatment)
M0=1
rM=0.05

# set remaining initial values
K=1000000
timesteps=100

# create a vector to store N's and set inital N
Ns=numeric(length=timesteps)
Ns[1]=N0

# create a vector to store M's and set initial M
Ms=numeric(length=timesteps)
Ms[1]=M0

# simulate over time
for(t in 1:100) {
  Ns[t + 1] <- Ns[t] + (rN*Ns[t]*(1 - ((Ns[t] + Ms[t])/K)))
  Ms[t + 1] <- Ms[t] + (rM*Ms[t]*(1 - ((Ns[t] + Ms[t])/K)))
}

# plot simulation
# library(ggplot2)
# sim <- data.frame(time=1:length(Ns),N=Ns)
# sim2 <- data.frame(time=1:length(Ms),M=Ms)

plot(1:length(Ms), Ms, type="l", col="red", main="Number of mutant (drug-resistant) cells vs. non-mutant cells after drug treatment", xlab="Time", ylab="Number of cells")
lines(1:length(Ns), Ns, col="blue", lwd=2)
legend(0, 135, legend=c("Mutant cells","Non-Mutant Cells"),col=c("red","blue"), lty = 1)



# ggplot(data=sim,aes(x=time,y=N)) + geom_line(aes(color="Non-mutant cells")) + geom_line(data=sim2,aes(x=time,y=M)) + geom_line(aes(color="Mutant Cells")) + labs(color="Legend") + labs(y="Number of cells",x="Time") + theme_classic()

# ggplot(data=sim,aes(x=time,y=N)) + geom_line(data=sim2,aes(x=time,y=M),color="") + scale_color_manual("", breaks=c("Non-mutant cells","Mutant cells"),values=c("red","blue")) + theme_classic() + labs(y="Number of cells",x="Time")

# ggplot() + geom_line(data=sim,aes(x=time,y=N), color = "blue") + geom_line(data=sim2,aes(x=time,y=M), color = "red") + theme_classic() + labs(y="Number of cells",x="Time")
# legend(0, 130, legend=c("Mutant cells","Non-mutant Cells"),col=c("red","blue"), lty=1)




# ggplot(data=sim,aes(x=time,y=N))z + geom_line(data=sim2,aes(x=time,y=M)) + geom_line(aes(color="Mutant Cells")) + labs(color="Legend") + labs(y="Number of cells",x="Time")
