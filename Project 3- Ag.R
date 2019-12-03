#1
# in the money

sig <- 0.2
r <- 0.02
s <- 50
k <- 52
tau <- 0.5

bs_f <- function(s,k,sig,r,tau){
  d2 <- (log(s/k)+(r-(sig^2)/2)*tau)/(sqrt(tau)*sig)
  d1 <- d2+sig*sqrt(tau)
  c_price <- s*pnorm(d1)- k*exp(-r*tau)*pnorm(d2)
  return(c_price)
}

bs_f(s,k,sig,r,tau)
# [1] 2.179827

t_seq <- seq(0, tau , length=10000)
p_time <- sapply(t_seq, function(x) bs_f(s,k,sig,r,x))
plot(p_time~t_seq, type='l', main= "European Call price using BS", xlab= "Time",
     ylab= "Price")

#MC
dt <- 1/252
sim_n <- function(n){
  r_d <- (r-(sig^2)/2)*dt+sig*sqrt(dt)*rnorm(tau*252)
  #sum(r_d)
  s_tau <- s*exp(cumsum(r_d))
  return(s_tau)
}
s_tau
plot(s_tau, type="l")
abline(h=k, lty=2)

N <- 10^4
s_mat <- sapply(1:N, sim_n)
dim(s_mat)
# [1]   126 10000

#ploting the first simulation (random)
plot(s_mat[,1], type = "l", main= "first simulation plot",ylab="Stock Price",
     xlab= "Simulation iteration")

last_s <- s_mat[nrow(s_mat),]
plot(density(last_s))
abline(v=k, col=2)

plot(density(last_s - k))
abline(v=0, col=2)

x <- last_s - k
x[x<0] <- 0
plot(density(x))
mean(x)*exp(-r*tau)
# [1] 2.166543

# Asian Option
s_mean <- apply(s_mat,2,mean)
plot(density(last_s),ylim=c(0,0.1))
lines(density(s_mean),col=2)
abline(v=k,lty=2)

payoff2 <- s_mean - k
payoff2[payoff2<0] <- 0
mean(payoff2)*exp(-r*tau)
# [1] 0.9456704

plot(density(x))
lines(density(payoff2),col=2)

# Knock-out Barrier Option
N <- 10^4
s_mat <- sapply(1:N, sim_n)
last_s <- s_mat[nrow(s_mat),]
payoff3 <- last_s - k
payoff3[payoff3<0] <- 0
payoff3

for(n in 1:ncol(s_mat)){
  if(any(s_mat[,n] < 48))
    payoff3[n] <- 0
}
payoff3
mean(payoff3)*exp(-r*tau)
# [1] 1.49639

