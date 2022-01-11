#delete all the existing variables, just for completeness
rm(list = ls())

data<-read.table("FamaFrenchFactors.txt", header = TRUE, sep = "", dec = ".")
rf<-data$RF
mkt.exret<-data$MktRF
smb<-data$SMB
hml<-data$HML
mkt<-mkt.exret+rf

returns<-cbind(mkt, hml, smb)

mu<-colMeans(returns)
print(mu)

sigma<-cov(returns)
print(sigma)

print(cov(smb, hml))

volatilities<-sqrt(diag(sigma))
SR.factors<-(mu-mean(rf))/volatilities
print(SR.factors)


w.equal<-array(1/3, c(3,1))
mu<-array(mu, c(3,1))
R.equal<-returns%*%w.equal

print(mean(R.equal))
print(sd(R.equal))
print((mean(R.equal) - mean(rf))/sd(R.equal))

mu.equal<-t(w.equal)%*%mu
vol.equal<- sqrt(t(w.equal)%*%sigma%*%w.equal)
SR.equal<-(mu.equal - mean(rf))/vol.equal
cat("Expected return is", mu.equal, "\n")
cat("Volatility is ", vol.equal, "\n")
cat("Monthly Sharpe ratio is ", SR.equal, "\n")

w.minvar<-solve(sigma)%*% array(1, c(3,1))
w.minvar<-w.minvar/sum(w.minvar)
print(w.minvar)
mu.minvar<-t(w.minvar)%*%mu
vol.minvar<- sqrt(t(w.minvar)%*%sigma%*%w.minvar)
SR.minvar<-(mu.minvar - mean(rf))/vol.minvar
cat("Expected return of the minimum variance portfolio is", mu.minvar, "\n")
cat("Volatility of the minimum variance portfolio is ", vol.minvar, "\n")
cat("Monthly Sharpe ratio of the minimum variance portfolio is ", SR.minvar, "\n")


w.tangency<-solve(sigma)%*% (mu - mean(rf))
w.tangency<-w.tangency/sum(w.tangency)
print(w.tangency)
mu.tangency<-t(w.tangency)%*%mu
vol.tangency<- sqrt(t(w.tangency)%*%sigma%*%w.tangency)
SR.tangency<-(mu.tangency - mean(rf))/vol.tangency
cat("Expected return of the tangency portfolio is", mu.tangency, "\n")
cat("Volatility of the tangency portfolio is ", vol.tangency, "\n")
cat("Monthly Sharpe ratio of the tangency portfolio is ", SR.tangency,  "\n")
cat("Annual Sharpe ratio of the tangency portfolio is ", sqrt(12)*SR.tangency,  "\n")


req.returns<-seq(from=0.21,to=0.8, by=0.005)
n<-length(req.returns)
means<-array(0, c(n,1))
volatilities<-array(0, c(n,1))

for (i in 1:n){
  mu.0<-req.returns[i]
  alpha<-as.numeric((mu.0-mu.minvar)/(mu.tangency -mu.minvar))
  w<-alpha*w.tangency+ (1-alpha)*w.minvar
  means[i]<-t(w)%*%mu
  volatilities[i]<-sqrt(t(w)%*%sigma%*%w)
}

plot(volatilities, means, main="Efficient portfolio frontier", 
     xlab="Volatility (st.dev.)", ylab="Expected return")



