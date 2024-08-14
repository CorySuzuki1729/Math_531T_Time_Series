

Psi <- ARMAtoMA(ar=0.9, ma = 0.5, 100)
#h= 0
sigma2 = 1
gam =numeric()
gam[1] = sum(Psi^2)
#h = 1
gam[2] = sum(Psi[-100]*Psi[-1])
#h = 2
gam[3] = sum(Psi[-c(99:100)]*Psi[-c(1:2)])
rho = gam/gam[1]
rho
test = arima.sim(n =200, list(ar =0.9, ma = 0.5) )
acf(test)

ACF.ARMA = function(ar, ma, sig2, maxpsi){
  Psi <- ARMAtoMA(ar=ar, ma = ma, maxpsi)
  gam =numeric()
  for(h in 1:maxpsi){
    gam[h] = sig2*sum(Psi[1:(maxpsi-h+1)]*Psi[h:maxpsi])
  }
  rho=gam/gam[1]
  return(rho)
}

test1 <- ACF.ARMA(ar = c(0.5), ma = 0.5, sig2=1, maxpsi = 100e1)
plot(seq(0:20),test1[1:21], type = "l", xlim = c(0,20))
abline(h=0, col = 2)
lines(seq(0:20),ARMAacf(ar = c(0.5), ma = 0.5, lag.max = 20), col = 3)

