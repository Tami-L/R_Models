> # An ARIMA simulation
   z <- arima.sim(list(order = c(0,0,0)), n = 500)
  par(mfrow=c(2,2),oma=c(2,2,2,2))
  ts.plot(ts(z))
  acf(z)
  pacf(z)
  spec.pgram(z)
  # An ARIMA simulation
  RW <- arima.sim(list(order = c(0,1,0)), n = 500)
  par(mfrow=c(1,1))
  acf(RW)
  
  
   alpha=0.9
  xt=arima.sim(1000,model=list(ar=alpha))
  par(mfrow=c(3,1),oma=c(2,2,2,2))
  ts.plot(ts(xt))
  acf(xt,lag=100)
  acf(xt,type="partial",lag=100)
  mtext("AR(1) process with alpha =0.9,sigma^2=1",outer=T,cex=1.1)