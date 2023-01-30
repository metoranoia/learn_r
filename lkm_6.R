# Victoria Anggia Alexandra
# NIM 082111833089
#no.1
prog.1 <- function(miu,var)
{
  n <- as.numeric(readline("Masukkan jumlah x : "))
  prob <- vector()
  x <- vector()
  for (i in 0:n+1)
  {
    x[i] <- i-1
    prob[i] <- dnorm(i-1,miu,sqrt(var))
  }
  matprob <- data.frame(cbind(x,round(prob,3)))
  colnames(matprob) <- c("X","F(x)")
  print(matprob,row.names = F)
}
prog.1(10,8)

#no.2
prog.2 <- function(miu,var)
{
  n <- as.numeric(readline("Masukkan jumlah x : "))
  prob <- vector()
  prob2 <- vector()
  x <- vector()
  for (i in 0:n+1)
  {
    x[i] <- i-1
    prob[i] <- dnorm(i-1,miu,sqrt(var))
    prob2[i] <- pnorm(i-1,miu,sqrt(var))
  }
  matprob <- data.frame(cbind(x,round(prob,3),round(prob2,3)))
  colnames(matprob) <- c("X","F(x)","f(x)")
  print(matprob,row.names = F)
}
prog.2(10,8)

#no.3
prog.3 <- function(miu,var)
{
  n <- as.numeric(readline("Masukkan jumlah x : "))
  prob <- vector()
  x <- vector()
  for (i in 0:n+1)
  {
    x[i] <- i-1
    prob[i] <- dnorm(i-1,miu,sqrt(var))
  }
  matprob <- data.frame(cbind(x,round(prob,3)))
  colnames(matprob) <- c("X","F(x)")
  print(matprob,row.names = F)
  i = which.min(matprob[,2])
  cat("Nilai x yg membuat probabilitas minimum :",i-1,"\n")
  xmin = prob[i]
  cat("Dengan nilai probabilitas :",round(xmin,3),"\n")
}
prog.3(0,1)

#no.4
prog.4 <- function(miu=5,sd=1)
{
  n <- as.numeric(readline("Masukkan akhiran x : "))
  a <- as.numeric(readline("Masukkan awalan x : "))
  by <- as.numeric(readline("Masukkan pemisah x : "))
  x <- as.vector(seq(a,n,by))
  l <- length(x)
  prob <- vector()
  for (i in 1:l)
  {
    prob[i] <- dnorm(x[i],miu,sd)
  }
  matprob <- data.frame(cbind(x,round(prob,3)))
  colnames(matprob) <- c("X","F(x)")
  print(matprob,row.names=F)
  win.graph()
  par(mfrow=c(1,2))
  plot(x,prob,type="l")
  plot(x,prob,type="p")
}
prog.4()

#no.5
data <- tabel_distribusi_frekuensi
data <- as.matrix(data)
colnames(data) <- NULL
prog.5 <- function(data)
{
  cat("PROGRAM MENGHITUNG MEAN, VARIANS, MEDIAN DAN MODUS \n")
  cat("Pilihan : \n")
  cat("1. Mean \n")
  cat("2. Variansi \n")
  cat("3. Median \n")
  cat("4. Modus \n")
  pilihan <- as.numeric(readline("Masukkan pilihan : "))
  xi <- (data[,3]+data[,2])/2
  fr <- cumsum(data[,4])
  data_new <- cbind(data,fr)
  mean <- function(){(sum(xi*data[,4])/sum(data[,4]))}
  if(pilihan == 1)
  {
    return(mean())
  }
  else if(pilihan == 2)
  {
    variansi <- sum(data[,4]*((xi-mean())^2))/(sum(data[,4])-1)
    return(variansi)
  }
  else if(pilihan == 3)
  {
    med <- fr[nrow(data)]/2
    for (i in 1:nrow(data))
    {
      if(med<=data_new[i,5])
      {
        i
        break
      }
    }
    lmed <- data_new[i,2]-0.5
    c <- data[2,2]-data[1,2]
    median <- lmed + (c*((data_new[5,5]/2)-data_new[i-1,5])/data_new[i,4])
    median <- as.vector(median)
    return(median)
  }
  else if(pilihan == 4)
  {
    i <- which.max(data[,4])
    lmod <- data[i,2]-0.5
    c <- data[2,2]-data[1,2]
    a <- data[i,4]-data[i-1,4]
    b <- data[i,4]-data[i+1,4]
    modus <- lmod + c*(a/(a+b))
    return(modus)
  }
  else 
  {
    print("Pilihan Error")
  }
}
prog.5(data)

#no.6
prog.6  <- function()
{
  n <- as.numeric(readline("Masukkan n percobaan : "))
  p <- as.numeric(readline("Masukkan peluang sukses : "))
  while(p>1||p<0)
  {
    cat("Nilai p harus di antara 0 sampai 1, silahkan input p ulang")
    p <- as.numeric(readline("Masukkan peluang sukses : "))
  }
  x <- as.numeric(readline("Masukkan jumlah x : "))
  while(x>n)
  {
    cat("Nilai x harus kurang dari dan sama dengan n, silahkan input x ulang")
    x <- as.numeric(readline("Masukkan jumlah x : "))
  }
  prob <- vector()
  l <- vector()
  for (i in 0:x+1)
  {
    l[i] <- i-1
    prob[i] <- dbinom(i-1,n,p)
  }
  matprob <- data.frame(cbind(l,round(prob,3)))
  colnames(matprob) <- c("X","F(x)")
  print(matprob,row.names = F)
}
prog.6()

#no.7
prog.7 <- function()
{
  cat ("\n")  
  cat ("Metode Iterasi Sederhana")
  e <- as.integer(readline("Input batas toleransi: "))
  xa <- as.integer(readline("Input nilai x awal: "))
  f <- function(x)
  {
    return(2+exp(-2*x))
  }
  xb <- f(xa)
  delta <- abs(xb-xa)
  while(delta>e)
  {
    xb <- f(xa)
    delta <- abs(xb-xa)
    xa=xb
  }
  cat ("Nilai akar persamaan tersebut adalah : ",xb)
}
prog.7()

#no.8
prog.8 <-function()
{
  cat ("\n")  
  cat ("Metode Newton Rapshon")
  i=0
  e=as.numeric(readline("Input batas toleransi: "))
  x0=as.numeric(readline("Input nilai x awal: "))
  f=function(x){(exp(-2*x0)+(x0*x0)-(4*x0))}
  fT=function(X){((2*x0)-4-(2*(exp(-2*x0))))}
  err=abs(f(x0))
  while (err>e)
  {
    x=x0-(f(x0)/fT(X0))
    fx1=f(x)
    fTx1=fT(X)
    err=abs(fx1)
    x0=x
    i=i+1
  }
  cat ("Nilai akar persamaan tersebut adalah : ",x)
}
prog.8()