jip <- function(n)
{
  jml <- 0
  for (i in 1:n)
  {
    jml <- jml+i
  }
  return(jml)
}
jip(10)

tabelnilplot <- function(nilawal)
{
  x <- nilawal
  vecx <- rep(0,20)
  vecfx <- rep(0,20)
  cat("---------------------\n")
  cat("  x      f(x)        \n")
  cat("---------------------\n")
  for(i in 1:20)
  {
    fx <- exp(-x)
    vecx[i] <- x
    vecfx[i] <- fx
    cat(" ",x," ",fx,"\n")
    x <- x + 0.2
  }
  cat("---------------------\n")
  plot(vecx,vecfx,type="l")
}
tabelnilplot(1)


prog1 <- function(x)
{
  n <- length(x)
  jml <- 0
  k <- 0
  while(k<n)
  {
    k <- k+1
    jml <- jml+x[k]
  }
  rat <- jml/n
  return(rat)
}
x <- c(1:20)
prog1(x)

prog2 <- function(x)
{
  n <- length(x)
  jml <- 0
  k <- 1
  while(k<=n)
  {
    jml <- jml+x[k]
    k <- k+1
  }
  rat <- jml/n
  return(rat)
}
prog2(x)

prog3 <- function(x)
{
  n <- length(x)
  jml <- 0
  k <- 0
  repeat
  {
    k <- k+1
    jml <- jml+x[k]
    if (k==n) break
  }
  rat <- jml/n
  return(rat)
}
x <- c(1:20)
prog3(x)

prog4 <- function(x)
{
  n <- length(x)
  jml <- 0
  k <- 1
  repeat
  {
    jml <- jml+x[k]
    if (k==n) break
    k <- k+1
  }
  rat <- jml/n
  return(rat)
}
prog4(x)

prognested0 <- function(n)
{
  for (i in 1:n)
  {
    for (j in 1:n) 
    {
      cat(" ",j)
    }
    cat("\n")
  }
}
prognested0(5)

prognested1 <- function(n)
{
  for (i in 1:n)
  {
    for (j in 1:i) 
    {
      cat(" ",j)
    }
    cat("\n")
  }
}
prognested1(5)

prognested2 <- function(n)
{
  for (i in 1:n)
  {
    for (j in i:n) 
    {
      cat(" ",j)
    }
    cat("\n")
  }
}
prognested2(5)

prognested3 <- function(n)
{
  for (i in 1:n)
  {
    for (j in n:i) 
    {
      cat(" ",j)
    }
    cat("\n")
  }
}
prognested3(5)

prognested4 <- function(n)
{
  for (i in n:1)
  {
    for (j in 1:i) 
    {
      cat(" ",j)
    }
    cat("\n")
  }
}
prognested4(5)

#6.4
prog6.4 <- function()
{
  bb <- as.numeric(readline("input batas bawah : "))
  ba <- as.numeric(readline("input batas atas : "))
  while(bb>ba)
  {
    cat("Batas bawah harus kurang dari batas atas, silahkan input batas atas ulang")
    ba <- as.numeric(readline("input batas atas : "))
  }
  increament <- as.numeric(readline("input increment : "))
  vecx <- seq(bb,ba,increament)
  n <- length(vecx)
  vfx <- (1/2)*vecx^2+exp(-vecx)
  print(vfx)
  nilmin = min(vfx)
  i <- 1
  for i in n
  {
    if (vfx[i]==nilmin)
    {
      
    }
  }
  
}
prog6.4()
