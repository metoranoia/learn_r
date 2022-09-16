misal.1 <- function(x)
{
  y <- 0
  if(x!=0){
  y <- exp(x)/x
  }
  return (y)
}
x <- 5
misal.1(x)

misal.2 <- function(x)
{
  if(x<5){
    y <- x^2
  }
  else if (x<15){
    y <- 2*x-sqrt(x)
  }
  else {
    y <- x^(1/3)
  }
  return(y)
}
x <- 14
misal.2(x)

ujizkanan <- function(x,miunol,alpha)
{
  x <- as.vector(x)
  n <- length(x)
  cat("\nDATA SAMPEL :\n")
  print(x)
  cat("\n")
  cat("---------------------------------\n")
  cat(" Uji-z SATU SAMPEL \n")
  cat(" UNTUK UJI HIPOTESIS SATU SISI KANAN \n ")
  cat("\n H0 : mean populasi =",miunol,"\n")
  cat("\n H1 : mean populasi >",miunol,"\n\n")
  cat("\n Tingkat signifikansi alpha =",alpha,"\n\n")
  zhit <- (mean(x)-miunol)/sqrt(var(x)/n)
  ztabel <- qnorm(1-alpha)
  if(zhit > ztabel) cat(" Keputusan : TOLAK H0 \n")
  else cat(" Keputusan : GAGAL TOLAK H0 \n")
  cat("-----------------------------------\n")
}
x <- c(2,6,4,8,3,9)
ujizkanan(x,7.5,0.05)

ujizkanan_pvalue <- function(x,miunol,alpha)
{
  x <- as.vector(x)
  n <- length(x)
  cat("\nDATA SAMPEL :\n")
  print(x)
  cat("\n")
  cat("---------------------------------\n")
  cat(" Uji-z SATU SAMPEL \n")
  cat(" UNTUK UJI HIPOTESIS SATU SISI KANAN \n ")
  cat("\n H0 : mean populasi =",miunol,"\n")
  cat("\n H1 : mean populasi >",miunol,"\n\n")
  cat("\n Tingkat signifikansi alpha =",alpha,"\n\n")
  zhit <- (mean(x)-miunol)/sqrt(var(x)/n)
  ztabel <- qnorm(1-alpha)
  pvalue <- pnorm(zhit,0,1,lower.tail = F)
  if(pvalue < alpha) cat(" Keputusan : TOLAK H0 \n")
  else cat(" Keputusan : GAGAL TOLAK H0 \n")
  cat("-----------------------------------\n")
}
x <- c(2,6,4,8,3,9)
ujizkanan_pvalue(x,7.5,0.05)

ujitkiri <- function(x,miunol,alpha)
{
  x <- as.vector(x)
  n <- length(x)
  cat("\nDATA SAMPEL :\n")
  print(x)
  cat("\n")
  cat("---------------------------------\n")
  cat(" Uji-t SATU SAMPEL \n")
  cat(" UNTUK UJI HIPOTESIS SATU SISI KIRI \n ")
  cat("\n H0 : mean populasi =",miunol,"\n")
  cat("\n H1 : mean populasi <",miunol,"\n\n")
  cat("\n Tingkat signifikansi alpha =",alpha,"\n\n")
  thit <- (mean(x)-miunol)/sqrt(var(x)/n)
  ttabel <- qt(1-alpha,n-1)
  if(thit < -ttabel) cat(" Keputusan : TOLAK H0 \n")
  else cat(" Keputusan : GAGAL TOLAK H0 \n")
  cat("-----------------------------------\n")
}
x <- c(2,6,4,8,3,9)
ujitkiri(x,7.5,0.05)

ujitkiri_pvalue <- function(x,miunol,alpha)
{
  x <- as.vector(x)
  n <- length(x)
  cat("\nDATA SAMPEL :\n")
  print(x)
  cat("\n")
  cat("---------------------------------\n")
  cat(" Uji-t SATU SAMPEL \n")
  cat(" UNTUK UJI HIPOTESIS SATU SISI KIRI \n ")
  cat("\n H0 : mean populasi =",miunol,"\n")
  cat("\n H1 : mean populasi <",miunol,"\n\n")
  cat("\n Tingkat signifikansi alpha =",alpha,"\n\n")
  thit <- (mean(x)-miunol)/sqrt(var(x)/n)
  ttabel <- qt(1-alpha,n-1)
  pvalue <- pt(thit,df=n-1,lower.tail = T)
  if(pvalue < alpha) cat(" Keputusan : TOLAK H0 \n")
  else cat(" Keputusan : GAGAL TOLAK H0 \n")
  cat("-----------------------------------\n")
}
ujitkiri_pvalue(x,7.5,0.05)
