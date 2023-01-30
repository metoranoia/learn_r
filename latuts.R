#soal no.1
ujikorelasi_twoway <- function(x,y,alpha)
{
  x <- as.vector(x)
  y <- as.vector(y)
  n <- length(x)
  cat("\nDATA SAMPEL I :\n")
  print(x)
  cat("\nDATA SAMPEL II :\n")
  print(y)
  r <- cor(x,y,method = "pearson")
  cat("\n")
  cat("---------------------------------\n")
  cat(" Uji Korelasi (DUA SAMPEL) \n")
  cat(" UNTUK UJI HIPOTESIS DUA SISI \n ")
  cat("\n H0 : ρ = 0 (Tidak Ada Korelasi) \n")
  cat("\n H1 : ρ ≠ 0 (Ada Korelasi) \n\n")
  cat("\n Tingkat signifikansi alpha =",alpha,"\n\n")
  thit <- (r*sqrt(n-2))/sqrt(1-r^2)
  ttabel <- qt(1-(alpha/2),n-2)
  if(abs(thit) > -ttabel) cat(" Keputusan : TOLAK H0 \n")
  else cat(" Keputusan : GAGAL TOLAK H0 \n")
  cat("-----------------------------------\n")
}
x <- c(2,6,4,8,3,9)
y <- c(3,7,5,2,6,1)
ujikorelasi_twoway(x,y,0.05)

#soal no.2

#soal no.3 uji hipotesis data berpasangan
x <- matrix(c(12,13,14,15,13,13,11,12,12,13,10,11,15,16,13,13,9,8),ncol=2,byrow = T)
tpaired <- function (x,d0,alpha)
{
  cat("Uji Hipotesis 2 Data Berpasangan (T-Test) \n")
  cat("-------------------------------------------\n")
  cat(" UNTUK UJI HIPOTESIS DUA SISI \n")
  cat("\n H0 : miu d = ",d0,"\n")
  cat("\n H1 : miu d ≠ ",d0,"\n\n")
  cat("Tingkat signifikansi alpha =",alpha,"\n\n")
  dbar <- mean(x[,2]-x[,1])
  sd <- sd(x[,2]-x[,1])
  n <- nrow(x)
  thit <- (dbar-d0)/(sd/sqrt(n))
  pvalue <- 2*pt(thit,n-1,lower.tail = F)
  if (pvalue < alpha)
  {
    cat("Keputusan : Menolak H0 \n")
  }
  else
  {
    cat("Keputusan : Gagal menolak H0 \n")
  }
}
tpaired(x,0,0.05)

# soal no.4
funkon <- function() {
  bb <- as.numeric(readline("Masukkan batas bawah : "))
  ba <- as.numeric(readline("Masukkan batas atas : "))
  incr <- as.numeric(readline("Masukkan increament : "))
  Vec <- seq(bb,ba,by=incr)
  y <- vector()
  for (i in 1:length(Vec)) 
  {      
    x <- Vec[i]    
    if (x < 0) 
    {       
      y[i] <- x^2 + 2*x + 3     
    } 
    else if(x < 2) 
    {       
      y[i] <- x + 3     
    } 
    else 
    {       
      y[i] <- x^2 + 4*x - 7     
    }
  }
  graph <- function(Vec,y)
  {
    win.graph()
    grafik <- plot(Vec, y, type='l')
    print(grafik)
  }
  fungsi <- list(Vec,y,graph(Vec,y))
  return(fungsi)
}
funkon()

#soal no.5
A <- matrix(c(27,6,-1,6,15,2,1,1,54),nrow=3,ncol=3,byrow=T)
b <- c(85,72,110)
iterasi <- function(A,b,t0,N)
{
  x0 <- rep(0,3)
  step = -2
  dinv <- diag(1/(diag(A)))
  r <- A-diag(diag(A))
  x1 <- dinv%*%(b-r%*%x0)
  error <- abs(sum(x1-x0))
  while(error > t0)
  {
    x1 <- dinv%*%(b-r%*%x0)
    error = abs(sum(x1-x0))
    x0 = x1
    step = step + 1
  }
  if(step>N)
  {
    cat("Warning : Melebihi batas iterasi maksimum \n")
  }
  else 
  {
    cat("Vektor x berdasarkan iterasi \n")
    print(round(x1,3))
    cat("Banyaknya iterasi : \n")
    print(step)
  }
}
iterasi(A,b,0.00000001,20)
