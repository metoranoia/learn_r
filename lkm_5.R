#1. Buatlah program untuk pengujian dua sisi untuk uji korelasi
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

#2.
ujitkiri <- function(x,miunol,alpha)
{
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

ujitkanan <- function(x,miunol,alpha)
{
  cat("---------------------------------\n")
  cat(" Uji-t SATU SAMPEL \n")
  cat(" UNTUK UJI HIPOTESIS SATU SISI KANAN \n ")
  cat("\n H0 : mean populasi =",miunol,"\n")
  cat("\n H1 : mean populasi >",miunol,"\n\n")
  cat("\n Tingkat signifikansi alpha =",alpha,"\n\n")
  thit <- (mean(x)-miunol)/sqrt(var(x)/n)
  ttabel <- qt(1-alpha,n-1)
  if(thit > ttabel) cat(" Keputusan : TOLAK H0 \n")
  else cat(" Keputusan : GAGAL TOLAK H0 \n")
  cat("-----------------------------------\n")
}

ujittwoway <- function(x,miunol,alpha)
{
  cat("---------------------------------\n")
  cat(" Uji-t SATU SAMPEL \n")
  cat(" UNTUK UJI HIPOTESIS DUA ARAH \n ")
  cat("\n H0 : mean populasi =",miunol,"\n")
  cat("\n H1 : mean populasi ",miunol,"\n\n")
  cat("\n Tingkat signifikansi alpha =",alpha,"\n\n")
  thit <- (mean(x)-miunol)/sqrt(var(x)/n)
  ttabel <- qt(1-(alpha/2),n-1)
  if(abs(thit) > ttabel) cat(" Keputusan : TOLAK H0 \n")
  else cat(" Keputusan : GAGAL TOLAK H0 \n")
  cat("-----------------------------------\n")
}

ujizkiri <- function(x,miunol,alpha)
{
  cat("---------------------------------\n")
  cat(" Uji-z SATU SAMPEL \n")
  cat(" UNTUK UJI HIPOTESIS SATU SISI KIRI \n ")
  cat("\n H0 : mean populasi =",miunol,"\n")
  cat("\n H1 : mean populasi <",miunol,"\n\n")
  cat("\n Tingkat signifikansi alpha =",alpha,"\n\n")
  zhit <- (mean(x)-miunol)/sqrt(var(x)/n)
  ztabel <- qnorm(1-alpha)
  if(zhit < -ztabel) cat(" Keputusan : TOLAK H0 \n")
  else cat(" Keputusan : GAGAL TOLAK H0 \n")
  cat("-----------------------------------\n")
}

ujizkanan <- function(x,miunol,alpha)
{
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

ujiztwoway <- function(x,miunol,alpha)
{
  cat("---------------------------------\n")
  cat(" Uji-z SATU SAMPEL \n")
  cat(" UNTUK UJI HIPOTESIS DUA ARAH \n ")
  cat("\n H0 : mean populasi =",miunol,"\n")
  cat("\n H1 : mean populasi ≠",miunol,"\n\n")
  cat("\n Tingkat signifikansi alpha =",alpha,"\n\n")
  zhit <- (mean(x)-miunol)/sqrt(var(x)/n)
  ztabel <- qt(1-(alpha/2),n-1)
  if(abs(zhit) > ztabel) cat(" Keputusan : TOLAK H0 \n")
  else cat(" Keputusan : GAGAL TOLAK H0 \n")
  cat("-----------------------------------\n")
}

uji_hipotesis <- function(x,miunol,alpha)
{
  x <- as.vector(x)
  n <- length(x)
  cat(" Pilihan arah uji \n")
  cat(" 1 = Uji satu sisi kanan \n")
  cat(" 2 = Uji satu sisi kiri \n")
  cat(" 3 = uji dua arah")
  pilihan <- as.integer(readline("Masukkan pilihan : "))
  pil <- print(pilihan)
  
  if (n >= 30) {
    if(pil == 1){
      ujizkanan(x,miunol,alpha) 
    }
    else if(pil == 2){
      ujizkiri(x,miunol,alpha) 
    }
    else if(pil == 3){
      ujiztwoway(x,miunol,alpha)
    }
  }
  else if (n < 30){
    if(pil == 1){
      ujitkanan(x,miunol,alpha) 
    }
    else if(pil == 2){
      ujitkiri(x,miunol,alpha) 
    }
    else if(pil == 3){
      ujittwoway(x,miunol,alpha)
    }
  }
  else {
    cat(" \n Pilihan eror")
  }
}
x <- c(2,6,4,8,3,9)
uji_hipotesis(x,7.5,0.05)

#3. Buat program untuk menghitung total tagihan
prog_listrik <- function()
{
  golongan <- as.numeric(readline("Masukkan golongan tarif listrik : "))
  kwh_pabl <- as.integer(readline("Masukkan kwh pemakaian akhir bulan lalu : "))
  kwh_pabs <- as.numeric(readline("Masukkan kwh pemakaian akhir bulan sekarang : "))
  total_pemakaiaan <- kwh_pabs - kwh_pabl
  if (total_pemakaiaan <= 100) {
    if (golongan == 1) {
      total_tagihan = 25000
      cat(sprintf("%.0f",total_tagihan))
    }
    else if (golongan == 2) {
      total_tagihan = 35000
      cat(sprintf("%.0f",total_tagihan))
    }
  }
  else if (total_pemakaiaan < 200){
    if (golongan == 1) {
      total_tagihan = 25000 + (total_pemakaiaan-100)*2000
      cat(sprintf("%.0f",total_tagihan))
    }
    else if (golongan == 2) {
      total_tagihan = 35000 + (total_pemakaiaan-100)*2200
      cat(sprintf("%.0f",total_tagihan))
    }
  }
  else if (total_pemakaiaan < 300){
    if (golongan == 1) {
      total_tagihan = 25000 + 100*2000 + (total_pemakaiaan-200)*2300
      cat(sprintf("%.0f",total_tagihan))
    }
    else if (golongan == 2) {
      total_tagihan = 35000 + 100*2200 + (total_pemakaiaan-200)*2600
      cat(sprintf("%.0f",total_tagihan))
    }
  }
  else {
    if (golongan == 1) {
      total_tagihan = 25000 + 100*2000 + 100*2300 + (total_pemakaiaan-300)*2500
      cat(sprintf("%.0f",total_tagihan))
    }
    else if (golongan == 2) {
      total_tagihan = 35000 + 100*2200 + 100*2300 + (total_pemakaiaan-300)*2500
      cat(sprintf("%.0f",total_tagihan))
    }
  }
}
prog_listrik()
