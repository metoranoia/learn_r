#1. Buatlah program untuk menhitung nilai akhir siswa
prakt2.1 <- function()
{
  x <- as.numeric(readline("Input nilai tugas : "))
  y <- as.numeric(readline("Input nilai UTS : "))
  z <- as.numeric(readline("Input nilai UAS :"))
  hasil <- 0.2*x + 0.4*y + 0.4*z
  cat("PROGRAM MENGHITUNG NILAI AKHIR MATA KULIAH \n")
  cat("------------------------------------------- \n")
  cat("Tugas : ",round(x,0),"\n")
  cat("UTS : ",round(y,0),"\n")
  cat("UAS : ",round(z,0),"\n")
  cat("Nilai akhir : ",round(hasil,0),"\n")
}
prakt2.1()

#2. Buatlah fungsi untuk menentukan volume dan luas akhir tabung
prakt2.2 <- function(r,t=10)
{
  luas_permukaan <- 2*pi*r*(t+r)
  volume <- pi*r^2*t
  cat("Luas permukaan tabung berjari-jari 10 cm dengan tinggi 10 cm adalah : \n")
  cat(round(luas_permukaan,2),"cm2 \n")
  cat("Volume tabung berjari-jari 10 cm dengan tinggi 10 cm adalah : \n")
  cat(round(volume,2),"cm2 \n")
}
prakt2.2(10)

#3. Buatlah program untuk menghitung nilai y = x^2 + ln(x)
prog3 <- function()
{
  cat("PROGRAM MENAMPILKAN HASIL FUNGSI y=x^2+ln(x) \n")
  cat("------------------------------------------- \n")
  x = as.numeric(readline("input nilai x :"))
  y <- log(x)+x^2
  cat("Hasilnya adalah... \n")
  cat("Nilai y asli :",y,"\n")
  cat("Nilai y dibulatkan 3 digit :",round(y,3),"\n")
  cat("Nilai y dibulatkan ke atas :",ceiling(y),"\n")
  cat("Nilai y dibulatkan ke bawah :",floor(y),"\n")
}
prog3()

#4. Buatlah program untuk menghitung nilai korelasi
prog4 <- function()
{
  x <- as.numeric(readline("Jumlah elemen vektor x: "))
  y <- as.numeric(readline("Jumlah elemen vektor y: "))
  z <- scan(n=x)
  w <- scan(n=y)
  vektor_x <- c(z)
  cat("Vektor X : \n")
  print(vektor_x)
  vektor_y <- c(w)
  cat("Vektor Y : \n")
  print(vektor_y)
  jmlxy <- sum(vektor_x*vektor_y)
  jmlerxy <- sum((vektor_x-mean(vektor_x))*(vektor_y-mean(vektor_y)))
  r <- (length(vektor_x)*jmlxy-sum(vektor_x)*sum(vektor_y))/
    (sqrt(length(vektor_x)*sum(vektor_x^2)-(sum(vektor_x))^2)*sqrt(length(vektor_y)*sum(vektor_y^2)-(sum(vektor_y))^2))
  cat("Nilai Korelasi antara X dan Y adalah : \n")
  print(r)
}
prog4()

#5. Buatlah program untuk menghitung probabilitas variabel random berdistribusi normal pada suatu nilai x tertentu
prog5 <- function(x,mean=2,variansi=0.3)
{
  n <- as.integer(readline("Input n : "))
  x <- c(0:n)
  data <- dnorm(x,mean,sqrt(variansi))
  print(data)
}
prog5(x)

#6. Buatlah program untuk menghasilkan matriks M yang merupakan matriks pasangan data regresi
prog6 <- function() 
{
  x <- as.numeric(readline("Jumlah n pengamatan: "))
  y <- as.numeric(readline("Jumlah p variabel prediktor: "))
  z <- scan(n=x)
  w <- scan(n=x*y)
  vektor_y <- c(z)
  cat("Vektor Y : \n")
  print(vektor_y)
  vektor_j <- rep(1,x) 
  matriks_x <- matrix(c(vektor_j,w),nrow=x,ncol=y+1)
  cat("Matriks X : \n")
  print(matriks_x)
  partisi_matriks_x <- matriks_x[,c(2:(y+1))]
  M <- cbind(vektor_y,partisi_matriks_x)
  colnames(M) <- NULL
  cat("Matriks pasangan data regresi (M) : \n")
  print(M)
}
prog6()

#7a. Carilah nilai-nilai y untuk X = (1,1.5,2,2.5,3,3.5,4,4.5,5)
prog7.a <- function(x)
{
  y <- x^3 - 2*x^2 + 8*x +3
  print(y)
}
x <- c(1,1.5,2,2.5,3,3.5,4,4.5,5)
prog7.a(x)

#7b. Buatlah plot antara x dan y type "point" (scatter plot)
prog7.b <- function()
{
  x <- c(1,1.5,2,2.5,3,3.5,4,4.5,5)
  z <- plot(x,prog7.a(x),type = "p",main = "Hubungan antara X dan Y",ylab = "y")
  print(z)
}
prog7.b()

#7c. Buatlah plot antara x dan y type "line"
prog7.c <- function()
{
  x <- c(1,1.5,2,2.5,3,3.5,4,4.5,5)
  z <- plot(x,prog7.a(x),type = "l",main = "Hubungan antara X dan Y",ylab = "y")
  print(z)
}
prog7.c()

#7d. Buatlah plot antara x dan y type "point + line"
prog7.d <- function()
{
  x <- c(1,1.5,2,2.5,3,3.5,4,4.5,5)
  z <- plot(x,prog7.a(x),type = "b",main = "Hubungan antara X dan Y",ylab = "y")
  print(z)
}
prog7.d()

#7e. Buatlah plot antara x dan y type "point" kemudian tammbahkan plot dengan perintah lines
prog7.e <- function()
{
  x <- c(1,1.5,2,2.5,3,3.5,4,4.5,5)
  prog7.b()
  w <- lines(x,prog7.a(x),col="red")
  print(w)
}
prog7.e()

#7f. Buatlah plot antara x dan y untuk beberapa type (b,c,d,e) dalam
prog7.f <- function()
{
  x <- c(1,1.5,2,2.5,3,3.5,4,4.5,5)
  win.graph()
  par(mfrow=c(2,2))
  a <- prog7.b()
  b <- prog7.c()
  c <- prog7.d()
  d <- prog7.e()
}
prog7.f()

#8a. Carilah nilai x yang membuat y minimum
prog8.a <- function()
{
   x <- c(1.7,3.55,0.75,2.4,4,0.7,2.2,2.05,2.9,1.2)
   y <- c(12.02,9.245,8.125,12.58,6.5,7.82,12.62,12.545,11.78,10.42)
   M <- matrix(cbind(x,y),nrow = length(x),ncol=2,byrow = F)
   x_minimum <- M[order(M[,2],decreasing = F),][1,1]
   print(x_minimum)
}
prog8.a()

#8b. Buatlah plot antara x dan y type "point" (scatter plot)
prog8.b <- function(x,y)
{
  z <- plot(x,y,type = "p",main = "Hubungan antara X dan Y",xlab="x",ylab = "y")
  print(z)
}
a <- c(1.7,3.55,0.75,2.4,4,0.7,2.2,2.05,2.9,1.2)
b <- c(12.02,9.245,8.125,12.58,6.5,7.82,12.62,12.545,11.78,10.42)
prog8.b(a,b)

#8c. Buatlah plot antara x dan y type "garis"
prog8.c <- function(x,y)
{
  z <- plot(x,y,type = "l",main = "Hubungan antara X dan Y",xlab="x",ylab = "y")
  print(z)
}
prog8.c(a,b)

#9a. Buatlah histogram data x (random berdistribusi normal mean=1, sd=sqrt(0.6))
prog9.a.1 <- function(n)
{
  x <- rnorm(n,1,sqrt(0.6))
  print(x)
}
prog9.a.2 <- function(x)
{
  x <- prog9.a.1(n)
  print(hist(x))
}
n <- 20
prog9.a.2(n)

#9b. Buatlah x dengan n = 40,80,200,1000 dan buatlah histogram masing-masing data X
prog9.b <- function()
{
  a <- prog9.a.2(40)
  b <- prog9.a.2(80)
  c <- prog9.a.2(200)
  d <- prog9.a.2(1000)
  print(hist(a,b,c,d))
}
prog9.b()

#10. Buatlah program untuk memecah uang menjadi pecahan mata uang Indonesia
prog.10 <- function()
{
  cat("PROGRAM MEMECAH UANG \n")
  cat("-------------------------------- \n")
  uang <- as.integer(readline("Input jumlah uang -> "))
  cat("Terdiri dari -> \n")
  cat("-------------------------------- \n")
  uang_pecahan <- c(100000, 50000, 20000, 10000, 5000, 2000, 1000, 500, 200, 100)
  jumlah_pecahan <- {}
  sisa <- uang
  cat("\n")
  for (pecahan in uang_pecahan)
  {
    if (sisa < pecahan)
    {
      banyak_pecahan = 0
    }
    banyak_pecahan = as.integer(sisa / pecahan)
    sisa = sisa - ( pecahan * banyak_pecahan )
    cat(banyak_pecahan, "lembar Rp ", sprintf("%.0f",pecahan),"\n")
  }
}
prog.10()