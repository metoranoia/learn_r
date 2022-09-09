#contoh 1 
rata <- function(x)
 {
   rata_rata <- sum(x)/length(x)
   rata_rata
 }
 x <- c(1,2,3,4,5)
 rata(x)
 
 #contoh 2
 prog1 <- function(x,a)
 {
   y <- x^2 + a
   return(y)
 }
 prog1(2,3)
 
 prog2 <- function(x,a)
 {
   y <- prog1(2,3)
   z <- y^3 + a
   return(z)
 }
 prog2(3,5)
 
 #contoh 3
 datanormal <- function(n=10,mean=15,variansi=9)
 {
   y3 <- rnorm(n,mean,sqrt(variansi))
   y3
 }
 datanormal()
 datanormal(20,9,6)
 
 #contoh 4
 datanormal1 <- function(n,mean,variansi=10)
 {
   data <- rnorm(n,mean,sqrt(variansi))
   print(data)
 }
 datanormal1(10,4)

 #contoh 5
 datanormal3 <- function(n=10,mean=15,variansi=9)
 {
   data <- rnorm(n,mean,sqrt(variansi))
   cat("=============\n")
   cat("List Data \n")
   cat("=============\n")
   cat (data, sep="\n")
   cat("=============\n")
 }
 datanormal3()
 
 #contoh 6, export ke file txt hasilnya
 datanormal4 <- function(n=10,mean=15,variansi=9)
 {
   data <- rnorm(n,mean,sqrt(variansi))
   cat("=============\n",file = "E:output.txt")
   cat("List Data \n",append = T,file = "E:output.txt")
   cat("=============\n",append = T,file = "E:output.txt")
   cat (data, sep="\n",append = T,file = "E:output.txt")
   cat("=============\n",append = T,file = "E:output.txt")
 }
datanormal4()

#contoh 7
datanormal5 <- function(n=10,mean=15,variansi=9)
{
  z <- rnorm(n,mean,sqrt(variansi))
  return(z)
}
datanormal5()

#contoh 8
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

#contoh 9
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
