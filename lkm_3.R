#1. misalkan terdapat sebuah vektor x, hitunglah
x <- c(4,5,3,8,5,9,7,10,4,10)
jmlx <- sum(x)
jmlx
jmlxx <- sum(x^2)
jmlxx
jmlerx <- sum((x-mean(x))^2)
jmlerx

#2. misalkan terdapat dua buah vektor x dan y
x2 <- c(4,6,3,11,12,14)
y2 <- c(10,4,7,6,10,12)
jmlxy <- sum(x2*y2)
jmlxy
jmlerxy <- sum((x2-mean(x2))*(y2-mean(y2)))
jmlerxy
r <- (length(x2)*jmlxy-sum(x2)*sum(y2))/
      (sqrt(length(x2)*sum(x2^2)-sum(x2)^2)*sqrt(length(y2)*sum(y2^2)-sum(y2)^2))
r

#3. Buatlah scatterplot yang menggambarkan hubungan antara X dan Y pada soal no.2
plot(x2,y2,type = "p",xlab ="Vector X",ylab = "Vector Y",main = "Hubungan antara X dan Y" )

#4. Misalkan diketahui fungsi y = |ln x - e^-x|
#4a. Berapa nilai y untuk x = 10
y3 <- abs(log(10)-exp(-10))
y3
#4b. Bulatkan nilai y jika dinyatakan 3 digit desimal
round(y3,3)
#4c. Bulatkan nilai y ke atas
ceiling(y3)
#4d. Bulatkan nilai y ke bawah
floor(y3)

#5a. Normal Baku
#P(X=6)
dnorm(x=6,mean=0,sd=1)
#P(X<6)
1-pnorm(q=6,mean=0,sd=1,lower.tail = F)
#P(X>=6)
pnorm(q=6,mean=0,sd=1,lower.tail = F)

#5b. Binomial (n=20 dan p=0.2)
#P(x=6)
dbinom(x=6,size=20,prob = 0.2)
#P(x<6)
1-pbinom(q=6,size = 20,prob = 0.2,lower.tail = F)
#P(X>=6)
pbinom(q=6,size = 20,prob = 0.2,lower.tail = F)

#5c. Poisson (lambda = 4)
#P(x=6)
dpois(x=6, lambda = 4)
#P(x<6)
1-ppois(q=6,lambda=4,lower.tail = F)
#P(X>=6)
ppois(q=6,lambda=4,lower.tail = F)

#6. Buat program untuk menentukan pada posisi (indeks) ke berapa terdapat nilai (a) minimum data ? (b) maksimum data ?
order(x,decreasing = F)[1] #(a) minimum data
order(x,decreasing = T)[1] #(b) maksimum data

#7. Misalkan diketahui fungsi y = 1+8x-x^2
#7a. Nilai y untuk x={3,4,5,6,7,8,9,10}
x7 <- c(3:10)
y7 <- 1+8*x7-x7^2 
fx7 <- matrix(c(x7,y7),ncol=2)
colnames(fx7) <- c("X","Y")
fx7
#7b. Carilah nilai x yang membuat y maksimum
fx7[order(fx7[,2],decreasing=T),][1,1]

#8. Buatlah matrik baru yg merupakan matrik terurut M berdasarkan X dengan urutan menaik
# Semisal matrik baru diberi nama N
M <- matrix(c(1,4,3,7,5,6,9,3,2,8),ncol=2,byrow = F)
colnames(M) <- c("Y","X")
N <- M[order(M[,2],decreasing=F),]
N

#9a. Carilah nilai Z_alfa untuk alfa = 0.025,0.05,0.075,0.10
qnorm(c(0.025,0.05,0.075,0.10),mean = 0,sd=1)
#9b. carilah p-value uji kiri Z hitung
pnorm(-1.78,mean = 0,sd=1,lower.tail = T)
#9c. carilah p-value uji kanan z hitung
pnorm(2.15,mean = 0,sd=1,lower.tail = F)
#9d. carilah p-value uji dua arah z hitung
2*pnorm(1.99,mean = 0,sd=1,lower.tail = F)

#10a. Carilah nilai t_alfa untuk alfa = 0.025,0.05,0.075,0.10 (df = 9)
qt(c(0.025,0.05,0.075,0.10),df=9)
#10b. carilah p-value uji kiri t hitung (df=9)
pt(-2.48,df=9,lower.tail = T)
#10c. carilah p-value uji kanan t hitung (df=9)
pt(1.89,df=9,lower.tail = F)
#10d. carilah p-value uji dua arah t hitung (df=9)
2*pt(2.21,df=9,lower.tail = F)

#11. Ambil sampel dari vektor x secara acak tanpa pengembalian sebanyak 10 sampel
x_11 <- c(25,24,28,23,21,20,16,22,14,18,21,21,20,22,24,31,29,25,24,20)
sample(x_11,10,replace = F)
