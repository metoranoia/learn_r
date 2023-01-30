#latihan no.1
prog1 <- function()
{
  x = as.numeric(readline("input nilai x :"))
  cat("Hasil nilai y adalah :","\n")
  y <- -exp(-x)-(x^2)+10
  print(y)
}
prog1()

#latihan no.2
prog2 <- function()
{
  x1 <- c(1,2,3,4,5)
  cat("Vector X :","\n")
  print(x1)
  jmlx <- sum(x1)
  cat("Jumlah elemen vektor X:","\n")
  print(jmlx)
  jmlxx <- sum(x1^2)
  cat("Jumlah elemen vektor X yang dikuadratkan :","\n")
  print(jmlxx)
  jmlerx <- sum((x1-mean(x1))^2)
  cat("Jumlah elemen vektor X dikurangi rata-rata X dan dikuadratkan :","\n")
  print(jmlerx)
}
prog2()

#latihan no.3
prog3 <- function()
{
  x = as.numeric(readline("input nilai x :"))
  y <- log(x)+exp(-x)
  cat("Nilai y tanpa pembatasan digit desimal :","\n")
  print(y)
  cat("Nilai y, desimalnya dibatasi sampai 3 digit :","\n")
  print(round(y,3))
  cat("Nilai y, dibulatkan ke atas :","\n")
  print(ceiling(y))
  cat("Nilai y, dibulatkan ke bawah :","\n")
  print(floor(y))
}
prog3()

