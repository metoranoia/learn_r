#Victoria Anggia Alexandra / NIM 082111833089
#1a. Membuat vektor x = (9,16,5,13,11,10,6,14,6,7,17,10,8,9,6)
x <- c(9,16,5,13,11,10,6,14,6,7,17,10,8,9,6)
x
#1b. Membuat vektor y = (4,5,6,7,8,9,10,11,12)
y <- c(4:12)
y
#1c. Membuat matriks M dengan elemen vektor X
M <- matrix(x,nrow = 5,ncol = 3,byrow=F)
M
#1d. Membuat matriks N dengan elemen vektor y
N <- matrix(y,nrow = 3,byrow=T)
N
#1e. Membuat matriks baru A, dengan elemen baris 2-4 matriks M
A <- M[c(2:4),]
A
#1f. Membuat matriks baru B, dengan elemen baris 1,2,4 dari matrik M
B <- M[c(1,2,4),]
B
#1g. Membuat matriks baru C, dengan cara menghilangkan kolom ketiga dari matrik M
C <- M[,-3]
C
#1h. Menentukan D, dengan D = invers M
D <- Ginv(M)
D
#1i. Menentukan E, dengan E=MxN
E <- M %*% N
E
#1j. Menentukan NxM, dan mengapa itu terjadi ?
N %*% M
#1k. Menentukan G = M'M
G <- t(M)%*%M
G
#1l. Menentukan G inverse
solve(G)
#1m. Membuat matrik baru H, dengan cara mengambil baris-baris yg kolom pertama dari matrik M yg < 10
H <- subset(M,M[,1]<10)
H
#1n. Membuat matrik K, gabungan baris dari matrik M dan N
K <- rbind(M,N)
K
#1o. Mencari banyaknya baris matriks M
nrow(M)
#1p. Mencari banyaknya kolom matriks M
ncol(M)
#1q. Menentukan determinan matriks N
det(N)

#2a. Buatlah matrik A
matrix_A <- matrix(c(5,7,12,14,6,5,8,15,7,7,10,22,12,17,8),nrow=5,ncol=3,byrow = T)
matrix_A
#2b. Buatlah matrik baru dengan elemen kolom ke-3 dari matrik A
matrix_B <- matrix(matrix_A[,3],nrow = 5,ncol = 1)
matrix_B
#2c. Buatlah matrik baru dengan mengambil baris 2 sampai 4 dari matrik A
matrix_C <- matrix_A[c(2:4),]
matrix_C
#2d. Buatlah matrik baru dengan mengambil baris 1,3,5 dari matrik A
matrix_D <- matrix_A[c(1,3,5),]
matrix_D
#2e. Buatlah matrik baru dengan mengambil baris-baris yg kolom pertama dari matrik A yg < 12
matrix_E <- subset(matrix_A,matrix_A[,1]<12)
matrix_E

#3a. Buatlah vektor c
vector_c <- c(1,1,1,1)
vector_c
#3b. Buatlah matriks X yang merupakan gabungan dari vektor C,X1, dan x2
x1 <- c(14,12,10,17)
x2 <- c(24,20,27,22)
X <- cbind(vector_c,x1,x2)
X
#3c. Hitunglah nilai dugaan koefisien regresi
Y <- c(5,8,7,4)
Btopi <- inv(t(X)%*%X)%*%t(X)%*%Y
Btopi

#4 Import data dari file .txt dan beri nama dataku, lakukan operasi terhadap dataku
dataku <- read.delim("C:/Users/tori/Documents/Learn R/statkom/contohdata_prakt2.txt",sep = "\t")
dataku*2
is.matrix(dataku)
is.data.frame(dataku)
#5 Tambahkan 1 kolom ytopi di kolom terakhir dataku, simpan dengan nama databaru, export ke file .txt
ytopi <- matrix(c(123:128),nrow = 6,ncol = 1)
databaru <- cbind.data.frame(dataku,ytopi)
databaru
write.table(databaru,"D:/databaru.txt",sep="\t")
