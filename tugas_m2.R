#Victoria Anggia Alexandra / NIM 082111833089
#Membuat Tabel Mahasiswa
Data_Victoria_A <- data.frame(c("0820123455","0820123456","0820123457",
                                "0820123458","0820123459"),
                              c("DINA KAMALIA","ADI SETIADI","BAGUS SANJAYA"
                                ,"PUTRI ZASKIA","MIRNA ASKIA"),
                              c(60,62,64,60,58),
                              c(3.57,3.76,3.78,3.64,3.38))
names(Data_Victoria_A)[1]="NIM"
names(Data_Victoria_A)[2]="NAMA"
names(Data_Victoria_A)[3]="JML SKS"
names(Data_Victoria_A)[4]="IPK"
Data_Victoria_A

#Import table from txt file
Data_Victoria_B<- read.delim("C:/Users/tori/Documents/Learn R/statkom/tabel_mahasiswa.txt",sep = "\t")
print(Data_Victoria_B,row.names = F)

#import table from excel file
library(readxl)
Data_Victoria_C <- read_excel("~/Learn R/statkom/tabel_mahasiswa.xlsx")

#export data R to excel
install.packages('writexl')
library(writexl)
write_xlsx(Data_Victoria_A, '~/Learn R/statkom/HasilExcel_Victoria.xlsx')



