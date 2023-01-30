library(tcltk)
library(tcltk2)
prog.1 <- function()
{
  jendela <- tktoplevel()
  tkwm.title(jendela,"Data Mahasiswa")
  #membuat font text
  teks1 <- tkfont.create(family='courier',size=20)
  #memberi teks pada jendela
  tkgrid(tklabel(jendela,text="Nama Mahasiswa :",font=teks1),sticky="W")
  tkgrid(tklabel(jendela,text="NIM :",font=teks1),sticky="W")
  tkgrid(tklabel(jendela,text="Prodi S1 Statistika",font=teks1),sticky="We")
  tkgrid(tklabel(jendela,text="Universitas Airlangga",font=teks1),sticky="We")
  #membuat tombol tutup
  tutup <- function() {
    tkmessageBox(type ="yesno",message=paste("Apakah anda ingin menutup jendela data mahasiswa?"),icon="question")
    tkdestroy(jendela)
  }
  tutup.but <- tkbutton(jendela, text="TUTUP", command=tutup)
  tkgrid(tutup.but)
}
prog.1()

prog.2 <- function()
{
  jendela <- tktoplevel()
  tkwm.title(jendela,"Latihan No 2")
  #membuat font text
  teks1 <- tkfont.create(family='courier',weight="bold",size=14)
  teks2 <- tkfont.create(family='times',size=12)
  teks3 <- tkfont.create(family='sans',size=12)
  #memberi teks pada jendela
  tkgrid(tklabel(jendela,text="Nama :",font=teks1),sticky="W")
  nama <- tkentry(jendela,width="50",textvariable=text)
  tkgrid(nama)
  tkgrid(tklabel(jendela,text="Umur :",font=teks2),sticky="W")
  umur <- tk2spinbox(jendela,from=1,to=99,increment=1)
  tkgrid(umur,sticky="we")
  tkgrid(tklabel(jendela,text="Kepuasan",font=teks3),sticky="W")
  slidervalue <-tclVar(init=50)
  kepuasan <- tkscale(jendela,from=0,to=10,showvalue=T,variable=slidervalue,orient="horizontal",length=10)
  tkgrid(kepuasan,sticky="we")
  #membuat tombol tutup
  tutup <- function() {
    tkmessageBox(type ="yesno",message=paste("Apakah anda ingin menutup jendela data mahasiswa?"),icon="question")
    tkdestroy(jendela)
  }
  tutup.but <- tkbutton(jendela, text="TUTUP", command=tutup)
  tkgrid(tutup.but)
}
prog.2()

prog.3 <- function()
{
  jendela <- tktoplevel()
  tkwm.title(jendela,"Latihan No 3")
  #memberi teks pada jendela
  tkgrid(tklabel(jendela,text="Isikan Nama Lengkap :"),sticky="We")
  nama <- tkentry(jendela,width="50",textvariable=text)
  tkgrid(nama,sticky="we")
  slidervalue <-tclVar(init=50)
  slidervaluelabel <- tklabel(jendela,text=as.character(tclvalue(slidervalue)))
  tkgrid(tklabel(jendela,text="Kepuasan Terhadap Pelayanan:"),slidervaluelabel,tklabel(jendela,text="%"),sticky="we")
  tkconfigure(slidervaluelabel,textvariable=slidervalue)
  kepuasan <- tkscale(jendela,from=0,to=100,showvalue=T,variable=slidervalue,orient="vertical")
  tkgrid(tklabel(jendela,text=" "))
  tkgrid(kepuasan,sticky="we")
  #membuat tombol tutup
  tutup <- function() {
    tkmessageBox(type ="yesno",message=paste("Apakah anda ingin menutup jendela data mahasiswa?"),icon="question")
    tkdestroy(jendela)
  }
  tutup.but <- tkbutton(jendela, text="TUTUP", command=tutup)
  tkgrid(tutup.but)
}
prog.3()

prog.4() <- function()
{
  ju <- tktoplevel()
  tkwm.title(ju,"Menu Makanan Favorit")
  tkgrid(tklabel(ju,text="Pilihan Menu Makanan : "),sticky="we")
  cb1 <- tkcheckbutton(ju)
  cbValue1 <- tclVar("0")
  tkconfigure(cb1,variable=cbValue1)
  tkgrid(tklabel(ju,text="Bakso"),cb1)
  cb2 <- tkcheckbutton(ju)
  cbValue2 <- tclVar("0")
  tkconfigure(cb2,variable=cbValue2)
  tkgrid(tklabel(ju,text="Sate"),cb2)
  cb3 <- tkcheckbutton(ju)
  cbValue3 <- tclVar("0")
  tkconfigure(cb3,variable=cbValue3)
  tkgrid(tklabel(ju,text="Nasi Goreng"),cb3)
  cb4 <- tkcheckbutton(ju)
  cbValue4 <- tclVar("0")
  tkconfigure(cb4,variable=cbValue4)
  tkgrid(tklabel(ju,text="Ayam Geprek"),cb4)
  cb5 <- tkcheckbutton(ju)
  cbValue5 <- tclVar("0")
  tkconfigure(cb5,variable=cbValue5)
  tkgrid(tklabel(ju,text="Mie Ayam"),cb5)
}
prog.4()

prog.5 <- function()
{
  require(tcltk)
  ju <- tktoplevel()
  tkwm.title(ju,"Pilihan Merk Laptop")
  rb1 <- tkradiobutton(ju)
  rb2 <- tkradiobutton(ju)
  rb3 <- tkradiobutton(ju)
  rb4 <- tkradiobutton(ju)
  rb5 <- tkradiobutton(ju)
  rbValue <- tclVar("Lenovo Thinkpad")
  tkconfigure(rb1,variable=rbValue,value="Lenovo Thinkpad")
  tkconfigure(rb2,variable=rbValue,value="ASUS")
  tkconfigure(rb3,variable=rbValue,value="Toshiba")
  tkconfigure(rb4,variable=rbValue,value="Dell")
  tkconfigure(rb5,variable=rbValue,value="Acer")
  tkgrid(tklabel(ju,text="Pilihan Merk Laptop Anda?"))
  tkgrid(tklabel(ju,text="Lenovo Thinkpad"),rb1)
  tkgrid(tklabel(ju,text="ASUS"),rb2)
  tkgrid(tklabel(ju,text="Toshiba"),rb3)
  tkgrid(tklabel(ju,text="Dell"),rb4)
  tkgrid(tklabel(ju,text="Acer"),rb5)
}
prog.5()
