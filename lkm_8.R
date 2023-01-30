library(tcltk)
library(tcltk2)
prog.1 <- function()
{
  jendela <- tktoplevel()
  tktitle(jendela) <- "Tipe Yang Anda Inginkan"
  topmenu <- tkmenu(jendela)
  tkconfigure(jendela, menu=topmenu)
  menu1 <- tkmenu(topmenu, tearoff=F)
  tkadd(menu1,"command",label="Luas Bangunan", command=function() tkdestroy(jendela))
  tkadd(menu1,"command",label="Fasilitas", command=function() tkdestroy(jendela))
  menu2 <- tkmenu(topmenu, tearoff=F)
  sm2 <- tkmenu(topmenu,tearoff=F)
  ssm2 <- tkmenu(topmenu,tearoff=F)
  tkadd(menu2,"cascade",label="Luas Bangunan",menu=sm2)
  tkadd(sm2,"command",label="Tipe Rumah 36",command=function() tkdestroy(jendela))
  tkadd(sm2,"cascade",label="Tipe Rumah 45",menu=ssm2)
  tkadd(ssm2,"command",label="Reguler",command=function() tkdestroy(jendela))
  tkadd(ssm2,"command",label="Penawaran Terbatas",command=function() tkdestroy(jendela))
  tkadd(menu2,"command",label="Fasilitas", command=function() tkdestroy(jendela))
  tkadd(menu2,"command",label="Harga", command=function() tkdestroy(jendela))
  
  tkadd(topmenu,"cascade",label="Tipe I",menu=menu1)
  tkadd(topmenu,"cascade",label="Tipe II",menu=menu2)
}
prog.1()

prog.2 <- function()
{
  getfile <- function(window) 
  {
    name <- tclvalue(tkgetOpenFile(filetypes = "{{CSV Files} {.csv}}"))
    if (name == "") return;
    #Ubah file csv menjadi data frame
    window$env$datafile <- as.data.frame(read.csv(name,header=T))
  }
  ujitkiri <- function(window)
  {
    # inisialisasi variabel
    d0var <- tclVar("")
    alphavar <- tclVar("0.90")
    
    tkgrid(tklabel(window,text="Uji T Data Berpasangan Kiri"),columnspan=3, pady=10)
    
    d0.entry <- tkentry(window,textvariable=d0var)
    tkgrid(tklabel(window,text="input d0"),d0.entry,columnspan=1,pady=10)
    
    alpha1 <- tkradiobutton(window)
    alpha2 <- tkradiobutton(window)
    alpha3 <- tkradiobutton(window)
    alpha4 <- tkradiobutton(window)
    tkconfigure(alpha1,variable=alphavar,value="0.90")
    tkconfigure(alpha2,variable=alphavar,value="0.95")
    tkconfigure(alpha3,variable=alphavar,value="0.975")
    tkconfigure(alpha4,variable=alphavar,value="0.99")
    tkgrid(tklabel(window,text="Pilih Confindence Level :"), pady=10, padx=10)
    tkgrid(alpha1,tklabel(window,text="90%"),pady=10,padx=10)
    tkgrid(alpha2,tklabel(window,text="95%"),pady=10,padx=10)
    tkgrid(alpha3,tklabel(window,text="97.5%"),pady=10,padx=10)
    tkgrid(alpha4,tklabel(window,text="99%"),pady=10,padx=10)
    
    submit <- function() {
      d0 <- as.numeric(tclvalue(d0var))
      alpha <- as.numeric(tclvalue(alphavar))
      n <- nrow(window$env$datafile)
      dbar <- mean(window$env$datafile[,1]-window$env$datafile[,2])
      sd <- sd(window$env$datafile[,1]-window$env$datafile[,2])
      thit <- (dbar-d0)/(sd/sqrt(n))
      pvalue <- pt(thit,n-1,lower.tail = T)
      if (pvalue < alpha)
      {
        hasil <- as.character("Menolak H0")
      }
      else
      {
        hasil <- as.character("Gagal Menolak H0")
      }
      tkmessageBox(message=paste("Keputusan : ", hasil, ", dengan p-value : ",pvalue, " dan alpha ",1-alpha, ""))
    }
    submit.but <- tkbutton(window, text="Submit", command=submit)
    
    reset <- function() {
      tclvalue(d0var)<-""
      tclvalue(alphavar)<-"0.90"
    }
    reset.but <- tkbutton(window, text="Reset", command=reset)
    
    tkgrid(submit.but, reset.but, pady= 10, padx= 10)
  }
  window <- tktoplevel()
  tkwm.title(window,"Program Statistik Data")
  topmenu <- tkmenu(window)
  tkconfigure(window, menu=topmenu)
  menu1 <- tkmenu(topmenu, tearoff=F)
  sm1 <- tkmenu(topmenu, tearoff=F)
  tkadd(topmenu,"cascade",label="Uji Hipotesis", menu=menu1)
  tkadd(menu1,"cascade",label="Uji T",menu=sm1)
  tkadd(sm1,"command",label="Uji T Kiri",command=function() ujitkiri(window))
  tkadd(sm1,"command",label="Uji T Kanan",command=function() tkdestroy(window))
  tkadd(sm1,"command",label="Uji T Dua Arah",command=function() tkdestroy(window))
  tkgrid(tklabel(window,text="Masukkan data dalam bentuk .csv :"),sticky="W")
  button.choose <- tkbutton(window, text = "Select CSV File to be analyzed", command = function(){getfile(window)})
  tkgrid(button.choose)
}
prog.2()
