library(tcltk)
library(tcltk2)
tclRequire("Tktable")
# callback that reads a file and stores the DF in the env of the toplevel:
getfile <- function(window) 
{
  name <- tclvalue(tkgetOpenFile(filetypes = "{{CSV Files} {.csv}}"))
  if (name == "") return;
  #Ubah file csv menjadi data frame
  window$env$datafile <- as.data.frame(read.csv(name,header=T))
}

# callback that does something to the data frame (should check for existence first!)    
dosomething <- function(window){
  tp <- tktoplevel()
  toTclArray<-function(dsn,dig=2) { 
    # http://r.789695.n4.nabble.com/Tck-tk-help-td1837711.html
    # Converts Data Frame/Matrix to a Tcl Array for Use in Displaying Tables 
    # dsn is the data set name 
    # dig is the number of digits to round to 
    require(tcltk) 
    tclarray1<-tclArray() 
    for (i in 0:(dim(dsn)[1])) { 
      for (j in 0:(dim(dsn)[2]-1)) { 
        # First Row is Set to Column Names to be Used as Labels 
        if (i==0) { 
          tclarray1[[i,j]]<-colnames(dsn)[j+1] 
        } else { 
          tem<-dsn[i,j+1] 
          tclarray1[[i,j]]<-ifelse(is.na(tem),".", 
                                   ifelse(is.numeric(tem),round(tem,digits=dig), 
                                          as.character(tem))) 
        } 
      } 
    } 
    return (tclarray1) 
  }
  temptable <- toTclArray(window$env$datafile)
  frame <- ttklabelframe(tp, text = "Data:")
  temptable <- toTclArray(window$env$datafile)
  # Add the table to the window environment to ensure killing it when the window is closed (= no more phantom calls to the data command handler)
  # Cache = TRUE: This greatly enhances speed performance when used with -command but uses extra memory.
  tp$env$table <- tkwidget(frame, "table", variable=temptable ,rows = nrow(window$env$datafile)+1, cols = ncol(window$env$datafile), titlerows = 1, selecttype = "cell", selectmode = "extended", cache = TRUE, yscrollcommand = function(...) tkset(scroll.y, ...), xscrollcommand = function(...) tkset(scroll.x, ...))
  
  scroll.x <- ttkscrollbar(frame, orient = "horizontal", command=function(...) tkxview(tp$env$table,...))  # command that performs the scrolling
  scroll.y <- ttkscrollbar(frame, orient = "vertical", command=function(...) tkyview(tp$env$table,...))  # command that performs the scrolling
  
  buttons <- ttkframe(tp)
  btn.read.only <- ttkbutton(buttons, text = "make read only", command = function() tkconfigure(tp$env$table, state = "disabled"))
  btn.read.write <- ttkbutton(buttons, text = "make writable", command = function() tkconfigure(tp$env$table, state = "normal"))
  
  tkgrid(btn.read.only, row = 0, column = 1)
  tkgrid(btn.read.write, row = 0, column = 2)
  
  tkpack(frame, fill = "both", expand = TRUE)
  tkpack(scroll.x, fill = "x", expand = FALSE, side = "bottom")
  tkpack(scroll.y, fill = "y", expand = FALSE, side = "right")
  tkpack(tp$env$table, fill = "both", expand = TRUE, side = "left")
  tkpack(buttons, side = "bottom")
}

# Make function t tests
ujitkanan <- function(window)
{
  # inisialisasi variabel
  d0var <- tclVar("")
  alphavar <- tclVar("0.90")
  
  #membuat frame 2
  frame2 <- tkframe(window, relief="groove",borderwidth=2)
  tkpack(tklabel(frame2,text="Uji T Data Berpasangan Kanan"),anchor='center')
  d0.entry <- tkentry(frame2,textvariable=d0var)
  tkpack(tklabel(frame2,text="input d0"),d0.entry,padx=5)
  alpha1 <- tkradiobutton(frame2,text="90 %",value="0.90",variable=alphavar)
  alpha2 <- tkradiobutton(frame2,text="95 %",value="0.95",variable=alphavar)
  alpha3 <- tkradiobutton(frame2,text="97.5 %",value="0.975",variable=alphavar)
  alpha4 <- tkradiobutton(frame2,text="99 %",value="0.99",variable=alphavar)
  tkpack(tklabel(frame2,text="Pilihlah Confindence Level :"),anchor='w')
  tkpack(alpha1,anchor='w')
  tkpack(alpha2,anchor='w')
  tkpack(alpha3,anchor='w')
  tkpack(alpha4,anchor='w')
  tkpack(frame2, fill="x")
  
  buttons <- ttkframe(window)
  submit <- function() {
    d0 <- as.numeric(tclvalue(d0var))
    alpha <- as.numeric(tclvalue(alphavar))
    n <- nrow(window$env$datafile)
    dbar <- mean(window$env$datafile[,2]-window$env$datafile[,1])
    sd <- sd(window$env$datafile[,2]-window$env$datafile[,1])
    thit <- (dbar-d0)/(sd/sqrt(n))
    pvalue <- 2*pt(abs(thit),n-1,lower.tail = F)
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
  submit.but <- tkbutton(buttons, text="Submit", command=submit)
  
  reset <- function() {
    tclvalue(d0var)<-""
    tclvalue(alphavar)<-"0.90"
  }
  reset.but <- tkbutton(buttons, text="Reset", command=reset)
  tkgrid(submit.but, row = 0, column = 1)
  tkgrid(reset.but, row = 0, column = 2)
  tkpack(buttons, side = "bottom")
}

ujitkanan <- function(window)
{
  # inisialisasi variabel
  d0var <- tclVar("")
  alphavar <- tclVar("0.90")
  
  #membuat frame 2
  frame2 <- tkframe(window, relief="groove",borderwidth=2)
  tkpack(tklabel(frame2,text="Uji T Data Berpasangan Kanan"),anchor='center')
  d0.entry <- tkentry(frame2,textvariable=d0var)
  tkpack(tklabel(frame2,text="input d0"),d0.entry,padx=5)
  alpha1 <- tkradiobutton(frame2,text="90 %",value="0.90",variable=alphavar)
  alpha2 <- tkradiobutton(frame2,text="95 %",value="0.95",variable=alphavar)
  alpha3 <- tkradiobutton(frame2,text="97.5 %",value="0.975",variable=alphavar)
  alpha4 <- tkradiobutton(frame2,text="99 %",value="0.99",variable=alphavar)
  tkpack(tklabel(frame2,text="Pilihlah Confindence Level :"),anchor='w')
  tkpack(alpha1,anchor='w')
  tkpack(alpha2,anchor='w')
  tkpack(alpha3,anchor='w')
  tkpack(alpha4,anchor='w')
  tkpack(frame2, fill="x")
  
  buttons <- ttkframe(window)
  submit <- function() {
    d0 <- as.numeric(tclvalue(d0var))
    alpha <- as.numeric(tclvalue(alphavar))
    n <- nrow(window$env$datafile)
    dbar <- mean(window$env$datafile[,2]-window$env$datafile[,1])
    sd <- sd(window$env$datafile[,2]-window$env$datafile[,1])
    thit <- (dbar-d0)/(sd/sqrt(n))
    pvalue <- pt(thit,n-1,lower.tail = F)
    if (pvalue < alpha)
    {
      hasil <- as.character("Menolak H0")
    }
    else
    {
      hasil <- as.character("Gagal Menolak H0")
    }
    tkmessageBox(message=paste("Keputusan : ", hasil, ", dengan p-value : ",round(pvalue,3), " dan alpha ",1-alpha, ""))
  }
  submit.but <- tkbutton(buttons, text="Submit", command=submit)
  
  reset <- function() {
    tclvalue(d0var)<-""
    tclvalue(alphavar)<-"0.90"
    tkdestroy(frame2)
    tkdestroy(buttons)
  }
  reset.but <- tkbutton(buttons, text="Reset", command=reset)
  tkgrid(submit.but, row = 0, column = 1)
  tkgrid(reset.but, row = 0, column = 2)
  tkpack(buttons, side = "bottom")
}

ujitkiri <- function(window)
{
  # inisialisasi variabel
  d0var <- tclVar("")
  alphavar <- tclVar("0.90")
  
  #membuat frame 2
  frame2 <- tkframe(window, relief="groove",borderwidth=2)
  tkpack(tklabel(frame2,text="Uji T Data Berpasangan Kiri"),anchor='center')
  d0.entry <- tkentry(frame2,textvariable=d0var)
  tkpack(tklabel(frame2,text="input d0"),d0.entry,padx=5)
  alpha1 <- tkradiobutton(frame2,text="90 %",value="0.90",variable=alphavar)
  alpha2 <- tkradiobutton(frame2,text="95 %",value="0.95",variable=alphavar)
  alpha3 <- tkradiobutton(frame2,text="97.5 %",value="0.975",variable=alphavar)
  alpha4 <- tkradiobutton(frame2,text="99 %",value="0.99",variable=alphavar)
  tkpack(tklabel(frame2,text="Pilihlah Confindence Level :"),anchor='w')
  tkpack(alpha1,anchor='w')
  tkpack(alpha2,anchor='w')
  tkpack(alpha3,anchor='w')
  tkpack(alpha4,anchor='w')
  tkpack(frame2, fill="x")
  
  buttons <- ttkframe(window)
  submit <- function() {
    d0 <- as.numeric(tclvalue(d0var))
    alpha <- as.numeric(tclvalue(alphavar))
    n <- nrow(window$env$datafile)
    dbar <- mean(window$env$datafile[,2]-window$env$datafile[,1])
    sd <- sd(window$env$datafile[,2]-window$env$datafile[,1])
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
    tkmessageBox(message=paste("Keputusan : ", hasil, ", dengan p-value : ",round(pvalue,3), " dan alpha ",1-alpha, ""))
  }
  submit.but <- tkbutton(buttons, text="Submit", command=submit)
  
  reset <- function() {
    tclvalue(d0var)<-""
    tclvalue(alphavar)<-"0.90"
    tkdestroy(frame2)
    tkdestroy(buttons)
  }
  reset.but <- tkbutton(buttons, text="Reset", command=reset)
  tkgrid(submit.but, row = 0, column = 1)
  tkgrid(reset.but, row = 0, column = 2)
  tkpack(buttons, side = "bottom")
}

ujittwoway <- function(window)
{
  # inisialisasi variabel
  d0var <- tclVar("")
  alphavar <- tclVar("0.90")
  
  #membuat frame 2
  frame2 <- tkframe(window, relief="groove",borderwidth=2)
  tkpack(tklabel(frame2,text="Uji T Data Berpasangan Dua Arah"),anchor='center')
  d0.entry <- tkentry(frame2,textvariable=d0var)
  tkpack(tklabel(frame2,text="input d0"),d0.entry,padx=5)
  alpha1 <- tkradiobutton(frame2,text="90 %",value="0.90",variable=alphavar)
  alpha2 <- tkradiobutton(frame2,text="95 %",value="0.95",variable=alphavar)
  alpha3 <- tkradiobutton(frame2,text="97.5 %",value="0.975",variable=alphavar)
  alpha4 <- tkradiobutton(frame2,text="99 %",value="0.99",variable=alphavar)
  tkpack(tklabel(frame2,text="Pilihlah Confindence Level :"),anchor='w')
  tkpack(alpha1,anchor='w')
  tkpack(alpha2,anchor='w')
  tkpack(alpha3,anchor='w')
  tkpack(alpha4,anchor='w')
  tkpack(frame2, fill="x")
  
  buttons <- ttkframe(window)
  submit <- function() {
    d0 <- as.numeric(tclvalue(d0var))
    alpha <- as.numeric(tclvalue(alphavar))
    n <- nrow(window$env$datafile)
    dbar <- mean(window$env$datafile[,2]-window$env$datafile[,1])
    sd <- sd(window$env$datafile[,2]-window$env$datafile[,1])
    thit <- (dbar-d0)/(sd/sqrt(n))
    pvalue <- 2*pt(abs(thit),n-1,lower.tail = F)
    if (pvalue < alpha)
    {
      hasil <- as.character("Menolak H0")
    }
    else
    {
      hasil <- as.character("Gagal Menolak H0")
    }
    tkmessageBox(message=paste("Keputusan : ", hasil, ", dengan p-value : ",round(pvalue,3), " dan alpha ",1-alpha, ""))
  }
  submit.but <- tkbutton(buttons, text="Submit", command=submit)
  
  reset <- function() {
    tclvalue(d0var)<-""
    tclvalue(alphavar)<-"0.90"
    tkdestroy(frame2)
    tkdestroy(buttons)
  }
  reset.but <- tkbutton(buttons, text="Reset", command=reset)
  tkgrid(submit.but, row = 0, column = 1)
  tkgrid(reset.but, row = 0, column = 2)
  tkpack(buttons, side = "bottom")
}

# make a dialog with a load button and a do something button:
window <- tktoplevel()
tkwm.title(window,"Program Statistik Data")
spec.frm <- tkframe(window,borderwidth=2)
left.frm <- tkframe(spec.frm)
right.frm <- tkframe(spec.frm)
topmenu <- tkmenu(window)
tkconfigure(window, menu=topmenu)
menu1 <- tkmenu(topmenu, tearoff=F)
sm1 <- tkmenu(topmenu, tearoff=F)
tkadd(topmenu,"cascade",label="Uji Hipotesis", menu=menu1)
tkadd(menu1,"cascade",label="Uji T",menu=sm1)
tkadd(sm1,"command",label="Uji T Kiri",command=function() ujitkiri(window))
tkadd(sm1,"command",label="Uji T Kanan",command=function() ujitkanan(window))
tkadd(sm1,"command",label="Uji T Dua Arah",command=function() ujittwoway(window))
#membuat frame 1
frame1 <- tkframe(window, relief="groove",borderwidth=2)
tkpack(tklabel(frame1, text="Masukkan data dalam bentuk .csv :"))
button.choose <- tkbutton(frame1, text = "Select CSV File to be analyzed", command = function(){getfile(window)})
tkpack(button.choose,anchor="center")
button.disdata <- tkbutton(frame1, text = "Tampilkan Data", command = function(){dosomething(window)})
tkpack(button.disdata,anchor="center")
tkpack(frame1, fill="x")
