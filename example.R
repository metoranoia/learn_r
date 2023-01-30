library(tcltk)
library(tcltk2)
y <- NULL
xlim <-NULL
a<-10
b<-5
size <- tclVar(50)
dist <- tclVar(1)
bw <- tclVar(1)
ik <-tclVar(0.975)
bw.sav <- 1 # in case replot.maybe is called too early
sg<-1
replot <- function(...) {
  if (is.null(y)) return() # too early...
  p <- as.numeric(tclObj(ik))
  plot(x,y,type='p',col="red",main="Diagram Pencar & Sabuk Keyakinan",ylim=c(-10,100))
  abline(a,b,col="blue")
  abline(reg1,col="red")
  x1<-seq(min(x),max(x),0.1)
  sep<-qt(p,n-2)*sqrt(var(y))*sqrt(1/n+(x1-mean(x))**2/sum((x-mean(x))**2))
  sa<-cf[1]+cf[2]*x1 + sep
  sb<-cf[1]+cf[2]*x1 - sep
  lines(x1,sa,col="red",lty=2)
  lines(x1,sb,col="red",lty=2)
  lines(x,0*x,col="green")
  ya<-seq(0,max(y),0.1)
  lines(0*ya,ya,col="green")
}
replot.maybe <- function(...)
{
  if (as.numeric(tclObj(bw)) != bw.sav) replot()
}

regen <- function(...) {
  n<<-as.numeric(tclObj(size))
  sg<-as.numeric(tclObj(bw))
  x<<-seq(0,15,length=n)
  mu<-a+b*x
  if (tclvalue(dist)=="1"){
    y<<-rnorm(n,mu,sg)
  }
  else {y<<-rnorm(n,mu,sg*sqrt(abs(x)))}
  #xlim <<- range(y) + c(-2,2)
  replot()
}

base <- tktoplevel()
tkwm.title(base, "Demo Regresi")
spec.frm <- tkframe(base,borderwidth=2)
left.frm <- tkframe(spec.frm)
right.frm <- tkframe(spec.frm)
#Satu bingkai
frame1 <- tkframe(base, relief="groove",borderwidth=2)
tkpack(tklabel(frame1, text="Distribusi"))
tkpack(tkradiobutton(frame1, command=regen,text="Normal", value=1, variable=dist),anchor="w")
tkpack(tkradiobutton(frame1, command=regen,text="T Normal", value=2, variable=dist),anchor="w")
## Dua bingkai:
frame3 <-tkframe(left.frm, relief="groove",borderwidth=2)
tkpack(tklabel(frame3, text="Ukuran Sample"))
for ( i in c(10,30,60,100) ) {
  tmp <- tkradiobutton(frame3, command=regen,text=i,value=i,variable=size)
  tkpack(tmp, anchor="w")
}
frame4 <-tkframe(right.frm, relief="groove",borderwidth=2)
tkpack(tklabel (frame4, text="StDev"))
tkpack(tkscale(frame4, command=regen, from=0.05,to=50.00, showvalue=T, variable=bw,resolution=0.05, orient="horiz"))
tkpack(tklabel (frame4, text="Interval Keyakinan"))
tkpack(tkscale(frame4, command=replot, from=0.90,to=0.995, showvalue=T, variable=ik,resolution=0.01, orient="horiz"))
tkpack(frame1, fill="x")
tkpack(frame3, frame4, fill="x")
tkpack(left.frm, right.frm,side="left",anchor="n")
## `Bottom frame' (on base):
q.but <- tkbutton(base,text="Selesai",
                  command=function()tkdestroy(base))
tkpack(spec.frm, q.but)
