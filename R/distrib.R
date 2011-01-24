distrib <-
function(sequence) {
  Dis=tktoplevel()
  tkwm.resizable(Dis,FALSE,FALSE)
  tktitle(Dis)="GrapheR"
  tkgrab.set(Dis)
  courbeValue=tclVar("normal")
  type.distrib=function(dis,par1,par2,par3,act2,act3) {
    tkconfigure(lab.titre,text=dis)
    tkconfigure(lab1,text=par1)
    tkconfigure(lab2,text=par2)
    tkconfigure(lab3,text=par3)
    tkconfigure(param1,textvariable=tclVar(""),state="normal")
    tkconfigure(param2,textvariable=tclVar(""),state=if (act2==TRUE) {"normal"} else {"disabled"})
    tkconfigure(param3,textvariable=tclVar(""),state=if (act3==TRUE) {"normal"} else {"disabled"})
  }
  rb1=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="B (n,p)",par1=Env$vocab[208,1],par2=Env$vocab[209,1],par3="",act2=TRUE,act3=FALSE)})
  rb2=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="P (n.p)",par1=Env$vocab[208,1],par2=Env$vocab[210,1],par3="",act2=TRUE,act3=FALSE)})
  rb3=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="BN (k,p)",par1=Env$vocab[211,1],par2=Env$vocab[209,1],par3="",act2=TRUE,act3=FALSE)})
  rb4=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="G (p)",par1=Env$vocab[209,1],par2="",par3="",act2=FALSE,act3=FALSE)})
  rb5=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="H (N,n,p)",par1=Env$vocab[212,1],par2=Env$vocab[213,1],par3=Env$vocab[209,1],act2=TRUE,act3=TRUE)})
  rb6=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="N (\U03BC,\U03C3)",par1=paste(Env$vocab[214,1],"\U03BC",Env$vocab[236,1],sep=""),par2=paste(Env$vocab[215,1],"\U03C3",Env$vocab[236,1],sep=""),par3="",act2=TRUE,act3=FALSE)})
  rb7=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="G (\U03B1,\U03B2)",par1=paste(Env$vocab[216,1],"\U03B1",Env$vocab[236,1],sep=""),par2=paste(Env$vocab[217,1],"\U03B2",Env$vocab[236,1],sep=""),par3="",act2=TRUE,act3=FALSE)})
  rb8=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="exp (\U03BB)",par1=paste(Env$vocab[218,1],"\U03BB",Env$vocab[236,1],sep=""),par2="",par3="",act2=FALSE,act3=FALSE)})
  rb9=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="\U03C7\U00B2 (\U03BD)",par1=paste(Env$vocab[219,1],"\U03BD",Env$vocab[236,1],sep=""),par2="",par3="",act2=FALSE,act3=FALSE)})
  rb10=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis=paste("F (\U03BD","1,\U03BD","2)",sep=""),par1=paste(Env$vocab[220,1],"\U03BD","1",Env$vocab[236,1],sep=""),par2=paste(Env$vocab[221,1],"\U03BD","2",Env$vocab[236,1],sep=""),par3="",act2=TRUE,act3=FALSE)})
  rb11=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="t (\U03BD)",par1=paste(Env$vocab[219,1],"\U03BD",Env$vocab[236,1],sep=""),par2="",par3="",act2=FALSE,act3=FALSE)})
  rb12=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="U (n1,n2)",par1=Env$vocab[222,1],par2=Env$vocab[223,1],par3="",act2=TRUE,act3=FALSE)})
  rb13=tkradiobutton(Dis,font=Env$police,command=function() {type.distrib(dis="V (n)",par1=Env$vocab[224,1],par2="",par3="",act2=FALSE,act3=FALSE)})
  tkconfigure(rb1,variable=courbeValue,value="binomial",text=Env$vocab[195,1])
  tkconfigure(rb2,variable=courbeValue,value="poisson",text=Env$vocab[196,1])
  tkconfigure(rb3,variable=courbeValue,value="nbinomial",text=Env$vocab[197,1])
  tkconfigure(rb4,variable=courbeValue,value="geom",text=Env$vocab[198,1])
  tkconfigure(rb5,variable=courbeValue,value="hypergeom",text=Env$vocab[199,1])
  tkconfigure(rb6,variable=courbeValue,value="normal",text=Env$vocab[200,1])
  tkconfigure(rb7,variable=courbeValue,value="gamma",text=Env$vocab[201,1])
  tkconfigure(rb8,variable=courbeValue,value="expo",text=Env$vocab[202,1])
  tkconfigure(rb9,variable=courbeValue,value="chi2",text=if (Env$lang=="fr") {paste(Env$vocab[203,1],"\U03C7\U00B2",sep="")} else if (Env$lang=="en") {paste("\U03C7\U00B2",Env$vocab[203,1],sep="")})
  tkconfigure(rb10,variable=courbeValue,value="fisher",text=Env$vocab[204,1])
  tkconfigure(rb11,variable=courbeValue,value="student",text=Env$vocab[205,1])
  tkconfigure(rb12,variable=courbeValue,value="mann",text=Env$vocab[206,1])
  tkconfigure(rb13,variable=courbeValue,value="wilcox",text=Env$vocab[207,1])
  lab.titre=tklabel(Dis,text="N (\U03BC,\U03C3)",font=tkfont.create(family="Arial",size=10,slant="italic"))
  lab1=tklabel(Dis,text=paste(Env$vocab[214,1],"\U03BC",Env$vocab[236,1],sep=""),font=Env$police)
  lab2=tklabel(Dis,text=paste(Env$vocab[215,1],"\U03C3",Env$vocab[236,1],sep=""),font=Env$police)
  lab3=tklabel(Dis,text="",font=Env$police)
  param1=tkentry(Dis,width=5,font=Env$police)
  param2=tkentry(Dis,width=5,font=Env$police)
  param3=tkentry(Dis,width=5,font=Env$police,state="disabled")
  typeValue=tclVar(Env$vocab[75,1])
  type.choose=ttkcombobox(Dis,font=Env$police,values=c(Env$vocab[75,1],Env$vocab[76,1],Env$vocab[77,1]),textvariable=typeValue,state="readonly")
  epValue=tclVar("1")
  ep.choose=tkscale(Dis,from=1,to=5,showvalue=TRUE,font=Env$police,variable=epValue,resolution=1,orient="horizontal")
  colValue=tclVar("black")
  col.choose=tkcanvas(Dis,width="40",height="25",bg=tclvalue(colValue))
  tkbind(col.choose,"<ButtonRelease-1>",function() {couleur(fen=Dis,titre=231,var=colValue,widg=col.choose,type="can",plusieurs=FALSE)})
  tracer=tkbutton(Dis,width=16,text=Env$vocab[181,1],font=Env$police,command=function() {
    trait=if (tclvalue(typeValue)==Env$vocab[75,1]) {1} else if (tclvalue(typeValue)==Env$vocab[76,1]) {2} else {3}
    if (tclvalue(courbeValue)=="binomial") {lines(seq(sequence[1],sequence[2],1),dbinom(seq(sequence[1],sequence[2],1),as.numeric(tclvalue(tkget(param1))),
      as.numeric(tclvalue(tkget(param2)))),type="o",pch=16,lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="poisson") {lines(seq(sequence[1],sequence[2],1),dpois(seq(sequence[1],sequence[2],1),as.numeric(tclvalue(tkget(param1)))*
      as.numeric(tclvalue(tkget(param2)))),type="o",pch=16,lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="nbinomial") {lines(seq(sequence[1],sequence[2],1),dnbinom(seq(sequence[1],sequence[2],1),as.numeric(tclvalue(tkget(param1))),
      as.numeric(tclvalue(tkget(param2)))),type="o",pch=16,lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="geom") {lines(seq(sequence[1],sequence[2],1),dgeom(seq(sequence[1],sequence[2],1),as.numeric(tclvalue(tkget(param1)))),
      type="o",pch=16,lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="hypergeom") {lines(seq(sequence[1],sequence[2],1),dhyper(seq(sequence[1],sequence[2],1),round(as.numeric(tclvalue(tkget(param3)))*as.numeric(tclvalue(tkget(param1))),0),
      round((1-as.numeric(tclvalue(tkget(param3))))*as.numeric(tclvalue(tkget(param1))),0),as.numeric(tclvalue(tkget(param2)))),type="o",pch=16,lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="normal") {lines(seq(sequence[1],sequence[2],sequence[3]),dnorm(seq(sequence[1],sequence[2],sequence[3]),as.numeric(tclvalue(tkget(param1))),
      as.numeric(tclvalue(tkget(param2)))),lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="gamma") {lines(seq(sequence[1],sequence[2],sequence[3]),dgamma(seq(sequence[1],sequence[2],sequence[3]),shape=as.numeric(tclvalue(tkget(param1))),
      scale=as.numeric(tclvalue(tkget(param2)))),lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="expo") {lines(seq(sequence[1],sequence[2],sequence[3]),dexp(seq(sequence[1],sequence[2],sequence[3]),as.numeric(tclvalue(tkget(param1)))),
      lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="chi2") {lines(seq(sequence[1],sequence[2],sequence[3]),dchisq(seq(sequence[1],sequence[2],sequence[3]),as.numeric(tclvalue(tkget(param1)))),
      lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="fisher") {lines(seq(sequence[1],sequence[2],sequence[3]),df(seq(sequence[1],sequence[2],sequence[3]),as.numeric(tclvalue(tkget(param1))),
      as.numeric(tclvalue(tkget(param2)))),lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="student") {lines(seq(sequence[1],sequence[2],sequence[3]),dt(seq(sequence[1],sequence[2],sequence[3]),as.numeric(tclvalue(tkget(param1)))),
      lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="mann") {lines(seq(sequence[1],sequence[2],1),dwilcox(seq(sequence[1],sequence[2],1),as.numeric(tclvalue(tkget(param1))),
      as.numeric(tclvalue(tkget(param2)))),type="o",pch=16,lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))} else
    if (tclvalue(courbeValue)=="wilcox") {lines(seq(sequence[1],sequence[2],1),dsignrank(seq(sequence[1],sequence[2],1),as.numeric(tclvalue(tkget(param1)))),
      type="o",pch=16,lty=trait,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))}
  })
  fermer=tkbutton(Dis,width=16,text=Env$vocab[182,1],font=Env$police,command=function() {tkgrab.release(Dis);tkdestroy(Dis);tkfocus(Env$Toolbar)})
  tkgrid(tklabel(Dis,text="",font=Env$police))
  tkgrid(tklabel(Dis,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Dis,text=Env$vocab[194,1],font=tkfont.create(family="Arial",size=11,weight="bold")),row=1,column=1,columnspan=3)
  tkgrid(tklabel(Dis,text="        ",font=Env$police),row=1,column=4)
  tkgrid(tklabel(Dis,text="",font=tkfont.create(family="Arial",size=4)))
  tkgrid(rb1,row=3,column=2,sticky="w")
  tkgrid(rb2,row=4,column=2,sticky="w")
  tkgrid(rb3,row=5,column=2,sticky="w")
  tkgrid(rb4,row=6,column=2,sticky="w")
  tkgrid(rb5,row=7,column=2,sticky="w")
  tkgrid(rb6,row=3,column=1,sticky="w")
  tkgrid(rb7,row=4,column=1,sticky="w")
  tkgrid(rb8,row=5,column=1,sticky="w")
  tkgrid(rb9,row=6,column=1,sticky="w")
  tkgrid(rb10,row=7,column=1,sticky="w")
  tkgrid(rb11,row=8,column=1,sticky="w")
  tkgrid(tklabel(Dis,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(rb12,row=10,column=1,columnspan=2,sticky="w")
  tkgrid(rb13,row=11,column=1,columnspan=2,sticky="w")
  tkgrid(tklabel(Dis,text=" ",font=tkfont.create(family="Arial",size=3)))
  tkgrid(lab.titre,row=13,column=1,columnspan=2)
  tkgrid(tklabel(Dis,text=" ",font=tkfont.create(family="Arial",size=3)))
  tkgrid(lab1,row=15,column=1,sticky="e")
  tkgrid(param1,row=15,column=2,sticky="w")
  tkgrid(lab2,row=16,column=1,sticky="e")
  tkgrid(param2,row=16,column=2,sticky="w")
  tkgrid(lab3,row=17,column=1,sticky="e")
  tkgrid(param3,row=17,column=2,sticky="w")
  tkgrid(tklabel(Dis,text="",font=Env$police))
  tkgrid(tklabel(Dis,text=Env$vocab[74,1],font=Env$police),row=19,column=1,sticky="e")
  tkgrid(type.choose,row=19,column=2,sticky="w")
  tkgrid(tklabel(Dis,text=Env$vocab[78,1],font=Env$police),row=20,column=1,sticky="e")
  tkgrid(ep.choose,row=20,column=2,sticky="w")
  tkgrid(tklabel(Dis,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(tklabel(Dis,text=Env$vocab[56,1],font=Env$police),row=22,column=1,sticky="e")
  tkgrid(col.choose,row=22,column=2,sticky="w")
  tkgrid(tklabel(Dis,text="",font=Env$police))
  tkgrid(tracer,row=24,column=1)
  tkgrid(fermer,row=24,column=2)
  tkgrid(tklabel(Dis,text=" ",font=tkfont.create(family="Arial",size=4)))
}

