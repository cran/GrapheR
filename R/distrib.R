distrib <-
function() {
  fr7.close()
  for (i in 1:length(Env$l.fr7)) {tkdestroy(Env$l.fr7[[i]])}
  Env$l.fr7<-list()
  tkconfigure(Env$l.frames$Fr7,borderwidth=3)
  type.distrib<-function(dist,par1,par2,par3,act2,act3) {
    tclvalue(Env$l.var$add.param1)<-""
    tclvalue(Env$l.var$add.param2)<-""
    tclvalue(Env$l.var$add.param3)<-""
    tkconfigure(Env$l.fr7$distrib.lab,text=dist)
    tkconfigure(Env$l.fr7$param1.lab,text=par1)
    tkconfigure(Env$l.fr7$param2.lab,text=par2)
    tkconfigure(Env$l.fr7$param3.lab,text=par3)
    tkconfigure(Env$l.fr7$param1.wdg,state="normal")
    tkconfigure(Env$l.fr7$param2.wdg,state=ifelse(act2==TRUE,"normal","disabled"))
    tkconfigure(Env$l.fr7$param3.wdg,state=ifelse(act3==TRUE,"normal","disabled"))
  }
  Env$l.fr7$titre.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[169,1],font=Env$police3)
  Env$l.fr7$rb1<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="norm",text=Env$voc[170,1],
    command=function() {type.distrib(dist="N (\U03BC,\U03C3)",par1=paste(Env$voc[189,1],"\U03BC",Env$voc[200,1],sep=""),
    par2=paste(Env$voc[190,1],"\U03C3",Env$voc[200,1],sep=""),par3="",act2=TRUE,act3=FALSE)})
  Env$l.fr7$rb2<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="binom",text=Env$voc[171,1],
    command=function() {type.distrib(dist="B (n,p)",par1=Env$voc[183,1],par2=Env$voc[184,1],par3="",act2=TRUE,act3=FALSE)})
  Env$l.fr7$rb3<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="gamma",text=Env$voc[172,1],
    command=function() {type.distrib(dist="G (\U03B1,\U03B2)",par1=paste(Env$voc[191,1],"\U03B1",Env$voc[200,1],sep=""),
    par2=paste(Env$voc[192,1],"\U03B2",Env$voc[200,1],sep=""),par3="",act2=TRUE,act3=FALSE)})
  Env$l.fr7$rb4<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="poiss",text=Env$voc[173,1],
    command=function() {type.distrib(dist="P (n.p)",par1=Env$voc[183,1],par2=Env$voc[185,1],par3="",act2=TRUE,act3=FALSE)})
  Env$l.fr7$rb5<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="expo",text=Env$voc[174,1],
    command=function() {type.distrib(dist="exp (\U03BB)",par1=paste(Env$voc[193,1],"\U03BB",Env$voc[200,1],sep=""),
    par2="",par3="",act2=FALSE,act3=FALSE)})
  Env$l.fr7$rb6<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="nbinom",text=Env$voc[175,1],
    command=function() {type.distrib(dist="BN (k,p)",par1=Env$voc[186,1],par2=Env$voc[184,1],par3="",act2=TRUE,act3=FALSE)})
  Env$l.fr7$rb7<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="chi",
    text=if (Env$lang=="fr") {paste(Env$voc[176,1],"\U03C7\U00B2",sep="")} else
    if (Env$lang=="en") {paste("\U03C7\U00B2",Env$voc[176,1],sep="")},
    command=function() {type.distrib(dist="\U03C7\U00B2 (\U03BD)",par1=paste(Env$voc[194,1],"\U03BD",Env$voc[200,1],sep=""),
    par2="",par3="",act2=FALSE,act3=FALSE)})
  Env$l.fr7$rb8<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="geom",text=Env$voc[177,1],
    command=function() {type.distrib(dist="G (p)",par1=Env$voc[184,1],par2="",par3="",act2=FALSE,act3=FALSE)})
  Env$l.fr7$rb9<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="fish",text=Env$voc[178,1],
    command=function() {type.distrib(dist=paste("F (\U03BD","1,\U03BD","2)",sep=""),par1=paste(Env$voc[195,1],"\U03BD","1",Env$voc[200,1],sep=""),
    par2=paste(Env$voc[196,1],"\U03BD","1",Env$voc[200,1],sep=""),par3="",act2=TRUE,act3=FALSE)})
  Env$l.fr7$rb10<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="hyper",text=Env$voc[179,1],
    command=function() {type.distrib(dist="H (N,n,p)",par1=Env$voc[187,1],par2=Env$voc[188,1],par3=Env$voc[184,1],
    act2=TRUE,act3=TRUE)})
  Env$l.fr7$rb11<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="stud",text=Env$voc[180,1],
    command=function() {type.distrib(dist="t (\U03BD)",par1=paste(Env$voc[194,1],"\U03BD",Env$voc[200,1],sep=""),
    par2="",par3="",act2=FALSE,act3=FALSE)})
  Env$l.fr7$rb12<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="mann",text=Env$voc[181,1],
    command=function() {type.distrib(dist="U (n1,n2)",par1=Env$voc[197,1],par2=Env$voc[198,1],par3="",act2=TRUE,act3=FALSE)})
  Env$l.fr7$rb13<-tkradiobutton(Env$l.frames$Fr7,font=Env$police,variable=Env$l.var$add.distrib,value="wilcox",text=Env$voc[182,1],
    command=function() {type.distrib(dist="V (n)",par1=Env$voc[199,1],par2="",par3="",act2=FALSE,act3=FALSE)})
  Env$l.fr7$distrib.lab<-tklabel(Env$l.frames$Fr7,text="N (\U03BC,\U03C3)",font=Env$police6)
  Env$l.fr7$param1.lab<-tklabel(Env$l.frames$Fr7,text=paste(Env$voc[189,1],"\U03BC",Env$voc[200,1],sep=""),font=Env$police)
  Env$l.fr7$param1.wdg<-tkentry(Env$l.frames$Fr7,width=5,font=Env$police,textvariable=Env$l.var$add.param1)
  tkbind(Env$l.fr7$param1.wdg,"<Enter>",function() {msg(text=Env$voc[155,1],type="warning")})
  tkbind(Env$l.fr7$param1.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr7$param2.lab<-tklabel(Env$l.frames$Fr7,text=paste(Env$voc[190,1],"\U03C3",Env$voc[200,1],sep=""),font=Env$police)
  Env$l.fr7$param2.wdg<-tkentry(Env$l.frames$Fr7,width=5,font=Env$police,textvariable=Env$l.var$add.param2)
  tkbind(Env$l.fr7$param2.wdg,"<Enter>",function() {msg(text=Env$voc[155,1],type="warning")})
  tkbind(Env$l.fr7$param2.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr7$param3.lab<-tklabel(Env$l.frames$Fr7,text="",font=Env$police)
  Env$l.fr7$param3.wdg<-tkentry(Env$l.frames$Fr7,width=5,font=Env$police,textvariable=Env$l.var$add.param3,state="disabled")
  tkbind(Env$l.fr7$param3.wdg,"<Enter>",function() {msg(text=Env$voc[155,1],type="warning")})
  tkbind(Env$l.fr7$param3.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr7$trait.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[59,1],font=Env$police)
  Env$l.fr7$trait.wdg<-ttkcombobox(Env$l.frames$Fr7,font=Env$police,values=c(Env$voc[60:62,1]),textvariable=Env$l.var$add.trait,state="readonly")
  Env$l.fr7$epaisseur.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[63,1],font=Env$police)
  Env$l.fr7$epaisseur.wdg<-tkscale(Env$l.frames$Fr7,from=1,to=5,showvalue=TRUE,font=Env$police,variable=Env$l.var$add.epaisseur1,resolution=1,orient="horizontal")
  Env$l.fr7$col.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[45,1],font=Env$police)
  Env$l.fr7$col.wdg<-tkcanvas(Env$l.frames$Fr7,width="40",height="25",bg=tclvalue(Env$l.var$add.col1))
  tkbind(Env$l.fr7$col.wdg,"<ButtonRelease-1>",function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$add.col1),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$add.col1)<-temp
	tkconfigure(Env$l.fr7$col.wdg,bg=tclvalue(Env$l.var$add.col1))
    }
  })
  Env$l.fr7$tracer<-tkbutton(Env$l.frames$Fr7,width=16,text=Env$voc[72,1],font=Env$police,command=function() {
    if (dev.cur()>1) {
	if (!is.null(Env$l.var$add.seq)) {
	  if (tclvalue(Env$l.var$add.distrib)=="norm") {lines(Env$l.var$add.seq,dnorm(Env$l.var$add.seq,as.numeric(tclvalue(Env$l.var$add.param1)),
	    as.numeric(tclvalue(Env$l.var$add.param2))),lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1))} else
	  if (tclvalue(Env$l.var$add.distrib)=="binom") {lines(Env$l.var$add.seq2,dbinom(Env$l.var$add.seq2,as.numeric(tclvalue(Env$l.var$add.param1)),
          as.numeric(tclvalue(Env$l.var$add.param2))),lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1),type="o",pch=16)} else
	  if (tclvalue(Env$l.var$add.distrib)=="gamma") {lines(Env$l.var$add.seq,dgamma(Env$l.var$add.seq,shape=as.numeric(tclvalue(Env$l.var$add.param1)),
          scale=as.numeric(tclvalue(Env$l.var$add.param2))),lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1))} else
	  if (tclvalue(Env$l.var$add.distrib)=="poiss") {lines(Env$l.var$add.seq2,dpois(Env$l.var$add.seq2,as.numeric(tclvalue(Env$l.var$add.param1))*
          as.numeric(tclvalue(Env$l.var$add.param2))),lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1),type="o",pch=16)} else
	  if (tclvalue(Env$l.var$add.distrib)=="expo") {lines(Env$l.var$add.seq,dexp(Env$l.var$add.seq,as.numeric(tclvalue(Env$l.var$add.param1))),
	    lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1))} else
	  if (tclvalue(Env$l.var$add.distrib)=="nbinom") {lines(Env$l.var$add.seq2,dnbinom(Env$l.var$add.seq2,as.numeric(tclvalue(Env$l.var$add.param1)),
          as.numeric(tclvalue(Env$l.var$add.param2))),lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1),type="o",pch=16)} else
	  if (tclvalue(Env$l.var$add.distrib)=="chi") {lines(Env$l.var$add.seq,dchisq(Env$l.var$add.seq,as.numeric(tclvalue(Env$l.var$add.param1))),
	    lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1))} else
	  if (tclvalue(Env$l.var$add.distrib)=="geom") {lines(Env$l.var$add.seq2,dgeom(Env$l.var$add.seq2,as.numeric(tclvalue(Env$l.var$add.param1))),
	    lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1),type="o",pch=16)} else
	  if (tclvalue(Env$l.var$add.distrib)=="fish") {lines(Env$l.var$add.seq,df(Env$l.var$add.seq,as.numeric(tclvalue(Env$l.var$add.param1)),
          as.numeric(tclvalue(Env$l.var$add.param2))),lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1))} else
	  if (tclvalue(Env$l.var$add.distrib)=="hyper") {lines(Env$l.var$add.seq2,dhyper(Env$l.var$add.seq2,round(as.numeric(tclvalue(Env$l.var$add.param3))*
	    as.numeric(tclvalue(Env$l.var$add.param1)),0),round((1-as.numeric(tclvalue(Env$l.var$add.param3)))*as.numeric(tclvalue(Env$l.var$add.param1)),0),
	    as.numeric(tclvalue(Env$l.var$add.param2))),lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1),type="o",pch=16)} else
	  if (tclvalue(Env$l.var$add.distrib)=="stud") {lines(Env$l.var$add.seq,dt(Env$l.var$add.seq,as.numeric(tclvalue(Env$l.var$add.param1))),
	    lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1))} else
	  if (tclvalue(Env$l.var$add.distrib)=="mann") {lines(Env$l.var$add.seq2,dwilcox(Env$l.var$add.seq2,as.numeric(tclvalue(Env$l.var$add.param1)),
          as.numeric(tclvalue(Env$l.var$add.param2))),lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1),type="o",pch=16)} else
	  if (tclvalue(Env$l.var$add.distrib)=="wilcox") {lines(Env$l.var$add.seq2,dsignrank(Env$l.var$add.seq2,as.numeric(tclvalue(Env$l.var$add.param1))),
	    lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	    col=tclvalue(Env$l.var$add.col1),type="o",pch=16)}
	} else {
	  msg(text=Env$voc[201,1],type="error")
	}
    } else {
	msg(text=Env$voc[163,1],type="error")
    }
  })
  Env$l.fr7$fermer<-tkbutton(Env$l.frames$Fr7,width=16,text=Env$voc[152,1],font=Env$police,command=fr7.close)
  Env$l.fr7$espace.ver1<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver2<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver3<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver4<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver5<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver6<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver7<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.hor1<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  Env$l.fr7$espace.hor2<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  tkgrid(Env$l.fr7$espace.hor1,row=1,column=0)
  tkgrid(Env$l.fr7$titre.lab,row=1,column=1,columnspan=3)
  tkgrid(Env$l.fr7$espace.hor2,row=1,column=4)
  tkgrid(Env$l.fr7$espace.ver1)
  tkgrid(Env$l.fr7$rb1,row=3,column=1,sticky="w")
  tkgrid(Env$l.fr7$rb2,row=3,column=2,sticky="w")
  tkgrid(Env$l.fr7$rb3,row=4,column=1,sticky="w")
  tkgrid(Env$l.fr7$rb4,row=4,column=2,sticky="w")
  tkgrid(Env$l.fr7$rb5,row=5,column=1,sticky="w")
  tkgrid(Env$l.fr7$rb6,row=5,column=2,sticky="w")
  tkgrid(Env$l.fr7$rb7,row=6,column=1,sticky="w")
  tkgrid(Env$l.fr7$rb8,row=6,column=2,sticky="w")
  tkgrid(Env$l.fr7$rb9,row=7,column=1,sticky="w")
  tkgrid(Env$l.fr7$rb10,row=7,column=2,sticky="w")
  tkgrid(Env$l.fr7$rb11,row=8,column=1,sticky="w")
  tkgrid(Env$l.fr7$espace.ver2)
  tkgrid(Env$l.fr7$rb12,row=10,column=1,columnspan=2,sticky="w")
  tkgrid(Env$l.fr7$rb13,row=11,column=1,columnspan=2,sticky="w")
  tkgrid(Env$l.fr7$espace.ver3)
  tkgrid(Env$l.fr7$distrib.lab,row=13,column=1,columnspan=2)
  tkgrid(Env$l.fr7$espace.ver4)
  tkgrid(Env$l.fr7$param1.lab,row=15,column=1,sticky="e")
  tkgrid(Env$l.fr7$param1.wdg,row=15,column=2,sticky="w")
  tkgrid(Env$l.fr7$param2.lab,row=16,column=1,sticky="e")
  tkgrid(Env$l.fr7$param2.wdg,row=16,column=2,sticky="w")
  tkgrid(Env$l.fr7$param3.lab,row=17,column=1,sticky="e")
  tkgrid(Env$l.fr7$param3.wdg,row=17,column=2,sticky="w")
  tkgrid(Env$l.fr7$espace.ver5)
  tkgrid(Env$l.fr7$trait.lab,row=19,column=1,sticky="e")
  tkgrid(Env$l.fr7$trait.wdg,row=19,column=2,sticky="w")
  tkgrid(Env$l.fr7$epaisseur.lab,row=20,column=1,sticky="e")
  tkgrid(Env$l.fr7$epaisseur.wdg,row=20,column=2,sticky="w")
  tkgrid(Env$l.fr7$col.lab,row=21,column=1,sticky="e")
  tkgrid(Env$l.fr7$col.wdg,row=21,column=2,sticky="w")
  tkgrid(Env$l.fr7$espace.ver6)
  tkgrid(Env$l.fr7$tracer,row=23,column=1)
  tkgrid(Env$l.fr7$fermer,row=23,column=2)
  tkgrid(Env$l.fr7$espace.ver7)
}

