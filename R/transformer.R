transformer <-
function() {
  if (is.na(as.numeric(tclvalue(tkcurselection(Env$var.num.list))))==FALSE) {
    numero=as.numeric(tclvalue(tkcurselection(Env$var.num.list)))+1
    Fen3=tktoplevel()
    tkwm.resizable(Fen3,FALSE,FALSE)
    tktitle(Fen3)="GrapheR"
    tkgrab.set(Fen3)
    tkfocus(Fen3)
    rbnoregroup=tkradiobutton(Fen3,font=Env$police,command=function() {
      tkconfigure(rblong,state="disabled")
      tkconfigure(rbeff,state="disabled")
      tkconfigure(curseur,state="disabled",foreground="grey")
      tkconfigure(nb.classes.lab,foreground="grey")
    })
    rbregroup=tkradiobutton(Fen3,font=Env$police,command=function() {
      tkconfigure(rblong,state="normal")
      tkconfigure(rbeff,state="normal")
      tkconfigure(curseur,state="normal",foreground="black")
      tkconfigure(nb.classes.lab,foreground="black")
    })
    regroupValue=tclVar("noregroup")
    tkconfigure(rbnoregroup,variable=regroupValue,value="noregroup",text=paste(Env$vocab[40,1],nlevels(as.factor(Env$datas.GrapheR[,which(Env$variables=="N")[numero]])),Env$vocab[41,1],sep=""))
    tkconfigure(rbregroup,variable=regroupValue,value="regroup",text=Env$vocab[42,1])
    rblong=tkradiobutton(Fen3,font=Env$police,state="disabled")
    rbeff=tkradiobutton(Fen3,font=Env$police,state="disabled")
    classValue=tclVar("long")
    tkconfigure(rblong,variable=classValue,value="long",text=Env$vocab[43,1])
    tkconfigure(rbeff,variable=classValue,value="eff",text=Env$vocab[44,1])
    nb.classes.lab=tklabel(Fen3,text=Env$vocab[45,1],foreground="grey",font=Env$police)
    curseurValue=tclVar("2")
    curseur=tkscale(Fen3,from=2,to=20,showvalue=TRUE,font=Env$police,variable=curseurValue,resolution=1,orient="horizontal",state="disabled",foreground="grey")
    ok=tkbutton(Fen3,text=Env$vocab[1,1],width=16,font=Env$police,command=function() {
      if (tclvalue(regroupValue)=="noregroup") {
	  Env$datas.GrapheR[,which(Env$variables=="N")[numero]]=as.factor(Env$datas.GrapheR[,which(Env$variables=="N")[numero]])	  
      }
	if (tclvalue(regroupValue)=="regroup" & tclvalue(classValue)=="long") {
	  Env$datas.GrapheR[,which(Env$variables=="N")[numero]]=cut(Env$datas.GrapheR[,which(Env$variables=="N")[numero]],breaks=as.numeric(tclvalue(curseurValue)),labels=as.character(1:as.numeric(tclvalue(curseurValue))))
	}
	if (tclvalue(regroupValue)=="regroup" & tclvalue(classValue)=="eff") {
	  Env$datas.GrapheR[,which(Env$variables=="N")[numero]]=cut(Env$datas.GrapheR[,which(Env$variables=="N")[numero]],breaks=quantile(Env$datas.GrapheR[,which(Env$variables=="N")[numero]],probs=seq(0,1,1/as.numeric(tclvalue(curseurValue))),na.rm=TRUE),labels=as.character(1:as.numeric(tclvalue(curseurValue))))
	}
      variables=NULL
      for (i in 1:length(names(Env$datas.GrapheR))) {
        if(is.numeric(Env$datas.GrapheR[,i])==TRUE) {variables=c(variables,"N")}
        if(is.factor(Env$datas.GrapheR[,i])==TRUE) {variables=c(variables,"F")}
      }
	assign("variables",variables,pos=Env)
      tkdelete(Env$var.num.list,0,"end")
	tkdelete(Env$fact.list,0,"end")
      for (i in 1:length(Env$variables)) {
        if (Env$variables[i]=="N") {tkinsert(Env$var.num.list,"end",names(Env$datas.GrapheR)[i])}
        if (Env$variables[i]=="F") {tkinsert(Env$fact.list,"end",names(Env$datas.GrapheR)[i])}
      }
	tkconfigure(Env$info,state="normal")
	tkdelete(Env$info,"0.0","end")
	tkconfigure(Env$info,state="disabled")
      tkgrab.release(Fen3)
      tkdestroy(Fen3)
    })
    annuler=tkbutton(Fen3,text=Env$vocab[2,1],width=16,font=Env$police,command=function() {tkgrab.release(Fen3);tkdestroy(Fen3)})
    tkgrid(tklabel(Fen3,text="",font=Env$police))
    tkgrid(tklabel(Fen3,text=paste(Env$vocab[39,1],names(Env$datas.GrapheR)[Env$variables=="N"][numero]),font=tkfont.create(family="Arial",size=11,weight="bold")),row=1,column=1,columnspan=4,sticky="we")
    tkgrid(tklabel(Fen3,text="",font=Env$police))
    tkgrid(tklabel(Fen3,text="         ",font=Env$police),row=3,column=0)
    tkgrid(rbnoregroup,row=3,column=1,columnspan=4,sticky="w")
    tkgrid(rbregroup,row=4,column=1,columnspan=4,sticky="w")
    tkgrid(tklabel(Fen3,text="          ",font=Env$police),row=5,column=1)
    tkgrid(rblong,row=5,column=2,sticky="w")
    tkgrid(tklabel(Fen3,text="               ",font=Env$police),row=5,column=3)
    tkgrid(curseur,row=5,column=4)
    tkgrid(tklabel(Fen3,text="                 ",font=Env$police),row=3,column=5)
    tkgrid(rbeff,row=6,column=2,sticky="w")
    tkgrid(nb.classes.lab,row=6,column=4,sticky="we")
    tkgrid(tklabel(Fen3,text="",font=Env$police))
    tkgrid(ok,row=8,column=2,sticky="we")
    tkgrid(annuler,row=8,column=4,sticky="we")
    tkgrid(tklabel(Fen3,text="",font=tkfont.create(family="Arial",size=4)))
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[6,1],icon="error",type="ok")}
}

