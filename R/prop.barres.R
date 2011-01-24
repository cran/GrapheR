prop.barres <-
function(var,prop,beside,erreur.lab,erreur.choose,col.erreur.lab,col.erreur,col.erreur.choose,segment.lab,segment.check,legende.lab,legende.check,
  nom.legende.lab,nom.legende.but,legende.pos.lab,legende.pos.choose) {
  if (nchar(var)>0) {
    Fen17=tktoplevel()
    tkwm.resizable(Fen17,FALSE,FALSE)
    tktitle(Fen17)="GrapheR"
    tkgrab.set(Fen17)
    tkfocus(Fen17)
    niv.liste=tklistbox(Fen17,height=5,font=Env$police,selectmode="multiple",yscrollcommand=function(...) tkset(niv.liste.scroll,...))
    niv.liste.scroll=tkscrollbar(Fen17,repeatinterval=5,command=function(...) tkyview(niv.liste,...))
    for (i in 1:nlevels(Env$datas.GrapheR[,var])) {tkinsert(niv.liste,"end",levels(Env$datas.GrapheR[,var])[i])}
    for (i in 1:length(strsplit(prop,split=" ")[[1]])) {tkselection.set(niv.liste,as.numeric(strsplit(prop,split=" ")[[1]][i]))}
    prop.ok1=function(act) {
      tkconfigure(legende.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
      tkconfigure(legende.check,state=if (act==TRUE) {"normal"} else {"disabled"})
      tkconfigure(nom.legende.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
      tkconfigure(nom.legende.but,state=if (act==TRUE) {"normal"} else {"disabled"})
      tkconfigure(legende.pos.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
      tkconfigure(legende.pos.choose,state=if (act==TRUE) {"readonly"} else {"disabled"})	  
    }
    prop.ok2=function(act) {
	tkconfigure(erreur.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  	tkconfigure(erreur.choose,state=if (act==TRUE) {"readonly"} else {"disabled"})
    	tkconfigure(col.erreur.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
    	tclvalue(col.erreur)=if (act==TRUE) {"black"} else {"grey"}
    	tkconfigure(col.erreur.choose,bg=tclvalue(col.erreur))
    	tkconfigure(segment.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
    	tkconfigure(segment.check,state=if (act==TRUE) {"normal"} else {"disabled"})
    }
    ok=tkbutton(Fen17,text=Env$vocab[1,1],font=Env$police,command=function() {
	prop=tclvalue(tkcurselection(niv.liste))
      assign("niv.prop",prop,pos=Env)
	if (nchar(prop)>1) {
        assign("nb.niv2",length(strsplit(prop,split=" ")[[1]]),pos=Env)
        noms2=NULL
	  for (i in 1:Env$nb.niv2) {noms2=c(noms2,levels(Env$datas.GrapheR[,var])[as.numeric(strsplit(prop,split=" ")[[1]][i])+1])}
        assign("noms2",noms2,pos=Env)
	  assign("legende.titre",c(""),pos=Env)
	  assign("col.str1",gray.colors(Env$nb.niv2),pos=Env)
	  assign("col.str2",rep("black",Env$nb.niv2),pos=Env)
	  assign("dens.motif",rep(0,Env$nb.niv2),pos=Env)
	  assign("ang.motif",rep(0,Env$nb.niv2),pos=Env)
	  assign("col.motif",rep("black",Env$nb.niv2),pos=Env)
	  prop.ok1(act=TRUE)
	  if (beside==0) {prop.ok2(act=TRUE)} else {prop.ok2(act=FALSE)}
	} else {
	  prop.ok1(act=FALSE)
	  prop.ok2(act=TRUE)
	}
	tkgrab.release(Fen17)
	tkdestroy(Fen17)
    })
    annuler=tkbutton(Fen17,text=Env$vocab[2,1],font=Env$police,command=function() {tkgrab.release(Fen17);tkdestroy(Fen17)})
    tkgrid(tklabel(Fen17,text=" ",font=tkfont.create(family="Arial",size=3)))
    tkgrid(tklabel(Fen17,text="     ",font=Env$police),row=1,column=0)
    tkgrid(tklabel(Fen17,text=Env$vocab[155,1],font=Env$police),row=1,column=1)
    tkgrid(tklabel(Fen17,text="     ",font=Env$police),row=1,column=2)
    tkgrid(tklabel(Fen17,text=" ",font=tkfont.create(family="Arial",size=1)))
    tkgrid(niv.liste,niv.liste.scroll,column=1)
    tkgrid.configure(niv.liste.scroll,sticky="ens")
    tkgrid(tklabel(Fen17,text="",font=Env$police))
    tkgrid(ok,column=1,sticky="we")
    tkgrid(tklabel(Fen17,text="",font=tkfont.create(family="Arial",size=4)))
    tkgrid(annuler,column=1,sticky="we")
    tkgrid(tklabel(Fen17,text="",font=tkfont.create(family="Arial",size=4)))
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[7,1],icon="error",type="ok")}
}

