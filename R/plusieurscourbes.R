plusieurscourbes <-
function(nb.niv,noms,lig,tra,ep) {
  Fen35=tktoplevel()
  tkwm.resizable(Fen35,FALSE,FALSE)
  tktitle(Fen35)="GrapheR"
  tkgrab.set(Fen35)
  tkfocus(Fen35)
  lig2=tclVar(lig[1])
  tra2=tclVar(tra[1])
  ep2=tclVar(ep[1])
  niv.liste=tklistbox(Fen35,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(niv.liste.scroll,...))
  niv.liste.scroll=tkscrollbar(Fen35,repeatinterval=5,command=function(...) tkyview(niv.liste,...))
  for (i in 1:nb.niv) {tkinsert(niv.liste,"end",noms[i])}
  tkselection.set(niv.liste,0)
  niv.select=tclVar()
  tclvalue(niv.select)=tkcurselection(niv.liste)
  tkbind(niv.liste,"<ButtonRelease-1>",function() {
    tclvalue(niv.select)<<-tkcurselection(niv.liste)
    tclvalue(lig2)<<-lig[as.numeric(tkcurselection(niv.liste))+1]
    tclvalue(tra2)<<-tra[as.numeric(tkcurselection(niv.liste))+1]
    tclvalue(ep2)<<-ep[as.numeric(tkcurselection(niv.liste))+1]
  })
  ligne.choose=ttkcombobox(Fen35,font=Env$police,values=c(Env$vocab[164,1],Env$vocab[165,1],Env$vocab[166,1],Env$vocab[167,1],Env$vocab[168,1]),textvariable=lig2,state="readonly")
  tkbind(ligne.choose,"<<ComboboxSelected>>",function() {
    tkselection.set(niv.liste,as.numeric(tclvalue(niv.select)))
    lig[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(lig2)
  })
  trait.choose=ttkcombobox(Fen35,font=Env$police,values=c(Env$vocab[75,1],Env$vocab[76,1],Env$vocab[77,1]),textvariable=tra2,state="readonly")
  tkbind(trait.choose,"<<ComboboxSelected>>",function() {
    tkselection.set(niv.liste,as.numeric(tclvalue(niv.select)))
    tra[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(tra2)
  })
  ep.choose=tkscale(Fen35,from=1,to=5,showvalue=TRUE,font=Env$police,variable=ep2,resolution=1,orient="horizontal",command=function(...) {
    ep[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(ep2)
  })
  ok=tkbutton(Fen35,text=Env$vocab[1,1],font=Env$police,width=16,command=function() {
    assign("lignes",lig,pos=Env)
    assign("traits",tra,pos=Env)
    assign("ep.lignes",ep,pos=Env)
    tkgrab.release(Fen35)
    tkdestroy(Fen35)
  })
  annuler=tkbutton(Fen35,text=Env$vocab[2,1],font=Env$police,width=16,command=function() {tkgrab.release(Fen35);tkdestroy(Fen35)})
  tkgrid(tklabel(Fen35,text="",font=Env$police))
  tkgrid(tklabel(Fen35,text="     ",font=Env$police),row=1,column=0)
  tkgrid(niv.liste,niv.liste.scroll,row=1,column=1,rowspan=5)
  tkgrid.configure(niv.liste.scroll,sticky="ens")
  tkgrid(tklabel(Fen35,text="          ",font=Env$police),row=1,column=2)
  tkgrid(tklabel(Fen35,text=Env$vocab[163,1],font=Env$police),row=1,column=3,sticky="e")
  tkgrid(ligne.choose,row=1,column=4,sticky="w")
  tkgrid(tklabel(Fen35,text="     ",font=Env$police),row=1,column=5)
  tkgrid(tklabel(Fen35,text="",font=Env$police),row=2,column=3)
  tkgrid(tklabel(Fen35,text=Env$vocab[74,1],font=Env$police),row=3,column=3,sticky="e")
  tkgrid(trait.choose,row=3,column=4,sticky="w")
  tkgrid(tklabel(Fen35,text=" ",font=tkfont.create(family="Arial",size=1)),row=4,column=3)
  tkgrid(tklabel(Fen35,text=Env$vocab[78,1],font=Env$police),row=5,column=3,sticky="e")
  tkgrid(ep.choose,row=5,column=4,sticky="w")
  tkgrid(tklabel(Fen35,text="",font=Env$police))
  tkgrid(ok,row=7,column=1,columnspan=2)
  tkgrid(annuler,row=7,column=3,columnspan=2) 
  tkgrid(tklabel(Fen35,text=" ",font=tkfont.create(family="Arial",size=4)))
}

