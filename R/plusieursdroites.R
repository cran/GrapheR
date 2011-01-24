plusieursdroites <-
function(var1,var2,nb.niv,noms,drt,reg,horiz,vertic,coefa,coefb,tra,ep.lig) {
  Fen27=tktoplevel()
  tkwm.resizable(Fen27,FALSE,FALSE)
  tktitle(Fen27)="GrapheR"
  tkgrab.set(Fen27)
  tkfocus(Fen27)
  drt2=drt
  reg2=reg
  horiz2=horiz
  vertic2=vertic
  coefa2=coefa
  coefb2=coefb
  tra2=tra
  ep.lig2=ep.lig
  drtValue=tclVar(drt2[1])
  regValue=tclVar(reg2[1])
  horizValue=tclVar(horiz2[1])
  verticValue=tclVar(vertic2[1])
  coefaValue=tclVar(coefa2[1])
  coefbValue=tclVar(coefb2[1])
  traValue=tclVar(tra2[1])
  ep.ligValue=tclVar(ep.lig2[1])
  inactif=function() {
    tkconfigure(rb8,state="disabled")
    tkconfigure(rb9,state="disabled")
    tkconfigure(hor,state="disabled")
    tkconfigure(ver,state="disabled")
    tkconfigure(coeffa,state="disabled")
    tkconfigure(coeffb,state="disabled")
  }
  save=function() {
    drt2[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(drtValue)
    reg2[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(regValue)
    horiz2[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(horizValue)
    vertic2[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(verticValue)
    coefa[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(coefaValue)
    coefb[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(coefbValue)  
  }
  niv.liste=tklistbox(Fen27,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(niv.liste.scroll,...))
  niv.liste.scroll=tkscrollbar(Fen27,repeatinterval=5,command=function(...) tkyview(niv.liste,...))
  for (i in 1:nb.niv) {tkinsert(niv.liste,"end",noms[i])}
  tkselection.set(niv.liste,0)
  niv.select=tclVar()
  tclvalue(niv.select)=tkcurselection(niv.liste)
  tkbind(niv.liste,"<ButtonRelease-1>",function() {
    tclvalue(niv.select)=tkcurselection(niv.liste)
    tclvalue(drtValue)=drt2[as.numeric(tkcurselection(niv.liste))+1]
    tclvalue(regValue)=reg2[as.numeric(tkcurselection(niv.liste))+1]
    tclvalue(horizValue)=horiz2[as.numeric(tkcurselection(niv.liste))+1]
    tclvalue(verticValue)=vertic2[as.numeric(tkcurselection(niv.liste))+1]
    tclvalue(coefaValue)=coefa2[as.numeric(tkcurselection(niv.liste))+1]
    tclvalue(coefbValue)=coefb2[as.numeric(tkcurselection(niv.liste))+1]
    tclvalue(traValue)=tra2[as.numeric(tkcurselection(niv.liste))+1]
    tclvalue(ep.ligValue)=ep.lig2[as.numeric(tkcurselection(niv.liste))+1]
    tkconfigure(rb8,state=if (tclvalue(drtValue)=="reg") {"normal"} else {"disabled"})
    tkconfigure(rb9,state=if (tclvalue(drtValue)=="reg") {"normal"} else {"disabled"})
    tkconfigure(hor,state=if (tclvalue(drtValue)=="hor") {"normal"} else {"disabled"})
    tkconfigure(ver,state=if (tclvalue(drtValue)=="ver") {"normal"} else {"disabled"})
    tkconfigure(coeffa,state=if (tclvalue(drtValue)=="affine") {"normal"} else {"disabled"})
    tkconfigure(coeffb,state=if (tclvalue(drtValue)=="affine") {"normal"} else {"disabled"})
  })
  rb1=tkradiobutton(Fen27,font=Env$police,command=function() {save();inactif()})
  rb2=tkradiobutton(Fen27,font=Env$police,command=function() {save();inactif();tkconfigure(hor,state="normal")})
  rb3=tkradiobutton(Fen27,font=Env$police,command=function() {save();inactif();tkconfigure(ver,state="normal")})
  rb4=tkradiobutton(Fen27,font=Env$police,command=function() {save();inactif();tkconfigure(coeffa,state="normal");tkconfigure(coeffb,state="normal")})
  rb5=tkradiobutton(Fen27,font=Env$police,command=function() {save();inactif();tkconfigure(rb8,state="normal");tkconfigure(rb9,state="normal")})
  rb6=tkradiobutton(Fen27,font=Env$police,command=function() {save();inactif()})
  rb7=tkradiobutton(Fen27,font=Env$police,command=function() {save();inactif()})
  rb8=tkradiobutton(Fen27,font=Env$police,command=function() {save();inactif();tkconfigure(rb8,state="normal");tkconfigure(rb9,state="normal")})
  rb9=tkradiobutton(Fen27,font=Env$police,command=function() {save();inactif();tkconfigure(rb8,state="normal");tkconfigure(rb9,state="normal")})
  tkconfigure(rb1,variable=drtValue,value="aucune",text=Env$vocab[118,1])
  tkconfigure(rb2,variable=drtValue,value="hor",text=Env$vocab[169,1])
  tkconfigure(rb3,variable=drtValue,value="ver",text=Env$vocab[170,1])
  tkconfigure(rb4,variable=drtValue,value="affine",text=Env$vocab[171,1])
  tkconfigure(rb5,variable=drtValue,value="reg",text=Env$vocab[172,1])
  tkconfigure(rb6,variable=drtValue,value="quadra",text=Env$vocab[175,1])
  tkconfigure(rb7,variable=drtValue,value="gam",text=Env$vocab[176,1])
  tkconfigure(rb8,variable=regValue,value="mc",text=Env$vocab[173,1],state=if (tclvalue(drtValue)=="reg") {"normal"} else {"disabled"})
  tkconfigure(rb9,variable=regValue,value="mr",text=Env$vocab[174,1],state=if (tclvalue(drtValue)=="reg") {"normal"} else {"disabled"})
  hor=tkentry(Fen27,width=5,font=Env$police,textvariable=horizValue,state=if (tclvalue(drtValue)=="hor") {"normal"} else {"disabled"})
  ver=tkentry(Fen27,width=5,font=Env$police,textvariable=verticValue,state=if (tclvalue(drtValue)=="ver") {"normal"} else {"disabled"})
  coeffa=tkentry(Fen27,width=5,font=Env$police,textvariable=coefaValue,state=if (tclvalue(drtValue)=="affine") {"normal"} else {"disabled"})
  coeffb=tkentry(Fen27,width=5,font=Env$police,textvariable=coefbValue,state=if (tclvalue(drtValue)=="affine") {"normal"} else {"disabled"})
  tkbind(hor,"<ButtonRelease-1>",function() {tkselection.set(niv.liste,as.numeric(tclvalue(niv.select)))})
  tkbind(hor,"<KeyRelease>",function() {save()})
  tkbind(ver,"<ButtonRelease-1>",function() {tkselection.set(niv.liste,as.numeric(tclvalue(niv.select)))})
  tkbind(ver,"<KeyRelease>",function() {save()})
  tkbind(coeffa,"<ButtonRelease-1>",function() {tkselection.set(niv.liste,as.numeric(tclvalue(niv.select)))})
  tkbind(coeffa,"<KeyRelease>",function() {save()})
  tkbind(coeffb,"<ButtonRelease-1>",function() {tkselection.set(niv.liste,as.numeric(tclvalue(niv.select)))})
  tkbind(coeffb,"<KeyRelease>",function() {save()})
  trait.choose=ttkcombobox(Fen27,font=Env$police,values=c(Env$vocab[75,1],Env$vocab[76,1],Env$vocab[77,1]),textvariable=traValue,state="readonly")
  tkbind(trait.choose,"<<ComboboxSelected>>",function() {
    tkselection.set(niv.liste,as.numeric(tclvalue(niv.select)))
    tra2[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(traValue)
  })
  ep.choose=tkscale(Fen27,from=1,to=5,showvalue=TRUE,font=Env$police,variable=ep.ligValue,resolution=1,orient="horizontal",command=function(...) {
    ep.lig2[as.numeric(tkcurselection(niv.liste))+1]<<-tclvalue(ep.ligValue)
  })
  ok=tkbutton(Fen27,text=Env$vocab[1,1],font=Env$police,width=16,command=function() {
    assign("droite",drt2,pos=Env)
    assign("regression",reg2,pos=Env)
    assign("hor",horiz2,pos=Env)
    assign("ver",vertic2,pos=Env)
    assign("coeffa",coefa2,pos=Env)
    assign("coeffb",coefb2,pos=Env)
    assign("traits",tra2,pos=Env)
    assign("ep.lignes",ep.lig2,pos=Env)
    tkgrab.release(Fen27)
    tkdestroy(Fen27)
  })
  annuler=tkbutton(Fen27,text=Env$vocab[2,1],font=Env$police,width=16,command=function() {tkgrab.release(Fen27);tkdestroy(Fen27)})
  tkgrid(tklabel(Fen27,text="",font=Env$police))
  tkgrid(tklabel(Fen27,text="     ",font=Env$police),row=1,column=0)
  tkgrid(niv.liste,niv.liste.scroll,row=1,column=1,rowspan=5)
  tkgrid.configure(niv.liste.scroll,sticky="ens")
  tkgrid(tklabel(Fen27,text="          ",font=Env$police),row=1,column=2)
  tkgrid(rb1,row=1,column=3,columnspan=2,sticky="w")
  tkgrid(tklabel(Fen27,text="       ",font=Env$police),row=1,column=9)
  tkgrid(rb2,row=2,column=3,columnspan=2,sticky="w")
  tkgrid(tklabel(Fen27,text=paste(var2,"  =  "),font=Env$police),row=2,column=5,sticky="e")
  tkgrid(hor,row=2,column=6,sticky="w")
  tkgrid(rb3,row=3,column=3,columnspan=2,sticky="w")
  tkgrid(tklabel(Fen27,text=paste(var1,"  =  "),font=Env$police),row=3,column=5,sticky="e")
  tkgrid(ver,row=3,column=6,sticky="w")
  tkgrid(rb4,row=4,column=3,columnspan=2,sticky="w")
  tkgrid(tklabel(Fen27,text=paste(var2,"  =  "),font=Env$police),row=4,column=5,sticky="e")
  tkgrid(coeffa,row=4,column=6,sticky="w")
  tkgrid(tklabel(Fen27,text=paste("",var1,"  +  "),font=Env$police),row=4,column=7,sticky="w")
  tkgrid(coeffb,row=4,column=8,sticky="w")
  tkgrid(rb5,row=5,column=3,columnspan=6,sticky="w")
  tkgrid(rb8,row=6,column=4,columnspan=5,sticky="w")
  tkgrid(rb9,row=7,column=4,columnspan=5,sticky="w")
  tkgrid(rb6,row=8,column=3,columnspan=6,sticky="w")
  tkgrid(rb7,row=9,column=3,columnspan=6,sticky="w")
  tkgrid(tklabel(Fen27,text=" ",font=Env$police))
  tkgrid(tklabel(Fen27,text=Env$vocab[74,1],font=Env$police),row=11,column=3,sticky="e")
  tkgrid(trait.choose,row=11,column=4,columnspan=2,sticky="w")
  tkgrid(tklabel(Fen27,text=Env$vocab[78,1],font=Env$police),row=12,column=3,sticky="e")
  tkgrid(ep.choose,row=12,column=4,columnspan=2,sticky="w")
  tkgrid(tklabel(Fen27,text="",font=Env$police))
  tkgrid(ok,row=14,column=1,columnspan=3)
  tkgrid(annuler,row=14,column=4,columnspan=5)
  tkgrid(tklabel(Fen27,text="",font=tkfont.create(family="Arial",size=4)))
}

