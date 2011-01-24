plusieursmotifs <-
function(nb.niv,noms,dens.motif,ang.motif,col.motif) {
  Fen16=tktoplevel()
  tkwm.resizable(Fen16,FALSE,FALSE)
  tktitle(Fen16)="GrapheR"
  tkgrab.set(Fen16)
  tkfocus(Fen16)
  dens.motif2=dens.motif
  ang.motif2=ang.motif
  col.motif2=col.motif
  liste.motifs=list()
  for (i in 1:9) {liste.motifs[[i]]=tklabel(Fen16,height=35,width=35,font=Env$police,relief="groove",borderwidth=0,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Hachures",i-1,".gif",sep=""),fsep=.Platform$file.sep)))}
  niv.liste=tklistbox(Fen16,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(niv.liste.scroll,...))
  niv.liste.scroll=tkscrollbar(Fen16,repeatinterval=5,command=function(...) tkyview(niv.liste,...))
  for (i in 1:nb.niv) {tkinsert(niv.liste,"end",noms[i])}
  tkselection.set(niv.liste,0)
  if (dens.motif2[1]==0 & ang.motif2[1]==0) {tkconfigure(liste.motifs[[1]],borderwidth=2)}
  if (dens.motif2[1]==4 & ang.motif2[1]==135) {tkconfigure(liste.motifs[[2]],borderwidth=2)}
  if (dens.motif2[1]==4 & ang.motif2[1]==90) {tkconfigure(liste.motifs[[3]],borderwidth=2)}
  if (dens.motif2[1]==4 & ang.motif2[1]==45) {tkconfigure(liste.motifs[[4]],borderwidth=2)}
  if (dens.motif2[1]==4 & ang.motif2[1]==0) {tkconfigure(liste.motifs[[5]],borderwidth=2)}
  if (dens.motif2[1]==15 & ang.motif2[1]==135) {tkconfigure(liste.motifs[[6]],borderwidth=2)}
  if (dens.motif2[1]==15 & ang.motif2[1]==90) {tkconfigure(liste.motifs[[7]],borderwidth=2)}
  if (dens.motif2[1]==15 & ang.motif2[1]==45) {tkconfigure(liste.motifs[[8]],borderwidth=2)}
  if (dens.motif2[1]==15 & ang.motif2[1]==0) {tkconfigure(liste.motifs[[9]],borderwidth=2)}
  col.motif2.choose=tkcanvas(Fen16,width="40",height="25",bg=tclvalue(tclVar(col.motif2[as.numeric(tkcurselection(niv.liste))+1])))
  tkbind(niv.liste,"<ButtonRelease-1>",function() {
    for (i in 1:9) {tkconfigure(liste.motifs[[i]],borderwidth=0)}
    if (dens.motif2[as.numeric(tkcurselection(niv.liste))+1]==0 & ang.motif2[as.numeric(tkcurselection(niv.liste))+1]==0) {tkconfigure(liste.motifs[[1]],borderwidth=2)}
    if (dens.motif2[as.numeric(tkcurselection(niv.liste))+1]==4 & ang.motif2[as.numeric(tkcurselection(niv.liste))+1]==135) {tkconfigure(liste.motifs[[2]],borderwidth=2)}
    if (dens.motif2[as.numeric(tkcurselection(niv.liste))+1]==4 & ang.motif2[as.numeric(tkcurselection(niv.liste))+1]==90) {tkconfigure(liste.motifs[[3]],borderwidth=2)}
    if (dens.motif2[as.numeric(tkcurselection(niv.liste))+1]==4 & ang.motif2[as.numeric(tkcurselection(niv.liste))+1]==45) {tkconfigure(liste.motifs[[4]],borderwidth=2)}
    if (dens.motif2[as.numeric(tkcurselection(niv.liste))+1]==4 & ang.motif2[as.numeric(tkcurselection(niv.liste))+1]==0) {tkconfigure(liste.motifs[[5]],borderwidth=2)}
    if (dens.motif2[as.numeric(tkcurselection(niv.liste))+1]==15 & ang.motif2[as.numeric(tkcurselection(niv.liste))+1]==135) {tkconfigure(liste.motifs[[6]],borderwidth=2)}
    if (dens.motif2[as.numeric(tkcurselection(niv.liste))+1]==15 & ang.motif2[as.numeric(tkcurselection(niv.liste))+1]==90) {tkconfigure(liste.motifs[[7]],borderwidth=2)}
    if (dens.motif2[as.numeric(tkcurselection(niv.liste))+1]==15 & ang.motif2[as.numeric(tkcurselection(niv.liste))+1]==45) {tkconfigure(liste.motifs[[8]],borderwidth=2)}
    if (dens.motif2[as.numeric(tkcurselection(niv.liste))+1]==15 & ang.motif2[as.numeric(tkcurselection(niv.liste))+1]==0) {tkconfigure(liste.motifs[[9]],borderwidth=2)}
    tkconfigure(col.motif2.choose,bg=tclvalue(tclVar(col.motif2[as.numeric(tkcurselection(niv.liste))+1])))
  })
  tkbind(col.motif2.choose,"<ButtonRelease-1>",function() {col.motif2[as.numeric(tkcurselection(niv.liste))+1]<<-couleur(fen=Fen16,titre=227,var=col.motif2[as.numeric(tkcurselection(niv.liste))+1],widg=col.motif2.choose,type="can",plusieurs=TRUE)})
  motif.choix=function(num,dens,ang) {
    for (i in 1:9) {tkconfigure(liste.motifs[[i]],borderwidth=0)}
    tkconfigure(liste.motifs[[num]],borderwidth=2)
    dens.motif2[as.numeric(tkcurselection(niv.liste))+1]<<-dens
    ang.motif2[as.numeric(tkcurselection(niv.liste))+1]<<-ang
  }
  tkbind(liste.motifs[[1]],"<ButtonRelease-1>",function() {motif.choix(num=1,dens=0,ang=0)})
  tkbind(liste.motifs[[2]],"<ButtonRelease-1>",function() {motif.choix(num=2,dens=4,ang=135)})
  tkbind(liste.motifs[[3]],"<ButtonRelease-1>",function() {motif.choix(num=3,dens=4,ang=90)})
  tkbind(liste.motifs[[4]],"<ButtonRelease-1>",function() {motif.choix(num=4,dens=4,ang=45)})
  tkbind(liste.motifs[[5]],"<ButtonRelease-1>",function() {motif.choix(num=5,dens=4,ang=0)})
  tkbind(liste.motifs[[6]],"<ButtonRelease-1>",function() {motif.choix(num=6,dens=15,ang=135)})
  tkbind(liste.motifs[[7]],"<ButtonRelease-1>",function() {motif.choix(num=7,dens=15,ang=90)})
  tkbind(liste.motifs[[8]],"<ButtonRelease-1>",function() {motif.choix(num=8,dens=15,ang=45)})
  tkbind(liste.motifs[[9]],"<ButtonRelease-1>",function() {motif.choix(num=6,dens=15,ang=0)})
  ok=tkbutton(Fen16,text=Env$vocab[1,1],width=16,font=Env$police,command=function() {
    assign("dens.motif",dens.motif2,pos=Env)
    assign("ang.motif",ang.motif2,pos=Env)
    assign("col.motif",col.motif2,pos=Env)
    tkgrab.release(Fen16)
    tkdestroy(Fen16)
  })
  annuler=tkbutton(Fen16,text=Env$vocab[2,1],width=16,font=Env$police,command=function() {tkgrab.release(Fen16);tkdestroy(Fen16)})
  tkgrid(tklabel(Fen16,text="",font=Env$police))
  tkgrid(tklabel(Fen16,text="     ",font=Env$police),row=1,column=0)
  tkgrid(niv.liste,niv.liste.scroll,row=1,column=1,rowspan=3,sticky="n")
  tkgrid.configure(niv.liste.scroll,sticky="ens")
  tkgrid(tklabel(Fen16,text="     ",font=Env$police),row=1,column=2)
  tkgrid(liste.motifs[[1]],row=1,column=3)
  tkgrid(liste.motifs[[2]],row=2,column=3)
  tkgrid(liste.motifs[[3]],row=2,column=4)
  tkgrid(liste.motifs[[4]],row=2,column=5)
  tkgrid(liste.motifs[[5]],row=2,column=6)
  tkgrid(tklabel(Fen16,text="   ",font=Env$police),row=1,column=7)
  tkgrid(liste.motifs[[6]],row=3,column=3)
  tkgrid(liste.motifs[[7]],row=3,column=4)
  tkgrid(liste.motifs[[8]],row=3,column=5)
  tkgrid(liste.motifs[[9]],row=3,column=6)
  tkgrid(tklabel(Fen16,text=" ",font=tkfont.create(family="Arial",size=2)),row=4,column=3)
  tkgrid(tklabel(Fen16,text=Env$vocab[56,1],font=Env$police),row=5,column=3,columnspan=2,sticky="e")
  tkgrid(col.motif2.choose,row=5,column=5,columnspan=2,sticky="w")
  tkgrid(tklabel(Fen16,text="",font=Env$police))
  tkgrid(ok,row=7,column=1)
  tkgrid(annuler,row=7,column=3,columnspan=4)	  
  tkgrid(tklabel(Fen16,text=" ",font=tkfont.create(family="Arial",size=4))) 
}

