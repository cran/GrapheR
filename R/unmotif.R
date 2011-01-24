unmotif <-
function(dens.motif,ang.motif,col.motif) {
  Fen15=tktoplevel()
  tkwm.resizable(Fen15,FALSE,FALSE)
  tktitle(Fen15)="GrapheR"
  tkgrab.set(Fen15)
  tkfocus(Fen15)
  dens.motif2=dens.motif
  ang.motif2=ang.motif
  col.motif2=tclVar(col.motif)
  col.motif.choose=tkcanvas(Fen15,width="40",height="25",bg=tclvalue(col.motif2))
  tkbind(col.motif.choose,"<ButtonRelease-1>",function() {couleur(fen=Fen15,titre=227,var=col.motif2,widg=col.motif.choose,type="can",plusieurs=FALSE)})
  liste.motifs=list()
  for (i in 1:9) {
    liste.motifs[[i]]=tklabel(Fen15,height=35,width=35,font=Env$police,relief="groove",borderwidth=0,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Hachures",i-1,".gif",sep=""),fsep=.Platform$file.sep)))
  }
  if (dens.motif2==0 & ang.motif2==0) {tkconfigure(liste.motifs[[1]],borderwidth=2)}
  if (dens.motif2==4 & ang.motif2==135) {tkconfigure(liste.motifs[[2]],borderwidth=2)}
  if (dens.motif2==4 & ang.motif2==90) {tkconfigure(liste.motifs[[3]],borderwidth=2)}
  if (dens.motif2==4 & ang.motif2==45) {tkconfigure(liste.motifs[[4]],borderwidth=2)}
  if (dens.motif2==4 & ang.motif2==0) {tkconfigure(liste.motifs[[5]],borderwidth=2)}
  if (dens.motif2==15 & ang.motif2==135) {tkconfigure(liste.motifs[[6]],borderwidth=2)}
  if (dens.motif2==15 & ang.motif2==90) {tkconfigure(liste.motifs[[7]],borderwidth=2)}
  if (dens.motif2==15 & ang.motif2==45) {tkconfigure(liste.motifs[[8]],borderwidth=2)}
  if (dens.motif2==15 & ang.motif2==0) {tkconfigure(liste.motifs[[9]],borderwidth=2)}
  motif.choix=function(num,dens,ang) {
    for (i in 1:9) {tkconfigure(liste.motifs[[i]],borderwidth=0)}
    tkconfigure(liste.motifs[[num]],borderwidth=2)
    dens.motif2<<-dens
    ang.motif2<<-ang
  }
  tkbind(liste.motifs[[1]],"<ButtonRelease-1>",function() {motif.choix(num=1,dens=0,ang=0)})
  tkbind(liste.motifs[[2]],"<ButtonRelease-1>",function() {motif.choix(num=2,dens=4,ang=135)})
  tkbind(liste.motifs[[3]],"<ButtonRelease-1>",function() {motif.choix(num=3,dens=4,ang=90)})
  tkbind(liste.motifs[[4]],"<ButtonRelease-1>",function() {motif.choix(num=4,dens=4,ang=45)})
  tkbind(liste.motifs[[5]],"<ButtonRelease-1>",function() {motif.choix(num=5,dens=4,ang=0)})
  tkbind(liste.motifs[[6]],"<ButtonRelease-1>",function() {motif.choix(num=6,dens=15,ang=135)})
  tkbind(liste.motifs[[7]],"<ButtonRelease-1>",function() {motif.choix(num=7,dens=15,ang=90)})
  tkbind(liste.motifs[[8]],"<ButtonRelease-1>",function() {motif.choix(num=8,dens=15,ang=45)})
  tkbind(liste.motifs[[9]],"<ButtonRelease-1>",function() {motif.choix(num=9,dens=15,ang=0)})
  ok=tkbutton(Fen15,text=Env$vocab[1,1],font=Env$police,width=16,command=function() {
    assign("dens.motif",dens.motif2,pos=Env)
    assign("ang.motif",ang.motif2,pos=Env)
    assign("col.motif",tclvalue(col.motif2),pos=Env)
    tkgrab.release(Fen15)
    tkdestroy(Fen15)
  })
  annuler=tkbutton(Fen15,text=Env$vocab[2,1],font=Env$police,width=16,command=function() {tkgrab.release(Fen15);tkdestroy(Fen15)})
  tkgrid(tklabel(Fen15,text="",font=Env$police))
  tkgrid(tklabel(Fen15,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Fen15,text="     ",font=Env$police),row=1,column=5)
  tkgrid(liste.motifs[[1]],row=1,column=1)
  tkgrid(liste.motifs[[2]],row=2,column=1)
  tkgrid(liste.motifs[[3]],row=2,column=2)
  tkgrid(liste.motifs[[4]],row=2,column=3)
  tkgrid(liste.motifs[[5]],row=2,column=4)
  tkgrid(liste.motifs[[6]],row=3,column=1)
  tkgrid(liste.motifs[[7]],row=3,column=2)
  tkgrid(liste.motifs[[8]],row=3,column=3)
  tkgrid(liste.motifs[[9]],row=3,column=4)
  tkgrid(tklabel(Fen15,text=" ",font=tkfont.create(family="Arial",size=2)))
  tkgrid(tklabel(Fen15,text=Env$vocab[56,1],font=Env$police),row=5,column=1,columnspan=2,sticky="e")
  tkgrid(col.motif.choose,row=5,column=3,columnspan=2,sticky="w")
  tkgrid(tklabel(Fen15,text="",font=Env$police))
  tkgrid(ok,column=1,columnspan=4)
  tkgrid(tklabel(Fen15,text="",font=tkfont.create(family="Arial",size=4)))
  tkgrid(annuler,column=1,columnspan=4)
  tkgrid(tklabel(Fen15,text="",font=tkfont.create(family="Arial",size=4)))
}

