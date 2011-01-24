pvalue <-
function(abscisses,hauteurs) {
  Pval=tktoplevel()
  tkwm.resizable(Pval,FALSE,FALSE)
  tktitle(Pval)="GrapheR"
  tkgrab.set(Pval)
  text1Value=tclVar("")
  text1=tkentry(Pval,width=10,textvariable=text1Value,font=Env$police)
  text2Value=tclVar("")
  text2=tkentry(Pval,width=10,textvariable=text2Value,font=Env$police)
  taille1Value=tclVar("1")
  taille1.choose=tkscale(Pval,from=0.5,to=3,showvalue=TRUE,font=Env$police,variable=taille1Value,resolution=0.25,orient="horizontal")
  taille2Value=tclVar("1")
  taille2.choose=tkscale(Pval,from=0.5,to=3,showvalue=TRUE,font=Env$police,variable=taille2Value,resolution=0.25,orient="horizontal")
  col1Value=tclVar("black")
  col1.choose=tkbutton(Pval,text="Aa",font=tkfont.create(family="Arial",size=10,weight="bold"),command=function() {couleur(fen=Pval,titre=232,var=col1Value,widg=col1.choose,type="but",plusieurs=FALSE)})
  col2Value=tclVar("black")
  col2.choose=tkbutton(Pval,text="Aa",font=tkfont.create(family="Arial",size=10,weight="bold"),command=function() {couleur(fen=Pval,titre=232,var=col2Value,widg=col2.choose,type="but",plusieurs=FALSE)})
  matrice=matrix(numeric(length(abscisses)^2),nrow=length(abscisses),dimnames=list(1:length(abscisses),1:length(abscisses)))
  for (i in 1:length(abscisses)) {
    for (j in 1:length(abscisses)) {
      matrice[j,i]=max(hauteurs[i:j])
    }
  }
  ecart1=0.15*max(hauteurs)
  ecart2=0.08*max(hauteurs)
  but1=tkbutton(Pval,text=Env$vocab[190,1],font=Env$police,width=16,command=function(...) {
    coord=locator(n=2)
    diff1=numeric(length(abscisses))
    diff2=numeric(length(abscisses))
    for (i in 1:length(abscisses)) {
      diff1[i]=abs(coord$x[1]-abscisses[i])
      diff2[i]=abs(coord$x[2]-abscisses[i])
    }
    x1=which(diff1==min(diff1))
    x2=which(diff2==min(diff2))
    segments(min(abscisses[x1],abscisses[x2]),matrice[x2,x1]+ecart1,max(abscisses[x1],abscisses[x2]),matrice[x2,x1]+ecart1)
    segments(abscisses[x1],hauteurs[x1]+ecart2,abscisses[x1],matrice[x2,x1]+ecart1)
    segments(abscisses[x2],hauteurs[x2]+ecart2,abscisses[x2],matrice[x2,x1]+ecart1)
    text(abscisses[x1]+(abscisses[x2]-abscisses[x1])/2,matrice[x2,x1]+ecart1+ecart2,tclvalue(text1Value),cex=as.numeric(tclvalue(taille1Value)),col=tclvalue(col1Value))
    hauteurs[x1:x2]<<-matrice[x2,x1]+ecart1+ecart2
    for (i in 1:length(abscisses)) {
      for (j in 1:length(abscisses)) {
        matrice[i,j]<<-max(hauteurs[i:j])
      }
    }
  })
  but2=tkbutton(Pval,text=Env$vocab[190,1],font=Env$police,width=16,command=function(...) {
    coord=locator(n=2)
    diff1=numeric(length(abscisses))
    diff2=numeric(length(abscisses))
    for (i in 1:length(abscisses)) {
      diff1[i]=abs(coord$x[1]-abscisses[i])
      diff2[i]=abs(coord$x[2]-abscisses[i])
    }
    x1=which(diff1==min(diff1))
    x2=which(diff2==min(diff2))
    segments(min(abscisses[x1],abscisses[x2])-0.5,matrice[x2,x1]+ecart1,max(abscisses[x1],abscisses[x2])+0.5,matrice[x2,x1]+ecart1)
    text(abscisses[x1]+(abscisses[x2]-abscisses[x1])/2,matrice[x2,x1]+ecart1+ecart2,tclvalue(text2Value),cex=as.numeric(tclvalue(taille2Value)),col=tclvalue(col2Value))
    hauteurs[x1:x2]<<-matrice[x2,x1]+ecart1+ecart2
    for (i in 1:length(abscisses)) {
      for (j in 1:length(abscisses)) {
        matrice[i,j]<<-max(hauteurs[i:j])
      }
    }
  })
  fermer=tkbutton(Pval,text=Env$vocab[182,1],font=Env$police,width=16,command=function() {tkgrab.release(Pval);tkdestroy(Pval);tkfocus(Env$Toolbar)})
  tkgrid(tklabel(Pval,text=" ",font=Env$police))
  tkgrid(tklabel(Pval,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Pval,text=Env$vocab[233,1],font=tkfont.create(family="Arial",size=11,weight="bold")),row=1,column=1,columnspan=5)
  tkgrid(tklabel(Pval,text=" ",font=tkfont.create(family="Arial",size=5)))
  tkgrid(tklabel(Pval,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Pvalue1.gif",fsep=.Platform$file.sep))),row=3,column=1,columnspan=2)
  tkgrid(tklabel(Pval,text="                         ",font=Env$police),row=3,column=3)
  tkgrid(tklabel(Pval,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Pvalue2.gif",fsep=.Platform$file.sep))),row=3,column=4,columnspan=2)
  tkgrid(tklabel(Pval,text=" ",font=Env$police))
  tkgrid(tklabel(Pval,text=Env$vocab[186,1],font=Env$police),row=5,column=1,sticky="e")
  tkgrid(text1,row=5,column=2,sticky="w")
  tkgrid(tklabel(Pval,text=Env$vocab[186,1],font=Env$police),row=5,column=4,sticky="e")
  tkgrid(text2,row=5,column=5,sticky="w")
  tkgrid(tklabel(Pval,text=Env$vocab[57,1],font=Env$police),row=6,column=1,sticky="e")
  tkgrid(taille1.choose,row=6,column=2,sticky="w")
  tkgrid(tklabel(Pval,text=Env$vocab[57,1],font=Env$police),row=6,column=4,sticky="e")
  tkgrid(taille2.choose,row=6,column=5,sticky="w")
  tkgrid(tklabel(Pval,text="     ",font=Env$police),row=6,column=6)
  tkgrid(tklabel(Pval,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(tklabel(Pval,text=Env$vocab[56,1],font=Env$police),row=8,column=1,sticky="e")
  tkgrid(col1.choose,row=8,column=2,sticky="w")
  tkgrid(tklabel(Pval,text=Env$vocab[56,1],font=Env$police),row=8,column=4,sticky="e")
  tkgrid(col2.choose,row=8,column=5,sticky="w")
  tkgrid(tklabel(Pval,text=" ",font=Env$police))
  tkgrid(tklabel(Pval,text=Env$vocab[188,1],font=Env$police),row=10,column=1,columnspan=2)
  tkgrid(tklabel(Pval,text=Env$vocab[189,1],font=Env$police),row=10,column=4,columnspan=2)
  tkgrid(tklabel(Pval,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(but1,row=12,column=1,columnspan=2)
  tkgrid(but2,row=12,column=4,columnspan=2)
  tkgrid(tklabel(Pval,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(fermer,column=1,columnspan=5)
  tkgrid(tklabel(Pval,text=" ",font=tkfont.create(family="Arial",size=4)))
}

