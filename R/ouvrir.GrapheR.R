ouvrir.GrapheR <-
function() {
  Env$Fen<-tktoplevel()
  tktitle(Env$Fen)<-"GrapheR"
  tkwm.geometry(Env$Fen, "+30+30")
  ### Barre de navigation
  Env$l.wdg$vide1<-tklabel(Env$Fen,text="  ",font=Env$police)
  Env$l.wdg$but.data<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_data.gif",fsep=.Platform$file.sep)),command=function() {navigation(type="data")})
  Env$l.wdg$sep1<-tklabel(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Sep.gif",fsep=.Platform$file.sep)))
  Env$l.wdg$but.his<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_hist.gif",fsep=.Platform$file.sep)),command=function() {navigation(type="hist")})
  Env$l.wdg$but.box<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_box.gif",fsep=.Platform$file.sep)),command=function() {navigation(type="moust")})
  Env$l.wdg$but.bar<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_bar.gif",fsep=.Platform$file.sep)),command=function() {navigation(type="barres")})
  Env$l.wdg$but.pie<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_cam.gif",fsep=.Platform$file.sep)),command=function() {navigation(type="cam")})
  Env$l.wdg$but.crb<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_courb.gif",fsep=.Platform$file.sep)),command=function() {navigation(type="courbe")})
  Env$l.wdg$but.sct<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_nuage.gif",fsep=.Platform$file.sep)),command=function() {navigation(type="nuage")})
  Env$l.wdg$sep2<-tklabel(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Sep.gif",fsep=.Platform$file.sep)))
  Env$l.wdg$but.win<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_window.gif",fsep=.Platform$file.sep)),command=new.window)
  Env$l.wdg$sep3<-tklabel(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Sep.gif",fsep=.Platform$file.sep)))
  Env$l.wdg$but.drw<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_draw.gif",fsep=.Platform$file.sep)),command=pretracer)
  Env$l.wdg$sep4<-tklabel(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Sep.gif",fsep=.Platform$file.sep)))
  Env$l.wdg$but.hor<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_hor.gif",fsep=.Platform$file.sep)),command=horizontal)
  Env$l.wdg$but.ver<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_ver.gif",fsep=.Platform$file.sep)),command=vertical)
  Env$l.wdg$but.drt<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_drt.gif",fsep=.Platform$file.sep)),command=affine)
  Env$l.wdg$but.dis<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_dis.gif",fsep=.Platform$file.sep)),command=distrib)
  Env$l.wdg$but.txt<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_text.gif",fsep=.Platform$file.sep)),command=texte)
  Env$l.wdg$but.p<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_p.gif",fsep=.Platform$file.sep)),command=pval)
  Env$l.wdg$sep5<-tklabel(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Sep.gif",fsep=.Platform$file.sep)))
  Env$l.wdg$but.save<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_save.gif",fsep=.Platform$file.sep)),command=enregistrer)
  Env$l.wdg$sep6<-tklabel(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Sep.gif",fsep=.Platform$file.sep)))
  Env$l.wdg$but.lang<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_lang.gif",fsep=.Platform$file.sep)),command=language.change)
  Env$l.wdg$but.help<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","But_help.gif",fsep=.Platform$file.sep)),command=aide)
  tkbind(Env$l.wdg$but.data,"<Enter>",function() {msg(text=Env$voc[221,1],type="info")})
  tkbind(Env$l.wdg$but.data,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.his,"<Enter>",function() {msg(text=Env$voc[222,1],type="info")})
  tkbind(Env$l.wdg$but.his,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.box,"<Enter>",function() {msg(text=Env$voc[223,1],type="info")})
  tkbind(Env$l.wdg$but.box,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.bar,"<Enter>",function() {msg(text=Env$voc[224,1],type="info")})
  tkbind(Env$l.wdg$but.bar,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.pie,"<Enter>",function() {msg(text=Env$voc[225,1],type="info")})
  tkbind(Env$l.wdg$but.pie,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.crb,"<Enter>",function() {msg(text=Env$voc[226,1],type="info")})
  tkbind(Env$l.wdg$but.crb,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.sct,"<Enter>",function() {msg(text=Env$voc[227,1],type="info")})
  tkbind(Env$l.wdg$but.sct,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.win,"<Enter>",function() {msg(text=Env$voc[228,1],type="info")})
  tkbind(Env$l.wdg$but.win,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.drw,"<Enter>",function() {msg(text=Env$voc[229,1],type="info")})
  tkbind(Env$l.wdg$but.drw,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.hor,"<Enter>",function() {msg(text=Env$voc[230,1],type="info")})
  tkbind(Env$l.wdg$but.hor,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.ver,"<Enter>",function() {msg(text=Env$voc[231,1],type="info")})
  tkbind(Env$l.wdg$but.ver,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.drt,"<Enter>",function() {msg(text=Env$voc[232,1],type="info")})
  tkbind(Env$l.wdg$but.drt,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.dis,"<Enter>",function() {msg(text=Env$voc[233,1],type="info")})
  tkbind(Env$l.wdg$but.dis,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.txt,"<Enter>",function() {msg(text=Env$voc[234,1],type="info")})
  tkbind(Env$l.wdg$but.txt,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.p,"<Enter>",function() {msg(text=Env$voc[235,1],type="info")})
  tkbind(Env$l.wdg$but.p,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.save,"<Enter>",function() {msg(text=Env$voc[236,1],type="info")})
  tkbind(Env$l.wdg$but.save,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.lang,"<Enter>",function() {msg(text=Env$voc[237,1],type="info")})
  tkbind(Env$l.wdg$but.lang,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.wdg$but.help,"<Enter>",function() {msg(text=Env$voc[238,1],type="info")})
  tkbind(Env$l.wdg$but.help,"<Leave>",function() {msg(text="",type="info")})
  tkgrid(Env$l.wdg$vide1,Env$l.wdg$but.data,Env$l.wdg$sep1,Env$l.wdg$but.his,Env$l.wdg$but.box,Env$l.wdg$but.bar,Env$l.wdg$but.pie,Env$l.wdg$but.crb,
    Env$l.wdg$but.sct,Env$l.wdg$sep2,Env$l.wdg$but.win,Env$l.wdg$sep3,Env$l.wdg$but.drw,Env$l.wdg$sep4,Env$l.wdg$but.hor,Env$l.wdg$but.ver,
    Env$l.wdg$but.drt,Env$l.wdg$but.dis,Env$l.wdg$but.txt,Env$l.wdg$but.p,Env$l.wdg$sep5,Env$l.wdg$but.save,Env$l.wdg$sep6,Env$l.wdg$but.lang,
    Env$l.wdg$but.help)
  tkgrid(tklabel(Env$Fen,text="",font=Env$police2),row=1,column=0)
  ### Cadre de texte pour les messages
  Env$l.wdg$message.wdg<-tkentry(Env$Fen,width=100,font=Env$police5,textvariable=Env$l.var$message,state="readonly",readonlybackground="white")
  tkgrid(Env$l.wdg$message.wdg,row=2,column=0,columnspan=25)
  tkgrid(tklabel(Env$Fen,text="",font=Env$police2),row=3,column=0)
  msg(text=Env$voc[1,1],type="info")
  ### Frame gauche
  Env$l.frames$Frg<-tkframe(Env$Fen)
  Env$l.lab$lab1<-tklabel(Env$l.frames$Frg,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[1,1],fsep=.Platform$file.sep)))
  Env$l.wdg$but.lab1<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)),command=function() {
	if (Env$l.frames$Fr1.status==1) {fr1.close()} else {
	if (Env$l.var$ecran=="D") {fr1.openD()} else
	if (Env$l.var$ecran=="H") {fr1.openH()} else
	if (Env$l.var$ecran=="M") {fr1.openM()} else
	if (Env$l.var$ecran=="B") {fr1.openB()} else
	if (Env$l.var$ecran=="Ca") {fr1.openCa()} else
	if (Env$l.var$ecran=="Co") {fr1.openCo()} else
	if (Env$l.var$ecran=="N") {fr1.openN()}
    }
  })
  tkgrid(Env$l.lab$lab1,Env$l.wdg$but.lab1)
  ## Frame 1
  Env$l.frames$Fr1<-tkframe(Env$l.frames$Frg)
  Env$l.fr1$vide<-tklabel(Env$l.frames$Fr1,text="",font=Env$police2)
  tkgrid(Env$l.fr1$vide)
  fr1.openD()
  tkgrid(Env$l.frames$Fr1)
  tkgrid(tklabel(Env$l.frames$Frg,text="",font=Env$police2))
  Env$l.lab$lab2<-tklabel(Env$l.frames$Frg,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[2,1],fsep=.Platform$file.sep)))
  Env$l.wdg$but.lab2<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)),command=function() {
    if (Env$l.frames$Fr2.status==1) {fr2.close()} else {
	if (Env$l.var$ecran=="D") {fr2.openD()} else
	if (Env$l.var$ecran%in%c("H","M","B","Ca","Co","N")) {fr2.opengraphe()}
    }
  })
  tkgrid(Env$l.lab$lab2,Env$l.wdg$but.lab2)
  ## Frame 2
  Env$l.frames$Fr2<-tkframe(Env$l.frames$Frg)
  Env$l.fr2$vide<-tklabel(Env$l.frames$Fr2,text="",font=Env$police2)
  tkgrid(Env$l.fr2$vide)
  fr2.close()
  tkgrid(Env$l.frames$Fr2)
  tkgrid(tklabel(Env$l.frames$Frg,text="",font=Env$police2))
  Env$l.lab$lab3<-tklabel(Env$l.frames$Frg,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[3,1],fsep=.Platform$file.sep)))
  Env$l.wdg$but.lab3<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)),command=function() {
    if (Env$l.frames$Fr3.status==1) {fr3.close()} else {
	if (Env$l.var$ecran=="D") {fr3.openD()} else
	if (Env$l.var$ecran=="H") {fr3.openH()} else
	if (Env$l.var$ecran=="M") {fr3.openM()} else
	if (Env$l.var$ecran=="B") {fr3.openB()} else
	if (Env$l.var$ecran=="Ca") {fr3.openCa()} else
	if (Env$l.var$ecran%in%c("Co","N")) {fr3.openCoN()}
    }
  })
  tkgrid(Env$l.lab$lab3,Env$l.wdg$but.lab3)
  ## Frame 3
  Env$l.frames$Fr3<-tkframe(Env$l.frames$Frg)
  Env$l.fr3$vide<-tklabel(Env$l.frames$Fr3,text="",font=Env$police2)
  tkgrid(Env$l.fr3$vide)
  fr3.close()
  tkgrid(Env$l.frames$Fr3)
  tkgrid(tklabel(Env$l.frames$Frg,text="",font=Env$police2))
  Env$l.lab$lab4<-tklabel(Env$l.frames$Frg,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[4,1],fsep=.Platform$file.sep)))
  Env$l.wdg$but.lab4<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)),command=function() {
    if (Env$l.frames$Fr4.status==1) {fr4.close()} else {
	if (Env$l.var$ecran=="D") {fr4.openD()} else
	if (Env$l.var$ecran=="H") {fr4.openH()} else
	if (Env$l.var$ecran=="M") {fr4.openM()} else
	if (Env$l.var$ecran=="B") {fr4.openB()} else
	if (Env$l.var$ecran=="Ca") {fr4.openCa()} else
	if (Env$l.var$ecran=="Co") {fr4.openCo()} else
	if (Env$l.var$ecran=="N") {fr4.openN()}
    }
  })
  tkgrid(Env$l.lab$lab4,Env$l.wdg$but.lab4)
  ## Frame 4
  Env$l.frames$Fr4<-tkframe(Env$l.frames$Frg)
  Env$l.fr4$vide<-tklabel(Env$l.frames$Fr4,text="",font=Env$police2)
  tkgrid(Env$l.fr4$vide)
  fr4.close()
  tkgrid(Env$l.frames$Fr4)
  tkgrid(tklabel(Env$l.frames$Frg,text="",font=Env$police2))
  Env$l.lab$lab5<-tklabel(Env$l.frames$Frg,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[19,1],fsep=.Platform$file.sep)))
  Env$l.wdg$but.lab5<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)),command=function() {
    if (Env$l.frames$Fr5.status==1) {fr5.close()} else {
	if (Env$l.var$ecran=="D") {fr5.openD()} else
	if (Env$l.var$ecran=="H") {fr5.openH()} else
	if (Env$l.var$ecran=="M") {fr5.openM()} else
	if (Env$l.var$ecran=="B") {fr5.openB()} else
	if (Env$l.var$ecran=="Ca") {fr5.openCa()} else
	if (Env$l.var$ecran=="Co") {fr5.openCo()} else
	if (Env$l.var$ecran=="N") {fr5.openN()}
    }
  })
  tkgrid(Env$l.lab$lab5,Env$l.wdg$but.lab5)
  ## Frame 5
  Env$l.frames$Fr5<-tkframe(Env$l.frames$Frg)
  Env$l.fr5$vide<-tklabel(Env$l.frames$Fr5,text="",font=Env$police2)
  tkgrid(Env$l.fr5$vide)
  fr5.close()
  tkgrid(Env$l.frames$Fr5)
  tkgrid(tklabel(Env$l.frames$Frg,text="",font=Env$police2))
  Env$l.lab$lab6<-tklabel(Env$l.frames$Frg,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Lab0.gif",fsep=.Platform$file.sep)))
  Env$l.wdg$but.lab6<-tkbutton(Env$Fen,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)),command=function() {
    if (Env$l.frames$Fr6.status==1) {fr6.close()} else {
	if (Env$l.var$ecran=="M") {fr6.openM()} else
	if (Env$l.var$ecran=="B") {fr6.openB()} else
	if (Env$l.var$ecran=="Co") {fr6.openCo()} else
	if (Env$l.var$ecran=="N") {fr6.openN()}
    }
  },state="disabled")
  tkgrid(Env$l.lab$lab6,Env$l.wdg$but.lab6)
  ## Frame 6
  Env$l.frames$Fr6<-tkframe(Env$l.frames$Frg)
  Env$l.fr6$vide<-tklabel(Env$l.frames$Fr6,text="",font=Env$police2)
  tkgrid(Env$l.fr6$vide)
  fr6.close()
  tkgrid(Env$l.frames$Fr6)
  tkgrid(tklabel(Env$l.frames$Frg,text="",font=Env$police2))
  tkgrid(Env$l.frames$Frg,row=4,column=0,columnspan=25)
  ### Frame droite
  Env$l.frames$Frd<-tkframe(Env$Fen)
  ## Frame 7
  Env$l.frames$Fr7<-tkframe(Env$l.frames$Frd,relief="ridge",borderwidth=0)
  Env$l.fr7$vide<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  tkgrid(Env$l.fr7$vide)
  tkgrid(Env$l.frames$Fr7)
  tkgrid(Env$l.frames$Frd,row=1,column=26,rowspan=4,sticky="n")
  ### Frame droite finale
  Env$l.frames$Frd2<-tkframe(Env$Fen)
  Env$l.wdg$vide2<-tklabel(Env$l.frames$Frd2,text="",font=Env$police)
  tkgrid(Env$l.wdg$vide2)
  tkgrid(Env$l.frames$Frd2,row=0,column=27)
}
