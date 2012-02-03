navigation <-
function(type) {
  if (type%in%c("data","hist","moust","barres","cam","courbe","nuage")) {
  fr2.close()
  fr3.close()
  fr4.close()
  fr5.close()
  fr6.close()
    if (type=="data") {
	Env$l.var$ecran<-"D"
	tkconfigure(Env$l.lab$lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[1,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab2,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[2,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab3,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[3,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[4,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[19,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab6,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Lab0.gif",fsep=.Platform$file.sep)))
	tkconfigure(Env$l.wdg$but.lab6,state="disabled")
	fr1.openD()
    }
    if (type=="hist") {
	Env$l.var$ecran<-"H"
	msg(text="",type="info")
	tkconfigure(Env$l.lab$lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[5,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab2,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[6,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab3,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[7,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[8,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[9,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab6,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Lab0.gif",fsep=.Platform$file.sep)))
	tkconfigure(Env$l.wdg$but.lab5,state="normal")
	tkconfigure(Env$l.wdg$but.lab6,state="disabled")
	reinit.variables()
	fr1.openH()
    }
    if (type=="moust") {
	Env$l.var$ecran<-"M"
	msg(text="",type="info")
	tkconfigure(Env$l.lab$lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[5,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab2,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[6,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab3,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[7,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[10,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[11,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab6,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[12,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.wdg$but.lab5,state="normal")
	tkconfigure(Env$l.wdg$but.lab6,state="normal")
	reinit.variables()
	fr1.openM()
    }
    if (type=="barres") {
	Env$l.var$ecran<-"B"
	msg(text="",type="info")
	tkconfigure(Env$l.lab$lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[5,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab2,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[6,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab3,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[7,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[8,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[13,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab6,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[14,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.wdg$but.lab5,state="normal")
	tkconfigure(Env$l.wdg$but.lab6,state="normal")
	reinit.variables()
	fr1.openB()
    }
    if (type=="cam") {
	Env$l.var$ecran<-"Ca"
	msg(text="",type="info")
	tkconfigure(Env$l.lab$lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[5,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab2,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[6,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab3,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[15,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[16,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[14,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab6,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Lab0.gif",fsep=.Platform$file.sep)))
	tkconfigure(Env$l.wdg$but.lab5,state="normal")
	tkconfigure(Env$l.wdg$but.lab6,state="disabled")
	reinit.variables()
	fr1.openCa()
    }
    if (type=="courbe") {
	Env$l.var$ecran<-"Co"
	msg(text="",type="info")
	tkconfigure(Env$l.lab$lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[5,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab2,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[6,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab3,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[7,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[17,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[13,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab6,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[14,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.wdg$but.lab5,state="normal")
	tkconfigure(Env$l.wdg$but.lab6,state="normal")
	reinit.variables()
	fr1.openCo()
    }
    if (type=="nuage") {
	Env$l.var$ecran<-"N"
	msg(text="",type="info")
	tkconfigure(Env$l.lab$lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[5,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab2,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[6,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab3,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[7,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[18,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[20,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.lab$lab6,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$img[14,1],fsep=.Platform$file.sep)))
	tkconfigure(Env$l.wdg$but.lab5,state="normal")
	tkconfigure(Env$l.wdg$but.lab6,state="normal")
	reinit.variables()
	fr1.openN()
    }
  }
}
