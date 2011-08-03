courbe.prop <-
function() {
  tkconfigure(Env$l.fr1$titre1,foreground="grey")
  tkconfigure(Env$l.fr1$moyvarX.lab,foreground="grey")
  tkconfigure(Env$l.fr1$moyvarX.wdg,state="disabled")
  tkconfigure(Env$l.fr1$moyvarY.lab,foreground="grey")
  tkconfigure(Env$l.fr1$moyvarY.wdg,state="disabled")
  tkconfigure(Env$l.fr1$titre2,foreground="black")
  tkconfigure(Env$l.fr1$propvarX.lab,foreground="black")
  tkconfigure(Env$l.fr1$propvarX.wdg,state="readonly")
  tkconfigure(Env$l.fr1$propvarY.lab,foreground="black")
  tkconfigure(Env$l.fr1$propvarY.wdg,state="readonly")
  tkconfigure(Env$l.fr1$propvarY.niv.lab,foreground="black")
  tkconfigure(Env$l.fr1$propvarY.niv.wdg,state="readonly")
  if (exists("type.wdg",where=Env$l.fr5)) {
    tclvalue(Env$l.var$erreur)<-""
    tkconfigure(Env$l.fr5$type.wdg,values=Env$voc[c(95,97,98),1])
  }
}

