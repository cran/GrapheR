courbe.moy <-
function() {
  tkconfigure(Env$l.fr1$titre1,foreground="black")
  tkconfigure(Env$l.fr1$moyvarX.lab,foreground="black")
  tkconfigure(Env$l.fr1$moyvarX.wdg,state="readonly")
  tkconfigure(Env$l.fr1$moyvarY.lab,foreground="black")
  tkconfigure(Env$l.fr1$moyvarY.wdg,state="readonly")
  tkconfigure(Env$l.fr1$titre2,foreground="grey")
  tkconfigure(Env$l.fr1$propvarX.lab,foreground="grey")
  tkconfigure(Env$l.fr1$propvarX.wdg,state="disabled")
  tkconfigure(Env$l.fr1$propvarY.lab,foreground="grey")
  tkconfigure(Env$l.fr1$propvarY.wdg,state="disabled")
  tkconfigure(Env$l.fr1$propvarY.niv.lab,foreground="grey")
  tkconfigure(Env$l.fr1$propvarY.niv.wdg,state="disabled")
  if (exists("type.wdg",where=Env$l.fr5)) {
    tclvalue(Env$l.var$erreur)<-""
    tkconfigure(Env$l.fr5$type.wdg,values=Env$voc[c(95:98),1])
  }
}

