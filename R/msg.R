msg<-function(text,type) {
  if (type=="error") {
    tkconfigure(Env$l.wdg$message.wdg,foreground="red")
    tclvalue(Env$l.var$message)<-paste(Env$voc[18,1],text)
  }
  if (type=="warning") {
    tkconfigure(Env$l.wdg$message.wdg,foreground="darkgreen")
    tclvalue(Env$l.var$message)<-paste(Env$voc[160,1],text)
  }
  if (type=="info") {
    tkconfigure(Env$l.wdg$message.wdg,foreground="darkblue")
    tclvalue(Env$l.var$message)<-text
  }
  return(invisible())
}