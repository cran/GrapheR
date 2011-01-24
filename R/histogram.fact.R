histogram.fact <-
function(factValue,nivValue,niv.lab,niv.choose) {
  if (tclvalue(factValue)==Env$vocab[48,1]) {
    tkconfigure(niv.lab,foreground="grey")
    tkconfigure(niv.choose,state="normal")
    tkdelete(niv.choose,0,"end")
    tkconfigure(niv.choose,values=c(""))
    tkconfigure(niv.choose,state="disabled")
  } else {
    tkconfigure(niv.lab,foreground="black")
    tkconfigure(niv.choose,state="normal")
    tkdelete(niv.choose,0,"end")
    tkconfigure(niv.choose,values=levels(Env$datas.GrapheR[,tclvalue(factValue)]))
    tclvalue(nivValue)=levels(Env$datas.GrapheR[,tclvalue(factValue)])[1]
    tkconfigure(niv.choose,state="readonly")
  }
}

