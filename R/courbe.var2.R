courbe.var2 <-
function(type,niv.lab,niv.choose,var2Value,nivValue,typeValue,rb1,rb2,rb3) {
  tkconfigure(niv.lab,foreground=if (type=="fact") {"black"} else {"grey"})
  tkconfigure(niv.choose,state="normal")
  tkdelete(niv.choose,0,"end")
  if (type=="fact") {
    tkconfigure(niv.choose,values=levels(Env$datas.GrapheR[,tclvalue(var2Value)]))
    tclvalue(nivValue)=levels(Env$datas.GrapheR[,tclvalue(var2Value)])[1]
  } else {
    tclvalue(nivValue)=""
  }
  tkconfigure(niv.choose,state=if (type=="fact") {"readonly"} else {"disabled"})
  tclvalue(typeValue)=type
  tkconfigure(rb1,state=if (type=="fact") {"disabled"} else {"normal"})
  tkconfigure(rb2,state=if (type=="fact") {"disabled"} else {"normal"})
  tkconfigure(rb3,state=if (type=="fact") {"normal"} else {"disabled"})
}

