histogram.distrib <-
function(act,distrib.lab,distrib.check,col.distrib.lab,col.distrib,col.distrib.choose,type.distrib.lab,type.distrib.choose,ep.distrib.lab,ep.distrib.choose) {
  tkconfigure(distrib.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tkconfigure(distrib.check,state=if (act==TRUE) {"normal"} else {"disabled"})
  tkconfigure(col.distrib.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tclvalue(col.distrib)=if (act==TRUE) {"black"} else {"grey"}
  tkconfigure(col.distrib.choose,bg=tclvalue(col.distrib))
  tkconfigure(type.distrib.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tkconfigure(type.distrib.choose,state=if (act==TRUE) {"readonly"} else {"disabled"})
  tkconfigure(ep.distrib.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tkconfigure(ep.distrib.choose,state=if (act==TRUE) {"normal"} else {"disabled"},foreground=if (act==TRUE) {"black"} else {"grey"})
}

