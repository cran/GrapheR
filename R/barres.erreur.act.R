barres.erreur.act <-
function(act,erreur.lab,erreur.choose,col.erreur.lab,col.erreur,col.erreur.choose,segment.lab,segment.check) {
  tkconfigure(erreur.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tkconfigure(erreur.choose,state=if (act==TRUE) {"readonly"} else {"disabled"})
  tkconfigure(col.erreur.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tclvalue(col.erreur)=if (act==TRUE) {"black"} else {"grey"}
  tkconfigure(col.erreur.choose,bg=tclvalue(col.erreur))
  tkconfigure(segment.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tkconfigure(segment.check,state=if (act==TRUE) {"normal"} else {"disabled"})
}

