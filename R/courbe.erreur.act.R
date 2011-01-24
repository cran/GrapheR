courbe.erreur.act <-
function(act,erreur.lab,erreur.choose,segment.lab,segment.check) {
  tkconfigure(erreur.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tkconfigure(erreur.choose,state=if (act==TRUE) {"readonly"} else {"disabled"})
  tkconfigure(segment.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tkconfigure(segment.check,state=if (act==TRUE) {"normal"} else {"disabled"})
}

