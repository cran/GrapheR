nuage.legende <-
function(act,legende.check.lab,legende.check,nom.legende.lab,nom.legende.but,legende.pos.lab,legende.pos.choose) {
  tkconfigure(legende.check.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tkconfigure(legende.check,state=if (act==TRUE) {"normal"} else {"disabled"})
  tkconfigure(nom.legende.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tkconfigure(nom.legende.but,state=if (act==TRUE) {"normal"} else {"disabled"})
  tkconfigure(legende.pos.lab,foreground=if (act==TRUE) {"black"} else {"grey"})
  tkconfigure(legende.pos.choose,state=if (act==TRUE) {"readonly"} else {"disabled"})
}

