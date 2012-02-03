active.erreur <-
function() {
  if (exists("type.lab",where=Env$l.fr5)) {
    if (tclvalue(Env$l.var$stack)==1) {
	tkconfigure(Env$l.fr5$type.lab,foreground="grey")
	tkconfigure(Env$l.fr5$type.wdg,state="disabled")
	tkconfigure(Env$l.fr5$col.lab,foreground="grey")
	tkconfigure(Env$l.fr5$col.wdg,bg="grey")
    } else {
	tkconfigure(Env$l.fr5$type.lab,foreground="black")
	tkconfigure(Env$l.fr5$type.wdg,state="readonly")
	tkconfigure(Env$l.fr5$col.lab,foreground="black")
	tkconfigure(Env$l.fr5$col.wdg,bg=tclvalue(Env$l.var$couleur2A))
    }
  }
}
