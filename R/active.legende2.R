active.legende2 <-
function() {
  if (tclvalue(Env$l.var$cam.lien)==1) {
    if (exists("legende.lab",where=Env$l.fr5)) {
	tkconfigure(Env$l.fr5$legende.lab,foreground="grey")
	tkconfigure(Env$l.fr5$legende.wdg,state="disabled")
	tkconfigure(Env$l.fr5$titre.lab,foreground="grey")
	tkconfigure(Env$l.fr5$titre.wdg,state="disabled")
	tkconfigure(Env$l.fr5$position.lab,foreground="grey")
	tkconfigure(Env$l.fr5$position.wdg,state="disabled")
    }
  } else {
    if (exists("legende.lab",where=Env$l.fr5)) {
	tkconfigure(Env$l.fr5$legende.lab,foreground="black")
	tkconfigure(Env$l.fr5$legende.wdg,state="normal")
	tkconfigure(Env$l.fr5$titre.lab,foreground="black")
	tkconfigure(Env$l.fr5$titre.wdg,state="normal")
	tkconfigure(Env$l.fr5$position.lab,foreground="black")
	tkconfigure(Env$l.fr5$position.wdg,state="readonly")
    }
  }
}

