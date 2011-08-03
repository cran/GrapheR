active.legende <-
function() {
  if (tclvalue(Env$l.var$plusieurs)==0) {
    if (exists("legende.lab",where=Env$l.fr5)) {
	tkconfigure(Env$l.fr5$legende.lab,foreground="grey")
	tkconfigure(Env$l.fr5$legende.wdg,state="disabled")
	tkconfigure(Env$l.fr5$titre.lab,foreground="grey")
	tkconfigure(Env$l.fr5$titre.wdg,state="disabled")
	tkconfigure(Env$l.fr5$position.lab,foreground="grey")
	tkconfigure(Env$l.fr5$position.wdg,state="disabled")
	tkconfigure(Env$l.fr5$noms.lab1,foreground="grey")
	tkdelete(Env$l.fr5$noms.list,0,"end")
	tkconfigure(Env$l.fr5$noms.list,state="disabled")
	tkconfigure(Env$l.fr5$noms.lab2,foreground="grey")
	tkdelete(Env$l.fr5$noms.wdg,0,"end")
	tkconfigure(Env$l.fr5$noms.wdg,state="disabled")
    }
    if (exists("legende.lab",where=Env$l.fr6)) {
	tkconfigure(Env$l.fr6$legende.lab,foreground="grey")
	tkconfigure(Env$l.fr6$legende.wdg,state="disabled")
	tkconfigure(Env$l.fr6$titre.lab,foreground="grey")
	tkconfigure(Env$l.fr6$titre.wdg,state="disabled")
	tkconfigure(Env$l.fr6$position.lab,foreground="grey")
	tkconfigure(Env$l.fr6$position.wdg,state="disabled")
	tkconfigure(Env$l.fr6$noms.lab1,foreground="grey")
	tkdelete(Env$l.fr6$noms.list,0,"end")
	tkconfigure(Env$l.fr6$noms.list,state="disabled")
	tkconfigure(Env$l.fr6$noms.lab2,foreground="grey")
	tkdelete(Env$l.fr6$noms.wdg,0,"end")
	tkconfigure(Env$l.fr6$noms.wdg,state="disabled")
    }
  } else {
    if (exists("legende.lab",where=Env$l.fr5)) {
	tkconfigure(Env$l.fr5$legende.lab,foreground="black")
	tkconfigure(Env$l.fr5$legende.wdg,state="normal")
	tkconfigure(Env$l.fr5$titre.lab,foreground="black")
	tkconfigure(Env$l.fr5$titre.wdg,state="normal")
	tkconfigure(Env$l.fr5$position.lab,foreground="black")
	tkconfigure(Env$l.fr5$position.wdg,state="readonly")
	tkconfigure(Env$l.fr5$noms.lab1,foreground="black")
	tkconfigure(Env$l.fr5$noms.list,state="normal")
	tkconfigure(Env$l.fr5$noms.lab2,foreground="black")
	tkconfigure(Env$l.fr5$noms.wdg,state="normal")
    }
    if (exists("legende.lab",where=Env$l.fr6)) {
	tkconfigure(Env$l.fr6$legende.lab,foreground="black")
	tkconfigure(Env$l.fr6$legende.wdg,state="normal")
	tkconfigure(Env$l.fr6$titre.lab,foreground="black")
	tkconfigure(Env$l.fr6$titre.wdg,state="normal")
	tkconfigure(Env$l.fr6$position.lab,foreground="black")
	tkconfigure(Env$l.fr6$position.wdg,state="readonly")
	tkconfigure(Env$l.fr6$noms.lab1,foreground="black")
	tkconfigure(Env$l.fr6$noms.list,state="normal")
	tkconfigure(Env$l.fr6$noms.lab2,foreground="black")
	tkconfigure(Env$l.fr6$noms.wdg,state="normal")
    }
  }
}

