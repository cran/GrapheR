fr7.close <-
function() {
  for (i in 1:length(Env$l.fr7)) {tkdestroy(Env$l.fr7[[i]])}
  Env$l.fr7<-list()
  Env$l.fr7$vide<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  tkgrid(Env$l.fr7$vide)
  tkconfigure(Env$l.frames$Fr7,borderwidth=0)
  reinit.fr7()
}

