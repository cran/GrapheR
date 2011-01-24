back <-
function(fen) {
  if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar);assign("toolbar.GrapheR",0,pos=Env)}
  tkdestroy(fen)
  tkwm.deiconify(Env$Fen4)
  tkfocus(Env$Fen4)
}

