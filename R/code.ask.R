code.ask <-
function() {
  question<-tkmessageBox(message=Env$voc[240,1],icon="info",type="yesno")
  if (tclvalue(question)=="yes") {
    Env$l.code$save<-TRUE
    Env$l.code$folder<-choose.dir()
    code.open()
  } else {
    Env$l.code$save<-FALSE
  }
}

