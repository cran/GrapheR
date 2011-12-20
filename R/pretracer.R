pretracer <-
function() {
  test<-tracer()
  if (test==TRUE) {
    if (Env$l.code$ask==FALSE) {
	Env$l.code$ask<-TRUE
	code.ask()
    } else {
	if (Env$l.code$save==TRUE) {
	  code.open()
	}
    }
  }
}

