\name{run.GrapheR}
\alias{run.GrapheR}
\title{
Run GrapheR
}
\description{
The function launches GrapheR. Use it after closing the interface to re-launch it.
}
\usage{
run.GrapheR(lang = NULL, path.to.save = NULL, figurej = FALSE)
}
\arguments{
  \item{lang}{
language, when interface relaunching.
}
  \item{path.to.save}{
if not \code{NULL}, code is automatically generated and save when drawing a graph, without asking any question. The file is created in the given folder.
}
  \item{figurej}{
if \code{TRYE}, allow to choose the "FigureJ" option when saving a graph. This will save the graph (and the associated code) in a format  adapted for the pulgin FigureJ of the Software ImageJ.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}