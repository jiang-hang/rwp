% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rwordpress.R
\name{embed_png}
\alias{embed_png}
\title{embed png in the markdown}
\usage{
embed_png(path, dpi = 200)
}
\arguments{
\item{path}{value}

\item{dpi}{value}
}
\value{
returndes
}
\description{
work for both http and local image , 
for html , the image
  if path started with http, it will be put at the img tag directly
}
\details{
otherwise, path should have the "rfigures" in it and the png file 
  should be existed in http://www.bagualu.net/wordpress/rfigures/
  it will not copy the files there , so you need to do it manually

for pdf , both http and local img are supported, local image does 
  not need to be on the website
}
\examples{
x=c(1,2,3) 
}

