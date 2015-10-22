\name{nparACT_base_loop}
\alias{nparACT_base_loop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Classic Nonparametric Actigraphy Measures for Multiple Actigraphy Files
}
\description{
Function returns IS, IV, RA, L5, M10 as well as L5 and M10 start times for multiple actigraphy files. The function also returns a plot for hourly grand averages across 24 hours. 
}
\usage{
nparACT_base_loop(path, SR, cutoff = 1, plot = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
Character string containing path to location of actigraphy files. Actigraphy files must contain one column with actigraphy values and one column with a date/time vector,  which must be character strings of form \code{YYYY-MM-DD HH:MM:SS} or \code{HH:MM:SS}.
}
  \item{SR}{
Sampling rate in Hz.
}
  \item{cutoff}{
Can be used to define a cutoff for actigraphy data. Default value is \code{1}, i.e. all values are taken into account.
}
  \item{plot}{
If \code{TRUE} a plot is produced. Default is \code{TRUE}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Data frame containing IS, IV and RA values for each file. Besides this, L5 and M10 values are given along with the respective start times.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Christine Blume <christine.blume@sbg.ac.at>
Nayantara Santhi <n.santhi@surrey.ac.uk>
Manuel Schabus <manuel.schabus@sbg.ac.at>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# example function nparACT_loop
data(sleepstudy)
wd <- getwd()
name <- "sleepstudy_example"
newdir <- paste(wd,name, sep="/")
dir.create(newdir)
setwd(newdir)
write.table(sleepstudy, file = "sleepstudy.txt", row.names=FALSE, col.names = FALSE)
r <- nparACT_base_loop(newdir, SR = 4/60)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line