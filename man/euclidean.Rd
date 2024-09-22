\name{euclidean}
\alias{euclidean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
euclidean(num1, num2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num1}{
%%     ~~Describe \code{num1} here~~
}
  \item{num2}{
%%     ~~Describe \code{num2} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (num1, num2) 
{
    stopifnot(is.numeric(num1) & length(num1) == 1, is.numeric(num2) & 
        length(num2) == 1)
    num1 <- abs(num1)
    num2 <- abs(num2)
    if (num1 >= num2) {
        a <- num1
        b <- num2
    }
    else {
        b <- num1
        a <- num2
    }
    while (b != 0) {
        temp <- b
        b <- a\%\%b
        a <- temp
    }
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
