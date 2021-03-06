% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_horizon.R
\name{stat_horizon}
\alias{stat_horizon}
\title{Plot a time series as a horizon plot}
\usage{
stat_horizon(mapping = NULL, data = NULL, show.legend = NA,
  inherit.aes = TRUE, na.rm = T, bandwidth = NULL, ...)
}
\description{
A horizon plot breaks the Y dimension down using colours. This is useful
when visualising y values spanning a vast range and / or trying to highlight
outliers without losing context of the rest of the data.\cr \cr Horizon
plots are best viewed in an apsect ratio of very low vertical length.
}
\section{Aesthetics}{
 x, y, fill. Fill argument is overridden internally but
is required for ggplot to assign a colour / fill scale.
}

\section{Cosmetic Tips}{
 The minimalist look can be achieved by appending the
following chunk of code to the output object:
\code{ \cr
+ \cr
xlab('') + \cr
ylab('') + \cr
scale_fill_continuous(low = 'green', high = 'red') + \cr
theme( \cr
   axis.text = element_blank(), \cr
   axis.ticks = element_blank(), \cr
   legend.position = 'none', \cr
   strip.background = element_blank(), \cr
   # strip.text = element_blank(), # useful if only one year of data \cr
   plot.background = element_blank(), \cr
   panel.border = element_blank(), \cr
   panel.background  = element_blank(), \cr
   panel.grid = element_blank(), \cr
   panel.border = element_blank() \cr
) \cr
}
}

\section{Also See}{
 \code{\link{ggplot_horizon}}, a more polished but less
flexible alternative.
}
\examples{
ggplot(data.frame(x = 1:89, y = as.numeric(unlist(austres))), aes(x =x, y = y, fill = y) )+
   stat_horizon() +
   scale_fill_continuous(low = 'white', high = 'red')

set.seed(10)
ggplot(data.frame(x = 1:1000, y = cumsum(rnorm(1000))), aes(x =x, y = y, fill = y) )+
   stat_horizon() +
   scale_fill_continuous(low = 'white', high = 'red')
}

