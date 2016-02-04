#' Transforms data for a horizon plot
StatHorizon <- ggproto(
   "StatHorizon",
   Stat,
   required_aes = c("x", "y"),
   setup_params = function(data, params) {

      # calculating a default bandwidth
      if (is.null(params$bandwidth)) {
         params$bandwidth = diff(range(data$y)) / 4
      }

     params

  },
  compute_group = function(data, scales, bandwidth) {

      # finding the lowest value. All horizon band y variables will need to
      # be offset by this value
      nMinY = min(data$y, na.rm = T)

      # calculating the band in which the values fall
      data$fill = ((data$y - nMinY) %/% bandwidth) + 1

      # calculating the banded y value
      data$y = data$y - (bandwidth * (data$fill - 1)) - nMinY



      # adding the band extending to the full bandwidth for the bandwidth - 1th
      # band.
      data2 = data
      data2$y[data2$fill > 1] = bandwidth
      data2$y[!data2$fill > 1] = 0
      data2$fill = data2$fill - 1

      # combining
      data = rbind(data2,data)
      data$fill = (data$fill * bandwidth) + nMinY
      # data$fill = (data$fill * bandwidth) + 1

      data

  }
)


#' Plot a time series as a horizon plot
#'
#' A horizon plot breaks the Y dimension down using colours. This is useful
#' when visualising y values spanning a vast range and / or trying to highlight
#' outliers without losing context of the rest of the data.\cr \cr Horizon
#' plots are best viewed in an apsect ratio of very low vertical length.
#'
#' @section Aesthetics: x, y, fill. Fill argument is overridden internally but
#' is required for ggplot to assign a colour / fill scale.
#' @section Cosmetic Tips: The minimalist look can be achieved by appending the
#' following chunk of code to the output object:
#' \code{ \cr
#' + \cr
#' xlab('') + \cr
#' ylab('') + \cr
#' scale_fill_continuous(low = 'green', high = 'red') + \cr
#' theme( \cr
#'    axis.text = element_blank(), \cr
#'    axis.ticks = element_blank(), \cr
#'    legend.position = 'none', \cr
#'    strip.background = element_blank(), \cr
#'    # strip.text = element_blank(), # useful if only one year of data \cr
#'    plot.background = element_blank(), \cr
#'    panel.border = element_blank(), \cr
#'    panel.background  = element_blank(), \cr
#'    panel.grid = element_blank(), \cr
#'    panel.border = element_blank() \cr
#' ) \cr
#' }
#' @section Also See: \code{\link{ggplot_horizon}}, a more polished but less
#' flexible alternative.
#' @export
#' @import ggplot2
#' @examples
#' ggplot(data.frame(x = 1:89, y = as.numeric(unlist(austres))), aes(x =x, y = y, fill = y) )+
#'    stat_horizon() +
#'    scale_fill_continuous(low = 'white', high = 'red')
#'
#' set.seed(10)
#' ggplot(data.frame(x = 1:1000, y = cumsum(rnorm(1000))), aes(x =x, y = y, fill = y) )+
#'    stat_horizon() +
#'    scale_fill_continuous(low = 'white', high = 'red')
stat_horizon = function(
   mapping = NULL,
   data = NULL,
   show.legend = NA,
   inherit.aes = TRUE,
   na.rm = T,
   bandwidth = NULL,
   ...
) {

   list(
      layer(
         stat = StatHorizon, data = data, mapping = mapping, geom = 'bar',
         position = 'identity', show.legend = show.legend, inherit.aes = inherit.aes,
         params = list(bandwidth = bandwidth, na.rm = na.rm, ...)
      )
   )

}

