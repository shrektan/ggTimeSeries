#' Plots a calendar heatmap
#'
#' A calendar heatmap provides context for weeks, and day of week which makes
#' it a better way to visualise daily data than line charts. Largely uses
#' Codoremifa's code from
#' stackoverflow.com/questions/22815688/calendar-time-series-with-r.
#'
#'
#' @param dtDateValue Data set which may include other columns apart from date
#' and values.
#' @param cDateColumnName Column name of the dates.
#' @param cValueColumnName Column name of the data.
#' @param vcGroupingColumnNames The set of columns which together define the group
#' for the chart to operate within If you plan to facet your plot,
#' you should specify the same column names to this argument. The function
#' will automatically add the veriable for the year to the facet.
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
#' @section Also See: \code{\link{stat_calendar_heatmap}}, a
#' flexible but less polished alternative.
#' @return Returns a gpplot friendly object which means the user can use
#' ggplot scales to modify the look, add more geoms, etc.
#' @import data.table
#' @import ggplot2
#' @export
#' @examples
#' set.seed(1)
#' dtData = data.table(
#'       DateCol = seq(
#'          as.Date("1/01/2014", "%d/%m/%Y"),
#'          as.Date("31/12/2015", "%d/%m/%Y"),
#'          "days"
#'       ),
#'       ValueCol = runif(730)
#'    )
#' # you could also try categorical data with
#' # ValueCol = sample(c('a','b','c'), 730, replace = T)
#' p1 = ggplot_calendar_heatmap(
#'    dtData,
#'    'DateCol',
#'    'ValueCol'
#' )
#' p1
#' # add new geoms
#' p1 +
#' geom_text(label = '!!!') +
#' scale_colour_continuous(low = 'red', high = 'green')
ggplot_calendar_heatmap = function(
   dtDateValue,
   cDateColumnName = '',
   cValueColumnName = '',
   vcGroupingColumnNames = 'Year'
) {

   setDT(dtDateValue)
   dtDateValue[, Year := as.integer(strftime(get(cDateColumnName), '%Y'))]
   vcGroupingColumnNames = unique(c(vcGroupingColumnNames, 'Year'))

   # ensuring that there is an entry for each date for each group of
   # columns that the user has specified
   dtDateValue = merge(
      dtDateValue,
      setnames(
         dtDateValue[,
            list(
               DateCol = seq(
                  min(get(cDateColumnName)),
                  max(get(cDateColumnName)),
                  'days'
               )
            ),
            vcGroupingColumnNames
         ],
         'DateCol',
         cDateColumnName
      ),
      c(vcGroupingColumnNames,cDateColumnName),
      all = T
   )

   # Pre-processing ============================================================

   dtDateValue[, MonthOfYear := as.integer(strftime(get(cDateColumnName), '%m'))]
   dtDateValue[, WeekOfYear := 1 + as.integer(strftime(get(cDateColumnName), '%W'))]
   dtDateValue[, DayOfWeek := as.integer(strftime(get(cDateColumnName), '%w'))]
   dtDateValue[DayOfWeek == 0L, DayOfWeek := 7L]


   # Heatmap-ish layout to chalk out the blocks of colour on dates =============
   ggplotcalendar_heatmap <- ggplot(
      data = dtDateValue[,list(WeekOfYear, DayOfWeek)],
      aes(
         x = WeekOfYear,
         y = DayOfWeek
      )
   ) +
      geom_tile(
         data = dtDateValue,
         aes_string(
            fill = cValueColumnName
         ),
         color = 'black'
      ) +
      coord_fixed() +
      xlab('Month') +
      ylab('DoW') +
      facet_wrap(as.formula(paste("~", paste(vcGroupingColumnNames, collapse = '+'))))


   # adding borders for change of month ========================================
   # vertical borders ( Between a week ) ----------------------------------------
   setkeyv(dtDateValue,c(vcGroupingColumnNames, "DayOfWeek", "WeekOfYear", "MonthOfYear"))
   dtDateValue[, MonthChange := c(1, diff(MonthOfYear)), c(vcGroupingColumnNames, 'DayOfWeek')]
   dtMonthChangeDatasetBetweenWeeks = dtDateValue[MonthChange == 1]
   dtMonthChangeDatasetBetweenWeeks[, WeekOfYear := WeekOfYear - 0.5]
   dtMonthChangeDatasetBetweenWeeks = rbind(
      dtMonthChangeDatasetBetweenWeeks[, c('DayOfWeek', 'WeekOfYear', vcGroupingColumnNames), with = F],
      dtDateValue[, list(WeekOfYear = 0.5 + max(WeekOfYear)), c(vcGroupingColumnNames, 'DayOfWeek')]
   )
   if ( nrow(dtMonthChangeDatasetBetweenWeeks) > 0 ) {
      ggplotcalendar_heatmap <- ggplotcalendar_heatmap +
         geom_segment(
            data = dtMonthChangeDatasetBetweenWeeks,
            aes(
               x = WeekOfYear,
               xend = WeekOfYear,
               y = DayOfWeek - 0.5,
               yend = DayOfWeek + 0.5
            ),
            size = 2
         )
   }


   # horizontal borders ( within a week ) --------------------------------------
   setkeyv(dtDateValue, c(vcGroupingColumnNames,"WeekOfYear","DayOfWeek","MonthOfYear"))
   dtDateValue[, MonthChange := c(1,diff(MonthOfYear)), vcGroupingColumnNames]
   MonthChangeDatasetWithinWeek = dtDateValue[MonthChange==1 & (DayOfWeek != 1)]
   MonthChangeDatasetWithinWeek[, DayOfWeek := DayOfWeek - 0.5]
   MonthChangeDatasetWithinWeek = rbind(
      MonthChangeDatasetWithinWeek[, c('DayOfWeek', 'WeekOfYear', vcGroupingColumnNames), with = F],
      dtDateValue[,list(DayOfWeek = c(min(DayOfWeek) - 0.5, max(DayOfWeek) + 0.5)), c(vcGroupingColumnNames, 'WeekOfYear')]
   )
   if ( nrow(MonthChangeDatasetWithinWeek) > 0 ) {
      ggplotcalendar_heatmap <- ggplotcalendar_heatmap +
         geom_segment(
            data = MonthChangeDatasetWithinWeek,
            aes(
               x = WeekOfYear - 0.5,
               xend = WeekOfYear + 0.5,
               y = DayOfWeek,
               yend = DayOfWeek
            ),
            size = 2
         )
   }

   # adding axis labels and ordering Y axis Mon-Sun ============================
   dtMonthLabels <- dtDateValue[,
      list(meanWeekOfYear = mean(WeekOfYear)),
      by = c('MonthOfYear')
   ]
   dtMonthLabels[, MonthOfYear := month.abb[MonthOfYear]]
   ggplotcalendar_heatmap = ggplotcalendar_heatmap +
      scale_x_continuous(
         breaks = dtMonthLabels[,meanWeekOfYear],
         labels = dtMonthLabels[, MonthOfYear],
         expand = c(0, 0)
      ) +
      scale_y_continuous(
         trans = 'reverse',
         breaks = c(1:7),
         labels = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun'),
         expand = c(0, 0)
      )

   return (ggplotcalendar_heatmap)
}
