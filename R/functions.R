#' ExtractPatients from a connection
#'
#' @param connection connection to the Common Data Model (CDM)
#'
#' @return data.frame with count of condition concepts ID over year and month.
#' @export

extractPatients <- function(connection) {
  sql <- "SELECT COUNT(*) AS CONDITION_COUNT, CONCEPT_ID, CONCEPT_NAME, YEAR(CONDITION_START_DATE) AS YEAR, MONTH(CONDITION_START_DATE) AS MONTH
           FROM @cdm.condition_occurrence
           INNER JOIN @cdm.concept
           ON @cdm.condition_occurrence.CONDITION_CONCEPT_ID =  @cdm.concept.CONCEPT_ID
           GROUP BY CONCEPT_ID, CONCEPT_NAME,YEAR, MONTH;"
  total <- DatabaseConnector::renderTranslateQuerySql(connection, sql, cdm = "main")
}


#' Plot of condition counts
#'
#' @param data data.frame with count of condition concepts ID over year and month.
#' @param byMonth Boolean display the counts by month?
#'
#' @return ggplotly with count of conditions by month or by year
#' @export

plotTrend <- function(data, byMonth = FALSE) {
  # Binding the variable locally to the function
  CONCEPT_ID <- CONCEPT_NAME <- YEAR <- CONDITION_COUNT <- MONTH <-DATE <- NULL

  # Set a color for each condition in the dataset
  n <- length(unique(data$CONCEPT_ID))
  col_total = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  set.seed(1234)
  color_vector = base::sample(col_total, n)


  if (!byMonth) {
    dataPlot = data %>%
                dplyr::group_by(CONCEPT_ID, CONCEPT_NAME, YEAR) %>%
                dplyr::summarise(CONDITION_COUNT = sum(CONDITION_COUNT))

    p0 <- ggplot2::ggplot(data = dataPlot,
                          ggplot2::aes(
                           x = YEAR,
                           y = CONDITION_COUNT,
                           group = CONCEPT_NAME,
                           text = paste(
                             'CONCEPT_ID:',
                             CONCEPT_ID,
                             '<br>CONCEPT_NAME:',
                             CONCEPT_NAME,
                             '<br>Year: ',
                             YEAR,
                             '<br>Obs: ',
                             CONDITION_COUNT
                           )
                         )) +
      ggplot2::geom_line(ggplot2::aes(color = CONCEPT_NAME)) +
      ggplot2::geom_point(ggplot2::aes(fill = CONCEPT_NAME),
                 color = "black",
                 size = 0.5) +
      ggplot2::labs(title = "Condition count per year", x = "Year", y = "Condition count") +
      ggplot2::xlim(min(dataPlot$YEAR), max(dataPlot$YEAR)) +
      ggplot2::ylim(0, max(dataPlot$CONDITION_COUNT))

  } else{
    dataPlot = data %>%
      dplyr::mutate(DATE = base::as.Date(paste(
        YEAR, ifelse(MONTH < 10, paste0("0", MONTH), MONTH), "01", sep = "-"
      )))

    p0 <- ggplot2::ggplot(data = dataPlot,
                          ggplot2::aes(
                             x = DATE,
                             y = CONDITION_COUNT,
                             group = CONCEPT_NAME,
                             text = paste(
                               'CONCEPT_ID:',
                               CONCEPT_ID,
                               '<br>CONCEPT_NAME:',
                               CONCEPT_NAME,
                               '<br>Date: ',
                               format(DATE, "%b-%Y"),
                               '<br>Obs: ',
                               CONDITION_COUNT
                             )
                           )) +
      ggplot2::geom_line(ggplot2::aes(color = CONCEPT_NAME)) +
      ggplot2::geom_point(ggplot2::aes(fill = CONCEPT_NAME),
                 color = "black",
                 size = 0.5) +
      ggplot2::labs(title = "Condition count per month", x = "Year", y = "Condition count") +
      ggplot2::scale_x_date(limit = c(min(dataPlot$DATE), max(dataPlot$DATE))) +
      ggplot2::ylim(0, max(dataPlot$CONDITION_COUNT))
  }

  p <- p0 +
    ggplot2::theme_minimal()+
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_fill_manual(values = color_vector) +
    ggplot2::scale_colour_manual(values = color_vector)

  plotly::ggplotly(p , tooltip = c("text"))


}
