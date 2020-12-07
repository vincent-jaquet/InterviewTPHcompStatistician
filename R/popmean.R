#' National prevalence
#'
#' @description
#' Calculate the national prevalence of an intervention type
#' for a range of years.
#'
#' @param data Data frame containing the following columns:
#' zone, population, year, none, IRS, ITN, IRS+ITN
#' @param intervention unquoted expression indicating the intervention.
#' @param years integer vector of the years the prevalence are calculated.
#'
#' @return a data frame (tibble) with two columns: the year and the national
#' prevalence for the inputted intervention.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' popmean(dat, intervention = IRS, years = 2020:2022)
#' popmean(dat, intervention = ITN, years = 2020:2022)

popmean <-
  function(data, intervention, years){

    year <- PR <- population <- NULL

    intervention_quoted <- dplyr::ensym(intervention)

    # Tranform dat into long format (like exercise 1)
    dat2 <- data %>% tidyr::pivot_longer(cols = c("none", "IRS", "ITN", "IRS.ITN"),
                                         names_to = "intervention", values_to = "PR")

    results <- dat2 %>%
      dplyr::filter(intervention == dplyr::sym(intervention_quoted),
                    year %in% years) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        weighted_mean = stats::weighted.mean(PR, population), .groups = 'drop')

    return(results)
  }
