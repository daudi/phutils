##' Life expectancy calculator


##' @details  Adapted from SEPHO Life expectancy calculator spreadsheet
##'   downloaded from http://www.sepho.org.uk/viewResource.aspx?id=8943 For the
##'   methodology used in this spreadsheet, along with a review of the available
##'   options for life expectancy calculation for small areas, see: Eayres DP,
##'   Williams ES, Evaluation of methodologies for small area life expectancy
##'   estimation, J Epidemiol Community Health 2004;58:243-249. Note that this function
##'   currently requires you to use the specific age groups in the SEPHO spreadsheet;
##'   it could be adapted to become more generic.

##' @param x Dataframe containing age-bands, deaths and population. There should
##'   be 19 rows (one for each age-band) and at least 3 columns.
##' @param deaths The name of the variable in x containing a numeric vector of 
##'   deaths
##' @param population The name of the variable in x containing a numeric vector 
##'   of population counts
##' @param age.bands The name of the variable in x containing a vector of 
##'   age-bands. There should be 19 in total beginning "0, 1-4, 5-9, 10-14, ... 
##'   85+".
##' @param output Default is "simple" which trims number of variables in output 
##'   dataframe. Alternative is "full" which shows all the variables involved in
##'   LE calculations.
##' @param LE.age Default is NULL which selects all rows from output dataframe. 
##'   Specifying a vector of one or more integers will return a dataframe of 
##'   Life expectancies at those ages rounded to the nearest 5.
##' @author Mark Chambers mark.chambers@@nhs.net
##' @references http://www.sepho.org.uk/viewResource.aspx?id=8943
##' @references Eayres DP, Williams ES, Evaluation of methodologies for small
##'   area life expectancy estimation, J Epidemiol Community Health
##'   2004;58:243-249.
##' @export
##' @return A data.frame with one row per age group requested in the output (see LE.age)
##' @examples 
##' \dontrun{
##' # # Sample data
##' test <- readRDS("V:/33 - Pembroke Court/Public Health Intelligence/Tools and Tutorials/R/dev/life-expectancy-example.Rds")

##' # Life expectancy at birth only
##' LE(x = test, deaths = "Deaths", population = "pop", age.bands = "age.band", output = "simple", LE.age = 0)

##' # Life expectancy at 65 and 85
##' LE(x = test, deaths = "Deaths", population = "pop", age.bands = "age.band", output = "simple", LE.age = c(65, 85))
##' }
##' 
##' 


LE <- function(x, deaths, population, age.bands, output = c("simple", "full")[1], LE.age = NULL) {
  
  # Error trapping
  # stopifnot(class(x) == "data.frame")
  # stopifnot(nrow(x) == 19)
  # stopifnot(ncol(x) >= 3)
  # stopifnot(class(x[, deaths]) == "numeric")
  # stopifnot(class(x[, population]) == "numeric")
  
  # Simplify data frame
  x <- x[, c(age.bands, deaths, population)]
  names(x) <- c("age.bands", "deaths", "population")
  
  # Order age bands
  x <- x[order(x$age.band), ]  
  
  # Death rate in interval
  x$death.rate <- x$deaths / x$population
  
  # Set interval widths
  x$interval.width <- c(1, 4, rep(5, 16), 2/x$death.rate[19])
  
  # Set fraction of last age interval of life
  x$fraction.last.age <- c(0.1, rep(0.5, 18))
  
  # Probability of dying in interval
  x$prob.death <- (x$interval.width * x$death.rate) / (1 + x$interval.width * (1 - x$fraction.last.age) * x$death.rate)
  x$prob.death[x$deaths > (x$population / (x$interval.width * x$fraction.last.age))] <- 1
  x$prob.death[19] <- 1
  
  # Probability surviving interval
  x$prob.survival <- 1 - x$prob.death
  
  # Number alive at start of interval
  x$alive <- rep(NA, nrow(x))
  x$alive[1] <- 100000
  
  for(i in 2:19) {
    x$alive[i] <- x$prob.survival[i - 1] * x$alive[i - 1]
  }
  
  # Number dying in interval
  x$dying <- rep(NA, nrow(x))
  
  for(i in 1:18) {
    x$dying[i] <- x$alive[i] - x$alive[i + 1]
  }
  
  x$dying[19] <- x$alive[19]
  
  # Number of years lived in interval
  x$years.lived <- rep(NA, nrow(x))
  
  for(i in 1:18) {
    x$years.lived[i] <- x$interval.width[i] * (x$alive[i + 1] + (x$fraction.last.age[i] * x$dying[i]))
  }
  x$years.lived[19] <- x$alive[19] / x$death.rate[19]
  
  # Total years lived beyond start of interval
  x$years.lived.beyond <- rep(NA, nrow(x))
  x$years.lived.beyond[19] <- x$years.lived[19]
  
  for(i in 18:1) {
    x$years.lived.beyond[i] <- x$years.lived[i] + x$years.lived.beyond[i + 1]
  }
  
  # Life expectancy at each interval
  x$LE <- x$years.lived.beyond / x$alive
  x$LE[x$alive == 0] <- 0
  
  # Sample Variance Of Proportion Surviving In Interval
  x$var.survival <- (x$prob.death^2 * (1 - x$prob.death)) / x$deaths
  x$var.survival[x$deaths == 0] <- 0
  x$var.survival[19] <- (x$death.rate[19] * (1 - x$death.rate[19])) / x$population[19]
  
  # Weighted Variance Of Proportion Surviving In Interval
  x$var.weighted <- rep(NA, nrow(x))
  
  for(i in 1:18) {
    x$var.weighted[i] <- (x$alive[i]^2) * (((1 - x$fraction.last.age[i]) * x$interval.width[i] + x$LE[i + 1])^2) * x$var.survival[i]
  }
  
  x$var.weighted[19] <- (x$alive[19]^2 / x$death.rate[19]^4) * x$var.survival[19]
  
  # Sample Variance Of Total Number Of Years Lived Beyond Start Of Interval
  x$var.years.lived.beyond <- rep(NA, nrow(x))
  
  x$var.years.lived.beyond[19] <- x$var.weighted[19]
  
  for(i in 18:1) {
    x$var.years.lived.beyond[i] <- x$var.weighted[i] + x$var.years.lived.beyond[i + 1]
  }
  
  # Sample Variance of Observed Expectation Of Life At Start Of Interval
  x$var.LE <- x$var.years.lived.beyond / x$alive^2
  
  # Sample Std Err. of Observed Expectation Of Life At Start Of Interval
  x$se.LE <- sqrt(x$var.LE)
  
  # Lower 95% Confidence Interval 
  x$LL95 <- x$LE - (1.96 * x$se.LE)
  
  # Upper 95% Confidence Interval 
  x$UL95 <- x$LE + (1.96 * x$se.LE)
  
  # Add variable for Life expectancy at different ages
  x$LE.age <- as.numeric(gsub("(.*)[-\\+].*", "\\1", x$age.band))
  
  # Trim x according to chosen parameters
  if(output == "simple") {
    x <- x[, c("LE.age", "LE", "LL95", "UL95")]
  }
  
  if(!is.null(LE.age)) {
    LE.age.rounded <- 5 * round(LE.age / 5)
    chosen.ages <- x$LE.age %in% LE.age.rounded
    x <- x[chosen.ages, ]
  }
  
  
  # Print table
  x
  
}

