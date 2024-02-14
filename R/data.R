#' Monte Carlo-simulated two-level dataset
#'
#' A dataset generated through monte carlo simulation. See Mplus 8 example 12.11.
#'
#' @format ## `mc_twolevel`
#' A data frame with 1,000 rows and 4 columns:
#' \describe{
#'   \item{Y}{dependent variable}
#'   \item{X, M}{independent and mediating variable}
#'   \item{CLUSTER}{gives the cluster ID}
#' }
#' @source <https://www.statmodel.com/usersguide/chapter12.shtml>
"mc_twolevel"

#' Monte Carlo-simulated dataset with continuous factor indicators
#'
#' A dataset generated through monte carlo simulation. See Mplus 8 example 12.1.
#'
#' @format ## `mc_items`
#' A data frame with 500 rows and 10 columns:
#' \describe{
#'   \item{firstscale_1, firstscale_2, firstscale_3, firstscale_4, firstscale_5}{indicators of first factor}
#'   \item{secondscale_1, secondscale_2, secondscale_3, secondscale_4, secondscale_5}{indicators of second factor, with secondscale_5 being negatively related to factor.}
#' }
#' @source <https://www.statmodel.com/usersguide/chapter12.shtml>
"mc_items"
