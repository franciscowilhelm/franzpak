% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreItemsMultiFast.R
\name{scoreItemsMultiFast}
\alias{scoreItemsMultiFast}
\title{Automatically score multiple scales using scoreFast}
\usage{
scoreItemsMultiFast(
  scalenames,
  dataframe,
  manual_keys = NULL,
  exclude = TRUE,
  ...
)
}
\arguments{
\item{scalenames}{character vector of scale names}

\item{dataframe}{dataframe holding the items to be scored}

\item{manual_keys}{Optional. A named list with manual keys for scales, formatted
like in \code{\link[psych:score.items]{psych::scoreItems()}} or \code{\link[psych:score.items]{psych::scoreFast()}}. Items to be reversed
must be prefixed with "-". If provided, keys must be supplied for \emph{all}
scales listed in \code{scalenames}. If \code{NULL} (the default), the function attempts
to find items based on naming convention (items starting with \code{scalename} followed
by \code{.} or \verb{_}) and assumes \strong{no items are reverse-coded}.}

\item{exclude}{Boolean indicating whether to exclude participant responses where
more than 1/3 of a scale's items (as determined by the keys used) are NA.
Defaults to TRUE.}

\item{...}{additional arguments to be passed to \code{\link[psych:score.items]{psych::scoreFast()}}.}
}
\value{
a list object holding:
\item{scores}{A dataframe containing the mean score for each scale.}
\item{negative_index}{A list indicating which items (by position within the
key list for that scale) were treated as reversed. This is based on
the provided \code{manual_keys} if supplied, otherwise all are \code{FALSE}.}
}
\description{
This is a convenience wrapper for \code{\link[psych:score.items]{psych::scoreFast()}} that allows automatically
scoring multiple scales at once. It is designed for speed and for cases
with few observations where PCA or reliability estimation might fail.
Unlike \code{scoreItemsMulti}, this function does \emph{not} calculate Cronbach's alpha
or perform PCA. By default, it assumes no items are reverse-coded unless
\code{manual_keys} are provided.
}
\details{
\code{scoreItemsMultiFast} uses \code{psych::scoreFast} under the hood.

If \code{manual_keys} is provided, it behaves like the original function, requiring
keys for all \code{scalenames} and respecting the "-" prefix for reverse coding.

If \code{manual_keys} is \code{NULL} (default):
\itemize{
\item A \strong{warning} is issued reminding the user that no automatic reverse
coding detection occurs.
\item The function attempts to identify items for each scale by looking for
column names in \code{dataframe} that start with the \code{scalename} followed by
either a period (\code{.}) or an underscore (\verb{_}). For example, for \code{scalename = "myScale"},
it would look for items like \code{myScale.1}, \code{myScale_A}, etc.
\item It assumes \strong{none} of these automatically identified items are reverse-coded.
\item If no items matching the naming convention are found for a given scale, an error occurs.
}

It uses \code{missing = FALSE} (no imputation) and \code{totals = FALSE} (calculates mean scores)
as defaults passed to \code{scoreFast}, but these can be overridden via \code{...}.
}
\examples{
# Create some dummy data
set.seed(123)
mc_items_fast <- data.frame(
 firstscale.1 = sample(1:5, 20, replace = TRUE),
 firstscale.2 = sample(1:5, 20, replace = TRUE),
 firstscale_3r = sample(1:5, 20, replace = TRUE), # conceptually reversed
 secondscale_1 = sample(1:7, 20, replace = TRUE),
 secondscale_2r = sample(1:7, 20, replace = TRUE), # conceptually reversed
 secondscale_3 = sample(1:7, 20, replace = TRUE),
 unrelated_item = rnorm(20)
)
# Add some NAs to test exclude
mc_items_fast[1, 1:2] <- NA # More than 1/3 NA for firstscale for P1 (2 out of 3)
mc_items_fast[2, 4] <- NA   # Less than 1/3 NA for secondscale for P2 (1 out of 3)

scalenames_fast <- c("firstscale", "secondscale")

# --- Example 1: Providing manual keys (required explicit reversal) ---
manual_keys_fast <- list(
  firstscale = c("firstscale.1", "firstscale.2", "-firstscale_3r"),
  secondscale = c("secondscale_1", "-secondscale_2r", "secondscale_3")
)

scored_data_manual <- scoreItemsMultiFast(
  scalenames = scalenames_fast,
  dataframe = mc_items_fast,
  manual_keys = manual_keys_fast,
  exclude = TRUE
)

print("Scores (Manual Keys, exclude = TRUE):")
print(scored_data_manual$scores)
print("Negative Index (Manual Keys):")
print(scored_data_manual$negative_index)

# --- Example 2: Omitting manual keys (automatic item finding, NO reversal) ---
# Note: This will treat firstscale_3r and secondscale_2r as *not* reversed.
scored_data_auto <- scoreItemsMultiFast(
  scalenames = scalenames_fast,
  dataframe = mc_items_fast,
  # manual_keys = NULL, # This is the default
  exclude = TRUE
)

print("Scores (Auto Keys, exclude = TRUE):")
print(scored_data_auto$scores) # Scores will differ from above due to no reversal
print("Negative Index (Auto Keys):")
print(scored_data_auto$negative_index) # Should be all FALSE

# Example without excluding participants with too many NAs (using Auto Keys)
scored_data_auto_noexclude <- scoreItemsMultiFast(
  scalenames = scalenames_fast,
  dataframe = mc_items_fast,
  manual_keys = NULL,
  exclude = FALSE
)
print("Scores (Auto Keys, exclude = FALSE):")
print(scored_data_auto_noexclude$scores)

}
