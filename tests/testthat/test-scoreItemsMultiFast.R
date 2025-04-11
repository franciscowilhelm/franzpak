
# --- Example Usage from Docstring ---
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

# Will use manual keys and reverse specified items
scored_data_manual <- scoreItemsMultiFast(
  scalenames = scalenames_fast,
  dataframe = mc_items_fast,
  manual_keys = manual_keys_fast,
  exclude = TRUE
)

print("--- Results with Manual Keys ---")
print("Scores (Manual Keys, exclude = TRUE):")
print(scored_data_manual$scores)
# Expected: P1 firstscale score = NA due to exclusion
# Expected: other scores calculated with reversals applied
print("Negative Index (Manual Keys):")
print(scored_data_manual$negative_index)
# Expected: firstscale = c(F, F, T), secondscale = c(F, T, F)

# --- Example 2: Omitting manual keys (automatic item finding, NO reversal) ---
# Will issue a warning. Will find items starting firstscale., firstscale_, secondscale., secondscale_
# Will NOT reverse firstscale_3r or secondscale_2r
scored_data_auto <- scoreItemsMultiFast(
  scalenames = scalenames_fast,
  dataframe = mc_items_fast,
  # manual_keys = NULL, # This is the default
  exclude = TRUE
)

print("--- Results with Auto Keys (Default) ---")
print("Scores (Auto Keys, exclude = TRUE):")
print(scored_data_auto$scores) # Scores will differ from above due to no reversal
# Expected: P1 firstscale score = NA due to exclusion (based on 3 items found)
# Expected: other scores calculated with NO reversals
print("Negative Index (Auto Keys):")
print(scored_data_auto$negative_index) # Should be all FALSE for both scales

# Example without excluding participants with too many NAs (using Auto Keys)
scored_data_auto_noexclude <- scoreItemsMultiFast(
  scalenames = scalenames_fast,
  dataframe = mc_items_fast,
  manual_keys = NULL,
  exclude = FALSE
)
print("Scores (Auto Keys, exclude = FALSE):")
print(scored_data_auto_noexclude$scores)
# Expected: P1 firstscale score calculated based on the single non-NA item (firstscale_3r), NOT reversed.

