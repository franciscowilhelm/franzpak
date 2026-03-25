# =============================================================================
# Monte Carlo Simulation: Latent Moderated Structural Equations (LMS)
# for Polynomial Congruence Models
# =============================================================================
#
# Model: Z = b1*X + b2*Y + b3*X² + b4*XY + b5*Y²
# Where X, Y, Z are latent factors each with 3 indicators
#
# Congruence surface parameters:
#   Congruence slope (cs)     = b1 + b2
#   Congruence curvature (cc) = b3 + b4 + b5
#   Incongruence slope (is)   = b1 - b2
#   Incongruence curvature (ic) = b3 - b4 + b5
#
# We use the product-indicator approach (matched pairs) as the lavaan-native
# method for latent interactions, since lavaan does not natively support LMS.
# For true LMS, one would use Mplus or the nlsem package.
# Here we provide both: (A) a product-indicator approach in lavaan, and
# (B) a direct simulation + known-parameter recovery check.
# =============================================================================

library(lavaan)
library(MASS)     # mvrnorm

# ---- 1. Simulation Parameters ------------------------------------------------

set.seed(2026)

n_reps    <- 500    # number of MC replications
n_obs     <- 500    # sample size per replication

# -- True population parameters --

# Factor loadings (first indicator fixed to 1 for identification)
lambda_X <- c(1, 0.9, 0.85)
lambda_Y <- c(1, 0.95, 0.8)
lambda_Z <- c(1, 0.85, 0.9)

# Residual variances for indicators
theta_X <- c(0.3, 0.35, 0.4)
theta_Y <- c(0.3, 0.30, 0.35)
theta_Z <- c(0.3, 0.35, 0.30)

# Structural coefficients: Z = b1*X + b2*Y + b3*X² + b4*XY + b5*Y² + disturbance
b1 <- 0.30   # X -> Z
b2 <- 0.25   # Y -> Z
b3 <- -0.10  # X² -> Z   (quadratic X)
b4 <- 0.15   # XY -> Z   (interaction)
b5 <- -0.08  # Y² -> Z   (quadratic Y)

# Implied congruence surface parameters (true values):
cs_true <- b1 + b2            # congruence slope
cc_true <- b3 + b4 + b5       # congruence curvature
is_true <- b1 - b2            # incongruence slope
ic_true <- b3 - b4 + b5       # incongruence curvature

cat("=== True Population Parameters ===\n")
cat(sprintf("b1 = %.2f, b2 = %.2f, b3 = %.2f, b4 = %.2f, b5 = %.2f\n",
            b1, b2, b3, b4, b5))
cat(sprintf("Congruence slope (cs)      = %.2f\n", cs_true))
cat(sprintf("Congruence curvature (cc)  = %.2f\n", cc_true))
cat(sprintf("Incongruence slope (is)    = %.2f\n", is_true))
cat(sprintf("Incongruence curvature (ic)= %.2f\n", ic_true))
cat("\n")

# Latent factor means and covariance (X and Y are exogenous, correlated)
mu_XY     <- c(0, 0)
phi_X     <- 1.0    # variance of X
phi_Y     <- 1.0    # variance of Y
rho_XY    <- 0.40   # correlation between X and Y (positive, as requested)
Phi_XY    <- matrix(c(phi_X,          rho_XY * sqrt(phi_X * phi_Y),
                       rho_XY * sqrt(phi_X * phi_Y), phi_Y),
                     nrow = 2)

# Disturbance variance of Z
psi_Z <- 0.50


# ---- 2. Data-Generating Function --------------------------------------------

generate_data <- function(n) {
  # Draw latent exogenous factors
  eta_XY <- mvrnorm(n, mu = mu_XY, Sigma = Phi_XY)
  eta_X  <- eta_XY[, 1]
  eta_Y  <- eta_XY[, 2]

  # Structural equation for Z (with nonlinear terms)
  disturbance_Z <- rnorm(n, 0, sqrt(psi_Z))
  eta_Z <- b1 * eta_X + b2 * eta_Y +
            b3 * eta_X^2 + b4 * eta_X * eta_Y + b5 * eta_Y^2 +
            disturbance_Z

  # Generate indicators
  # X indicators
  x1 <- lambda_X[1] * eta_X + rnorm(n, 0, sqrt(theta_X[1]))
  x2 <- lambda_X[2] * eta_X + rnorm(n, 0, sqrt(theta_X[2]))
  x3 <- lambda_X[3] * eta_X + rnorm(n, 0, sqrt(theta_X[3]))

  # Y indicators
  y1 <- lambda_Y[1] * eta_Y + rnorm(n, 0, sqrt(theta_Y[1]))
  y2 <- lambda_Y[2] * eta_Y + rnorm(n, 0, sqrt(theta_Y[2]))
  y3 <- lambda_Y[3] * eta_Y + rnorm(n, 0, sqrt(theta_Y[3]))

  # Z indicators
  z1 <- lambda_Z[1] * eta_Z + rnorm(n, 0, sqrt(theta_Z[1]))
  z2 <- lambda_Z[2] * eta_Z + rnorm(n, 0, sqrt(theta_Z[2]))
  z3 <- lambda_Z[3] * eta_Z + rnorm(n, 0, sqrt(theta_Z[3]))

  data.frame(x1, x2, x3, y1, y2, y3, z1, z2, z3)
}

write_mplus_example_files <- function(
  n = n_obs,
  seed = 2026L,
  output_dir = "inst/extdata",
  base_name = "congruence_sim"
) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  old_seed <- .Random.seed
  on.exit({
    if (exists("old_seed", inherits = FALSE)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  dat <- generate_data(n)

  data_path <- file.path(output_dir, paste0(base_name, ".dat"))
  inp_path <- file.path(output_dir, paste0(base_name, ".inp"))

  utils::write.table(
    dat,
    file = data_path,
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE,
    sep = " ",
    na = "."
  )

  inp_lines <- c(
    "TITLE:",
    "  Simulated latent congruence model for RSA_mplus examples",
    "",
    "DATA:",
    sprintf('  FILE = "%s";', basename(data_path)),
    "",
    "VARIABLE:",
    "  NAMES =",
    "    x1 x2 x3",
    "    y1 y2 y3",
    "    z1 z2 z3;",
    "",
    "  USEVARIABLES =",
    "    x1 x2 x3",
    "    y1 y2 y3",
    "    z1 z2 z3;",
    "",
    "  MISSING = .;",
    "",
    "ANALYSIS:",
    "  TYPE = RANDOM;",
    "  ALGORITHM = INTEGRATION;",
    "  PROCESSORS = 4;",
    "",
    "MODEL:",
    "  X BY x1 x2 x3;",
    "  Y BY y1 y2 y3;",
    "  Z BY z1 z2 z3;",
    "",
    "  XY | X XWITH Y;",
    "  XS | X XWITH X;",
    "  YS | Y XWITH Y;",
    "",
    "  Z ON X  (b1)",
    "       Y  (b2)",
    "       XS (b3)",
    "       XY (b4)",
    "       YS (b5);",
    "",
    "MODEL CONSTRAINT:",
    "  NEW(cs cc is ic a5",
    "      x0 y0 p10 p11 p20 p21);",
    "",
    "  cs  = b1 + b2;",
    "  cc  = b3 + b4 + b5;",
    "  is  = b1 - b2;",
    "  ic  = b3 - b4 + b5;",
    "  a5  = b3 - b5;",
    "",
    "  x0  = (b2*b4 - 2*b1*b5) / (4*b3*b5 - b4*b4);",
    "  y0  = (b1*b4 - 2*b2*b3) / (4*b3*b5 - b4*b4);",
    "  p11 = (b5 - b3 + SQRT((b3 - b5)*(b3 - b5) + b4*b4)) / b4;",
    "  p21 = (b5 - b3 - SQRT((b3 - b5)*(b3 - b5) + b4*b4)) / b4;",
    "  p10 = y0 - p11*x0;",
    "  p20 = y0 - p21*x0;",
    "",
    "OUTPUT:",
    "  STDYX CINTERVAL TECH1;"
  )

  writeLines(inp_lines, inp_path)

  invisible(list(
    data = dat,
    data_path = data_path,
    inp_path = inp_path
  ))
}


# ---- 3. Product Indicator Approach (Matched Pairs) in lavaan ----------------
#
# We use the "matched-pair" strategy (Marsh, Wen & Hau, 2004):
#   - Mean-center all indicators before forming products
#   - Form matched products: x1*y1, x2*y2, x3*y3 for XY
#   - Form matched products: x1*x1, x2*x2, x3*x3 for X²
#   - Form matched products: y1*y1, y2*y2, y3*y3 for Y²
#
# Then define latent interaction/quadratic factors from these products.
# This is an approximation to LMS but runs in standard lavaan.
# =============================================================================

# -- Prepare product indicators and fit model --

prepare_product_indicators <- function(dat) {
  # Mean-center first
  dat_c <- as.data.frame(scale(dat, center = TRUE, scale = FALSE))

  # Interaction products (matched pairs): XY
  dat_c$xy1 <- dat_c$x1 * dat_c$y1
  dat_c$xy2 <- dat_c$x2 * dat_c$y2
  dat_c$xy3 <- dat_c$x3 * dat_c$y3

  # Quadratic X products: X²
  dat_c$xs1 <- dat_c$x1 * dat_c$x1
  dat_c$xs2 <- dat_c$x2 * dat_c$x2
  dat_c$xs3 <- dat_c$x3 * dat_c$x3

  # Quadratic Y products: Y²
  dat_c$ys1 <- dat_c$y1 * dat_c$y1
  dat_c$ys2 <- dat_c$y2 * dat_c$y2
  dat_c$ys3 <- dat_c$y3 * dat_c$y3

  return(dat_c)
}

# lavaan model syntax (product-indicator approach)
model_pi <- '
  # Measurement model
  X  =~ x1 + x2 + x3
  Y  =~ y1 + y2 + y3
  Z  =~ z1 + z2 + z3

  # Latent interaction & quadratic factors (product indicators)
  XY =~ xy1 + xy2 + xy3
  XS =~ xs1 + xs2 + xs3
  YS =~ ys1 + ys2 + ys3

  # Structural model
  Z ~ b1*X + b2*Y + b3*XS + b4*XY + b5*YS

  # Congruence surface parameters (Edwards & Parry, 1993)
  cs := b1 + b2       # congruence line slope
  cc := b3 + b4 + b5  # congruence line curvature
  is := b1 - b2       # incongruence line slope
  ic := b3 - b4 + b5  # incongruence line curvature
'


# ---- 4. Run the Monte Carlo Simulation --------------------------------------

cat("Running Monte Carlo simulation with product-indicator approach...\n")
cat(sprintf("Replications: %d | N per rep: %d\n\n", n_reps, n_obs))

# Storage for results
param_names <- c("b1", "b2", "b3", "b4", "b5", "cs", "cc", "is", "ic")
results <- matrix(NA, nrow = n_reps, ncol = length(param_names) * 3)
# columns: est, se, pvalue for each parameter
colnames(results) <- c(paste0(param_names, "_est"),
                        paste0(param_names, "_se"),
                        paste0(param_names, "_pval"))

convergence <- logical(n_reps)

pb <- txtProgressBar(min = 0, max = n_reps, style = 3)

for (i in 1:n_reps) {
  # Generate data
  dat_raw <- generate_data(n_obs)
  dat     <- prepare_product_indicators(dat_raw)

  # Fit model
  fit <- tryCatch(
    sem(model_pi, data = dat, se = "robust", test = "yuan.bentler",
        meanstructure = FALSE),
    error   = function(e) NULL,
    warning = function(w) {
      suppressWarnings(
        sem(model_pi, data = dat, se = "robust", test = "yuan.bentler",
            meanstructure = FALSE)
      )
    }
  )

  if (!is.null(fit) && lavInspect(fit, "converged")) {
    convergence[i] <- TRUE
    pe <- parameterEstimates(fit)

    for (p in param_names) {
      row_idx <- which(pe$label == p)
      if (length(row_idx) == 1) {
        results[i, paste0(p, "_est")]  <- pe$est[row_idx]
        results[i, paste0(p, "_se")]   <- pe$se[row_idx]
        results[i, paste0(p, "_pval")] <- pe$pvalue[row_idx]
      }
    }
  } else {
    convergence[i] <- FALSE
  }

  setTxtProgressBar(pb, i)
}
close(pb)


# ---- 5. Summarise Results ---------------------------------------------------

cat("\n\n========================================================\n")
cat("         MONTE CARLO SIMULATION RESULTS\n")
cat("========================================================\n\n")

cat(sprintf("Convergence rate: %d / %d (%.1f%%)\n\n",
            sum(convergence), n_reps, 100 * mean(convergence)))

# True values
true_vals <- c(b1 = b1, b2 = b2, b3 = b3, b4 = b4, b5 = b5,
               cs = cs_true, cc = cc_true, is = is_true, ic = ic_true)

# Compute summary statistics for converged replications
res_conv <- results[convergence, ]

summary_table <- data.frame(
  Parameter = param_names,
  True      = true_vals,
  Mean_Est  = colMeans(res_conv[, paste0(param_names, "_est")], na.rm = TRUE),
  SD_Est    = apply(res_conv[, paste0(param_names, "_est")], 2, sd, na.rm = TRUE),
  Mean_SE   = colMeans(res_conv[, paste0(param_names, "_se")], na.rm = TRUE),
  Bias      = NA,
  Rel_Bias  = NA,
  Power     = NA,
  stringsAsFactors = FALSE
)

summary_table$Bias     <- summary_table$Mean_Est - summary_table$True
summary_table$Rel_Bias <- ifelse(summary_table$True != 0,
                                  summary_table$Bias / summary_table$True * 100,
                                  NA)

# Power = proportion of replications where p < .05
for (j in seq_along(param_names)) {
  pvals <- res_conv[, paste0(param_names[j], "_pval")]
  summary_table$Power[j] <- mean(pvals < 0.05, na.rm = TRUE)
}

# Coverage: proportion of 95% CIs containing the true value
summary_table$Coverage <- NA
for (j in seq_along(param_names)) {
  ests <- res_conv[, paste0(param_names[j], "_est")]
  ses  <- res_conv[, paste0(param_names[j], "_se")]
  lower <- ests - 1.96 * ses
  upper <- ests + 1.96 * ses
  summary_table$Coverage[j] <- mean(lower <= true_vals[j] & true_vals[j] <= upper,
                                     na.rm = TRUE)
}

# Pretty print
cat(sprintf("%-12s %7s %8s %8s %8s %8s %8s %8s %8s\n",
            "Param", "True", "M(Est)", "SD(Est)", "M(SE)", "Bias", "%Bias",
            "Power", "Cover"))
cat(paste(rep("-", 85), collapse = ""), "\n")

for (j in 1:nrow(summary_table)) {
  cat(sprintf("%-12s %7.3f %8.3f %8.3f %8.3f %8.3f %7.1f%% %7.1f%% %7.1f%%\n",
              summary_table$Parameter[j],
              summary_table$True[j],
              summary_table$Mean_Est[j],
              summary_table$SD_Est[j],
              summary_table$Mean_SE[j],
              summary_table$Bias[j],
              summary_table$Rel_Bias[j],
              summary_table$Power[j] * 100,
              summary_table$Coverage[j] * 100))
}

cat("\n")

# ---- 6. Quick Sanity Check: Correlations in Generated Data ------------------

cat("=== Sanity Check: Mean Latent Correlations (across reps) ===\n")
# We check that the generated indicator composites correlate positively
cor_XZ <- cor_YZ <- cor_XY_obs <- numeric(min(100, n_reps))
for (i in 1:length(cor_XZ)) {
  d <- generate_data(n_obs)
  X_comp <- rowMeans(d[, c("x1", "x2", "x3")])
  Y_comp <- rowMeans(d[, c("y1", "y2", "y3")])
  Z_comp <- rowMeans(d[, c("z1", "z2", "z3")])
  cor_XZ[i]    <- cor(X_comp, Z_comp)
  cor_YZ[i]    <- cor(Y_comp, Z_comp)
  cor_XY_obs[i] <- cor(X_comp, Y_comp)
}
cat(sprintf("  r(X, Y) = %.3f  [should be positive]\n", mean(cor_XY_obs)))
cat(sprintf("  r(X, Z) = %.3f  [should be positive]\n", mean(cor_XZ)))
cat(sprintf("  r(Y, Z) = %.3f  [should be positive]\n", mean(cor_YZ)))
cat("\n")


# ---- 7. Optional: Save Results -----------------------------------------------

example_export <- write_mplus_example_files()

cat("Single simulated draw exported to:\n")
cat(sprintf("  %s\n", example_export$data_path))
cat(sprintf("  %s\n\n", example_export$inp_path))

saveRDS(list(
  summary    = summary_table,
  raw        = results,
  converged  = convergence,
  true_vals  = true_vals,
  settings   = list(n_reps = n_reps, n_obs = n_obs,
                     b1 = b1, b2 = b2, b3 = b3, b4 = b4, b5 = b5,
                     rho_XY = rho_XY, psi_Z = psi_Z)
), file = "lms_mc_results.rds")

cat("Results saved to lms_mc_results.rds\n")
cat("Done.\n")
