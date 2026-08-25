# RSA_mplus warns and forwards to rsahelpers

    Code
      old_result <- do.call(RSA_mplus, args)
    Condition
      Warning:
      'franzpak::RSA_mplus' is deprecated.
      Use 'rsahelpers::RSA_mplus' instead.
      See help("Deprecated") and help("franzpak-deprecated").

# RSA_mplus explains how to install a missing rsahelpers

    Code
      RSA_mplus(model = "model.out", outcome = "Z", pred_x = "X", pred_y = "Y",
        pred_x2 = "XS", pred_xy = "XY", pred_y2 = "YS", b0 = 0, plot = FALSE)
    Condition
      Warning in `RSA_mplus()`:
      'franzpak::RSA_mplus' is deprecated.
      Use 'rsahelpers::RSA_mplus' instead.
      See help("Deprecated") and help("franzpak-deprecated").
      Error in `RSA_mplus()`:
      ! Package `rsahelpers` is required to use `franzpak::RSA_mplus()`.
      i Install it with `pak::pak("franciscowilhelm/rsahelpers")`.

