 Explored
  └ Read callr_wrappers.R, test-callr_wrappers.R, DESCRIPTION, NAMESPACE, cortable_multilevel.Rd

─ Worked for 9m 06s ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

• Refactored the background-job helpers into proper package code with documentation and a regression test.

  - R/callr_wrappers.R:1 creates roxygen2 docs for every wrapper and helper, wiring the background job registry into the package namespace.
  - R/callr_wrappers.R:62 formalises .bgjm_start_job() and the exported bgjm_* lifecycle functions, including richer parameter handling and
    artefact capture notes.
  - tests/testthat/test-callr_wrappers.R:1 turns the previous usage example into a lifecycle test that exercises start → poll → collect → read
    log behaviour.
  - NAMESPACE:8 exports the new public API and records the needed callr imports; DESCRIPTION:16 lists callr as an import so the wrappers load
    successfully.
  - man/cortable_multilevel.Rd:12 now documents the existing between argument (side-effect of regenerating docs).

  Tests were not run because the sandbox lacks the callr dependency; once it is installed you can run roxygen2::roxygenise() and
  testthat::test_local() (or devtools::test()) to regenerate Rd files and confirm the suite passes.

  