## Submission

This is a new submission.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Test environments

* local Windows 11 x64, R 4.5.3 (`devtools::check(remote = TRUE, manual = TRUE)`):
  0 errors, 0 warnings, 1 note (New submission)
* Ubuntu, R 4.3 (`R CMD check --as-cran`, with and without the suggested
  packages): 0 errors, 0 warnings
* win-builder (R-devel)
* R-hub (linux, macos, windows)

## Notes for the reviewers

* The package contains only R and JavaScript code written for it; it
  bundles no third-party JavaScript library.
* Packages in Suggests ('vcd', 'vcdExtra', 'robustbase', 'ca', 'gnm') are
  used conditionally in examples, tests and vignettes.
