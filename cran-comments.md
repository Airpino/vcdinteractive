## Submission

This is a new submission.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* The words flagged as possibly misspelled in DESCRIPTION are correct:
  Anscombe, Zeileis and Hornik are surnames (Anscombe residuals; Meyer,
  Zeileis and Hornik, 2006), and doubledecker, rootograms and strucplot are
  established names of displays for categorical data (see the 'vcd'
  package).

## Test environments

* local Windows 11 x64, R 4.5.3 (`devtools::check(remote = TRUE, manual = TRUE)`):
  0 errors, 0 warnings, 1 note (New submission)
* Ubuntu, R 4.3 (`R CMD check --as-cran`, with and without the suggested
  packages): 0 errors, 0 warnings
* win-builder, R-devel (2026-11-04 r90836 ucrt), Windows Server 2022:
  0 errors, 0 warnings, 1 note (New submission, possibly misspelled words)
* R-hub (GitHub Actions), R-devel: linux and macos: 0 errors, 0 warnings,
  1 note (the CI folder `.github` was included in the build; it is now
  excluded in `.Rbuildignore`); windows:
  the check did not start, because the R-hub runner failed while installing
  the dependencies (`pak` error "cannot open the connection"); the
  win-builder R-devel check above covers Windows.

## Notes for the reviewers

* The package contains only R and JavaScript code written for it; it
  bundles no third-party JavaScript library.
* Packages in Suggests ('vcd', 'vcdExtra', 'robustbase', 'ca', 'gnm') are
  used conditionally in examples, tests and vignettes.
