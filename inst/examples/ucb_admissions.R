## UC Berkeley graduate admissions, 1973 (Bickel, Hammel and O'Connell, 1975)
## The data are in base R: datasets::UCBAdmissions (Admit x Gender x Dept)
library(vcdinteractive)

## 1. Aggregated over departments: women seem to be discriminated against
imosaic(~ Gender + Admit, data = UCBAdmissions,
        main = "UCB admissions, aggregated: Gender x Admit")

## 2. Model [AD][GD]: admission independent of gender within each department.
##    Only department A departs from it, and in favour of women.
imosaic(~ Dept + Gender + Admit, data = UCBAdmissions,
        expected = ~ Admit * Dept + Gender * Dept,
        main = "UCB admissions: model [AD][GD]")
## Drag 'Dept' to the last position to compare the departments within
## each gender.

## 3. Why the aggregate is misleading: women applied mostly to the
##    departments with low admission rates (Dept x Gender and Dept x Admit)
imosaic(~ Dept + Gender, data = UCBAdmissions, main = "Who applies where")
imosaic(~ Dept + Admit,  data = UCBAdmissions, main = "Admission rate by department")

## The numbers
significant_cells(UCBAdmissions, expected = ~ Admit * Dept + Gender * Dept)
