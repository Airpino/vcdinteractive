## Demo of vcdinteractive
library(vcdinteractive)

## 1. Hair and eye colour: drag the variables to reorder, hover the tiles
imosaic(HairEyeColor, main = "Hair and eye colour")

## 2. Titanic, robust residuals at start-up
imosaic(Titanic, residuals = "robust", main = "Titanic")

## 3. Partial independence: Survived independent of the joint (Class, Sex, Age)
imosaic(Titanic, expected = ~ Class * Sex * Age + Survived,
        main = "Titanic: [CSA][S]")

## 4. The underlying numbers
cells <- mosaic_cells(Titanic, expected = ~ Class * Sex * Age + Survived)
cells
significant_cells(cells, type = "standardized", adjust = "BH")

## 5. Save as a stand-alone HTML page
## htmlwidgets::saveWidget(imosaic(HairEyeColor), "hair_eye.html")

## 6. The other views of imosaic and model comparison
imosaic(HairEyeColor, display = "assoc")
imosaic(margin.table(HairEyeColor, 1:2), display = "sieve")
imosaic(Titanic, display = "doubledecker",
        models = c("mutual", "joint", "conditional", "markov", "saturated"))

## 7. The other widgets
ipairs(HairEyeColor)
ifourfold(UCBAdmissions)
irootogram(c("0" = 109, "1" = 65, "2" = 22, "3" = 3, "4" = 1), xlab = "Deaths by horse kicks")
icdplot(Species ~ Petal.Length, data = iris)
imcaplot(HairEyeColor)
iassoc_graph(Titanic)
