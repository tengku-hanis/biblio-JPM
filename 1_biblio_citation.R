# Bibliometric analysis - citation related metrics
# Tengku Hanis (https://tengkuhanis.netlify.app/)
# Oct20, 2022

# Packages ----------------------------------------------------------------

install.packages("bibliometrix")
install.packages("tidyverse")
install.packages("ggrepel")

library(bibliometrix)
library(tidyverse)
theme_set(theme_bw())

# Tags --------------------------------------------------------------------

data("bibtag"); View(bibtag)


# Data --------------------------------------------------------------------

link <- "https://raw.githubusercontent.com/tengku-hanis/bibliometrics-Jan18-2022/main/mbc.bib"
dat <- convert2df(file = link, dbsource = "scopus", format = "bibtex")
names(dat)
dim(dat)


# NAs ---------------------------------------------------------------------

dat %>% 
  select(TI, AB) %>% 
  summarise(TI = sum(is.na(TI)), AB = sum(is.na(AB)))

dat %>% 
  filter(is.na(AB)) %>% 
  select(TI, AB) %>% 
  slice(1:3) 


# Duplicate ---------------------------------------------------------------

dat %>% 
  select(DI) %>%
  summarise(DI = sum(duplicated(DI)))


# Descriptive -------------------------------------------------------------

result <- biblioAnalysis(dat)
summary(result, k=10)

P <- plot(result, k = 10)

P$MostProdAuthors
P$MostProdCountries
P$AnnualScientProd
P$AverArtCitperYear
P$AverTotCitperYear


# Funded research ---------------------------------------------------------

table(is.na(dat$FU)) %>% 
  prop.table()*100 #31.8% funded


# Citation related metrics  -----------------------------------------------

## References for first paper
dat$CR[1] #separator is ;
result$MostCitedPapers %>% 
  head()

## 1) Frequently cited manuscripts ----
fc <- citations(dat, field = "article", sep = ";")
cbind("Freq" = fc$Cited[1:5])

## 2) Frequently cited first authors ----
fcfa <- citations(dat, field = "author", sep = ";")
cbind("Freq" = fcfa$Cited[1:10])






