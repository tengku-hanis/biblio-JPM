# Bibliometric analysis - relationship related metrics
# Tengku Hanis (https://tengkuhanis.netlify.app/)
# Oct20, 2022

# Packages ----------------------------------------------------------------

library(bibliometrix)
library(tidyverse)
theme_set(theme_bw())

# Data --------------------------------------------------------------------

link <- "https://raw.githubusercontent.com/tengku-hanis/bibliometrics-Jan18-2022/main/mbc.bib"
dat <- convert2df(file = link, dbsource = "scopus", format = "bibtex")


# Relationship related metrics --------------------------------------------

# Details see ?biblioNetwork

## 1) Collaboration ----

#authors, universities, countries
MT <- metaTagExtraction(dat, Field = "AU_CO", sep = ";")
country_collab <- biblioNetwork(MT, analysis = "collaboration",  network = "countries")

# Plot
set.seed(123)
networkPlot(country_collab, n = 30, cluster = "none", #try "optimal"
            Title = "Countries collaboration", type = "circle",
            size.cex = T)

## 2) Co-citation ----

#authors, references, sources
ref_cc <- biblioNetwork(dat, analysis = "co-citation", network = "references", sep = ";")

set.seed(123)
networkPlot(ref_cc, n = 30, cluster = "none", 
            Title = "Co-citation of references", type = "circle",
            size.cex = T)

## 3) Coupling ----

#authors, references, sources, countries
auth_couple <- biblioNetwork(dat, analysis = "coupling", network = "authors", sep = ";")

set.seed(123)
networkPlot(auth_couple, n = 30, cluster = "none", 
            Title = "Bibliographic coupling of the authors", type = "circle",
            size.cex = T)

## 4) Co-word analysis ----

#authors, sources, keywords, author_keywords, titles, abstracts
kw_co <- biblioNetwork(dat, analysis = "co-occurrences", network = "keywords", sep = ";")

set.seed(123)
networkPlot(kw_co, n = 30, cluster = "none", 
            Title = "Keyword co-occurrences", type = "circle",
            size.cex = T)
