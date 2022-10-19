# Bibliometric analysis - theory-related metrics
# Tengku Hanis (https://tengkuhanis.netlify.app/)
# Oct20, 2022

# Packages ----------------------------------------------------------------

library(bibliometrix)
library(tidyverse)
theme_set(theme_bw())

# Data --------------------------------------------------------------------

link <- "https://raw.githubusercontent.com/tengku-hanis/bibliometrics-Jan18-2022/main/mbc.bib"
dat <- convert2df(file = link, dbsource = "scopus", format = "bibtex")


# Miscellaneous metrics ----------------------------------------------------

## 1) Thematic map ----

Map <- thematicMap(dat, field = "ID", #"ID","DE", "TI", "AB"
                   minfreq = 3, stemming = FALSE, n.labels = 3, repel = T)
plot(Map$map)

Map$documentToClusters %>% view()
Map$documentToClusters %>% 
  filter(Assigned_cluster == "genetics") %>% 
  select(TI, DI)


## 2) Trending keywords ----

trend_kw <- fieldByYear(dat, field = "ID", timespan = c(2010,2019),
                        min.freq = 1, n.items = 5, graph = TRUE) 
trend_kw$graph 

# Another way to plot trending keywords
dat_kw <- trend_kw$df_graph

ggplot(dat_kw, aes(year_med, freq)) + 
  geom_point() +
  ggrepel::geom_text_repel(aes(label = tolower(dat_kw$item)), max.overlaps = 50) +
  scale_x_continuous(breaks = seq(2010, 2019, 1)) +
  xlab("Year") +
  ylab("Frequency")

## 3) Authors' dominance ----

result <- biblioAnalysis(dat)
dom <- dominance(result, k=10)
dom
?dominance #detail how dominance factor calculated

## 4) Top-author productivity over time ----
# dat <- convert2df(file = link, dbsource = "scopus", format = "bibtex")
topAU <- authorProdOverTime(dat, k=10)
topAU$graph +
  theme_bw()

head(topAU$dfAU) #author's productivity per year
head(topAU$dfPapersAU) #author's document list

## 5) Three fields plot
threeFieldsPlot(dat, fields = c("AU", "DE", "SO"), n = c(20, 20, 20))
