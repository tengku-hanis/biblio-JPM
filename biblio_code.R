# Bibliometric analysis
# Tengku Hanis (https://tengkuhanis.netlify.app/)
# Oct20, 2022

# Packages ----------------------------------------------------------------

library(bibliometrix)
library(tidyverse)
theme_set(theme_bw())

# Tags --------------------------------------------------------------------

data("bibtag"); View(bibtag)


# Data --------------------------------------------------------------------

link <- "https://raw.githubusercontent.com/tengku-hanis/webinar_biblio24-09-2020/master/scopus_acanthoma.bib"
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

## Extract all duplicates - TI
dat[duplicated(dat$DI) | duplicated(dat$DI, fromLast = T), "DI"] 


# Descriptive -------------------------------------------------------------

result <- biblioAnalysis(dat)
summary(result, k=10)

P <- plot(result, k=10)

P$MostProdAuthors
P$MostProdCountries
P$AnnualScientProd
P$AverArtCitperYear
P$AverTotCitperYear


# Funded research ---------------------------------------------------------

table(is.na(dat$FU)) %>% 
  prop.table()*100 #2.8% funded


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


# Relationship related metrics --------------------------------------------

# Details see ?biblioNetwork

## 1) Collaboration ----

#authors, universities, countries
MT <- metaTagExtraction(dat, Field = "AU_CO", sep = ";")
country_collab <- biblioNetwork(MT, analysis = "collaboration",  network = "countries")
summary(networkStat(country_collab))

# Plot
set.seed(123)
ccPlot <- networkPlot(country_collab, n = 30, cluster = "none", #try "optimal"
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


# Theory related metrics --------------------------------------------------

## 1) Lotka's law ----

L <- lotka(result)

L$AuthorProd #observed distribution of author productivity
L$Beta #beta coeeficient of Lotka's law
L$R2 #GOF of Lotka's law (r^2)

# P value of K-S two sample test
L$p.value #there is no sig diff btwn observed and theoretical distribut.

# Theoretical distribution with Beta = 2
Theoretical <- 10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

# Using ggplot
ldata <- 
  L$AuthorProd %>% 
  bind_cols(theory = Theoretical) %>% 
  pivot_longer(cols = c(Freq, theory), names_to = "distribution", values_to = "val_distr") %>% 
  mutate(distribution = as.factor(distribution), 
         distribution = fct_recode(distribution, Observed = "Freq", Theoretical = "theory"))

ldata %>% 
  ggplot(aes(N.Articles, val_distr, color = distribution)) +
  geom_line() +
  labs(color = "Distribution:") +
  ylab("Frequency of authors") +
  xlab("Number of articles") +
  theme(legend.position = "top") +
  annotate(geom = "text", x = 8.5, y = 0.2, label = paste("P-value = ", round(L$p.value, digits = 3)))

## 2) Bradford's law ----

bl <- bradford(dat)
bl

# Summary for each zone
bl$table %>% 
  group_by(Zone) %>% 
  summarise(n = n())

# Core journals
bl$table %>% 
  filter(Zone == "Zone 1") %>% 
  select(-SO)


# Miscellaneous metrics ----------------------------------------------------

# ## 1) Conceptual structure ----
# 
# conceptualStructure(dat, field = "ID", stemming = F)
# 
# ## 2) History network ----
# 
# histData <- histNetwork(dat, sep = ";")
# histPlot(histData)

## 3) Thematic map ----

Map <- thematicMap(dat, field = "ID", #"ID","DE", "TI", "AB"
                   minfreq = 3, stemming = FALSE, n.labels = 3, repel = T)
plot(Map$map)

Map$documentToClusters %>% view()
Map$documentToClusters %>% 
  filter(Assigned_cluster == "epidermoid") %>% 
  select(TI, DI)

## 4) Thematic evolution ----

years <- c(2000)
thematicEvolution(dat, field = "DE", #"ID","DE", "TI", "AB"
                  years = years, n = 100, minFreq = 3)

## 5) Trending keywords ----

trend_kw <- fieldByYear(dat, field = "ID", timespan = c(2010,2019),
                        min.freq = 1, n.items = 5, graph = TRUE) 
trend_kw$graph 

# Another way to plot trending keywords
dat <- trend_kw$df_graph

ggplot(dat, aes(year_med, freq)) + 
  geom_point() +
  ggrepel::geom_text_repel(aes(label = tolower(dat$item)), max.overlaps = 50) +
  scale_x_continuous(breaks = seq(2010, 2019, 1)) +
  xlab("Year") +
  ylab("Frequency")

## 6) Authors' dominance ----

dom <- dominance(result, k=10)
dom
?dominance #detail how dominance factor calculated

## 7) Top-author productivity over time ----

topAU <- authorProdOverTime(dat, k=10)
topAU$graph +
  theme_bw()

head(topAU$dfAU) #author's productivity per year
head(topAU$dfPapersAU) #author's document list


# Biblioshiny -------------------------------------------------------------

biblioshiny()

