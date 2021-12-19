---
title: "Eksploracja metadanych przeglądu literatury dot. szczepionek mRNA "
author: "zamaro@outlook.com"
output: html_document
---

```{r setup, include=F, echo=F}
library(bibliometrix)
library(pubmedR)
library(pubmed.mineR)
library(lsa)
library(bibliometrix)
library(pubmedR)
library(knitr)
library(wordcloud)
```

```{r include=F, echo = FALSE}
api_key = "adb7c4029ba414a3108f116d2f609e4aae08" 
query <- "(mrna vaccine*[Title/Abstract]) AND human[mh]"
res <- pmQueryTotalCount(query, api_key)
D <- pmApiRequest(query=query, limit=res$total_count, api_key=api_key)
M <- pmApi2df(D, format = "bibliometrix")
M$AU1_CO<- M$SO_CO
M$AU_CO<- M$SO_CO
M$CR <- M$SO_CO
results <- biblioAnalysis(M)
sum <- summary(results, k=10, pause=F, width=130)
```

```{r include = F, echo = F}
abs <- readabs("mrna_vaccine.txt")
word_abs <- word_atomizations(abs)
```


```{r include=F, echo=F}
main <- kable(sum$MainInformationDF[2:8,1:2], row.names = F)
plots <- plot(x=results, k=10, pause=F)
growrate <- sum$AnnualGrowthRate
documents <- kable(sum$MainInformationDF[10:15,1:2], row.names = F)
authors <- kable(sum$MainInformationDF[21:29,1:2], row.names = F)
authors_prod <- kable(sum$MostProdAuthors[,1:2], row.names = F)
keywords <- kable(sum$MostRelKeywords, row.names = F)
sources <- kable(sum$MostRelSources, row.names = F)
countries <- kable(sum$MostProdCountries[,1:4], row.names = F)
countries_prod <- plot(x=results, k=10, pause=F)[2]
wordsplot <- wordcloud(word_abs$words, word_abs$Freq, random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"), lang = "english")
gene_abs <- gene_atomization(abs)
CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust= 4, stemming=F, labelsize=7)
dend <- CS$graph_dendogram
concept <- CS$graph_terms
```


---


Analiza przeprowadzona na podstawie danych bibliometrycznych biblioteki PubMed. Dane dotycza publikacji zawierajacych w tytulach i abstraktach zwrot "vaccine*", "mRNA" oraz deskryptor "human[HT]" wraz z translacją słownika MeSH




> ## **Dane wejściowe**


**Zastosowana kwerenda:**

``` {r echo = F}
query
```

**Translacja MeSH zapytania:** 

```{r echo = FALSE}
res$query_translation
```

**Liczba otrzymanych wyników:**

``` {r echo = FALSE}
res$total_count
```


---

> ## **Charakterystyka otrzymanego zbioru**

Zakres czasowy otrzymanych publikacji obejmuje lata 2009-2020. Do analizy włączono 94 dokumenty, z czego 59 dotyczy publikacji naukowych. Liczba publikacji wzrastała średnio o 33,5% w porównaniu do roku poprzedniego.

```{r echo=F}
main
```

## **Roczny wskaźnik wzrostu liczby publikacji**

```{r echo = F}
growrate
```

---

W 2020 r. opublikowano 24 publikacje dotyczące poruszanej tematyki. 

#### **Rozkład publikacji w ujęciu rocznym**


```{r echo = F, fig.width=12, fig.height=5}
plots$AnnualScientProd
```

---




> ### **Analiza bibliograficzna**

Przegląd zawiera 77 artykułów opublikowanych w czasopismach naukowych. 6 publikacji dotyczy badań klinicnzych I fazy. 


**Typy publikacji**

```{r echo = F}
documents
```

---

> ### **Sieć współpracy naukowej**

Sieć współpracy naukowej określa relację, w której obiektami są autorzy publikacji, natomiast nicie pomiędzy obiektami oznaczają autorów współtworzących publikację. Do

```{r echo = F , fig.width=20, fig.height=20}
NetMatrix <- biblioNetwork(M, analysis = "collaboration",  network = "authors", sep = ";")
net=networkPlot(NetMatrix,  n = 30, Title = "Sieć relacji autorów (n=30)",type = "auto", size=15,size.cex=T,edgesize = 3,labelsize=1.5)
```


### **Rozkład produktywności autorów i liczby cytowań w ujęciu rocznym**

An

```{r echo = F, fig.width= 10}
top <- authorProdOverTime(M, k = 10, graph = TRUE)

```

---

### **Charakterystyka ośrodków naukowych**

```{r echo = F, fig.width=20, fig.height=20}
NetMatrix <- biblioNetwork(M, analysis = "collaboration",  network = "universities", sep = ";")
net=networkPlot(NetMatrix,  n =30, Title = "Sieć relacji ośrodków naukowych (n=30)",type = "circle", size=,size.cex=T,edgesize = 3,labelsize=1.5)
```

---

### **Liczba artykułów w podziale na źródło publikacji - top n=10**


```{r echo = F}
sources
```

---

> ## **Współwystępowanie słów kluczowych**


```{r echo = F, fig.width=20, fig.height=10}
biblio_keywords <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(biblio_keywords, normalize="association", weighted=T, n = 30, Title = "Współwystępowanie słów kluczowych (n=30)", type = "auto", size=T,edgesize = 5,labelsize=0.7)
```

### **Analiza CO-WORD**


Eksploracja wyrazów ze względu na częstotliwość wystąpień w przeglądzie literatury


```{r echo = F}
wordsplot
```

> ## **Hierarchiczna analiza klastrów HCA**

Wysokość klamry osi y wskazuje dystans między klastrami. Im wyższa wysokość połączenia między obserwacjami, tym mniej podobne są do siebie obiekty. Odległość na osi x nie stanowi kryterium podobieństwa. 

```{r echo = F}
dend
```

> ## **Analiza struktur pojęciowych**

W analizie wykorzystano analizę korespondencji w celu wydobycia struktury pojęciowej wyrazów przez redukcję ich odmian i pochodnych. i Na podstawie otrzymanej struktury metodą k-średnich wyznaczono współrzędne k-punktów, będących środkiem klastra. Otrzymane klastry skupiają wyrazy o wspólnych pojęchach zredukowane do ich podstaw lub rdzenia. 

```{r echo = F}
concept
```

### **Mapa tematyczna**

Mapa tematyczna klasyfikuje klastry wyrazów na podstawie stopnia jej wykorzystania w literaturze. 

1. Prawa górna ćwiartka - terminologia techniczna (niezwiązana bezpośrednio z poruszanym tematem)
2. Prawa dolna ćwiartka - terminologia podstawowa (tworząca fundament literatury)
3. Lewa górna ćwiartka - terminologia bardzo specjalistyczna lub niszowa 
4. Lewa dolna ćwiartka-  terminologia dodatkowa (terminy związane z analizowanym tematem, które w literaturze pojawiają się fakultatywnie)


```{r echo = F}
Map=thematicMap(M, field = "ID", n = 250, minfreq = 5,
                stemming = FALSE, size = 0.5, n.labels=3, repel = TRUE)
plot(Map$map)
```


## **Częstotliwość wystąpień nazw genów**

Poni



