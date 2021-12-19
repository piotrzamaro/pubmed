---
title: ""
output: 
  flexdashboard::flex_dashboard:
    orientation: row
    vertical_layout: scroll
    social: menu
    source_code: embed
runtime: shiny
---

```{r setup, include=FALSE}
library(flexdashboard)
library(scales)
library(dimensionsR)
library(bibliometrix)
library(pubmedR)
library(pubmed.mineR)
library(readxl)
library(RISmed)
library(dplyr)
require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(quanteda.corpora)
require(ggplot2)
library(knitr)
library(kableExtra)
library(tidyr)
library(DT)
library(mgcv)


load("~/ANALITYKA/AOTMiT/COVID-19/PUBMED/COVID-19/general_data.RData")
load("~/ANALITYKA/AOTMiT/COVID-19/PUBMED/COVID-19/sum_data.RData")


dataset <- general %>%
    na.omit() %>%
    separate(AU, sep = ";", into = "Autor") %>%
    select("Kategoria" = label, "Dokument" = DT, "Nazwa" = item, "Tytuł" = TI,  Autor, "Źródło" = J9, PMID, "Rok" = PY, "Kraj" = SO_CO, "Język" = LA) 
dataset$Rok <- as.character(dataset$Rok)

```

```{r echo=F, eval = F}
api_key = "adb7c4029ba414a3108f116d2f609e4aae08" 



treat <-'LitCTreatment[filter] AND human[mh] AND clinical trial[pt]'
res_treat <- pmQueryTotalCount(treat, api_key)
D_treat <- pmApiRequest(query=treat, limit=res_treat$total_count, api_key=api_key)
M_treat <- pmApi2df(D_treat, format = "bibliometrix")
M_treat <- M_treat %>%
  mutate("search filter" = "TREATMENT")
```



Metodyka
=========================================

Row 
-----------------------------------------------------------------------

### **Metodyka**

Baza COVID-19 jest biblioteką skupiającą najnowsze źródła dotyczące globalnej pandemii wywołanej wirusem SARS-COV2.
Zbiór zawiera źródła naukowe dotyczące badań klinicznych związanych z pandemią COVID-19. Źródłem danych jest baza NCBI PubMed . To przeszukania bazy PubMed użyto filtrów *LitCovid* zalecanych przez NCBI. Adnotacje zmiennych „Kategoria i Nazwa” stanowiące kluczowe konspecje semantyczne literatury biomedycznej uzyskano za pośrednictwem systemu PubTator łącząc je z numerami PMID. 
Dostęp do bazy NCBI PubMed uzyskano przy użyciu klucza API. Uzyskane publikacje wraz z załączonymi metadanymi przetworzono na format zgony z wymaganiami pozostałych funkcji. 
Analizę bibliometryczną przeprowadzono przy użyciu pakietu *Bibliometrix”. Adnotacje zmiennych „Kategoria i Nazwa” stanowiące kluczowe konspekcie semantyczne literatury biomedycznej uzyskano za pośrednictwem systemu PubTator łącząc je z numerami PMID. 
Tabele dotyczące częstości występowania słów kluczowych oraz częstości adnotacji dotyczących substancji chemicznych i genów przedstawiają 15 pierwszych wystąpień użytego terminu biomedycznego. Liczba wystąpień rozumiana jest jako pojedyncze wystąpienie numeru identyfikacyjnego PubMed (PMID). 

### **Kroki w procesie tworzenia zbioru**

1. Przeszukanie bazy PubMed
2. Wyszukanie adnotacji w bazie PubTator
3. Przetworzenie uzyskanego zbioru
4, Analiza bibliometryczna
5. Budowa bazy źródeł naukowych 
6. Wizualna eksploracja danych


Row 
-----------------------------------------------------------------------

### **Słownik**

Badania eksperymentalne
=========================================

Row 
-----------------------------------------------------------------------



### Zbiór zawiera źródła naukowe dotyczące badań klinicznych w kierunku leczenia COVID-19. Źródłem danych jest baza NCBI PubMed . Do przeszukania bazy użyto strategii *’LitCTreatment[filter] AND human[mh] AND clinical trial[pt]'*. Adnotacje zmiennych „Kategoria i Nazwa” stanowiące kluczowe konspecje semantyczne literatury biomedycznej uzyskano za pośrednictwem systemu PubTator łącząc je z numerami PMID. 
```{r eval=F, fig.width=11}

articles <- "Metodyka"

valueBox(articles, icon = "ion-erlenmeyer-flask")
```


### Liczba publikacji

```{r eval=F, fig.width=3}
articles <- general %>%
    summarise(n = n_distinct(PMID)) %>%
    select(n)
valueBox(articles, icon = "ion-erlenmeyer-flask")
```

###  Badania fazy I 

```{r eval=F, fig.width=3}
phase1 <- general %>%
  filter(DT == "CLINICAL TRIAL, PHASE I") %>%
    summarise(n = n_distinct(PMID)) %>%
    select(n)
valueBox(phase1, icon = "ion-erlenmeyer-flask")


```

### Badania fazy II

```{r eval=F, fig.width=3}
phase2 <- general %>%
  filter(DT == "CLINICAL TRIAL, PHASE II") %>%
    summarise(n = n_distinct(PMID)) %>%
    select(n)
valueBox(phase2, icon = "ion-erlenmeyer-flask")

```


### Badania fazy III

```{r, eval=F, fig.width=3}
phase3 <- general %>%
  filter(DT == "CLINICAL TRIAL, PHASE III") %>%
    summarise(n = n_distinct(PMID)) %>%
    select(n)
valueBox(phase3, icon = "ion-erlenmeyer-flask")

    
```



Column 
-----------------------------------------------------------------------

### **Rodzaj źródła**

```{r echo = F, eval= T, fig.width=4}
general %>%
    group_by(DT) %>%
    summarise(n = n_distinct(PMID)) %>%
    arrange(desc(n)) %>%
    kableExtra::kbl(longtable = F, col.names = c("Dokument", "Liczba publikacji"))
```



### **Częstość słów kluczowych**

```{r echo = F, eval= T}
knitr::kable(sum$MostRelKeywords) 
```


### **Substancje**

```{r echo = F, eval= T, fig.width=4}


general %>%
    filter(label == "chemical") %>%
group_by(item) %>%
    summarise(n = n_distinct(PMID)) %>%
    arrange(desc(n)) %>%
    head(15) %>%
    kableExtra::kbl(longtable = F, col.names = c("Nazwa chemiczna", "Liczba publikacji"))
```

### **Geny**

```{r echo = F, eval= T, fig.width=4}
general %>%
    filter(label == "gene") %>%
group_by(item) %>%
    summarise(n = n_distinct(PMID)) %>%
    arrange(desc(n)) %>%
    head(15) %>%
    kableExtra::kbl(longtable = F, col.names = c("Nazwa genu", "Liczba publikacji"))


```


Row
-----------------------------------------------------------------------



```{r echo = F, eval=F}
general %>%
    mutate("http" = "https://doi.org/") %>%
    unite("Link", http, DI, sep = "") %>%
    separate(AU, sep = ";", into = "Autor") %>%
    select("Kategoria" = label, "Nazwa" = item, "Tytuł" = TI, "Typ dokumentu" = DT, Autor, "Źródło" = SO, "Słowa kluczowe" = ID, PMID, "Rok" = PY, Link) %>%
    datatable(editable = T, extensions = 'Buttons', filter = 'top',
              options = list(dom = 'Blfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             lengthMenu = list(c(10,25,50,-1),
                                               c(10,25,50,"All"))))
```


Row
-----------------------------------------------------------------------



Badania obserwacyjne
=========================================



#Badania przedkliniczne
=========================================




Diagnostyka
=========================================


Eksploracja danych
=========================================

Inputs {.sidebar}
-----------------------------------------------------------------------

```{r echo=F, eval=F}
sliderInput('sampleSize', 'Liczba wystąpień obserwacji', min=1, max=nrow(dataset), dragRange = F,
            value=min(1000, nrow(dataset)), step=500, round=0)

checkboxInput('jitter', 'Jitter', value = TRUE)
checkboxInput('smooth', 'Smooth', value = TRUE)

selectInput('x', 'X', names(dataset))
selectInput('y', 'Y', names(dataset), names(dataset)[[2]])
selectInput('color', 'Kolor', c('None', names(dataset)))
selectInput('shape', 'Kształt', c('None', names(dataset)))


selectInput('facet_row', 'Podziałwzględem wiersza',
            c(None='.', names(dataset[sapply(dataset, is.factor)])))
selectInput('facet_col', 'Podziałwzględem kolumny',
            c(None='.', names(dataset[sapply(dataset, is.factor)])))
```

Outputs
-----------------------------------------------------------------------

### Eksploracja danych


```{r echo = F, fig.height=8}
dataset1 <- reactive({
  dataset[sample(nrow(dataset), input$sampleSize),]
})

renderPlot({
  p <- ggplot(dataset1(), aes_string(x=input$x, y=input$y)) + geom_point() + theme_minimal(base_size =15, )
  
  if (input$color != 'None')
    p <- p + aes_string(color=input$color)
  
  
  if (input$shape != 'None')
    p <- p + aes_string(shape=input$shape)
  
  facets <- paste(input$facet_row, '~', input$facet_col)
  if (facets != '. ~ .')
    p <- p + facet_grid(facets)
  
  if (input$jitter)
    p <- p + geom_jitter()
  if (input$smooth)
    p <- p + geom_smooth()
  
  print(p)
})

```
