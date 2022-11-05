Assignment 03: Web Scraping and Text Mining
================
Xiaofan Zhu
2022-11-04

``` r
library(httr)
library(rvest)
library(stringr)
library(xml2)
library(tidyverse)
library(tidytext)
library(forcats)
require(gridExtra)
```

## APIS

### Using the NCBI API, look for papers that show up under the term “sars-cov-2 trial vaccine.” Look for the data in the pubmed database, and then retrieve the details of the paper as shown in lab 7. How many papers were you able to find?

``` r
# Downloading the website
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2%20trial%20vaccine")
# Finding the counts
count1 <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]")
# Turning it into text
count1 <- as.character(count1)
# Extracting the data using regex
stringr::str_extract(count1, "[0-9,]+")
```

    ## [1] "4,009"

``` r
paste0("I have found ", count1, " papers that show up under the term: sars-cov-2 trial vaccine using rvest")
```

    ## [1] "I have found <div class=\"results-amount\">\n  \n    <span class=\"value\">4,009</span>\n    results\n  \n</div> papers that show up under the term: sars-cov-2 trial vaccine using rvest"

### Using the list of pubmed ids you retrieved, download each papers’ details using the query parameter rettype = abstract. If you get more than 250 ids, just keep the first 250.

``` r
# Pubmed ID number
query_ids <- GET(
  url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    term = "sars-cov-2 trial vaccine",
    retmax = 250) 
)
# Extracting the content of the response of GET
ids <- httr::content(query_ids) %>% 
  rvest::html_elements("Id") %>%
  as.character() %>%
  str_extract("[:digit:]+")
publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi", # the link now will be using efetch, instead of esearch
  query = list(
    db = "pubmed",
    id = str_c(ids, collapse = ","),
    rettype = "abstract"
    )
)
# Turning the output into character vector
publications <- httr::content(publications) %>% xml2::xml_children()
publications_txt <- as.character(publications)
```

``` r
# article titles
titles <- str_extract(publications_txt, "<ArticleTitle>.*</ArticleTitle>") %>% str_remove_all("</?[:alpha:]*>")
# names of journals
journals <- str_extract(publications_txt, "<ISOAbbreviation>.*</ISOAbbreviation>") %>% str_remove_all("</?[:alpha:]+>")
# publication dates
dates <- str_remove_all(publications_txt, "\\n") %>% 
  str_extract("<PubDate>.*</PubDate>") %>% 
  str_squish() %>% 
  str_remove_all("</?[:alpha:]+>") %>% 
  str_trim(side = "both")
# abstract texts
abstracts <- str_remove_all(publications_txt, "\\n") %>% 
  str_extract("<Abstract>.{1,}?</Abstract>") %>%
  str_remove_all("(</?AbstractText Label.{1,}?>|</?Abstract>|</?AbstractText>)") %>%
  str_trim(side = "both")
```

``` r
df <- data.frame(
  PubMed_Id = ids,
  title = titles,
  journal = journals,
  publication_date = dates,
  abstract = abstracts,
  check.rows = FALSE
)
```

``` r
head(df, n = 3) %>%
  knitr::kable()
```

| PubMed_Id | title                                                                                                                                                          | journal        | publication_date | abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------|:-----------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 36328399  | Immunogenicity and safety of a three-dose SARS-CoV-2 vaccination strategy in patients with immune-mediated inflammatory diseases on immunosuppressive therapy. | RMD Open       | 2022 Nov         | Humoral vaccine responses to SARS-CoV-2 vaccines are impaired and short lasting in patients with immune-mediated inflammatory diseases (IMID) following two vaccine doses. To protect these vulnerable patients against severe COVID-19 disease, a three-dose primary vaccination strategy has been implemented in many countries. The aim of this study was to evaluate humoral response and safety of primary vaccination with three doses in patients with IMID. Patients with IMID on immunosuppressive therapy and healthy controls receiving three-dose and two-dose primary SARS-CoV-2 vaccination, respectively, were included in this prospective observational cohort study. Anti-Spike antibodies were assessed 2-4 weeks, and 12 weeks following each dose. The main outcome was anti-Spike antibody levels 2-4 weeks following three doses in patients with IMID and two doses in controls. Additional outcomes were the antibody decline rate and adverse events. 1100 patients and 303 controls were included. Following three-dose vaccination, patients achieved median (IQR) antibody levels of 5720 BAU/mL (2138-8732) compared with 4495 (1591-6639) in controls receiving two doses, p=0.27. Anti-Spike antibody levels increased with median 1932 BAU/mL (IQR 150-4978) after the third dose. The interval between the vaccine doses and vaccination with mRNA-1273 or a combination of vaccines were associated with antibody levels following the third dose. Antibody levels had a slower decline-rate following the third than the second vaccine dose, p\<0.001. Adverse events were reported by 464 (47%) patients and by 196 (78%) controls. Disease flares were reported by 70 (7%) patients. This study shows that additional vaccine doses to patients with IMID contribute to strong and sustained immune-responses comparable to healthy persons vaccinated twice, and supports repeated vaccination of patients with IMID. NCT04798625. <CopyrightInformation>© Author(s) (or their employer(s)) 2022. Re-use permitted under CC BY-NC. No commercial re-use. See rights and permissions. Published by BMJ.</CopyrightInformation>                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| 36327352  | S-217622, a SARS-CoV-2 main protease inhibitor, decreases viral load and ameliorates COVID-19 severity in hamsters.                                            | Sci Transl Med | 2022 Nov 03      | In parallel with vaccination, oral antiviral agents are highly anticipated to act as countermeasures for the treatment of the coronavirus disease 2019 (COVID-19) pandemic caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2). Oral antiviral medication demands not only high antiviral activity, but also target specificity, favorable oral bioavailability, and high metabolic stability. Although a large number of compounds have been identified as potential inhibitors of SARS-CoV-2 infection in vitro, few have proven to be effective in vivo. Here, we show that oral administration of S-217622 (ensitrelvir), an inhibitor of SARS-CoV-2 main protease (M<sup>pro</sup>, also known as 3C-like protease), decreases viral load and ameliorates disease severity in SARS-CoV-2-infected hamsters. S-217622 inhibited viral proliferation at low nanomolar to sub-micromolar concentrations in cells. Oral administration of S-217622 demonstrated favorable pharmacokinetic properties and accelerated recovery from acute SARS-CoV-2 infection in hamster recipients. Moreover, S-217622 exerted antiviral activity against SARS-CoV-2 variants of concern (VOCs), including the highly pathogenic Delta variant and the recently emerged Omicron BA.5 and BA.2.75 variants. Overall, our study provides evidence that S-217622, an antiviral agent that is under evaluation in a phase 3 clinical trial (clinical trial registration no. jRCT2031210350), possesses remarkable antiviral potency and efficacy against SARS-CoV-2 and is a prospective oral therapeutic option for COVID-19.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| 36322837  | Covid-19 Vaccine Protection among Children and Adolescents in Qatar.                                                                                           | N Engl J Med   | 2022 Nov 02      | The BNT162b2 vaccine against coronavirus disease 2019 (Covid-19) has been authorized for use in children 5 to 11 years of age and adolescents 12 to 17 years of age but in different antigen doses. We assessed the real-world effectiveness of the BNT162b2 vaccine against infection with severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) among children and adolescents in Qatar. To compare the incidence of SARS-CoV-2 infection in the national cohort of vaccinated participants with the incidence in the national cohort of unvaccinated participants, we conducted three matched, retrospective, target-trial, cohort studies - one assessing data obtained from children 5 to 11 years of age after the B.1.1.529 (omicron) variant became prevalent and two assessing data from adolescents 12 to 17 years of age before the emergence of the omicron variant (pre-omicron study) and after the omicron variant became prevalent. Associations were estimated with the use of Cox proportional-hazards regression models. Among children, the overall effectiveness of the 10-μg primary vaccine series against infection with the omicron variant was 25.7% (95% confidence interval \[CI\], 10.0 to 38.6). Effectiveness was highest (49.6%; 95% CI, 28.5 to 64.5) right after receipt of the second dose but waned rapidly thereafter and was negligible after 3 months. Effectiveness was 46.3% (95% CI, 21.5 to 63.3) among children 5 to 7 years of age and 16.6% (95% CI, -4.2 to 33.2) among those 8 to 11 years of age. Among adolescents, the overall effectiveness of the 30-μg primary vaccine series against infection with the omicron variant was 30.6% (95% CI, 26.9 to 34.1), but many adolescents had been vaccinated months earlier. Effectiveness waned over time since receipt of the second dose. Effectiveness was 35.6% (95% CI, 31.2 to 39.6) among adolescents 12 to 14 years of age and 20.9% (95% CI, 13.8 to 27.4) among those 15 to 17 years of age. In the pre-omicron study, the overall effectiveness of the 30-μg primary vaccine series against SARS-CoV-2 infection among adolescents was 87.6% (95% CI, 84.0 to 90.4) and waned relatively slowly after receipt of the second dose. Vaccination in children was associated with modest, rapidly waning protection against omicron infection. Vaccination in adolescents was associated with stronger, more durable protection, perhaps because of the larger antigen dose. (Funded by Weill Cornell Medicine-Qatar and others.). <CopyrightInformation>Copyright © 2022 Massachusetts Medical Society.</CopyrightInformation> |

## Text Mining

### A new dataset has been added to the data science data repository <https://github.com/USCbiostats/data-science-data/tree/master/03_pubmed>. The dataset contains 3241 abstracts from articles across 5 search terms. Your job is to analyse these abstracts to find interesting insights.

1.  Tokenize the abstracts and count the number of each token. Do you
    see anything interesting? Does removing stop words change what
    tokens appear as the most frequent? What are the 5 most common
    tokens for each search term after removing stopwords?

``` r
pm <- read_csv("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/03_pubmed/pubmed.csv", col_types = c("c", "c"))
```

``` r
pm %>%
  unnest_tokens(token, abstract) %>%
  count(token) %>%
  top_n(10, n) %>%
  ggplot(aes(n, fct_reorder(token, n))) +
  geom_col()
```

![](README_files/figure-gfm/tokenize%20abstracts-1.png)<!-- --> I think
“covid” and “19” are interesting, since there are too much stop words.

Removing stip words:

``` r
pm %>%
  unnest_tokens(token, abstract) %>%
  group_by(term) %>%
  count(token, sort = TRUE) %>%
  filter(!(token %in% stop_words$word) & !grepl(pattern = "^[0-9]+$", x = token)) %>%
  top_n(5, n) %>%
  arrange(term, desc(n)) %>%
knitr::kable()
```

| term            | token        |    n |
|:----------------|:-------------|-----:|
| covid           | covid        | 7275 |
| covid           | patients     | 2293 |
| covid           | disease      |  943 |
| covid           | pandemic     |  800 |
| covid           | coronavirus  |  647 |
| covid           | health       |  647 |
| cystic fibrosis | fibrosis     |  867 |
| cystic fibrosis | cystic       |  862 |
| cystic fibrosis | cf           |  625 |
| cystic fibrosis | patients     |  586 |
| cystic fibrosis | disease      |  400 |
| meningitis      | patients     |  446 |
| meningitis      | meningitis   |  429 |
| meningitis      | meningeal    |  219 |
| meningitis      | csf          |  206 |
| meningitis      | clinical     |  187 |
| preeclampsia    | pre          | 2038 |
| preeclampsia    | eclampsia    | 2005 |
| preeclampsia    | preeclampsia | 1863 |
| preeclampsia    | women        | 1196 |
| preeclampsia    | pregnancy    |  969 |
| prostate cancer | cancer       | 3840 |
| prostate cancer | prostate     | 3832 |
| prostate cancer | patients     |  934 |
| prostate cancer | treatment    |  926 |
| prostate cancer | disease      |  652 |

I also created a list which contains the 5 most common tokens for each
search term.

2.  Tokenize the abstracts into bigrams. Find the 10 most common bigram
    and visualize them with ggplot2.

``` r
pm %>%
  unnest_ngrams(bigram, abstract, n = 2) %>%
  count(bigram, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = n, y = fct_reorder(bigram, n))) +
  geom_col() +
  labs(y = "bigram", title = "Top 10 Most Common Bigrams in PubMed Data Set") +
  theme_classic()
```

![](README_files/figure-gfm/top%2010%20bigrams-1.png)<!-- -->

``` r
pm %>%
  unnest_ngrams(bigram, abstract, n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  anti_join(stop_words, by = c("word1" = "word")) %>%
  anti_join(stop_words, by = c("word2" = "word")) %>%
  filter(!grepl(pattern = "^[0-9]+$", x = word1)) %>%
  filter(!grepl(pattern = "^[0-9]+$", x = word2)) %>%
  unite(bigram, c("word1", "word2"), sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = n, y = fct_reorder(bigram, n))) +
  geom_col() +
  labs(y = "bigram", title = "Top 10 Most Common Bigrams in PubMed Data Set (Stop Words Removed)") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10))
```

![](README_files/figure-gfm/top%2010%20bigrams%20after%20removing%20stop%20words-1.png)<!-- -->
After removing stop words, we can see health-related bigrams such as
“prostate cancer”, “coronavirus disease” and “pre eclampsia” are common
terms in the data set.

3.  Calculate the TF-IDF value for each word-search term combination.
    (here you want the search term to be the “document”) What are the 5
    tokens from each search term with the highest TF-IDF value? How are
    the results different from the answers you got in question 1?

``` r
pm %>%
  unnest_tokens(token, abstract) %>%
  group_by(term) %>%
  count(token, sort = TRUE) %>%
  filter(!(token %in% stop_words$word)) %>%
  top_n(5, n) %>%
  bind_tf_idf(token, term, n) %>%
  arrange(term, desc(tf_idf)) %>%
knitr::kable()
```

| term            | token        |    n |        tf |       idf |    tf_idf |
|:----------------|:-------------|-----:|----------:|----------:|----------:|
| covid           | covid        | 7275 | 0.3965442 | 1.6094379 | 0.6382133 |
| covid           | 19           | 7035 | 0.3834623 | 1.6094379 | 0.6171588 |
| covid           | pandemic     |  800 | 0.0436062 | 1.6094379 | 0.0701815 |
| covid           | patients     | 2293 | 0.1249864 | 0.2231436 | 0.0278899 |
| covid           | disease      |  943 | 0.0514009 | 0.5108256 | 0.0262569 |
| cystic fibrosis | fibrosis     |  867 | 0.2595808 | 1.6094379 | 0.4177792 |
| cystic fibrosis | cystic       |  862 | 0.2580838 | 1.6094379 | 0.4153699 |
| cystic fibrosis | cf           |  625 | 0.1871257 | 1.6094379 | 0.3011673 |
| cystic fibrosis | disease      |  400 | 0.1197605 | 0.5108256 | 0.0611767 |
| cystic fibrosis | patients     |  586 | 0.1754491 | 0.2231436 | 0.0391503 |
| meningitis      | meningitis   |  429 | 0.2885003 | 1.6094379 | 0.4643234 |
| meningitis      | meningeal    |  219 | 0.1472764 | 1.6094379 | 0.2370322 |
| meningitis      | csf          |  206 | 0.1385340 | 1.6094379 | 0.2229618 |
| meningitis      | clinical     |  187 | 0.1257566 | 1.6094379 | 0.2023974 |
| meningitis      | patients     |  446 | 0.2999328 | 0.2231436 | 0.0669281 |
| preeclampsia    | pre          | 2038 | 0.2525090 | 1.6094379 | 0.4063975 |
| preeclampsia    | eclampsia    | 2005 | 0.2484203 | 1.6094379 | 0.3998170 |
| preeclampsia    | preeclampsia | 1863 | 0.2308264 | 1.6094379 | 0.3715008 |
| preeclampsia    | women        | 1196 | 0.1481849 | 1.6094379 | 0.2384943 |
| preeclampsia    | pregnancy    |  969 | 0.1200595 | 1.6094379 | 0.1932283 |
| prostate cancer | cancer       | 3840 | 0.3770621 | 1.6094379 | 0.6068580 |
| prostate cancer | prostate     | 3832 | 0.3762765 | 1.6094379 | 0.6055937 |
| prostate cancer | treatment    |  926 | 0.0909269 | 1.6094379 | 0.1463413 |
| prostate cancer | disease      |  652 | 0.0640220 | 0.5108256 | 0.0327041 |
| prostate cancer | patients     |  934 | 0.0917125 | 0.2231436 | 0.0204651 |

This list shows the top 5 tokens with the highest TF-IDF values for each
search term. The results are more relevant to their terminology, and
it’s easier to pinpoint their primary tokens at a glance than the
approach in Question 1.
