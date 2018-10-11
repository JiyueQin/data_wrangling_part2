reading\_data\_from\_the\_web
================
JiyueQin
October 11, 2018

Extracting tables
=================

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
drug_use_xml = read_html(url)

drug_use_xml
```

    ## {xml_document}
    ## <html lang="en">
    ## [1] <head>\n<link rel="P3Pv1" href="http://www.samhsa.gov/w3c/p3p.xml">\ ...
    ## [2] <body>\r\n\r\n<noscript>\r\n<p>Your browser's Javascript is off. Hyp ...

``` r
## try to get tables
drug_use_xml %>%
  html_nodes(css = "table") # we extract all the tables.
```

    ## {xml_nodeset (15)}
    ##  [1] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [2] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [3] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [4] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [5] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [6] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [7] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [8] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ##  [9] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [10] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [11] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [12] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [13] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [14] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...
    ## [15] <table class="rti" border="1" cellspacing="0" cellpadding="1" width ...

``` r
table_marj = (drug_use_xml %>% html_nodes(css = "table"))[[1]] %>%
  html_table()  # sometimes works if lucky. generate the 
class(table_marj) 
```

    ## [1] "data.frame"

``` r
table_marj = (drug_use_xml %>% html_nodes(css = "table"))[[1]] %>%
  html_table() %>%
  .[-1,] %>% 
  as_tibble()

table_marj = (drug_use_xml %>% html_nodes(css = "table"))[[1]] %>%
  html_table() %>%
  slice(-1) %>%       
  as_tibble()
```

`slice()` can select rows by numbers, but you often want to filter by name because the numbers can change. thus, using filter is more safer.

``` r
url_2 = "https://www.bestplaces.net/cost_of_living/city/new_york/new_york"
newyork_xml = read_html(url_2)
newyork_table = newyork_xml %>% 
  html_nodes(css = "table") %>%
  .[[1]] %>%  
  html_table(header = TRUE)

# this doesn't work.
#newyork_table = newyork_xml %>% 
  #html_nodes(css = "table")[[1]] 
```

CSS selectors
=============

``` r
hpsaga_html = read_html("https://www.imdb.com/list/ls000630791/")

title_vec = hpsaga_html %>%
  html_nodes(".lister-item-header a") %>%
  html_text()

gross_rev_vec = hpsaga_html %>%
  html_nodes(".text-small:nth-child(7) span:nth-child(5)") %>%
  html_text()

runtime_vec = hpsaga_html %>%
  html_nodes(".runtime") %>%
  html_text()

hpsata_df = tibble(
  title = title_vec,
  rev = gross_rev_vec,
  runtime = runtime_vec
)
review_html = read_html("https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1")
review_title = review_html %>% 
  html_nodes("#cm_cr-review_list .a-color-base") %>% 
  html_text()
star = review_html %>% 
  html_nodes("#cm_cr-review_list .review-rating") %>% 
  html_text()
review_text = review_html %>% 
  html_nodes(".review-text") %>% 
  html_text()
review_df = tibble(review_title, star, review_text)

review_html = read_html("https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2")
review_title = review_html %>% 
  html_nodes("#cm_cr-review_list .a-color-base") %>% 
  html_text()
star = review_html %>% 
  html_nodes("#cm_cr-review_list .review-rating") %>% 
  html_text()
review_text = review_html %>% 
  html_nodes(".review-text") %>% 
  html_text()
tibble(review_title, star, review_text)
```

    ## # A tibble: 10 x 3
    ##    review_title              star        review_text                      
    ##    <chr>                     <chr>       <chr>                            
    ##  1 The greatest!             5.0 out of~ This is such a classic movie! it~
    ##  2 Five Stars                5.0 out of~ Favorite movie                   
    ##  3 Napoleon Dynamite is an ~ 5.0 out of~ Great entertainment.             
    ##  4 One Star                  1.0 out of~ The You Tube clips are funnier t~
    ##  5 "\U0001f62b"              5.0 out of~ Love this movie                  
    ##  6 Five Stars                5.0 out of~ Book report                      
    ##  7 Haha                      5.0 out of~ "\"Tina come get some ham\""     
    ##  8 Five Stars                5.0 out of~ Everyone loves Napoleon!         
    ##  9 Five Stars                5.0 out of~ Classic … still watch it.        
    ## 10 Hilarious                 5.0 out of~ Hilarious

Using an API
============

``` r
nyc_water = GET("https://data.cityofnewyork.us/resource/waf7-5gvc.csv") %>% 
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   new_york_city_population = col_double(),
    ##   nyc_consumption_million_gallons_per_day = col_double(),
    ##   per_capita_gallons_per_person_per_day = col_integer(),
    ##   year = col_integer()
    ## )

``` r
nyc_water = GET("https://data.cityofnewyork.us/resource/waf7-5gvc.json") %>% 
  content("text") %>%
  jsonlite::fromJSON() %>%
  as_tibble()

# using the download addresss
ny= GET("https://data.cityofnewyork.us/api/views/ia2d-e54m/rows.csv?accessType=DOWNLOAD") %>%
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_integer(),
    ##   `New York City Population` = col_double(),
    ##   `NYC Consumption(Million gallons per day)` = col_double(),
    ##   `Per Capita(Gallons per person per day)` = col_integer()
    ## )
