Women Research
================
jakinpilla
Wed Apr 17 22:37:08 2019

Import
    data

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 3.1.0     √ purrr   0.2.5
    ## √ tibble  1.4.2     √ dplyr   0.7.8
    ## √ tidyr   0.8.2     √ stringr 1.3.1
    ## √ readr   1.3.1     √ forcats 0.3.0

    ## -- Conflicts ------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(curl)
```

    ## Warning: package 'curl' was built under R version 3.5.3

    ## 
    ## Attaching package: 'curl'

    ## The following object is masked from 'package:readr':
    ## 
    ##     parse_date

``` r
women_research <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   field = col_character(),
    ##   percent_women = col_double()
    ## )

``` r
library(stringr)
women_research$field <-
  str_replace_all(women_research$field, 'Women inventores', 'Patent applications') %>%
  str_wrap(20)
```

Plot 1: Grouped by field

``` r
library(plotly)
```

    ## Warning: package 'plotly' was built under R version 3.5.3

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
women_research %>%
  plot_ly(x = ~percent_women * 100, y = ~field, color = ~country, type = 'bar') %>%
  layout(title = 'Share of published women researchers, by field (2011-2015)',
         barmode = 'group',
         xaxis = list(title = "Women's representation in academic publishing (2011-2015)",
                      ticksuffix = "%",
                      range = c(0,100)),
         yaxis = list(title = ''),
         font = list(family = 'Raleway',
                     color = '#34495e'),
         annotations = list(
           text = 'Data: Elsevier, via The Economist',
           x = 80,
           y = -0.4,
           showarrow = FALSE
         ))
```

    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
    ## Returning the palette you asked for with that many colors

    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
    ## Returning the palette you asked for with that many colors

<!--html_preserve-->

<div id="htmlwidget-389c381bb8b6b18c3079" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-389c381bb8b6b18c3079">{"x":{"visdat":{"3c18700379d":["function () ","plotlyVisDat"]},"cur_data":"3c18700379d","attrs":{"3c18700379d":{"x":{},"y":{},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Share of published women researchers, by field (2011-2015)","barmode":"group","xaxis":{"domain":[0,1],"automargin":true,"title":"Women's representation in academic publishing (2011-2015)","ticksuffix":"%","range":[0,100]},"yaxis":{"domain":[0,1],"automargin":true,"title":"","type":"category","categoryorder":"array","categoryarray":["Computer science,<br />maths","Engineering","Health sciences","Patent applications","Physical sciences"]},"font":{"family":"Raleway","color":"#34495e"},"annotations":[{"text":"Data: Elsevier, via The Economist","x":80,"y":-0.4,"showarrow":false}],"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[50,23,25,24,12],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"Australia","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[57,33,32,24,19],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"Brazil","marker":{"color":"rgba(211,164,122,1)","line":{"color":"rgba(211,164,122,1)"}},"textfont":{"color":"rgba(211,164,122,1)"},"error_y":{"color":"rgba(211,164,122,1)"},"error_x":{"color":"rgba(211,164,122,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[49,21,22,22,13],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"Canada","marker":{"color":"rgba(229,147,127,1)","line":{"color":"rgba(229,147,127,1)"}},"textfont":{"color":"rgba(229,147,127,1)"},"error_y":{"color":"rgba(229,147,127,1)"},"error_x":{"color":"rgba(229,147,127,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[43,23,22,16,19],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"Chile","marker":{"color":"rgba(156,159,194,1)","line":{"color":"rgba(156,159,194,1)"}},"textfont":{"color":"rgba(156,159,194,1)"},"error_y":{"color":"rgba(156,159,194,1)"},"error_x":{"color":"rgba(156,159,194,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[47,22,23,18,13],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"Denmark","marker":{"color":"rgba(194,150,199,1)","line":{"color":"rgba(194,150,199,1)"}},"textfont":{"color":"rgba(194,150,199,1)"},"error_y":{"color":"rgba(194,150,199,1)"},"error_x":{"color":"rgba(194,150,199,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[48,25,25,22,12],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"EU28","marker":{"color":"rgba(223,155,177,1)","line":{"color":"rgba(223,155,177,1)"}},"textfont":{"color":"rgba(223,155,177,1)"},"error_y":{"color":"rgba(223,155,177,1)"},"error_x":{"color":"rgba(223,155,177,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[48,24,25,22,17],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"France","marker":{"color":"rgba(182,204,108,1)","line":{"color":"rgba(182,204,108,1)"}},"textfont":{"color":"rgba(182,204,108,1)"},"error_y":{"color":"rgba(182,204,108,1)"},"error_x":{"color":"rgba(182,204,108,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[24,11,11,11,8],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"Japan","marker":{"color":"rgba(209,217,69,1)","line":{"color":"rgba(209,217,69,1)"}},"textfont":{"color":"rgba(209,217,69,1)"},"error_y":{"color":"rgba(209,217,69,1)"},"error_x":{"color":"rgba(209,217,69,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[46,25,26,22,18],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"Mexico","marker":{"color":"rgba(253,215,61,1)","line":{"color":"rgba(253,215,61,1)"}},"textfont":{"color":"rgba(253,215,61,1)"},"error_y":{"color":"rgba(253,215,61,1)"},"error_x":{"color":"rgba(253,215,61,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[57,37,36,27,26],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"Portugal","marker":{"color":"rgba(237,202,125,1)","line":{"color":"rgba(237,202,125,1)"}},"textfont":{"color":"rgba(237,202,125,1)"},"error_y":{"color":"rgba(237,202,125,1)"},"error_x":{"color":"rgba(237,202,125,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[45,21,22,21,12],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"United Kingdom","marker":{"color":"rgba(212,190,160,1)","line":{"color":"rgba(212,190,160,1)"}},"textfont":{"color":"rgba(212,190,160,1)"},"error_y":{"color":"rgba(212,190,160,1)"},"error_x":{"color":"rgba(212,190,160,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[46,20,22,22,14],"y":["Health sciences","Physical sciences","Engineering","Computer science,<br />maths","Patent applications"],"type":"bar","orientation":"h","name":"United States","marker":{"color":"rgba(179,179,179,1)","line":{"color":"rgba(179,179,179,1)"}},"textfont":{"color":"rgba(179,179,179,1)"},"error_y":{"color":"rgba(179,179,179,1)"},"error_x":{"color":"rgba(179,179,179,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

Plot 2: Grouped by country

``` r
women_research %>%
  plot_ly(x = ~percent_women * 100, y = ~country, color = ~field, type = 'bar') %>%
  layout(title = "Women's representation in academic publishing (2011-2015)",
         barmode = 'group',
         xaxis = list(title = 'Women, as % of total published authors',
                      ticksuffix = "%",
                      range = c(0,100)),
         yaxis = list(title = ''),
         font = list(family = 'Raleway',
                     color = '#34495e'),
         annotations = list(
           text = 'Data: Elsevier, via The Economist',
           x = 80,
           y = -0.4,
           showarrow = FALSE
         ))
```

<!--html_preserve-->

<div id="htmlwidget-9639c730a2a1a584c819" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-9639c730a2a1a584c819">{"x":{"visdat":{"3c18849e53":["function () ","plotlyVisDat"]},"cur_data":"3c18849e53","attrs":{"3c18849e53":{"x":{},"y":{},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Women's representation in academic publishing (2011-2015)","barmode":"group","xaxis":{"domain":[0,1],"automargin":true,"title":"Women, as % of total published authors","ticksuffix":"%","range":[0,100]},"yaxis":{"domain":[0,1],"automargin":true,"title":"","type":"category","categoryorder":"array","categoryarray":["Australia","Brazil","Canada","Chile","Denmark","EU28","France","Japan","Mexico","Portugal","United Kingdom","United States"]},"font":{"family":"Raleway","color":"#34495e"},"annotations":[{"text":"Data: Elsevier, via The Economist","x":80,"y":-0.4,"showarrow":false}],"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[11,16,21,22,22,18,22,22,22,24,24,27],"y":["Japan","Chile","United Kingdom","United States","Mexico","Denmark","EU28","France","Canada","Australia","Brazil","Portugal"],"type":"bar","orientation":"h","name":"Computer science,<br />maths","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[11,22,22,22,26,23,25,25,22,25,32,36],"y":["Japan","Chile","United Kingdom","United States","Mexico","Denmark","EU28","France","Canada","Australia","Brazil","Portugal"],"type":"bar","orientation":"h","name":"Engineering","marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[24,43,45,46,46,47,48,48,49,50,57,57],"y":["Japan","Chile","United Kingdom","United States","Mexico","Denmark","EU28","France","Canada","Australia","Brazil","Portugal"],"type":"bar","orientation":"h","name":"Health sciences","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[8,19,12,14,18,13,12,17,13,12,19,26],"y":["Japan","Chile","United Kingdom","United States","Mexico","Denmark","EU28","France","Canada","Australia","Brazil","Portugal"],"type":"bar","orientation":"h","name":"Patent applications","marker":{"color":"rgba(231,138,195,1)","line":{"color":"rgba(231,138,195,1)"}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[11,23,21,20,25,22,25,24,21,23,33,37],"y":["Japan","Chile","United Kingdom","United States","Mexico","Denmark","EU28","France","Canada","Australia","Brazil","Portugal"],"type":"bar","orientation":"h","name":"Physical sciences","marker":{"color":"rgba(166,216,84,1)","line":{"color":"rgba(166,216,84,1)"}},"textfont":{"color":"rgba(166,216,84,1)"},"error_y":{"color":"rgba(166,216,84,1)"},"error_x":{"color":"rgba(166,216,84,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->
