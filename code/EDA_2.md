R Notebook
================

``` r
library(ggplot2)
library(magrittr)
library(forcats)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
df_bank <- read.csv("bank-full.csv", sep=";")
```

``` r
df_bank %>%
  mutate(y = fct_relevel(y, "yes", "no")) %>%
  ggplot(aes(x = job)) +
  geom_bar(aes(fill = y), position = "stack", stat="count") + 
  geom_text(aes(label = after_stat(count), group = y), stat = "count", position = "stack", vjust = -.2) +
  theme(axis.text.x = element_text(vjust=1, hjust = 1, angle=70)) +
  coord_cartesian(ylim = c(0, 10500))
```

![](EDA_2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
library(ggstats)
```

``` r
df_bank %>%
  mutate(y = fct_relevel(y, "yes", "no")) %>%
  ggplot(aes(x = job)) +
  geom_bar(aes(fill = y), position = "fill", stat="count") +
  geom_text(stat = "prop", position = position_fill(.5)) + 
  theme(axis.text.x = element_text(vjust=1, hjust = 1, angle=70))
```

![](EDA_2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
library(ggplot2)

ggplot(mtcars, aes(mpg)) +
  geom_histogram(bins = 30) +
  stat_summary(
    aes(xintercept = after_stat(x), y = 0),
    fun = mean,
    geom = "vline",
    orientation = "y"
  ) +
  geom_text(
    x = 20.1, y = 4.75,
    label = "mean: 20.1"
  )
```

![](EDA_2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
p <- df_bank %>% ggplot(aes(x = job, color = y)) +
  geom_bar(stat = "count") +
  geom_text(aes(label = after_stat(max(prop))), stat = "prop")

ggplot_build(p)
```

    ## $data
    ## $data[[1]]
    ##     colour    y count prop  x flipped_aes PANEL group ymin ymax  xmin  xmax
    ## 1  #F8766D 5171  4540    1  1       FALSE     1     1  631 5171  0.55  1.45
    ## 2  #00BFC4  631   631    1  1       FALSE     1     2    0  631  0.55  1.45
    ## 3  #F8766D 9732  9024    1  2       FALSE     1     3  708 9732  1.55  2.45
    ## 4  #00BFC4  708   708    1  2       FALSE     1     4    0  708  1.55  2.45
    ## 5  #F8766D 1487  1364    1  3       FALSE     1     5  123 1487  2.55  3.45
    ## 6  #00BFC4  123   123    1  3       FALSE     1     6    0  123  2.55  3.45
    ## 7  #F8766D 1240  1131    1  4       FALSE     1     7  109 1240  3.55  4.45
    ## 8  #00BFC4  109   109    1  4       FALSE     1     8    0  109  3.55  4.45
    ## 9  #F8766D 9458  8157    1  5       FALSE     1     9 1301 9458  4.55  5.45
    ## 10 #00BFC4 1301  1301    1  5       FALSE     1    10    0 1301  4.55  5.45
    ## 11 #F8766D 2264  1748    1  6       FALSE     1    11  516 2264  5.55  6.45
    ## 12 #00BFC4  516   516    1  6       FALSE     1    12    0  516  5.55  6.45
    ## 13 #F8766D 1579  1392    1  7       FALSE     1    13  187 1579  6.55  7.45
    ## 14 #00BFC4  187   187    1  7       FALSE     1    14    0  187  6.55  7.45
    ## 15 #F8766D 4154  3785    1  8       FALSE     1    15  369 4154  7.55  8.45
    ## 16 #00BFC4  369   369    1  8       FALSE     1    16    0  369  7.55  8.45
    ## 17 #F8766D  938   669    1  9       FALSE     1    17  269  938  8.55  9.45
    ## 18 #00BFC4  269   269    1  9       FALSE     1    18    0  269  8.55  9.45
    ## 19 #F8766D 7597  6757    1 10       FALSE     1    19  840 7597  9.55 10.45
    ## 20 #00BFC4  840   840    1 10       FALSE     1    20    0  840  9.55 10.45
    ## 21 #F8766D 1303  1101    1 11       FALSE     1    21  202 1303 10.55 11.45
    ## 22 #00BFC4  202   202    1 11       FALSE     1    22    0  202 10.55 11.45
    ## 23 #F8766D  288   254    1 12       FALSE     1    23   34  288 11.55 12.45
    ## 24 #00BFC4   34    34    1 12       FALSE     1    24    0   34 11.55 12.45
    ##      fill linewidth linetype alpha
    ## 1  grey35       0.5        1    NA
    ## 2  grey35       0.5        1    NA
    ## 3  grey35       0.5        1    NA
    ## 4  grey35       0.5        1    NA
    ## 5  grey35       0.5        1    NA
    ## 6  grey35       0.5        1    NA
    ## 7  grey35       0.5        1    NA
    ## 8  grey35       0.5        1    NA
    ## 9  grey35       0.5        1    NA
    ## 10 grey35       0.5        1    NA
    ## 11 grey35       0.5        1    NA
    ## 12 grey35       0.5        1    NA
    ## 13 grey35       0.5        1    NA
    ## 14 grey35       0.5        1    NA
    ## 15 grey35       0.5        1    NA
    ## 16 grey35       0.5        1    NA
    ## 17 grey35       0.5        1    NA
    ## 18 grey35       0.5        1    NA
    ## 19 grey35       0.5        1    NA
    ## 20 grey35       0.5        1    NA
    ## 21 grey35       0.5        1    NA
    ## 22 grey35       0.5        1    NA
    ## 23 grey35       0.5        1    NA
    ## 24 grey35       0.5        1    NA
    ## 
    ## $data[[2]]
    ##     colour     label    y  x PANEL group by count         prop width
    ## 1  #F8766D 0.1995974 4540  1     1     1  1  4540 0.1004180399   0.9
    ## 2  #00BFC4 0.1995974  631  1     1     2  1   631 0.0139567804   0.9
    ## 3  #F8766D 0.1995974 9024  2     1     3  1  9024 0.1995974431   0.9
    ## 4  #00BFC4 0.1995974  708  2     1     4  1   708 0.0156599058   0.9
    ## 5  #F8766D 0.1995974 1364  3     1     5  1  1364 0.0301696490   0.9
    ## 6  #00BFC4 0.1995974  123  3     1     6  1   123 0.0027205769   0.9
    ## 7  #F8766D 0.1995974 1131  4     1     7  1  1131 0.0250160359   0.9
    ## 8  #00BFC4 0.1995974  109  4     1     8  1   109 0.0024109177   0.9
    ## 9  #F8766D 0.1995974 8157  5     1     9  1  8157 0.1804206941   0.9
    ## 10 #00BFC4 0.1995974 1301  5     1    10  1  1301 0.0287761828   0.9
    ## 11 #F8766D 0.1995974 1748  6     1    11  1  1748 0.0386631572   0.9
    ## 12 #00BFC4 0.1995974  516  6     1    12  1   516 0.0114131517   0.9
    ## 13 #F8766D 0.1995974 1392  7     1    13  1  1392 0.0307889673   0.9
    ## 14 #00BFC4 0.1995974  187  7     1    14  1   187 0.0041361616   0.9
    ## 15 #F8766D 0.1995974 3785  8     1    15  1  3785 0.0837185641   0.9
    ## 16 #00BFC4 0.1995974  369  8     1    16  1   369 0.0081617306   0.9
    ## 17 #F8766D 0.1995974  669  9     1    17  1   669 0.0147972838   0.9
    ## 18 #00BFC4 0.1995974  269  9     1    18  1   269 0.0059498795   0.9
    ## 19 #F8766D 0.1995974 6757 10     1    19  1  6757 0.1494547787   0.9
    ## 20 #00BFC4 0.1995974  840 10     1    20  1   840 0.0185795492   0.9
    ## 21 #F8766D 0.1995974 1101 11     1    21  1  1101 0.0243524806   0.9
    ## 22 #00BFC4 0.1995974  202 11     1    22  1   202 0.0044679392   0.9
    ## 23 #F8766D 0.1995974  254 12     1    23  1   254 0.0056181018   0.9
    ## 24 #00BFC4 0.1995974   34 12     1    24  1    34 0.0007520294   0.9
    ##    flipped_aes size angle hjust vjust alpha family fontface lineheight
    ## 1        FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 2        FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 3        FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 4        FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 5        FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 6        FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 7        FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 8        FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 9        FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 10       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 11       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 12       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 13       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 14       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 15       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 16       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 17       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 18       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 19       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 20       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 21       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 22       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 23       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 24       FALSE 3.88     0   0.5   0.5    NA               1        1.2
    ## 
    ## 
    ## $layout
    ## <ggproto object: Class Layout, gg>
    ##     coord: <ggproto object: Class CoordCartesian, Coord, gg>
    ##         aspect: function
    ##         backtransform_range: function
    ##         clip: on
    ##         default: TRUE
    ##         distance: function
    ##         expand: TRUE
    ##         is_free: function
    ##         is_linear: function
    ##         labels: function
    ##         limits: list
    ##         modify_scales: function
    ##         range: function
    ##         render_axis_h: function
    ##         render_axis_v: function
    ##         render_bg: function
    ##         render_fg: function
    ##         setup_data: function
    ##         setup_layout: function
    ##         setup_panel_guides: function
    ##         setup_panel_params: function
    ##         setup_params: function
    ##         train_panel_guides: function
    ##         transform: function
    ##         super:  <ggproto object: Class CoordCartesian, Coord, gg>
    ##     coord_params: list
    ##     facet: <ggproto object: Class FacetNull, Facet, gg>
    ##         compute_layout: function
    ##         draw_back: function
    ##         draw_front: function
    ##         draw_labels: function
    ##         draw_panels: function
    ##         finish_data: function
    ##         init_scales: function
    ##         map_data: function
    ##         params: list
    ##         setup_data: function
    ##         setup_params: function
    ##         shrink: TRUE
    ##         train_scales: function
    ##         vars: function
    ##         super:  <ggproto object: Class FacetNull, Facet, gg>
    ##     facet_params: list
    ##     finish_data: function
    ##     get_scales: function
    ##     layout: data.frame
    ##     map_position: function
    ##     panel_params: list
    ##     panel_scales_x: list
    ##     panel_scales_y: list
    ##     render: function
    ##     render_labels: function
    ##     reset_scales: function
    ##     resolve_label: function
    ##     setup: function
    ##     setup_panel_guides: function
    ##     setup_panel_params: function
    ##     train_position: function
    ##     super:  <ggproto object: Class Layout, gg>
    ## 
    ## $plot

![](EDA_2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## 
    ## attr(,"class")
    ## [1] "ggplot_built"

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ lubridate 1.9.3     ✔ stringr   1.5.1
    ## ✔ purrr     1.0.2     ✔ tibble    3.2.1
    ## ✔ readr     2.1.5     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ tidyr::extract()   masks magrittr::extract()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ purrr::set_names() masks magrittr::set_names()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
df <- data.frame( x = 1:12,
                  y = runif(12)
                  )
df %>% 
  mutate( x = factor(month.abb[x], levels = month.abb)) %T>% 
  print() %>% 
ggplot(., aes(x, y))+
  geom_col()
```

    ##      x         y
    ## 1  Jan 0.6442048
    ## 2  Feb 0.8067150
    ## 3  Mar 0.1659892
    ## 4  Apr 0.9598578
    ## 5  May 0.4183623
    ## 6  Jun 0.2047298
    ## 7  Jul 0.7521428
    ## 8  Aug 0.4033091
    ## 9  Sep 0.3779411
    ## 10 Oct 0.2643270
    ## 11 Nov 0.5080883
    ## 12 Dec 0.3868742

![](EDA_2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
