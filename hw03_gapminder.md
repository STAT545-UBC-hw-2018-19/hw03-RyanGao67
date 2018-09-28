STAT 545A Homework 3
================
Tian Gao
2018/9/27

# Bring rectangular data in

``` r
library(gapminder)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 3.0.0     √ purrr   0.2.5
    ## √ tibble  1.4.2     √ dplyr   0.7.6
    ## √ tidyr   0.8.1     √ stringr 1.3.1
    ## √ readr   1.1.1     √ forcats 0.3.0

    ## -- Conflicts ---------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

# Get the maximum and minimum of GDP per capita for all continents.

``` r
# get data of maximum and minimum of GDP per capita for all continents
gapminder %>% 
  # group by continent
  group_by(continent) %>% 
  # calculate summaries
  summarize(
    max_gdpPercap = max(gdpPercap),
    min_gdpPercap = min(gdpPercap),
  )
```

    ## # A tibble: 5 x 3
    ##   continent max_gdpPercap min_gdpPercap
    ##   <fct>             <dbl>         <dbl>
    ## 1 Africa           21951.          241.
    ## 2 Americas         42952.         1202.
    ## 3 Asia            113523.          331 
    ## 4 Europe           49357.          974.
    ## 5 Oceania          34435.        10040.

# Look at the spread of GDP per capita within the continents.

  - First I’ll show the table of min, max, mean, sd of GDP percap

<!-- end list -->

``` r
gapminder %>%
  group_by(continent) %>%
  summarize(
    min_GDPPercap = min(gdpPercap),
    max_GDPPercap = max(gdpPercap),
    mean_GDPPercap = mean(gdpPercap),
    sd_GDPPercap = sd(gdpPercap)
  )
```

    ## # A tibble: 5 x 5
    ##   continent min_GDPPercap max_GDPPercap mean_GDPPercap sd_GDPPercap
    ##   <fct>             <dbl>         <dbl>          <dbl>        <dbl>
    ## 1 Africa             241.        21951.          2194.        2828.
    ## 2 Americas          1202.        42952.          7136.        6397.
    ## 3 Asia               331        113523.          7902.       14045.
    ## 4 Europe             974.        49357.         14469.        9355.
    ## 5 Oceania          10040.        34435.         18622.        6359.

  - In order to show the spread of GDP per capita within the continents,
    a plot should be clear and easy to understand. I put the
    distribution of all continents in one plot, thus the difference is
    obvious.

<!-- end list -->

``` r
gapminder %>% 
  # gdpPercap as x axis
  ggplot(aes(x=gdpPercap, color=continent)) +
  # scale the x axis as the variance between continents is huge
  scale_x_log10()+
  # show the distribution of data
  geom_density()
```

![](hw03_gapminder_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

  - Then I’ll show the numbers in above table using box plot

<!-- end list -->

``` r
gapminder %>%
  ggplot(aes(x=continent, y=gdpPercap)) + 
  geom_boxplot()
```

![](hw03_gapminder_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> \#
Compute a trimmed mean of life expectancy for different years. Or a
weighted mean, weighting by population. Just try something other than
the plain vanilla mean. \* For this part I will calculate the weighted
mean, weighting by population

``` r
gapminder %>%
  group_by(year) %>%
  summarize(
    vanilla_mean_lifeExp = mean(lifeExp),
    weighted_mean_lifeExp = weighted.mean(lifeExp,pop)
  )
```

    ## # A tibble: 12 x 3
    ##     year vanilla_mean_lifeExp weighted_mean_lifeExp
    ##    <int>                <dbl>                 <dbl>
    ##  1  1952                 49.1                  48.9
    ##  2  1957                 51.5                  52.1
    ##  3  1962                 53.6                  52.3
    ##  4  1967                 55.7                  57.0
    ##  5  1972                 57.6                  59.5
    ##  6  1977                 59.6                  61.2
    ##  7  1982                 61.5                  62.9
    ##  8  1987                 63.2                  64.4
    ##  9  1992                 64.2                  65.6
    ## 10  1997                 65.0                  66.8
    ## 11  2002                 65.7                  67.8
    ## 12  2007                 67.0                  68.9

  - It is not obvious enough shown in the above table. Thus I try to put
    this into a plot.

<!-- end list -->

``` r
gapminder %>%
  group_by(year) %>%
  mutate(avgLifeExp = mean(lifeExp)) %>%
  ggplot(aes(x = continent, y = lifeExp)) + 
  geom_jitter(alpha = 0.5, aes(color = lifeExp>avgLifeExp))+ 
  facet_wrap( ~year)
```

![](hw03_gapminder_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> \#
How is life expectancy changing over time on different continents? \*
First I’ll document the life expectancy in a table for each continent \*
Asia

``` r
gapminder %>%
  filter(continent == 'Asia') %>%
  group_by(year) %>%
  summarize(Avg=mean(lifeExp))
```

    ## # A tibble: 12 x 2
    ##     year   Avg
    ##    <int> <dbl>
    ##  1  1952  46.3
    ##  2  1957  49.3
    ##  3  1962  51.6
    ##  4  1967  54.7
    ##  5  1972  57.3
    ##  6  1977  59.6
    ##  7  1982  62.6
    ##  8  1987  64.9
    ##  9  1992  66.5
    ## 10  1997  68.0
    ## 11  2002  69.2
    ## 12  2007  70.7

  - Africa

<!-- end list -->

``` r
gapminder %>%
  filter(continent == 'Africa') %>%
  group_by(year) %>%
  summarize(Avg=mean(lifeExp))
```

    ## # A tibble: 12 x 2
    ##     year   Avg
    ##    <int> <dbl>
    ##  1  1952  39.1
    ##  2  1957  41.3
    ##  3  1962  43.3
    ##  4  1967  45.3
    ##  5  1972  47.5
    ##  6  1977  49.6
    ##  7  1982  51.6
    ##  8  1987  53.3
    ##  9  1992  53.6
    ## 10  1997  53.6
    ## 11  2002  53.3
    ## 12  2007  54.8

  - Americas

<!-- end list -->

``` r
gapminder %>%
  filter(continent == 'Americas') %>%
  group_by(year) %>%
  summarize(Avg=mean(lifeExp))
```

    ## # A tibble: 12 x 2
    ##     year   Avg
    ##    <int> <dbl>
    ##  1  1952  53.3
    ##  2  1957  56.0
    ##  3  1962  58.4
    ##  4  1967  60.4
    ##  5  1972  62.4
    ##  6  1977  64.4
    ##  7  1982  66.2
    ##  8  1987  68.1
    ##  9  1992  69.6
    ## 10  1997  71.2
    ## 11  2002  72.4
    ## 12  2007  73.6

  - Europe

<!-- end list -->

``` r
gapminder %>%
  filter(continent == 'Europe') %>%
  group_by(year) %>%
  summarize(Avg=mean(lifeExp))
```

    ## # A tibble: 12 x 2
    ##     year   Avg
    ##    <int> <dbl>
    ##  1  1952  64.4
    ##  2  1957  66.7
    ##  3  1962  68.5
    ##  4  1967  69.7
    ##  5  1972  70.8
    ##  6  1977  71.9
    ##  7  1982  72.8
    ##  8  1987  73.6
    ##  9  1992  74.4
    ## 10  1997  75.5
    ## 11  2002  76.7
    ## 12  2007  77.6

  - Oceania

<!-- end list -->

``` r
gapminder %>%
  filter(continent == 'Oceania') %>%
  group_by(year) %>%
  summarize(Avg=mean(lifeExp))
```

    ## # A tibble: 12 x 2
    ##     year   Avg
    ##    <int> <dbl>
    ##  1  1952  69.3
    ##  2  1957  70.3
    ##  3  1962  71.1
    ##  4  1967  71.3
    ##  5  1972  71.9
    ##  6  1977  72.9
    ##  7  1982  74.3
    ##  8  1987  75.3
    ##  9  1992  76.9
    ## 10  1997  78.2
    ## 11  2002  79.7
    ## 12  2007  80.7

  - As table is not obvious enough to show the data, I’ll then draw a
    plot to reflect this.

<!-- end list -->

``` r
gapminder %>%
  group_by(continent, year) %>%
  summarize(
    mean_lifeExp = mean(lifeExp)
  )%>%
  # year as x axis and mean_lifeExp as y axis
  ggplot(aes(year, mean_lifeExp)) +
  
  # facetting by continent
  facet_wrap(~continent, scales = "free_y")+
  # make a line plot with points
  geom_line() +
  geom_point()
```

![](hw03_gapminder_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> \#
Report the absolute and/or relative abundance of countries with low life
expectancy over time by continent: Compute some measure of worldwide
life expectancy – you decide – a mean or median or some other quantile
or perhaps your current age. Then determine how many countries on each
continent have a life expectancy less than this benchmark, for each
year.

# Find countries with interesting stories. Open-ended and, therefore, hard. Promising but unsuccessful attempts are encouraged. This will generate interesting questions to follow up on in class.
