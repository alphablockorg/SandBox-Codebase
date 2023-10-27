\*\*\* IMPORTANT LEGAL DISCLAIMER \*\*\*

Yahoo!, Y!Finance, and Yahoo! finance are registered trademarks of Yahoo, Inc.

yfinance is not affiliated, endorsed, or vetted by Yahoo, Inc. It's an open-source tool that uses Yahoo's publicly available APIs, and is intended for research and educational purposes.

You should refer to Yahoo!'s terms of use (<a href=https://legal.yahoo.com/us/en/yahoo/terms/product-atos/apiforydn/index.html>here</a>, <a href=https://legal.yahoo.com/us/en/yahoo/terms/otos/index.html>here</a>, and <a href=https://policies.yahoo.com/us/en/yahoo/terms/index.htm>here</a>) for details on your rights to use the actual data downloaded. Remember - the Yahoo! finance data downloaded using the Rcode is intended for personal use only.

In case the data is not clean, has gaps or can't download certain symbols, please use alternative data sources to create the EOD csv file. The format of the file is :

![](images/Screen%20Shot%202022-10-04%20at%2010.28.56%20AM.png)

In case of warning messages like this, when downloading data :

*Warning message: AVAX-USD contains missing values. Some functions will not work if objects contain missing values in the middle of the series. Consider using na.omit(), na.approx(), na.fill(), etc to remove or replace them.*

Fill in the gaps of the missing symbol from other sources.

In case of warning messages like:

*Warning: FB download failed; trying again.*

*[1] "Symbol Missing data from Yahoo FB"*

Corporate actions e.g. change of Facebook [FB] name to Meta was the reason for incomplete data download. Without proper data, the code won't work. The issue has been fixed for now, but such issues can happen again.

Other sources :

S&P - <https://rapidapi.com/blog/best-stock-api/>

Crypto - <https://coinmarketcap.com>

# R-GenerateRankingAndPortfolios

------------------------------------------------------------------------

-   Introduction
-   Requirements
-   Installation\
-   Input files
-   How to Run
-   Data disclaimer

------------------------------------------------------------------------

## Introduction

This Project is a collection of multiple R codes that does 4 things: <br> - downloads yahoo EOD data (DownloadYahooData.R)<br> - generates rankings (Main_GenerateRanking.R)<br> - creates portfolios using the rankings that don't rebalance and are held for 1, 2, or 3 years. (Main_GeneratePortfolios.R)</br> - creates portfolios using the rankings that rebalance every year(Main_Generate_ExceptionalandRichIndexes.R) and run until the last date of the input CSV. </br> All main files take as input a comma-separated values (CSV) file containing the EOD (End of day) price data of a group of symbols. A set of CSV files have been provided for S&P100, S&P500, Crypto10, TSX60, BSE100 and Indonesia30.

`DownloadYahooData.R` Using existing public Yahoo API, this code will download Yahoo EOD data using a series of tickers given as input. It will output two csv files that will be use in Main_GenerateRanking.R and Main_GeneratePortfolios.R as input. One file will be the group EOD data for 30 years and one file will be the Benchmark of the Group EOD data for the same period

! Because BSE100 data downloaded from Yahoo has a lot of missing data in the time series, we use a function of Fill Gaps (function can be found in Utils.R). This function will fill in the missing data with previous data that in not NA in the time series. ! The data for Indonesia 30 and it's index has been provided and doesn't need to be downloaded. The data coming from yahoo doesn't have proper multiplications and stock splits applied

`Main_GenerateRanking.R`

This code will take a CSV file containing Yahoo EOD stocks data as input and generate one or multiple ranking files. The ranking of the symbols in the file will be generated using their average return of components over different quarters. Based on the number of quarters the user sets in the quarters variable the ranking files will be created. All stocks of the group will be percentile ranked from 1 to 100, 100 being the top scorer and 1 being the worst.

> Input

-   dataPointsFor1Quarter - a number representing the approximate number of days in a quarter that have EOD stocks price.
-   quarters - a list or a number of the quarter the code should generate the ranking file for
-   workingWith - the name of the product the code is running for (USE ONLY S&P100, S&P500, Crypto10, TSX60, BSE100, Indonesia30)
-   nameOfTheFileWithStockData - the input CSV file.

`Main_GeneratePortfolios.R`

This code will take as input one or more ranking files, the CSV with EOD data of the stocks, and a CSV file with EOD data of the benchmark. Using these 3 files the user can generate different types of Portfolios with symbols selected based on ranking and summary analysis on those portfolios. Based on the ranking provided, the code will categorize all stocks as: \* Value - if ranking is equal or below 20 i.e. bottom quintile (BQ) \* Core - if ranking is between 20 and 80 i.e. rest quintiles ignoring top and bottom quintile (RQ) \* Growth - if ranking is equal or above 80 i.e. top quintile (TQ) The types of portfolios that can be generated: Value (BQ), Core(RQ), Growth(TQ), Value Growth i.e. top and bottom quintile (TBQ), All (full basket) i.e. all quintiles (AQ). Each Portfolio created will be saved with the data for 1 year, 2 years, and 3 years. Based on this data the following files will be created as well: Summary Table, Index Draw-down, Index Draw Down Curve plot, and Daily Return.

If the input type is All (full basket) the code will generate an unequal weighted portfolio with value and growth having 40% each and core only 20% in case of non Crypto Portfolios, value 20%, growth20%, core 60% in case of Crypto 10.

All other types will create equal weighted portfolios.

> Input

-   workingWith - the name of the product the code is running for (USE ONLY S&P100, S&P500, Crypto10 , TSX60 , BSE100, Indonesia30)

-   minNumberOfSymbols - must be - any number between 90 and 100 for S&P 100 and BSE100 - any number between 485 and 500 for S&P 500 - 10 for Crypto10 - 60 for TSX60 - 30 for Indonesia30

-   generatePortfoliosWithRebalance - valid only if you generate with type="OnePortfolio". If TRUE it will generate Portfolios that do rebalance ( 3 years for Value and 1 year for Core and Growth)

-   rankingToUse - will indicate what Ranking file to use. Must be a number between 1,2,3,4,5,6,7,8,12,20

-   type - what type of portfolio should the code generate.

    Choose between

    -   Value(BQ) - it will make Portfolios only with value symbols - with no rebalance

    -   Core(RQ) - it will make Portfolios only with core symbols - with no rebalance

    -   Growth(TQ) - it will make Portfolios only with growth symbols - with no rebalance

    -   VG(TBQ) - it will make Portfolios only with value and growth symbols - with no rebalance

    -   ALL(AQ) - it will make a VCG portfolio - with no rebalance

        -   ALL1 - will generate P with value weight 10 and growth weight 80 - with no rebalance

        -   ALL2 - will generate P with value weight 20 and growth weight 70 - with no rebalance

        -   ALL3 - will generate P with value weight 30 and growth weight 60 - with no rebalance

        -   ALL4 - will generate P with value weight 40 and growth weight 50 - with no rebalance

        -   ALL5 - will generate P with value weight 50 and growth weight 40 - with no rebalance

        -   ALL6 - will generate P with value weight 60 and growth weight 30 - with no rebalance

        -   ALL7 - will generate P with value weight 70 and growth weight 20 - with no rebalance

        -   ALL8 - will generate P with value weight 80 and growth weight 10 - with no rebalance

        -   ALL9 - will generate P with value weight 10 and growth weight 70 - with no rebalance

        -   ALL10 - will generate P with value weight 20 and growth weight 60 - with no rebalance

        -   ALL11 - will generate P with value weight 30 and growth weight 50 - with no rebalance

        -   ALL12 - will generate P with value weight 40 and growth weight 40 - with no rebalance

        -   ALL13 - will generate P with value weight 50 and growth weight 30 - with no rebalance

        -   ALL14 - will generate P with value weight 60 and growth weight 20 - with no rebalance

        -   ALL15 - will generate P with value weight 70 and growth weight 10 - with no rebalance

        -   ALL16 - will generate P with value weight 10 and growth weight 60 - with no rebalance

        -   ALL17 - will generate P with value weight 20 and growth weight 50 - with no rebalance

        -   ALL18 - will generate P with value weight 30 and growth weight 40 - with no rebalance

        -   ALL19 - will generate P with value weight 40 and growth weight 30 - with no rebalance

        -   ALL20 - will generate P with value weight 50 and growth weight 20 - with no rebalance

        -   ALL21 - will generate P with value weight 60 and growth weight 10 - with no rebalance

        -   ALL22 - will generate P with value weight 10 and growth weight 50 - with no rebalance

        -   ALL23 - will generate P with value weight 20 and growth weight 40 - with no rebalance

        -   ALL24 - will generate P with value weight 30 and growth weight 30 - with no rebalance

        -   ALL25 - will generate P with value weight 40 and growth weight 20 - with no rebalance

        -   ALL26 - will generate P with value weight 50 and growth weight 10 - with no rebalance

        -   ALL27 - will generate P with value weight 10 and growth weight 40 - with no rebalance

        -   ALL28 - will generate P with value weight 20 and growth weight 30 - with no rebalance

        -   ALL29 - will generate P with value weight 30 and growth weight 20 - with no rebalance

        -   ALL30 - will generate P with value weight 40 and growth weight 10 - with no rebalance

        -   ALL31 - will generate P with value weight 10 and growth weight 30 - with no rebalance

        -   ALL32 - will generate P with value weight 20 and growth weight 20 - with no rebalance

        -   ALL33 - will generate P with value weight 30 and growth weight 10 - with no rebalance

        -   ALL34 - will generate P with value weight 10 and growth weight 20 - with no rebalance

        -   ALL35 - will generate P with value weight 20 and growth weight 10 - with no rebalance

        -   ALL36 - will generate P with value weight 10 and growth weight 10 - with no rebalance

    -   OnePortfolio - there are 2 options, to generate Portfolios without rebalance or with rebalance. In case OnePortfolio is TRUE, the next parameters have to be filled

        -    fromDate- an array of startdates

        -    toDate - one end date, usually until we have EOD data in the Symbols Time Index dataset

        -   core_bottom - lower threshold for Core components to be selected

        -   core_top - upper threshold for Core components to be selected

        -   value_weight - how much weight the Value components will have

        -   growth_weight - how much weight the Growth components will have. Core components weight will be calculated 100 - value_weight - growth_weight in all cases

            ```         
            - For Crypto10 only ALL must be run, otherwise the code will generate Portfolios with just 2 or 3 symbols.
            ```

Warning messages like

*Warning messages: 1: In GeneratePortfolioAndMPT(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, : NAs introduced by coercion*

or

*There were 50 or more warnings (use warnings() to see the first 50)*

can be ignored

`Main_Generate_ExceptionalandRichIndexes.R`

This code will take as input one or more ranking files, the CSV with EOD data of the stocks, and a CSV file with EOD data of the benchmark. Using these 3 files it will generate an unequal weighted portfolios with Value and Growth having 40% each and Core only 20%. Based on the ranking provided, the code will categorize all stocks as: \* Value - if ranking is equal or below 20 i.e. bottom quintile (BQ) \* Core - if ranking is between 20 and 80 i.e. rest quintiles ignoring top and bottom quintile (RQ) \* Growth - if ranking is equal or above 80 i.e. top quintile (TQ) Each Portfolio will rebalance every year the stocks of Core and Growth and every 3 years the Value stocks. Depending on the dimension of the EOD dataset, the Portfolios will start every month and will run until the end of the dataset. The first starting month will be selected when the EOD dataset has trading data for a number of stocks equal to input variable "minNumberOfSymbols". Based on this data the following files will be created as well: Summary Table, Index Draw-down, Index Draw Down Curve plot, and Daily Return.

> Input

-   workingWith - the name of the product the code is running for (USE ONLY S&P100, S&P500, Crypto10, TSX60, BSE100, Indonesia30 )
-   startDate - the date when the Index Portfolios should start (YYYY-mm-dd)
-   startPortfoliosEvery - will be Day for Crypto Portfolios, because Crypto has less history, and Month for the S&P100, S&P500, TSX60 , BSE100, Indonesia30
-   rankingToUse - will indicate what Ranking file that takes 7 quintiles to generate,and 3 for Crypto Portfolios

`Utils.R`

This file contains different functions for DateTime manipulation, Reading/Writing from files, ranking functions

`Functions.R`

Function for generating equal weighted portfolios, un equal weighted portfolios(in case of full basket) and rebalancing Portfolios

Function2.R

Functions for generating Portfolios from a given start date with or without rebalance.

`Summary Code MPT Stats Functions.R`

Function for generating MPT Stats table

`Summary Code For Continuous Portfolios`

Function for generating MPT Stats table

`Drawdown Functions.R`

Function for generating the draw-down analysis

# Requirements

To run this code, you will need:

```         
 * [R version 3.6.1 or later](https://cloud.r-project.org/)
 * [RStudio](https://www.rstudio.com/products/rstudio/download/)
 
```

# Installation

After the installation of R and RStudio is completed, some packages will need to be Installed in the Project. Open the Project file ( GenerateRankingAndPortfolios.Rproj) , go to Tools -\> Install Packages and install the following packages: xts, zoo, dplyr, lubridate, ggplot2, tidyquant

Or run in console:

install.packages("xts") install.packages("zoo") install.packages("dplyr") install.packages("lubridate") install.packages("ggplot2") install.packages("tidyquant")

# Input files

All input files will be generated using the script DownloadYahooData.R with one of the groups: S&P100, S&P500, Crypto10, TSX60, BSE100, Indonesia30

# How to Run

After downloading the Project into your local drive, this should be the steps to create your own results:

1.  Open DownloadYahooData.R to download End of day data from Yahoo and press
    -   Mac: option + command + R
    -   Windows: Ctrl + Alt + R
2.  Open Main_GenerateRanking.R to generate the Ranking Files and press
    -   Mac: option + command + R
    -   Windows: Ctrl + Alt + R
3.  Open Main_GeneratePortfolios.R to generate the Portfolio Files and press
    -   Mac: option + command + R
    -   Windows: Ctrl + Alt + R

# Data disclaimer

Data used to generate the results for S&P 500 was downloaded from Yahoo public API The tickers used in DownloadYahooData.R were the components of S&P 500 and S&P100 at the moment of writing this code in the last quarter of 2021.

For Crypto 10 we used the **Bitwise 10 Crypto Index Fund (BITW)** data. Since Crypto markets trade 24/7 the data downloaded from Yahoo includes weekend dates, but BITW doesn't .To make the code functional, weekend dates in the Crypto index have been filled with first previous day available.
