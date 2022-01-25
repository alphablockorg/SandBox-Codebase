# R-GenerateRankingAndPortfolios

***


* Introduction
* Requirements
* Installation      
* Input files

***

## Introduction

This Project is a collection of multiple R codes that does 2 things: generate rankings and create portfolios using the rankings. The portfolios are not rebalanced and are held for 1, 2, or 3 years. 
There are two main files, one for the generation of rankings and one for generating the portfolios using the ranking resulting from the first one. Both main files take as input a comma-separated values (CSV) file containing the EOD (End of day) price data of a group of symbols. A set of CSV files have been provided for S&P100 and S&P500.


`Main_GenerateRanking.R`

This code will take a CSV file containing EOD stocks data as input and generate one or multiple ranking files. The ranking of the symbols in the file will be generated using their average return of components over different quarters. Based on the number of quarters the user sets in the quarters variable the ranking files will be created. All stocks of the group will be percentile ranked from 1 to 100, 100 being the top scorer and 1 being the worst.
 
> Input

 * dataPointsFor1Quarter      - a number representing the approximative number of days in a quarter that have EOD stocks price. 
 * quarters                   -  a list or a number of the quarter the code should generate the ranking file for
 * productName                - the name of the product the code is running for (USE ONLY S&P100 or S&P500)
 * nameOfTheFileWithStockData - the input CSV file. An example of the format of the file is shown below.
 
 Date	   PYPL	TMUS KHC	GOOGL	RTX	    EXC	    PEP	         CL	      BK	       LIN	ORCL	   PCLN	BIIB
 
1990-01-02	NA	 NA	 NA	     NA	  4.365953	11.75	10.70849893	3.99225	  5.415917063	NA	0.59567931	NA	NA

1990-01-03	NA	 NA	 NA	     NA	  4.316787	11.625	10.60399894	3.96875	  5.49872925	NA	0.530864463	NA	NA

1990-01-04	NA	 NA	 NA	     NA	  4.306954	11.4375	10.49999895	3.92975	  5.465604375	NA	0.546296569	NA	NA

1990-01-05	NA	 NA	 NA	     NA	  4.306954	11.4375	10.35399896	3.789125  5.49872925	NA	0.515432357	NA	NA

1990-01-08	NA	 NA	 NA	     NA	  4.326621	11.5	10.54149895	3.80475	  5.449041938	NA	0.533950884	NA	NA

1990-01-09	NA	 NA  NA	     NA   4.277454	11.4375	10.39599896	3.851625  5.283417563	NA	0.509259514	NA	NA

`Main_GeneratePortfolios.R`

This code will take as input one ranking file, the CSV with EOD data of the stocks, and a CSV file with EOD data of the benchmark. Using these 3 files the user can generate different types of Portfolios with symbols selected based on ranking and summary analysis on those portfolios. Based on the ranking provided, the code will categorize all stocks as: 
    * Value - if ranking is equal or below 20 i.e. bottom quintile (BQ) 
    * Core - if ranking is between 20 and 80 i.e. rest quintiles ignoring top and bottom quintile (RQ) 
    * Growth - if ranking is equal or above 80 i.e. top quintile (TQ)
The types of portfolios that can be generated: Value (BQ), Core(RQ), Growth(TQ), Value Growth i.e. top and bottom quintile (TBQ), All (full basket) i.e. all quintiles (AQ). Each Portfolio created will be saved with the data for 1 year, 2 years, and 3 years. Based on this data the following files will be created as well: Summary Table, Index Drawdown, Index Draw Down Curve plot, and Daily Return.
If the input type is All (full basket) the code will generate an unequal weighted portfolio with value and growth having 40% each and core only 20%. All other types will create equal weighted portfolios.

> Input
 
 * workingWith                - the name of the product the code is running for (USE ONLY S&P100 or S&P500)
 * symbolsTimeIndexfileName   - the name of the CSV file containing the Stocks EOD data ("S&P100TimeIndex.csv" or "S&P500TimeIndex.csv")
 * benchmarkTimeIndexFileName - the name of the CSV file containing the Benchmark EOD data ("S&P100_Benchmark_Index.csv" or "S&P500Benchmark.csv")
 * minNumberOfSymbols         - must be 100 for S&P 100 and 500 for S&P 500.    
 * rankingToUse               - will indicate what Ranking file to use. Must be a number between 1,2,3,4,5,6,8,12,20 
 * type                       - what type of portfolio should the code generate. Choose between Value, Core, Growth, VG, ALL
    

`Utils.R`

This file contains different functions for DateTime manipulation, Reading/Writing from files, ranking functions

`Functions.R`

Function for generating equal weighted portfolios, un equal weighted portfolios(in case of full basket) and a function for generating MPT Statistics

`Summary Code MPT Stats Functions.R`

Function for generating MPT Stats table 

`Drawdown Functions.R`

Function for generating the drawdown analysis


# Requirements

 To run this code, you will need:
   
     * [R version 3.6.1 or later](https://cloud.r-project.org/)
     * [RStudio](https://www.rstudio.com/products/rstudio/download/)
     
# Installation            

 After the installation of R and RStudio is completed, some packages will need to be Installed in the Project.
 Open the Project file ( GenerateRankingAndPortfolios.Rproj) , go to Tools -> Install Packages and install the following packages: xts, zoo, dplyr, lubridate, ggplot2 
 
 Or run in Console:
 
   install.packages("xts")
   install.packages("zoo")
   install.packages("dplyr")
   install.packages("lubridate")
   install.packages("ggplot2")
   
# Input files

  For S&P 100 
     * S&P100TimeIndex.csv        - EOD data starting from  1990 for S&P 100 
     * S&P100_Benchmark_Index.csv - EOD data for S&P 100 Benchmark - OEX
  For S&P 500
     * S&P500TimeIndex.csv        - EOD data starting from  1990 for S&P 500 
     * S&P500Benchmark.csv        - EOD data for S&P 500 Benchmark - SPX
   
 

