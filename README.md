
<!-- README.md is generated from README.Rmd. Please edit that file -->
leadr
=====

The goal of leadr is to stream-line model organization in data science projects and Kaggle competitions. The main function `leadr::board` takes a [caret](https://github.com/topepo/caret) model and automatically builds a personal leaderboard for the entire project.

This leaderboard allows you to easily sort models by metric (accuracy, RMSE, etc.) and ensures that you never lose track of a good model during interactive analysis.

Installation
------------

The package is not currently available on CRAN. You can install the development version with:

``` r
# install.packages("devtools")
devtools::install_github("tmastny/leadr")
```

Getting Started
---------------

Let's say you want to build a classifier for the iris data set. We start by initializing an R project with this directory:

    .
    └── iris.Rproj

Then we fit our first model.

``` r
library(caret)
model <- train(Species ~ ., data = iris, method = 'glmnet')
```

Before leadr, we might create the script `glmnet_1.R` to record the model, save the `train` object as a [.RDS file](https://www.fromthebottomoftheheap.net/2012/04/01/saving-and-loading-r-objects/), and keep track of the accuracy in a spreadsheet.

With leadr, we only need to do the following:

``` r
leadr::board(model)
```

    ## # A tibble: 1 x 13
    ##    rank    id dir     model  metric score public method   num group index 
    ##   <dbl>  <id> <chr>   <chr>  <chr>  <dbl>  <dbl> <chr>  <dbl> <dbl> <list>
    ## 1    1.     1 models… glmnet Accur… 0.964     NA boot     25.    1. <list…
    ## # ... with 2 more variables: tune <list>, seeds <list>

`board` creates a personal leaderboard for your project that ranks and sorts your model based on the model's metric. The leaderboard tibble has all the information needed to successfully recreate and document any model.

`board` also modifies the project directory:

    .
    ├── iris.Rproj
    ├── leadrboard.RDS
    └── models_one
        └── model1.RDS

At the project root, `board` saves the leaderboard tibble as a `.RDS` file and creates a subdirectory (named `/models_one` by default) to save all the models. All future models passed to `board` will be added to the leaderboard and saved in the directory, unless otherwise specified.

Interactive
-----------

In the previous example, we did everything from the command line and leadr took care of the organization and documentation. In fact, leadr benefits from interactive use in other ways. For example, leadr uses [pillar](https://github.com/r-lib/pillar) and [crayon](https://github.com/r-lib/crayon) to programmatically color outputs:

<img src="vignettes/leadr_pic.png" width="100%" style="display: block; margin: auto;" />

Vignettes
---------

For a full description of the features, check out my vignettes hosted here:

1.  Introduction: walkthrough of the basic workflow of leadr

2.  The Leaderboard: a description of each column of the leaderboard
