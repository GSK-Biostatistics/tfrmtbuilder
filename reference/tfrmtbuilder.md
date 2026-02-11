# Run tfrmt Builder Shiny App

Run tfrmt Builder Shiny App

## Usage

``` r
tfrmtbuilder(tfrmt = NULL, data = NULL, mockmode = TRUE, run = TRUE)
```

## Arguments

- tfrmt:

  tfrmt object to be loaded into app from R session. Defaults to `NULL`

- data:

  data frame to be loaded into app from R session. Defaults to `NULL`

- mockmode:

  Whether to initialize the app in mock mode. Defaults to `TRUE`

- run:

  Boolean for whether the created object should be run directly. Set to
  `FALSE` for deployment

## Value

Shiny app for creating and modifying tfrmt objects

## Examples

``` r
if (interactive()){
  tfrmtbuilder()
}
```
