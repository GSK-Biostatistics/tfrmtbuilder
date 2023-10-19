## {tfrmtbuilder} Shiny App 

The {tfrmt} package provides a language for defining display-related metadata, which can then be used to automate and easily update output formats. {tfrmtbuilder} serves as an interface to the package, allowing users to quickly and easily modify existing or new table templates.

### App Features and Workflow

- Initialize tab
  - Load an existing tfrmt (JSON) or start fresh 
  - Specify whether you are generating a mock (no data values) or a table with data values
  - Define your data source:
    - Auto [mock only]: Use auto-generated mock data
    - Upload: Upload an existing dataset (most file formats)
      - Note: If in 'mock' mode, this will serve as a shell with any data values ignored
    - Example [mock only]: Use a pre-loaded example dataset 

- Edit tab
  - Select ARD column mappings
  - Format table via the 'plans' 
  - View the table as you modify
  
- Export tab
  - Download tfrmt metadata (JSON) 
  - Download tfrmt table (HTML, PNG) 
  
### Usage

{tfrmtbuilder} can be installed and used with the following code:

```r
# development version
devtools::install_github("GSK-Biostatistics/tfrmtbuilder")

# from CRAN
install.packages("tfrmtbuilder")

# Load app
library(tfrmtbuilder)
tfrmtbuilder()
```

### Useful Links 

- [Example app](https://bzkrouse.shinyapps.io/tfrmtbuilder/)
- [{tfrmt} User Guide](https://gsk-biostatistics.github.io/tfrmt/)
- [{tfrmt} GitHub Repository](https://github.com/GSK-Biostatistics/tfrmt)

