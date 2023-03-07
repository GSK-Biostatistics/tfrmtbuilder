## {tfrmtbuilder} Shiny App <img src="https://github.com/GSK-Biostatistics/tfrmt/blob/main/man/figures/tfrmt.png?raw=true" align="right" alt = "tfrmt logo" style="height:120px;"/> 


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
# install development version of {tfrmt}
devtools::install_github("GSK-Biostatistics/tfrmt")

# install {tfrmtbuilder}
devtools::install_github("GSK-Biostatistics/tfrmtbuilder")

# Load app
library(tfrmtbuilder)
tfrmtbuilder()
```

### Useful Links 

- [{tfrmt} User Guide](https://gsk-biostatistics.github.io/tfrmt/)
- [{tfrmt} GitHub Repository](https://github.com/GSK-Biostatistics/tfrmt)

