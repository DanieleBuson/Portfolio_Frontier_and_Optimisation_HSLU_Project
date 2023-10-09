# Portfolio Analysis Project

## Project Folders

### `project_folder`

This folder contains the core project files:

- `photos`: This directory holds images used in the project report.
- `app.R`: The script for the Shiny app.
- `PortfolioAnalysis.html`: A complete report of the project in HTML format.
- `PortfolioAnalysis.Rmd`: An Rmarkdown file containing the script behind the Rmarkdown document.
- `PortfolioAnalysisofSMIStocks.pptx`: A PowerPoint presentation of the project.
- `project_script.R`: The script on which the Shiny app is based.
- `readMe.txt`: The file you are currently reading.

### `photos`

This folder contains images used in the project report:

- `markovitz.jpeg`: An image of Markovitz used in the report.
- `michaud.jpeg`: An image of Michaud used in the report.
- `wall.jpeg`: The initial image of the report.

## Suggestions

1. **Adjust File Paths**: If you intend to run the code locally, make sure to adjust the file paths. Here are the lines of code that need modification:
    - In `App.R`: 
        - Line 10: `source("YOUR PATH/project_script.R")` or (if you are in this file directory) `source("project_script.R")`
    - In `PortfolioAnalysis.Rmd`:
        - Line 41: `knitr::include_graphics("YOUR PATH/photos/wall.jpeg")` or (if you are in this file directory) `knitr::include_graphics("photos/wall.jpeg")`
        - Line 77: `knitr::include_graphics("YOUR PATH/photos/markovitz.jpeg")` or (if you are in this file directory) `knitr::include_graphics("photos/markovitz.jpeg")`
        - Line 93: `knitr::include_graphics("YOUR PATH/photos/michaud.jpeg")` or (if you are in this file directory) `knitr::include_graphics("photos/michaud.jpeg")`

2. **Install Required Libraries**: Make sure to install the following libraries to run the code successfully:
    - "MASS", "quadprogXT", "ggfortify", "ggplot2", "plotly", "quantmod", "xts", "shiny", "shinydashboard"

## The Significance of Portfolio Creation

Creating portfolios is a crucial aspect of financial analysis and investment strategies. It allows investors to diversify their assets, manage risk, and optimize returns. This project delves into portfolio analysis using Monte Carlo simulation, providing insights into potential investment strategies. However, please note that the Shiny app's performance can be slow due to the heavy computations involved, especially when simulating with a high number of portfolio weights.

### App Performance

The Shiny app's performance may be affected by the methodology employed, specifically the Monte Carlo simulation. Simulating with a large number of portfolio weights can be computationally intensive. While the current implementation focuses on positive weights for simplicity, it's worth noting that in a different context, a more extensive and efficient approach could be developed.

We appreciate your interest in our "Portfolio Analysis" project. If you have any questions, suggestions, or insights on improving the project's methodology or performance, please feel free to reach out. Your feedback is highly valuable.
