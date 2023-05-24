1. Explanation of the folders. 

folder: project_folder
content: 
	folder: photos
	app.R (Shiny App script)
	PortfolioAnalysis.html (complete report of the project)
	PortfolioAnalysis.Rmd (file .Rmd containing the script behind the Rmarkdown document)
	PortfolioAnalysisofSMIStocks.pptx (Power Point of the presentation)
	project_script.R (script on which the Shiny App is based)
	readMe.txt (this file)
	
folder: photos
content: 
	markovitz.jpeg (image of Markovitz used in the report)
	michaud.jpeg (image of Michaud used in the report)
	wall.jpeg (initial image of the report)
	
2. Suggestion
	1. The reader should fix the paths, ours are made in Linux environment so they could be different. Here is a list of lines code that must be changed if you want to run the code locally: 
		- in App.R:
			line 10: source("YOUR PATH/project_script.R") or if you are working in the right directory leave source("project_script.R")
		
		- in PortfolioAnalysis.Rmd:
			line 41: knitr::include_graphics("YOUR PATH/photos/wall.jpeg") or if you are working in the right directory leave knitr::include_graphics("photos/wall.jpeg")
			
		- in PortfolioAnalysis.Rmd:
			line 77: knitr::include_graphics("YOUR PATH/photos/markovitz.jpeg") or if you are working in the right directory leave knitr::include_graphics("photos/markovitz.jpeg")
			
		- in PortfolioAnalysis.Rmd:
			line 93: knitr::include_graphics("YOUR PATH/photos/michaud.jpeg") or if you are working in the right directory leave knitr::include_graphics("photos/michaud.jpeg")
			
	2. Please install the following libraries: 
		- "MASS", "quadprogXT", "ggfortify", "ggplot2", "plotly", "quantmod", "xts", "shiny", "shinydashboard"
