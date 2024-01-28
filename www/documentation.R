library(rmarkdown)
library(bookdown)

render(input = system.file("doc/regional_innovation.Rmd", package = "aurininnovation"),
       output_format = bookdown::html_vignette2(),
       output_dir = "www/",
       output_file ="regional_innovation")
