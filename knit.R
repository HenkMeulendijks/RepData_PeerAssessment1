require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html 

SourceFile <- "./reproducible-research/week1/RepData_PeerAssessment1/PA1_template.Rmd"
DestinationFile  <- "./reproducible-research/week1/RepData_PeerAssessment1/PA1_template.md"
knit(SourceFile, DestinationFile) # creates md file