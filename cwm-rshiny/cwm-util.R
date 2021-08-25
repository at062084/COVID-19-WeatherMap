options(error = function() traceback(2))
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(knitr)
library(gt)
library(pivottabler)
library(kableExtra)
library(zoo)


# -------------------------------------------------------------------------------------------
# PivotTabler: CMW style pivot table helpers
# -------------------------------------------------------------------------------------------

# Create standard object for table rendering
makePT <- function(df, cdg=NULL) {
  # cdg: ColumnDataGroups
  pt <- PivotTable$new(tableStyle  =list("color"="black", "border-color"="black"),
                       headingStyle=list("color"="black", "border-color"="black", "background-color"="white", "font-style"="italic"), 
                       cellStyle   =list("color"="black", "border-color"="black", "background-color"="white"),
                       totalStyle  =list("color"="black", "border-color"="black", "background-color"="lightblue", "font-weight"="bold"))
  pt$addData(df)
  
  # Default global properties
  # pt$theme <- "default" # --> do not set, overrides sveral of the setings set below
  pt$setDefault(addTotal=FALSE)
  pt$setDefault(totalPosition="before")
  #pt$setDefault(totalCaption="Gesamt {value}") # --> does not seem to work
  
  pt$setDefault(outlineBefore=list(isEmpty=FALSE, caption="Gesamt {value}", totalPosition="before",
                                   groupStyleDeclarations=list("color"="black", "border-color"="black", "background-color"="lightgray", "font-style"="italic"),
                                   cellStyleDeclarations =list("color"="black", "border-color"="black", "background-color"="lightgray", "font-style"="italic")))
  pt$setDefault(outlineTotal=list(isEmpty=FALSE,  caption="Gesamt {value}", totalPosition="before", 
                                  groupStyleDeclarations=list("color"="black", "border-color"="black", "background-color"="lightgreen", "font-style"="italic"),
                                  cellStyleDeclarations =list("color"="black", "border-color"="black", "background-color"="lightgreen", "font-style"="italic")))
  if (!is.null(cdg)) {
    pt$addColumnDataGroups(cdg, totalPosition="before", totalCaption="Gesamt", addTotal=TRUE, 
                           styleDeclarations=list("color"="black", "border-color"="black", "background-color"="lightblue", "font-weight"="bold", "font-style"="italic"))  
  }
  # "border"="1px solid blue"
  return(invisible(pt))
}

defPTcalcRel <- function(pt, calName, summariseExp) {
  pt$defineCalculation(type="summary", calculationName=calName, summariseExpression=summariseExp,
                             headingStyleDeclarations=list("background-color"="lightblue",
                                                           "font-weight"="bold", 
                                                           "font-style"="italic", 
                                                           "color"="royalblue"),
                             cellStyleDeclarations=list("color"="royalblue"))
  return(invisible(pt))
}


defPTcalcAbs <- function(pt, calName, summariseExp) {
  pt$defineCalculation(type="summary", calculationName=calName, summariseExpression=summariseExp,
                       headingStyleDeclarations=list("background-color"="lightblue", 
                                                     "font-weight"="bold", 
                                                     "font-style"="italic",
                                                     "color"="black"),
                       cellStyleDeclarations=list("color"="black"))
  return(invisible(pt))
}


