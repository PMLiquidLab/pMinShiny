# pMinShiny

## Manuals

please, visit: http://www.pminer.info/progetti/pMineR/pMinShiny/docs/_book/index.html

## To install the package: 

```
library("remotes")
remotes::install_github("PMLiquidLab/pMinShiny")
```

Let's start by loading the library 

```
library(pMinShiny)

```
<br>

***Useful tips***: in case any errors occur when running the "careflow.mod()" function, please check that the DiagrammeR version installed is at least 1.0.9

## Running the GUIs

You can run the GUIs modules by the following instructions:

```
# for the Event Log and traces inspections: 
visual.mod()

# Process Discovery with CareFlow Miner
careFlow.mod()

# Process Discovery with a first order markov model
FOMM.mod()

```




