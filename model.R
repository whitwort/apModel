# Model implementation heavily inspired (practically copied verbatim from)
# Markus Gesmann's excellent blog post: 
# http://lamages.blogspot.co.uk/2012/06/hodgkin-huxley-model-in-r.html

# We'll use the deSolve package for our integration engine
library(deSolve)

# define the solver that we want to use
solver <- ode

# Constants
# Maximum conductance values (x 10-3 mho/cm^2)
gNa   = 120
gK    = 36
gL    = 0.3

# Equilibrium potentials
eNa   = 50
eK    = -77
eL    = -54.4

# Model parameters
parameters <- c(
    
  # Stimulation parameters
    C     = 1
  , I     = 0
  
)

# Initial values of state variables
state <- c(
    v     = -65
  , m     = 0.052
  , h     = 0.596
  , n     = 0.317
)

# Time window and step size
time <- c(
    start = 0
  , end   = 40
  , step  = 0.25
)

# Voltage dependent coefficients
am  <- function(v) { 0.1*(v+40)/(1-exp(-(v+40)/10)) }
bm  <- function(v) { 4*exp(-(v+65)/18) }
ah  <- function(v) { 0.07*exp(-(v+65)/20) }
bh  <- function(v) { 1/(1+exp(-(v+35)/10)) }
an  <- function(v) { 0.01*(v+55)/(1-exp(-(v+55)/10)) }
bn  <- function(v) { 0.125*exp(-(v+65)/80) }

# deSolve functional interface; t is the model time passed by the library
model <- function(t, state, parameters) {
  
  # We'll bind state and parameter variables to clean up our model code block
  with(as.list(c(state, parameters)), {
    
    # This function returns an ordered list of rate of change calculations: the
    # order should match that of the state vector
    return(list(c(

        dv <- (I - (gNa * h * (v-eNa) * m^3) - (gK * (v-eK) * n^4) - (gL * (v-eL)) ) / C
      , dm <- am(v) * (1-m) - bm(v) * m
      , dh <- ah(v) * (1-h) - bh(v) * h
      , dn <- an(v) * (1-n) - bn(v) * n
      
    )))
    
  })
  
}

# Named vector of functions that are availabe as response variable choices on
# the summary tab.  Functions should take one argument, the results data.frame,
# with a $time variable and named variables for all of the model states.
state.summary <- c()

# Header element describing this model
headerText        <- "Hodgkin-Huxley Model"

# Descriptive text blocks:  these strings are sent through the R markdown 
# preprocessor before being embeded in the UI.  For a full description of the
# syntax see:  http://www.rstudio.com/ide/docs/r_markdown
#
# If the string contains a path to local file, its contents are loaded instead. 
# 
sidebarHeader     <- "The simulation will update as you change the input parameters below."
sidebarFooter     <- "Version 0.1.  [Source code](https://github.com/whitwort/apModel) available on github."
modelDescription  <- "MODEL.md"

# Label formatters
stateFormat       <- function(name) { paste(name, " (mV)", sep = "") }
parameterFormat   <- function(name) { paste(name, " (10-3 mho/cm^2)", sep = "")  }
