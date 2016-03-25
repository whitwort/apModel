This model uses the following fixed values:

Maximum conductance values: (x 10^-3 mho/cm^2 )
* gNa   = 120
* gK    = 36
* gL    = 0.3

Equilibrium potentials
* eNa   = 50
* eK    = -77
* eL    = -54.4

Voltage dependent coefficient functions:

```r
am  <- function(v) { 0.1 * (v + 40) / (1 - exp( -(v + 40) / 10)) }
bm  <- function(v) { 4 * exp( -(v + 65) / 18) }

ah  <- function(v) { 0.07 * exp( -(v + 65) / 20) }
bh  <- function(v) { 1 / (1 +exp( -(v + 35) / 10)) }

an  <- function(v) { 0.01 * (v + 55) / (1 - exp( -(v + 55) / 10)) }
bn  <- function(v) { 0.125 * exp( -(v + 65) / 80) }
```

Model parameters:

* C - membrane capacitance per unit area
* I - total membrane current per unit area

Model states:

* V - the voltage across the membrane (in mV)
* m - sodium channel activation
* h - sodium channel inactivation
* n - potassium channel activation
