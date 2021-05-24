##########################
##### Single-stage ######
########################

# R setup ###########
require(data.table) # data.table instead of data.frame format for coding efficiency
require(asreml)     # Fit model with ASReml-R 4
#####################

##### Data input ######
ww <- read.csv("Winter Wheat 2016.csv",h=T)
ww <- na.omit(ww)

##### Change to factor #####
cols <- c("Alpha","Rep","Cultivar")
ww[cols] <- lapply(ww[cols], factor)

ww <- data.table(ww)

asreml.options(maxit=100) # Set asreml iteration
options("scipen"=100,"digits"= 4 ) # set numbering format

##### Fit a single-stage model #####
## incomplete block and replicate location-specific
## location-specifice residual variance
mod <- asreml(fixed       = Yield ~ Zone,
              random      =  ~ Rep:at(Location) + Rep:Alpha:at(Location) + Zone:Location + 
                             Cultivar + Cultivar:Zone:Location+ Cultivar:Zone,
              residual    = ~ dsum(~(units)|Location),
              data        = ww,
              predict     = predict.asreml(classify = "Cultivar:Zone"))

update.asreml(mod)

print(summary.asreml(mod)$varcomp) # print the variance components

blup.1<- data.table((mod$predictions$pvals[1:4])) # set the BLUP results as data.table

blup.1.a <- blup.1[order(Zone,-predicted.value),]  # Sort the results to see the highest yield in each zone