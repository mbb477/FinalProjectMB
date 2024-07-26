#Run my project API

library(plumber)
r <- plumb("myProjectAPI.R")

#run it on the port in the Dockerfile
r$run(port=8000)
