library(Eunomia)
library(DatabaseConnector)
library(JMcondition)
library(ggplot2)
library(testthat)
library(magick)

# Get info of reference data

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
connection <- DatabaseConnector::connect(connectionDetails)

# Get data
data <- extractPatients(connection)

# Plot the test and save it
plotTrend(data, byMonth = FALSE)
ggplot2::ggsave("testplot.png")

# Read control and test images
controlImage <- image_read("controlplot.png")
testImage <- image_read("testplot.png")

# test difference of both images the control and test
diffImage <- image_compare(controlImage, testImage, metric="AE")
distortion <- as.numeric(attr(diffImage, "distortion"))
expect_true(distortion != 0)

