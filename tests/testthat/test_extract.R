library(Eunomia)
library(DatabaseConnector)
library(JMcondition)
library(testthat)

# Get info of reference data

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
connection <- DatabaseConnector::connect(connectionDetails)
data <- extractPatients(connection)

# Begin tests
expect_true(exists('data') && is.data.frame(get('data')))
expect_true(nrow(data) == 20844)
expect_true(ncol(data) == 5)
