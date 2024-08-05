library(Eunomia)
library(DatabaseConnector)
library(JMcondition)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
connection <- DatabaseConnector::connect(connectionDetails)

data <- extractPatients(connection)
plotTrend(data, byMonth = FALSE)
plotTrend(data, byMonth = TRUE)

launchShinyApp(data)
