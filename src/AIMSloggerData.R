# This example requires the 'dataaimsr' package to be installed from https://docs.ropensci.org/dataaimsr/
library('dataaimsr')

# If you do not have an AIMS Data Platform API Key then go to https://open-aims.github.io/data-platform/key-request
# The API Key can be passed to the package functions as an additional api_key = "XXXX" argument. However, we
# strongly encourage users to maintain their API key as a private locally hidden environment variable
# (AIMS_DATAPLATFORM_API_KEY) in the .Renviron file for automatic loading at the start of an R session.

# If AIMS_DATA_PLATFORM_API_KEY is not set in the .Renviron uncomment the following line, paste your key where it
# says 'my-api-key' and add an 'api_key = my_api_key' parameter to the aims_data function call:

my_api_key <- "6JMAXPla1xauLxlL89yAo3OXlf3zQVrQaRnDdB0f"

# flat data taken from the lagoon on the northern side
flatTemp <- aims_data("temp_loggers", 
                      api_key = my_api_key, 
                      filters = list(
                        "series" = "HERFL1",
                        "from_date" = "2019-11-01T00:00:00",
                        "thru_date" = "2024-06-01T00:00:00"
                      )) 

saveRDS(flatTemp, "./src_outputs/AIMStempReefFlat.rds")

# Slope data taken from the HI channel
slopeTemp <- aims_data("temp_loggers", 
                       api_key = my_api_key, 
                       filters = list(
                         "series" = "HERSL1",
                         "from_date" = "2019-11-01T00:00:00",
                         "thru_date" = "2024-06-01T00:00:00"
                       )) 

saveRDS(slopeTemp, "./src_outputs/AIMStempReefSlope.rds")
