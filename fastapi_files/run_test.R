source('R/CochranDerSimonianLaird.R')

args <- commandArgs(trailingOnly = TRUE)
input_path <- args[1]
output_path <- args[2]

# Read input
input_data <- jsonlite::fromJSON(input_path)

res = DerSimonianLaird(
  input_data$measured_values,
  input_data$std_unc
)

response = list(
  "Test"="Cochrans Q Test for Homogeneity",
  "p-value"=res$Qp
)


# Write output as JSON
write(jsonlite::toJSON(response), output_path)