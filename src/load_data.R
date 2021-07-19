library(googlesheets4)

spreadsheet <- Sys.getenv("SPREADSHEET_URL")
email <- Sys.getenv("SPREADSHEET_EMAIL")

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = email
)

raw_flows<-read_sheet(spreadsheet, "Flows")
uah_rates<-read_sheet(spreadsheet, "Rates")
raw_holdings<-read_sheet(spreadsheet, "Holdings")
raw_types<-read_sheet(spreadsheet, "Types")

print("Data Loaded")