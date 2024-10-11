# Create directories to store data
dir.create(file.path(getwd(), "bm_files"))
dir.create(file.path(getwd(), "bm_files", "daily"))

# Extract daily-level nighttime lights data for Ghana's first administrative divisions.
# Save a separate dataset for each date in the `"~/Desktop/bm_files/daily"` directory.
# The code extracts data from January 1, 2023 to today. Given that daily nighttime lights
# data is produced on roughly a week delay, the function will only extract data that exists;
# it will skip extracting data for dates where data has not yet been produced by NASA Black Marble.
bm_extract(roi_sf = shapefile,
           product_id = "VNP46A2",
           date = seq.Date(from = ymd("2023-01-01"), to = Sys.Date(), by = 1),
           bearer = bearer,
           output_location_type = "file",
           file_dir = file.path(getwd(), "bm_files", "daily"))

# Append daily-level datasets into one file
file.path(getwd(), "bm_files", "daily") |>
  list.files(pattern = "*.Rds",
             full.names = T) |>
  map_df(readRDS) |>
  saveRDS(file.path(getwd(), "bm_files", "ntl_daily.Rds"))