suppressMessages(library(R.utils)) # to unzip gzip file

save_geographic_dir <- file.path("data", "1-raw-data", "geographic")
save_tabular_dir <- file.path("data", "1-raw-data", "tabular")

# remove previous files
f <- list.files(save_geographic_dir, full.names=TRUE)
status <- file.remove(f)
f <- list.files(save_tabular_dir, full.names=TRUE)
status <- file.remove(f)

# download and save geographic data 

url_netcdf = "https://data.giss.nasa.gov/pub/gistemp/"
file_names_netcdf <- c("gistemp1200_GHCNv4_ERSSTv5.nc",
                       "gistemp250_GHCNv4.nc")

for (i in 1:length(file_names_netcdf)) {
  zipped_file_name = paste(file_names_netcdf[i],".gz", sep = "")
  zipped_file_url = paste(url_netcdf, zipped_file_name, sep = "")
  zipped_save_path <- file.path(save_geographic_dir, zipped_file_name)
  download.file(zipped_file_url, zipped_save_path, mode = "wb")
  unzipped_file_path <- R.utils::gunzip(zipped_save_path)
}

# download and save tabular data 

url_csv = "https://data.giss.nasa.gov/gistemp/tabledata_v4/"
file_names_csv <- c("GLB.Ts+dSST.csv",
                    "NH.Ts+dSST.csv",
                    "SH.Ts+dSST.csv",
                    "ZonAnn.Ts+dSST.csv")

for (i in 1:length(file_names_csv)) {
  file_url <- paste(url_csv, file_names_csv[i], sep = "")
  save_path <- file.path(save_tabular_dir, file_names_csv[i])
  download.file(file_url, save_path, mode = "wb")
}