suppressMessages(library(R.utils)) # to unzip gzip file

save_netcdf_dir <- file.path("data", "1-raw-data", "gridded")
save_csv_dir <- file.path("data", "1-raw-data", "tabular")

# remove previous files
f <- list.files(save_netcdf_dir, full.names=TRUE)
status <- file.remove(f)
f <- list.files(save_csv_dir, full.names=TRUE)
status <- file.remove(f)

##### download and save gridded data (netcdf)

url_nc = "https://data.giss.nasa.gov/pub/gistemp/"
file_name_nc = "gistemp1200_GHCNv4_ERSSTv5.nc"
file_name_gz = paste(file_name_nc,".gz", sep="")
url_nc_gz = paste(url_nc, file_name_gz, sep="")

save_netcdf_path = file.path(save_netcdf_dir, file_name_gz)
download.file(url_nc_gz, save_netcdf_path , mode = "wb")

unzip_netcdf_path = file.path(save_netcdf_dir, file_name_nc)
unzip_netcdf_path <- R.utils::gunzip(save_netcdf_path)


file_name_nc = "gistemp250_GHCNv4.nc"
file_name_gz = paste(file_name_nc,".gz", sep="")
url_nc_gz = paste(url_nc, file_name_gz, sep="")

save_netcdf_path = file.path(save_netcdf_dir, file_name_gz)
download.file(url_nc_gz, save_netcdf_path , mode = "wb")

unzip_netcdf_path = file.path(save_netcdf_dir, file_name_nc)
unzip_netcdf_path <- R.utils::gunzip(save_netcdf_path)


### download and save tabular data (csv) 

url_csv = "https://data.giss.nasa.gov/gistemp/tabledata_v4/"
file_names_csv <- c("GLB.Ts+dSST.csv",
                    "NH.Ts+dSST.csv",
                    "SH.Ts+dSST.csv",
                    "ZonAnn.Ts+dSST.csv")

for (i in 1:length(file_names_csv)) {
  url_csv_path <- paste(url_csv, file_names_csv[i], sep = "")
  save_csv_path <- file.path(save_csv_dir, file_names_csv[i])
  download.file(url_csv_path , save_csv_path, mode = "wb")
}