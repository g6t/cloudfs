# cloudfs

`cloudfs` is an R package that offers a unified interface for simplifying cloud
storage interactions, including uploading, downloading, reading, and writing
files, with functions for both Google Drive and Amazon S3.

## Installation

```R
remotes::install_github("g6t/cloudfs")
```

## Key Features

1. **Concise Syntax**

   Upload files to your project's dedicated Google Drive folder with ease.
   ```R
   cloud_drive_upload("plots/my_plot.png")
   ```

2. **Unified Interface for Google Drive and S3**

   Uploading to S3? The process is just as straightforward.
   ```R
   cloud_s3_upload("plots/my_plot.png")
   ```

3. **Effortless cloud navigation**

   Browse Google Drive folders using 
   ```R
   cloud_drive_browse("my_folder")
   ```
    
   or list S3 contents with
   ```R
   cloud_s3_ls()
   ```

4. **Bulk File Management**

   Easily read all data files from an S3 folder in one go.
   ```R
   cloud_s3_ls("data") %>% cloud_s3_read_bulk()
   ```
