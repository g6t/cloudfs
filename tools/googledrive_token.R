googledrive::drive_auth()
token <- googledrive::drive_token()

saveRDS(token, "~/tmp/drive_token.rds")

# to read from rds
# googledrive::drive_auth(token = readRDS("~/tmp/drive_token.rds"))
