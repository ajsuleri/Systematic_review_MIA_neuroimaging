library(rsconnect)

# to be filled in based on account information
account_name <- XXX
insert_token <- XXX
insert_secret_code <- XXX

# deploy app 
rsconnect::setAccountInfo(name='account_name', token='insert_token', secret='insert_secret_code')

rsconnect::deployApp(appDir = "set_directory", appName = 'MIA_imaging_review_app')
