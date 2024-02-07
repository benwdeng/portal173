# README #
### What is this repository for? ###
This repository is designed for the Portal 173 dashboard

### How do you access the dashboard online ###


### Sources of data ###
Longtitude and latitude by postcode => https://www.matthewproctor.com/australian_postcodes
Information about Victoria councils => , https://knowyourcouncil.vic.gov.au/councils

### Who do I talk to? ###
Creator and designer is Edwin Zhang


### Docker instructions ###
# On R terminal
Navigate to folder
cd C:\GIT\portal173\TOPH
docker build -t onlineplanninghub .
docker run -p 8080:3838 onlineplanninghub 

# On Google Cloud Shell
docker tag onlineplanninghub gcr.io/the-online-planning-hub/onlineplanninghub
docker push gcr.io/the-online-planning-hub/onlineplanninghub



### Set up instructions on new computer ###
1. Download and install Cloud SDK - https://cloud.google.com/sdk/docs/install
2. Download and install Docker 
3. Run the following in Google cloud shell - gcloud services enable containerregistry.googleapis.com
3. Run the following command in SDK for credentials - gcloud auth login 
4. Run the following command in SDK use for authentication- gcloud auth configure-docker