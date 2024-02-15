###################################################################################################
#
# Portal 173 - Set up tables
#
###################################################################################################

library(data.table)

setwd("C:/GIT/portal173/TOPH/app")
getwd()
source("global.R")


# Adjust table
dataOverview <- readRDS("data/dataOverview")
dataDraftForms <- readRDS("data/dataDraftForms")
dataDocument <- readRDS("data/dataDocument")
dataUser <- readRDS("data/dataUser")
dataUserPermission <- readRDS("data/dataUserPermission")

# Function to send entire tables to FireBase
sentTabletoFirebase <- function(dt, db_endpoint, access_token){

  # check if data.table has firestore_id, if not add a dummy variables
  if(!"firestore_id" %in% colnames(dt)){
    dt[, firestore_id := "0"]
  }
  
  # Loop through each row
  for(entry in 1:nrow(dt)){
    if(dt[entry, firestore_id] == "0"){
      # Create a new entry within Firebase storage
      jsontable <- convertDTtoJSON(dt[entry])
      firestore_response <- sendJSONToFirestore(
        api_call = paste0(db_endpoint_api, db_endpoint),
        access_token = access_token,
        data = jsontable
      )
      
      # Add firestore_id
      firestore_id_entry <- getFirebaseIDFromPath(fromJSON(content(firestore_response,"text"))$name)
      dt[entry, firestore_id := firestore_id_entry]
    } else {}
    
    # Now with a firestore ID, send row again with firestore ID
    jsontable <- convertDTtoJSON(dt[entry])
    firestore_response <- sendJSONToFirestore(
      api_call = paste0(db_endpoint_api, db_endpoint, "/", dt[entry, firestore_id]), 
      access_token = access_token, 
      data = jsontable,
      replacement = TRUE
    )
  }

  flog.info("Completed sending table to Firebase")
  return(dt)
}




# Authorisation
sign.in <- function(email, password, api_key) {
  #https://firebase.google.com/docs/reference/rest/auth/#section-sign-in-email-password
  r <- POST(paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=", api_key),
            add_headers("Content-Type" = "application/json"),
            body = toJSON(list(email = email, password = password, returnSecureToken = TRUE), auto_unbox=TRUE))
  return(content(r))
}

my_auth_info <- sign.in("edwin_zhang2good@msn.com", "teiquidi", "AIzaSyB0mtV-T7LKAcHffieIlQhY26GuIRChm1k")
auth_token <- my_auth_info$idToken


# Export tables to Firebase
data <- readRDS(path_file_fact_document)
data[, deleted_flag := 0]
data <- sentTabletoFirebase(data, db_endpoint_dataDocument, auth_token)
saveRDS(data, path_file_fact_document)


# Send data
entry <- dataDraftForms[unique_id == 1]
dataJSON <- convertDTtoJSON(entry)

firestore_response <- sendJSONToFirestore(
  api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms),
  access_token = auth_token,
  data = dataJSON
)

# Add firestore_id
firestore_id_entry <- getFirebaseIDFromPath(fromJSON(content(firestore_response,"text"))$name)

entry[, firestore_id := firestore_id_entry]

firestore_data <- getJSONToFirestore(
  api_call = paste0(db_endpoint_api, db_endpoint_dataOverview),
  access_token = auth_token
)

firestore_data$status_code
data <- convertJSONtoDT(firestore_data)
data <- cleanFirestoreDataDocument(data)

colnames(data)
str(data)


# Get firebase data
#### Refresh dataOverview ####
firestore_data <- getJSONFromFirestore(
  api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms),
  access_token = auth_token
)

data <- convertJSONtoDT(firestore_data)
data <- cleanFirestoreDataDocument(data)
saveRDS(data, path_file_dim_user_permission)
