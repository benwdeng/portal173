###################################################################################################
#
# Portal 173 - Firestore functions
#
###################################################################################################

# Function to convert data.table to JSON
convertDTtoJSON <- function(DT){
  # List
  rowList = list()
  for (colnumber in 1:ncol(DT)){
    rowList[[length(rowList) + 1]] <- list("stringValue" = as.character(DT[1, colnumber, with = FALSE]))
  }
  names(rowList) <- names(DT)
  
  data <- toJSON(list(fields = rowList), auto_unbox=TRUE)
}

# Function to convert JSON to data.table
convertJSONtoDT <- function(JSONResponse){
  dataOutput <- data.table(fromJSON(content(JSONResponse,"text"))$documents)

  colnames(dataOutput) <- gsub("fields.", "", colnames(dataOutput))
  dataOutput[, matter_created_date := format(as.POSIXct(createTime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS"),
                                             tz = "Australia/Sydney", usetz = TRUE)]
  
  dataOutput[, last_modified_date := format(as.POSIXct(updateTime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS"),
                                            tz = "Australia/Sydney", usetz = TRUE)]
  dataOutput[, matter_created_date := gsub(" AEDT", "", matter_created_date)] 
  dataOutput[, last_modified_date := gsub(" AEDT", "", last_modified_date)]
  dataOutput[, updateTime := NULL]
  dataOutput[, createTime := NULL]
  
  # Clean Firestore ID
  dataOutput[, name := gsub('(?<![0-9])/(?![0-9])', '', name, perl = TRUE)]
  dataOutput[, firestore_id := substring(name, gregexpr('documents', name)[[1]][1]+21, nchar(name))]
  dataOutput[, name := NULL]
  
  return(dataOutput)
}

# Function to send JSON to Firestore
sendJSONToFirestore <- function(api_call, access_token, data){
  response <- POST(
    api_call, 
    add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", access_token)
    ),
    body = data
  )
  return(response)
}

# Function to get JSON from Firestore
getJSONToFirestore <- function(api_call, access_token){
  response <- GET(
    api_call, 
    add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", access_token)
    )
  )
  return(response)
}

# Clean specific tables
cleanFirestoreDataOverivew <- function(DTDataOverview){
  DTDataOverview[, permit_expiry_date := as.Date(permit_expiry_date, "%Y-%m-%d")]
  
  # Order of the columns
  DTDataOverviewOutput <- DTDataOverview[
    ,
    .(
      unique_id = as.numeric(unique_id),
      matter_id,
      address,
      address_postcode,
      council_name,
      client_name,
      client_role,
      client_postal_address,
      client_contact_number,
      client_email,
      matter_created_date = as.POSIXct(matter_created_date),
      matter_type,
      permit_expiry_date,
      status_of_matter,
      last_modified_date = as.POSIXct(last_modified_date),
      firestore_id
    )
  ]
  
  if(ncol(DTDataOverview) == ncol(DTDataOverviewOutput) &
     nrow(DTDataOverview) == nrow(DTDataOverviewOutput)){
    # No action
  } else {
    flog.warn("Rows or columns in Dataoverview doesn't match")
  }
  
  return(DTDataOverviewOutput)
}

# Refresh tables

refreshAllTables <- function(session){
  firestore_data <- getJSONToFirestore(
    api_call = paste0(db_endpoint_api, db_endpoint_dataOverview),
    access_token = session$userData$current_user()$stsTokenManager$accessToken
  )
  
  data <- convertJSONtoDT(firestore_data)
  data <- cleanFirestoreDataOverivew(data)
  saveRDS(data, "data/dataOverview")
}


#### Area to test ####
# sign.in <- function(email, password, api_key) {
#   #https://firebase.google.com/docs/reference/rest/auth/#section-sign-in-email-password
#   r <- POST(paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=", api_key),
#             add_headers("Content-Type" = "application/json"),
#             body = toJSON(list(email = email, password = password, returnSecureToken = TRUE), auto_unbox=TRUE))
#   return(content(r))
# }
# 
# my_auth_info <- sign.in("edwin_zhang2good@msn.com", "teiquidi", "AIzaSyB0mtV-T7LKAcHffieIlQhY26GuIRChm1k")
# auth_token <- my_auth_info$idToken
# 
# firestore_data <- getJSONToFirestore(
#   api_call = paste0(db_endpoint_api, db_endpoint_dataOverview),
#   access_token = auth_token
# )
# 
# firestore_data$status_code
# data <- convertJSONtoDT(firestore_data)
# data <- cleanFirestoreDataOverivew(data)
# 
# colnames(data)
# str(data)
