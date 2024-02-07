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
    if(inherits(DT[1, colnumber, with = FALSE][[1]], "Date")){
      rowList[[length(rowList) + 1]] <- list("stringValue" = as.character(format(DT[1, colnumber, with = FALSE], format="%Y-%m-%d")))
    } else if (is.na(DT[1, colnumber, with = FALSE][[1]])){
      rowList[[length(rowList) + 1]] <- list("stringValue" = as.character(""))
    } else {
      rowList[[length(rowList) + 1]] <- list("stringValue" = as.character(DT[1, colnumber, with = FALSE]))
    }
  }
  names(rowList) <- names(DT)
  
  data <- toJSON(list(fields = rowList), auto_unbox=TRUE)
}

# Function to convert JSON to data.table
convertJSONtoDT <- function(JSONResponse){
  dataOutput <- data.table(fromJSON(content(JSONResponse,"text"))$documents)

  colnames(dataOutput) <- gsub("fields.", "", colnames(dataOutput))
  dataOutput[, created_date := format(as.POSIXct(createTime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS"),
                                             tz = "Australia/Sydney", usetz = TRUE)]
  
  dataOutput[, last_modified_date := format(as.POSIXct(updateTime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS"),
                                            tz = "Australia/Sydney", usetz = TRUE)]
  dataOutput[, created_date := as.POSIXct(gsub(" AEDT", "", created_date))] 
  dataOutput[, last_modified_date := as.POSIXct(gsub(" AEDT", "", last_modified_date))]
  dataOutput[, updateTime := NULL]
  dataOutput[, createTime := NULL]
  
  # Clean Firestore ID
  dataOutput[, firestore_id := getFirebaseIDFromPath(name)]
  dataOutput[, name := NULL]
  
  return(dataOutput)
}

# Function to send JSON to Firestore
sendJSONToFirestore <- function(api_call, access_token, data, replacement = FALSE){
  if(replacement == TRUE){
    response <- PATCH(
      api_call, 
      add_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", access_token)
      ),
      body = data
    )
  } else {
    response <- POST(
      api_call, 
      add_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", access_token)
      ),
      body = data
    )
  }

  return(response)
}

# Get firebase from path
getFirebaseIDFromPath <- function(pathName){
  firebaseID <- substring(
    pathName, 
    regexpr("\\/[^\\/]*$", pathName) + 1, 
    # \/Matches for forward slash[^\/] matches everything but a forward slash
    # * specifies that the previous expression (everything but a dot) may occur between 0 and unlimited times
    # $ marks the end of the string.
    nchar(pathName)
  )
  return(firebaseID)
}

# Function to get JSON from Firestore
getJSONFromFirestore <- function(api_call, access_token){
  response <- GET(
    api_call, 
    add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", access_token)
    )
  )
  return(response)
}

#### Clean specific tables ####
# dataOverview
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
      created_date = as.POSIXct(created_date), # Merge two columns into 1
      matter_type,
      permit_expiry_date,
      status_of_matter,
      last_modified_date = as.POSIXct(last_modified_date),
      deleted_flag,
      firestore_id
    )
  ]
  
  # Check whether number of columns and rows match
  if(ncol(DTDataOverview) == ncol(DTDataOverviewOutput) &
     nrow(DTDataOverview) == nrow(DTDataOverviewOutput)){
    # No action
  } else {
    flog.warn("Rows or columns in Dataoverview doesn't match")
  }
  
  return(DTDataOverviewOutput)
}

# dataDraftForms
cleanFirestoreDataDraftForms <- function(DTDataDraftForms){
  DTDataDraftForms[, cert_of_title_date_issued := as.Date(cert_of_title_date_issued, "%Y-%m-%d")]
  DTDataDraftForms[, develop_permit_issue_date := as.Date(develop_permit_issue_date, "%Y-%m-%d")]
  DTDataDraftForms[, subdivision_permit_issue_date := as.Date(subdivision_permit_issue_date, "%Y-%m-%d")]
  
  # Order of the columns
  DTDataDraftFormsOutput <- DTDataDraftForms[
    ,
    .(
      unique_id = as.numeric(unique_id),
      form_id,
      matter_id,
      matter_type,
      subject_land_address,
      council_name,
      no_of_owners,
      owner_company_flag,
      owner_name,
      owner_address,
      owner_ACN,
      owner_company_director_no,
      cert_of_title_flag,
      cert_of_title_date_issued,
      cert_of_title_volume,
      cert_of_title_folio,
      develop_permit_number,
      develop_permit_issue_date,
      develop_permit_condition_number,
      subdivision_permit_number,
      subdivision_permit_issue_date,
      subdivision_permit_condition_number,
      mortgage_flag,
      mortgage_name,
      caveator_flag,
      caveator_name,
      draft_created, 
      draft_183_created,
      created_date = as.POSIXct(created_date),
      last_modified_date = as.POSIXct(last_modified_date),
      deleted_flag,
      firestore_id
    )
  ]
  
  # Check whether number of columns and rows match
  if(ncol(DTDataDraftForms) == ncol(DTDataDraftFormsOutput) &
     nrow(DTDataDraftForms) == nrow(DTDataDraftFormsOutput)){
    # No action
  } else {
    flog.warn("Rows or columns in dataDraftForms doesn't match")
  }
  
  return(DTDataDraftFormsOutput)
}

# Clean User permission table
cleanFirestoreDataDocument <- function(DTDataDocument){
  # Order of the columns
  DTDataDocumentOutput <- DTDataDocument[
    ,
    .(
      unique_id,
      matter_id,
      document_type,
      filename,
      date_uploaded = created_date,
      last_modified_date,
      deleted_flag,
      firestore_id
    )
  ]
  
  # Check whether number of columns and rows match
  if(ncol(DTDataDocument)- 1 == ncol(DTDataDocumentOutput) & # There should be 1 less columns
     nrow(DTDataDocument) == nrow(DTDataDocumentOutput)){
    # No action
  } else {
    flog.warn("Rows or columns in dataDraftDocuments doesn't match")
  }
  
  return(DTDataDocumentOutput)
}




# Clean User table
cleanFirestoreDataUser <- function(DTDataUser){
  # Order of the columns
  DTDataUserOutput <- DTDataUser[
    ,
    .(
      user,
      role,
      date_added = as.POSIXct(created_date),
      date_last_login = last_modified_date,
      deleted_flag,
      firestore_id
    )
  ]
  
  # Check whether number of columns and rows match
  if(ncol(DTDataUser)- 2 == ncol(DTDataUserOutput) & # There should be 2 less columns
     nrow(DTDataUser) == nrow(DTDataUserOutput)){
    # No action
  } else {
    flog.warn("Rows or columns in dataUser doesn't match")
  }
  
  return(DTDataUserOutput)
}

# Clean User permission table
cleanFirestoreDataUserPermission <- function(DTDataUserPermission){
  # Order of the columns
  DTDataUserPermissionOutput <- DTDataUserPermission[
    ,
    .(
      user,
      matter_id,
      date_added = created_date,
      last_modified_date,
      deleted_flag,
      firestore_id
    )
  ]
  
  # Check whether number of columns and rows match
  if(ncol(DTDataUserPermission)- 1 == ncol(DTDataUserPermissionOutput) & # There should be 1 less columns
     nrow(DTDataUserPermission) == nrow(DTDataUserPermissionOutput)){
    # No action
  } else {
    flog.warn("Rows or columns in dataUserPermission doesn't match")
  }
  
  return(DTDataUserPermissionOutput)
}



# Refresh tables

refreshAllTables <- function(session, accessToken){
  
  #### Refresh dataOverview ####
  firestore_dataOverview <- getJSONFromFirestore(
    api_call = paste0(db_endpoint_api, db_endpoint_dataOverview),
    access_token = accessToken
  )
  
  dataOverview <- convertJSONtoDT(firestore_dataOverview)
  dataOverview <- cleanFirestoreDataOverivew(dataOverview)
  saveRDS(dataOverview, path_file_fact_matter)
  
  #### Refresh dataDraftForms ####
  firestore_dataDraftForms <- getJSONFromFirestore(
    api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms),
    access_token = accessToken
  )
  
  dataDraftForms <- convertJSONtoDT(firestore_dataDraftForms)
  dataDraftForms <- cleanFirestoreDataDraftForms(dataDraftForms)
  saveRDS(dataDraftForms, path_file_fact_forms)
  
  #### Refresh dataDocument ####
  firestore_dataDocument <- getJSONFromFirestore(
    api_call = paste0(db_endpoint_api, db_endpoint_dataDocument),
    access_token = accessToken
  )
  
  dataDocument <- convertJSONtoDT(firestore_dataDocument)
  dataDocument <- cleanFirestoreDataDocument(dataDocument)
  saveRDS(dataDocument, path_file_fact_document)
  
  #### Refresh dataUser ####
  firestore_dataUser <- getJSONFromFirestore(
    api_call = paste0(db_endpoint_api, db_endpoint_dataUser),
    access_token = accessToken
  )
  
  dataUser <- convertJSONtoDT(firestore_dataUser)
  dataUser <- cleanFirestoreDataUser(dataUser)
  saveRDS(dataUser, path_file_dim_user)
  
  #### Refresh dataUserPermission ####
  firestore_dataUserPermission <- getJSONFromFirestore(
    api_call = paste0(db_endpoint_api, db_endpoint_dataUserPermission),
    access_token = accessToken
  )
  
  dataUserPermission <- convertJSONtoDT(firestore_dataUserPermission)
  dataUserPermission <- cleanFirestoreDataUserPermission(dataUserPermission)
  saveRDS(dataUserPermission, path_file_dim_user_permission)
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
# 
# # Line by line code
# dataOutput <- data.table(fromJSON(content(firestore_data,"text"))$documents)
# colnames(dataOutput) <- gsub("fields.", "", colnames(dataOutput))
# dataOutput[, test_date := as.Date("1900-01-01") + created_date]





