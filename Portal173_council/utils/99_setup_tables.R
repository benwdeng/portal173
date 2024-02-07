###################################################################################################
#
# Portal 173 - Set up tables
#
###################################################################################################

library(data.table)

setwd("C:/GIT/portal173/Portal173")
getwd()

#### Create empty data table for overview page to track status -----------------------------------------------
dataOverview = data.table(
  matter_id = character(),
  address = character(),
  client_name = character(),
  matter_created_date = as.POSIXct(character()),
  matter_type = character(),
  permit_expiry_date = as.Date(character()),
  status_of_matter = character(),
  last_modified_date = as.Date(character())
)

str(dataOverview)



saveRDS(dataOverview, "data/dataOverview")

# Adjust table
dataOverview <- readRDS("data/dataOverview")
dataOverview[, client_contact_number := ""]
dataOverview[, client_email := ""]


dataOverview <- dataOverview[,
  .(
    unique_id,
    matter_id,
    address,
    address_postcode,
    client_name,
    client_contact_number,
    client_email,
    matter_created_date,
    matter_type,
    permit_expiry_date,
    status_of_matter,
    last_modified_date = as.POSIXct(last_modified_date)
  )
]


#### Create empty data table for saved forms -----------------------------------------------
dataDraftForms = data.table(
  matter_id = character(),
  subject_land_address = character(),
  council = character(),
  last_modified_date = as.Date(character())
)

str(dataDraftForms)

setwd("C:/GIT/portal173/Portal173")
getwd()

saveRDS(dataDraftForms, "data/dataDraftForms")

# Adjust table
dataDraftForms <- readRDS("data/dataDraftForms")
dataDraftForms[, matter_type := "Lodge"]


dataDraftForms <- dataDraftForms[,
                             .(
                               unique_id,
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
                               last_modified_date
                             )
]

#### Create empty data table for document listing -----------------------------------------------
dataDocuments = data.table(
  matter_id = character(),
  type_of_document = character(),
  filename = character(),
  date_uploaded = as.POSIXct(character())
)

str(dataDocuments)

setwd("C:/GIT/portal173/Portal173")
getwd()

saveRDS(dataDocuments, "data/dataDocuments")

# Adjust table
dataDocuments <- readRDS("data/dataDocuments")
dataDocuments[, document_type := type_of_document]

dataDocuments <- dataDocuments[
  ,
  .(
    unique_id,
    matter_id,
    document_type,
    filename,
    date_uploaded
  )
  ]

