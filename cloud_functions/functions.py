# install sendgrid, firebase_admin
import os
import base64
import json
import firebase_admin
from firebase_admin import firestore, credentials
from sendgrid import SendGridAPIClient
from sendgrid.helpers.mail import Mail, Email, To

# Key variables
SENDER_EMAIL = 'toph@frontya.com'
SENDER_EMAIL_NAME = 'TOPH Portal'
SENDGRID_API_KEY = os.environ.get('SENDGRID_API_KEY')
REGISTRATION_EMAIL_ID = 'd-3f8f8076ef7c422f93294b57cf6588b3'
UNSUBSCRIBE_GROUP_ID = '23553'

# Uncomment below if running locally to connect to firebase (to generate a json prviate key go to Firebase/Settings/Service accounts/Generate new private key)
#cred = credentials.Certificate("path/to/your/firebase-adminsdk.json")
#firebase_admin.initialize_app(cred)

# Connect to firestore (only works when run in a cloud function)
if not firebase_admin._apps:
    firebase_admin.initialize_app()
db = firestore.client()

# Connect to sendgrid
sg = SendGridAPIClient(SENDGRID_API_KEY)

# Generic function to send emails
def send_email(from_email, from_name, to_email, template_id, dynamic_template_data, unsubscribe_group_id):
    message = Mail(
        from_email=Email(from_email, from_name),
        to_emails=To(to_email),
    )
    message.template_id = template_id
    message.dynamic_template_data = dynamic_template_data
    message.asm = {'group_id': int(UNSUBSCRIBE_GROUP_ID)}

    try:
        response = sg.send(message)
        print(response.status_code)
        print(response.body)
        print(response.headers)
    except Exception as e:
        print(e.message)

# Function to trigger on new document in the collection dataUser
def dataUser_new(event, context):
    # Parse the document ID from the resource name
    document_path = context.resource.split('/documents/')[1]
    document_id = document_path.split('/')[-1]

    # Fetch the document from Firestore
    doc_ref = db.document(document_path)
    doc = doc_ref.get()

    # Get relevant data from document
    if doc.exists:
        userData = doc.to_dict()
        dynamic_template_data = {
            'user': userData.get('user'),
            'role': userData.get('role'),
        }

        # Internal email
        send_email(
            from_email=SENDER_EMAIL,
            from_name=SENDER_EMAIL_NAME,
            to_email='robbiebaskin@gmail.com',
            template_id=REGISTRATION_EMAIL_ID,
            dynamic_template_data=dynamic_template_data,
            unsubscribe_group_id=int(UNSUBSCRIBE_GROUP_ID),
        )
    else:
        print(f'No data found for document with ID {document_id}.')