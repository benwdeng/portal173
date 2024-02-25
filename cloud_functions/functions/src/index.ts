import * as functions from "firebase-functions";
import * as admin from "firebase-admin";
import * as sgMail from "@sendgrid/mail";

// Assign variables
const SENDER_EMAIL = "toph@frontya.com";
const SENDER_EMAIL_NAME = "TOPH Portal";
const SENDGRID_API_KEY = functions.config().sendgrid.key;
const REGISTRATION_EMAIL_ID = functions.config().sendgrid.registration_email_id;
const UNSUBSCRIBE_GROUP_ID = parseInt(
    functions.config().sendgrid.unsubscribegroupid
  );

// Initialise firebase
admin.initializeApp();
const db = admin.firestore();

// Initialise Sendgrid
sgMail.setApiKey(SENDGRID_API_KEY);

// 1. New DataUser
interface DataUser {
 user: string;
 role: string;
 dateAdded: number;
}

export const newDataUser = functions.firestore
  .document("dataUser/{dataUserId}")
  .onCreate(async (change, context) => {
    const dataUserSnap = await db
      .collection("dataUser")
      .doc(context.params.dataUserId)
      .get();

      const data = dataUserSnap.data();
      if (!data) {
        console.error("Document data is undefined.");
        return null;
      }
      const dataUser: DataUser = {
        user: data.user,
        role: data.role,
        dateAdded: data.date_added,
      };

    const dynamicTemplateData = {
      user: dataUser.user,
      role: dataUser.role,
      dateAdded: dataUser.dateAdded,
  };
    // Customer email
    const msg = {
       to: "robbiebaskin@gmail.com",
       from: {
         email: SENDER_EMAIL,
         name: SENDER_EMAIL_NAME,
       },
       templateId: REGISTRATION_EMAIL_ID,
       dynamic_template_data: dynamicTemplateData,
       asm: {
         groupId: UNSUBSCRIBE_GROUP_ID,
       },
     };

    // Send it
    return Promise.all([sgMail.send(msg)]);
  });

// 2. Add here
