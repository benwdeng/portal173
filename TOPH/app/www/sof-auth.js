

const config = {
  apiKey: "AIzaSyB0mtV-T7LKAcHffieIlQhY26GuIRChm1k",
  authDomain: "529031071276-3bdfd3uucs748booun3edk38jrrfbbf5.apps.googleusercontent.com",
  projectId: "the-online-planning-hub",
  storageBucket: "gs://the-online-planning-hub.appspot.com"
}

firebase.initializeApp(config)

const auth = firebase.auth()

$(document).on("click", "#submit_sign_in", () => {
  const email = $("#email").val()
  const password = $("#password").val()

  auth.signInWithEmailAndPassword(email, password)
  .catch((error) => {
    showSnackbar("sign_in_snackbar", "Error: " + error.message)
    console.log("sign in error: ", error)
  })
})



$(document).on("click", "#submit_sign_out", () => {
  auth.signOut()
  .catch((error) => {
    console.log("Sign Out Error", error)
  })
})

$(document).on("click", "#verify_email_submit_sign_out", () => {
  auth.signOut()
  .catch((error) => {
    console.log("Sign Out Error", error)
  })
})

auth.onAuthStateChanged((user) => {
  // when user signs in or out send the info about that user to Shiny as
  // a Shiny input `input$sof_auth_user`
  Shiny.setInputValue('sof_auth_user', user);
})

$(document).on("click", "#resend_email_verification", () => {

  const user = auth.currentUser

  user.sendEmailVerification()
  .then(() => {
    showSnackbar("verify_email_snackbar", "verification email send to " + user.email)
  })
  .catch((error) => {
    showSnackbar("verify_email_snackbar", "Error: " + error.message)
    console.error('error sending email verification', error)
  })
})



$(document).on("click", "#submit_register", () => {

  const email = $("#register_email").val()
  const password = $("#register_password").val()
  const password_2 = $("#register_password_verify").val()
  console.log(
    "submit_register",
    email,
    password,
    password_2
  )
  if (password === password_2) {
    auth.createUserWithEmailAndPassword(email, password).then((user) => {

      user.user.sendEmailVerification()
      .catch((error) => {
        showSnackbar("register_snackbar", "Error: " + error.message)
        console.log("error sending email verification: ", error)
      })

    })
    .catch((error) => {
      showSnackbar("register_snackbar", "Error: " + error.message)
    })

  } else {
    showSnackbar("register_snackbar", "Error: the passwords do not match")
  }
})

$(document).on("click", "#reset_password", () => {
  const email = $("#email").val()

  console.log("reset password ran")
  auth.sendPasswordResetEmail(email).then(() => {
    showSnackbar("sign_in_snackbar", "Reset email sent to " + email)
  }).catch((error) => {
    showSnackbar("sign_in_snackbar", "Error: " + error.message)
    console.log("error resetting email: ", error)

  })
})

// the event handler listens to shiny for messages send by handler1
// if it receives a message, call the callback function doAwesomething and pass the message
Shiny.addCustomMessageHandler("folderLocation", grabMatterID);

// this function is called by the handler, which passes the message
function grabMatterID(sendMatterID){
  // Window is required to declar global variable
  window.uploadForMatterID = sendMatterID;
}

$(document).on("click", "#document_list_button_js", () => {
	const storage = firebase.storage();
	var storageRef = storage.ref("Documents/" + window.uploadForMatterID);
	storageRef.listAll()
	.then((fileList) => {
		var documentFileNameFromSource = [];  
		fileList.items.forEach((itemRef) => {
			var DocumentNameFromSource = itemRef.name;
			documentFileNameFromSource.push(DocumentNameFromSource);
		});
		Shiny.setInputValue("listOfDocuments", documentFileNameFromSource);
		console.log("Pulled list of documents")
	});
})

$(document).on("change", "#overview_tab-docUpload", () => {
	const selectedFile = $("#overview_tab-docUpload")[0].files[0];
	const storage = firebase.storage();
	storage
    .ref('Documents/' + window.uploadForMatterID + '/' + selectedFile.name)
    .put(selectedFile);
	Shiny.setInputValue("fileUploaded", selectedFile.name);
	console.log("File upload successful");
});

$(document).on("click", "#download_button_js", () => {
	const storage = firebase.storage();
	var storageRef = storage.ref("Documents/" + window.uploadForMatterID + 'S1730001 Section 173 Agreement draft.docx');
	
	storageRef
	.getDownloadURL()
	.then((url) => {
		document.getElementById('my_iframe').src = url;
		Shiny.setInputValue("downloadPostCode",  url);
		console.log("Download complete");
	});
	
})

const downloadFunction = (fileName, matterID) => {
  console.log(matterID + fileName);
  // The Math object contains the max method
  return("Done")
}