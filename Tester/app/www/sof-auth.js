

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




$(document).on("click", "#download_button_js", () => {
	const storage = firebase.storage();
	console.log("Check input start");
	
	alert("Step1");
	console.log("Check input end");
	var storageRef = storage.ref('Documents/MA0001/Certificate Title Example name 2.jpg');
	
	storageRef
	.getDownloadURL()
	.then((url) => {
		console.log(url);
		console.log("Within Download function");
		document.getElementById('my_iframe').src = url;
		Shiny.setInputValue("downloadPostCode",  url);
		console.log("Set value end");
	});
	
})

$(document).on("click", "#filelist_button_js", () => {
	const storage = firebase.storage();
	console.log("Start run");
	alert("Step2");
	var storageRef = storage.ref("Documents");
	storageRef.listAll()
	.then((fileList) => {
		var ListfileNameFromSource = [];   
		fileList.items.forEach((itemRef) => {
			var fileNameFromSource = itemRef.name;
			console.log(fileNameFromSource);
			ListfileNameFromSource.push(fileNameFromSource);
		});
		Shiny.setInputValue("listOfFiles", ListfileNameFromSource);
		// Check this out: onInputChange
	});
	console.log("End run");
	//alert("Hello! I am an alert box!!");	
})


$(document).on("change", "#file1", () => {
	console.log("Test");
	console.log($("#file1")[0].files[0].name);
	const selectedFile = $("#file1")[0].files[0];
	//const file = $("#file1").file();
	console.log("Step1");
	console.log(selectedFile);
	console.log("Step2");
	console.log($("#file1")[0]);
	console.log("Step2");
	console.log($("#file1"));
	const storage = firebase.storage();
	storage
    .ref('Documents/' + selectedFile.name)
    .put(selectedFile);
	//const fileConv = file.target.files[0];
	console.log("Success");
});



