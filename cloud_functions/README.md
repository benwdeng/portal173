# FrontYa Cloud Functions

This project contains the code for the cloud functions. The cloud functions are used to send emails to customers and FrontYa administrators when you receive new signups and email newsletter subscribers.

To install dependencies

```bash
# inside /functions folder
yarn
```

The next thing you need to is to install the Firebase CLI - https://firebase.google.com/docs/cli. Otherwise all the following commands will fail.

You can set Firebase environment variables with the command line:

```bash
firebase functions:config:set sendgrid.templateinternalemailnewsletter=EXAMPLE
```

You can view Firebase environment variables with the command line:

```bash
firebase functions:config:get
```

To deploy cloud functions, you first need to build your TypeScript files into JavaScript files

```bash
# inside /functions folder
yarn build
```

Then deploy your cloud functions to Firebase

```bash
# inside /functions folder
firebase deploy --only functions
```

To troubleshoot your cloud functions please look at the Firebase console - https://console.firebase.google.com/u/0/
