Steps to run locally:
1. Pip install all packages imported in functions.py
2. Save all environment variables locally (environment variables look like this os.environ.get('SENDGRID_API_KEY')). Follow these steps to save each environment variable
    - echo "export SENDGRID_API_KEY='YOUR_API_KEY'" > sendgrid.env
    - source ./sendgrid.env
3. Replace 

Steps to deploy in Google Cloud Functions: