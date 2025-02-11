# Things that need to be done before we can submit 

## Code changes
- Remove all hardcoded paths
  - replace with calls to here() [refers to project root]
- remove all setwd() call [setwd is evil]
  - fix all scripts with here() to work without setwd
- add empty folders to the repo to ensure paths exist to include files from 
  OSF (raw_data and data) [done]
  - maybe add a script to download OSF file to the data folder
- move preprocessing files into the raw_data directory 
   (nobody needs them unless they have raw data access) [done]
   - fix the preprocessing scripts, so they work [done]
- add information to Readme to reproduce plots
- add numbers infront of files to ensure correct order of execution
- remove unnecessary dependencies from renv.lock
- split the repo the repo in two repos for different papers
- add a license to the repo
- make the Jupityr Notbook reproducible (no absolute paths) or remove from repo

- make sure all r dependencies are available for binary installation for both windows and mac
  otherwise find versions and change dependencies in renv.lock manually
  then test script again to make sure it works [done]