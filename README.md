ACMT is a tool for gathering environmental measure (e.g. population, age) for a given area. 
The ACMT also has a built-in geocoder that can be used to locally geocode address data. 

Below are instructions for setting up the ACMT on your computer. 

**1 Install Docker**
If you are not the admin on your computer, you must have IT install Docker and add you to the docker-user group

*Adding non-admin users to the Docker User group*

- Run Computer Management as an administrator
- Navigate to Local Users and Groups > Groups > docker-users
- Right-click to add the user to the group
- Sign out and sign back in for the changes to take effect.

*If you have administrative privileges, you can download and install Docker yourself:*

- Download and install the Docker
- Go to https://www.docker.com/
- Click on the “Get Started” button in the top right corner
- Click on the Download button that works for your computer – if you have a Macbook with an apple chip, select apple chip.
- Once the download completes, follow the instructions to complete installation.

*NOTE: if you do not have administrator privileges, you will need your IT to download and install Docker for you and make you a user in the Docker-user group so that you can run Docker as an admin. See here for more instructions: https://docs.docker.com/desktop/install/windows-install/#install-docker-desktop-on-windows*

**2 Download the ACMT Source code**
- Go to https://github.com/2533245542/acmt-network
- Click on the green ‘Code’ button, then select Download ZIP.
- Unzip the ACMT Code to the documents folder on your computer (i.e., C:) and rename the folder to acmt-network.

**3 Edit the .env file to include specific states for geocoder**
- In the ‘acmt-network’ folder, find the file named .env (If you do not see the file named .env on MacOS, you may need to press “command + shift + .” to make finder show you hidden files) and open it in Notepad.
- Edit the GEOCODER_STATES line to include any state that you would like included:i.e., GEOCODER_STATES=WA, MN, TX
- WA does not need to be included, but if you do not include it, you may need to enter an address from your study instead in the testing section below
- To include all states, put GEOCODER_STATES=* (note that the more state you include the longer the ACMT will take to install)
- Save and close the file

**Mount Local Folders**
- If you need to retain participant addresses within a specific folder (i.e., if you are accessing participant address data on a server folder), you can update the 

**4 Setup the ACMT: Mount Docker Containers**
*Open Command Prompts (PC) or Terminal (Mac)*

- In Windows: https://www.howtogeek.com/235101/10-ways-to-open-the-command-prompt-in-windows-10/
- On a Mac: https://www.howtogeek.com/682770/how-to-open-the-terminal-on-a-mac/

*Navigate to the acmt-network folder in the terminal*
- Type cd then the folder path, see examples below
- You can also check the contents of the folder by typing ‘ls’ or ‘dir’, for example to ensure you are in the right folder.

*Build ACMT Docker Containers*
- In the terminal, type: **docker-compose up –build** (assure you have two dashes in front of build)

This process can take 30 minutes or longer (depending on how many states you selected to load in). Expect an additional 30 minutes or so for each additional state (the actual length depends on your computer speed).
See this Google Doc for installing and using ACMT.

**Set up ACMT**
- Once installation is complete, navigate to Docker and ensure all of the acmt-network containers are running. When they are green, they are running. Click the triangle play button to start the conatiners, or the square stop button to stop them when not using the ACMT (circled in red below).
- Start the acmt-network container in the Docker (play button)
- Navigate to http://localhost:8787/ in a web browser and you should see a Rstudio workspace in your browser.

**Run ACMT Shiny App**
- In Rstudio, navigate to workspace/AMCT_Shiny and find the ACMT Shiny App.R file.
- Open the file and click 'Run App' in the the top right.
- A new window will open the ACMT Shiny app, which will walk you through geocoding and pulling Neighborhood Measures. 
