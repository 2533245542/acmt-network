# ACMT-Network

## Overview
ACMT-Network takes advantage of Docker and provides an isolated environment for running ACMT. Users can run ACMT in a web-based RStudio connected to the isolated environment and can conveniently move documents back and forth between the isolated environment and their local computers.

## Prerequisites
Docker installed  
7 GB of storage space

## Installation
1) Download this repository using `git clone`. 

2) Enter the `acmt-network` folder.

3) Configure the `.env` file to specify the states you want to download geocoding files for, and the year these files are published.
 
4) Run `docker-compose up` in terminal. When installing this tool for the first time, it could take an hour to complete (downloading files for more states takes longer).

5) After `docker-compose up` is run to complete (usually when the terminal shows a message saying that the database is ready to accept connection `db_1     | 2020-12-25 23:11:41.680 UTC [1] LOG:  database system is ready to accept connections`), leave the terminal running and open `http://localhost:8787/` in a local browser. 

6) When asked to enter username and password, enter `rstudio` for username and `acmtPassword` for password. 

A web-based RStudio window will open and it is connected to the isolated environment for running ACMT. 

7) In the web-based RStudio, go into the `workspace` folder and set the `workspace` folder as working directory. The `workspace` folder is for exchanging data between the isolated environment and your local computer. The files in the isolated environment's `workspace` will be synchronized with your local computer's `acmt-network/src/app/workspace`.

8) Test if `acmt-network` is installed successfully by sourcing `TestDockerACMT.R`. If no error occurs, the installation succeeds.

You can pause running the environment (which is in fact an instance of the installed ACMT-network) by pressing `ctrl-c` in the terminal where you installed ACMT-Network, and the work will be saved.

To resume from where you paused the last time, run `docker-compose up` in the `acmt-network` folder again.

In the above, `docker-compose up` did two things, installing ACMT-Network and creating an instance of ACMT-Network (the isolated environment we just ran). The ACMT-Network is installed in the form of Docker images (`acmt-network_app`, `acmt-network_files`, `acmt-network_api` and `acmt-network_db`) and they can be found by clicking `Images` in the left panel of Docker Dashboard. The instance of ACMT-Network is created in the form of a Docker network and it can be found by clicking `Container/Apps` in the left panel of Docker Dashboard (`acmt-network`).

When pausing the environment we just created, we were only pausing the ACMT-Network instance. As long as we did not remove the instance from the Docker Dashboard, we can always resume from where we left. Nevertheless, we can always start a new instance by running `docker-compose up` in the `acmt-network` folder (after we have removed the old instance from Docker Dashboard).

## Removing an ACMT-Network instance 
To remove an ACMT-Network instance, open Docker Dashboard, select `Containers/Apps` on the left panel; on the right panel, hover on the ACMT-Network instance you want to remove, click the stop and delete button to remove the instance. Most of the data created when running the ACMT-Network instance will be removed except for the files in `workspace` which corresponds to your local computer's `acmt-network/src/app/workspace`.

We also remove the volumes that are created when running an ACMT-Network instance. A volume is used for storing most of the data generated when using the instance. We need to manually remove volumes because it is isolated from a ACMT-Network instance.

To find dangling volumes, type `docker volume ls -f dangling=true` in the terminal; use `docker volume inspect` and `docker system df -v` to select the volumes that we want to remove, and use `docker volume prune` or `docker volume rm` to remove them.

## Uninstalling ACMT-Network
An ACMT-Network instance is created based on four Docker images that were built when installing ACMT-Network. To remove these images, in Docker Dashboard, select `Images` on the left panel; on the right panel, remove `acmt-network_app`, `acmt-network_files`, `acmt-network_api` and `acmt-network_db`.

## Reinstalling ACMT-Network after editing
If you edited ACMT-Network (changed `Dockerfile`s, `docker-compose.yml`, etc.), you might want to reinstall and instantiate ACMT-Network to see the changes. This can be done by first removing the previous ACMT-Network instance and then typing `docker-compose up --build` in the terminal. It rebuilds the Docker images and creates a new ACMT-Network instance.

## References
ACMT: https://github.com/smooney27/ACMT  
Docker-ACMT (not fully dockerized): https://github.com/smooney27/docker-acmt  
Postgis-Docker (where we borrowed the geocoder): https://github.com/uwrit/postgis-docker  
Docker tutorials: https://docs.docker.com/get-started/  