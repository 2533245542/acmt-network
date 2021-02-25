# Conda environment setup --first trial
In order to understand ACMT, we need to do tests on our local computer. 

We created a new conda environment for R. 

However, to install the `sf` package (used in ACMT), several conda dependencies need to be installed. The good news is these dependencies will only be installed in the conda envrionment and not interfere with with the local computer.

```
conda create --name r_acmt --yes
conda deactivate
conda activate r_acmt
conda install -c r r-essentials --yes
conda install -c r r-sf --yes
conda uninstall r-sf --yes
conda install -c conda-forge r-sf --yes
```

Although `sf` is installed, anaconda is not able to launch RStudio from this environment. We need to start a terminal, manually set the environment variable for RStudio and launch it from the terminal. This needs to be repeated every time starting the RStudio.

```
export RSTUDIO_WHICH_R=/Users/wzhou87/anaconda3/envs/r_acmt/bin/R
open -na Rstudio
```

Now an RStudio connected to the R environment should pop up.