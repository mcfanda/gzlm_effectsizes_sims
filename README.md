# Material for the manuscript ""

Please, clone the repository to perform any of the simulations

# Simulations Results Plots

For each model type, simulations results data are contained in the model type folder. They can be plotted using:

* `plot_accuracy_r2.R`: Plot of the retrieved $R^2$ and $R_{adj}^2$ (Figure 1).
* `plot_accuracy_eta2.R`: Plot of the retrieved $\eta^2$ and $\gamma^2$ (Figure 2).
* `plot_power_r2.R`: Plot of the power estimates based on $R^2$ and $R_{adj}^2$ (Figure 3).
* `plot_power_eta2.R`: Plot of the power estimates based on $\eta^2$ and $\gamma^2$ (Figure 4).
* `plot_required_n.R`: Plot of the observed and estimated required N (Figure 5).

# Simulations Results

To obtain a new set of simulation results for each model type go into the corresponding model type folder and run:

* `simulate_r2.R`: simulate data to estimate $R^2$ and $R_{adj}^2$.
* `simulate_eta2.R`: simulate data to estimate $\eta^2$ and $\gamma^2$ .
* `simulate_n_r2.R`: simulate data to estimate the required sample size for different $R^2$.
* `simulate_n_eta2.R`: simulate data to estimate the required sample size for different $\gamma^2$ .

# Simulations from scratch

To obtain simulation results above, for each model a set of linear parameters corresponding to a given $R^2$ in the population are stored in the file `b_for_r2.RData` and `b_for_eta2.RData`. To rebuild these files from scratches, run `find_b.R` (it may be quite time-consuming).

# Estimation methods

All calculations are done with functions contained in the file `functions.R`. All checks can be done there. In the folders there's a soft link to the root folder `function.R` file. In operative systems that do not support soft-links, please copy the `function.R` file from the root to each model type folder. 

# Poisson folder

Poisson models are not simulated in the paper. The folder contains simulations for the accuracy of the indices, used to test the formulation beyond the paper material. 
