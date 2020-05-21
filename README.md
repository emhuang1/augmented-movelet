This is the R code corresponding to the paper, “Augmented Movelet Method for Activity Classification Using Smartphone Gyroscope and Accelerometer Data” by E. Huang and J.P. Onnela.

The code is organized as follows:

“functions” folder: This contains functions that are used in the code in the “movelet” folder and the “plotting_data” folder.

“movelet” folder: This contains the code to implement the movelet method to smartphone gyroscope and accelerometer data. The different scripts are for the different options for distance metric (L2 or correlation) and data type (tri-axial data or magnitude). 

“plotting_data” folder: This contains the code for plotting smartphone accelerometer and gyroscope data. We provide code for plotting the tri-axial data as well as the magnitude data.

“process_results” folder: This contains the code for processing the results from running the code in the “movelet” folder.

“uncertainty_quantification” folder: This contains the code for implementing our uncertainty quantification method.
