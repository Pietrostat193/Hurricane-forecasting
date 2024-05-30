# Yearly Hurricane Forecasting

Hurricane Forecasting Project. This is the repository of the paper "Simple yet effective: a comparative study of statistical models for
yearly hurricane forecasting" developed jointly with Phillip Otto (https://www.gla.ac.uk/schools/mathematicsstatistics/staff/philippotto/)
and Raffaele Mattera (https://web.uniroma1.it/disse/node/7826).

![Project Logo](path/to/logo.png

## Table of Contents

- [About the Project](#about-the-project)
- [Getting Started](#getting-started)
  - [Prerequisites R](#prerequisitesR)
  - [Prerequisity Python](#prerequisitesPython)
- [Usage](#usage)
- [Contact](#contact)
- [Acknowledgements](#acknowledgements)

## About the Project

we study the problem of forecasting the next year’s number of Atlantic
hurricanes, which is relevant in many fields of applications such as land-use planning,
hazard mitigation, reinsurance and long-term weather derivative market. Consider-
ing a set of well-known predictors, we compare the forecasting accuracy of both
machine learning and simpler models, showing that the latter may be more adequate
than the first. Quantile regression models, which are adopted for the first time for
forecasting hurricane numbers, provide the best results. Moreover, we construct a
new index showing good properties in anticipating the direction of the future num-
ber of hurricanes. We consider different evaluation metrics based on both magnitude
forecasting errors and directional accuracy.

## Getting Started

To Get start you need an R studio and Python working Installation and the Library listed below.
The repository contains a set of models that training using a R libraries, while LSTM is trained using 
a Python library.

### Prerequisites R

 List of libraries R libraries that need to be installed before setting up the project.

```r
# List of libraries
libraries <- c("dplyr", "car", "tscount", "quantreg", "ProbCast", "readr", 
               "tidyverse", "ggplot2", "plotly", "scatterplot3d")

# Function to check and install libraries
install_if_missing <- function(lib) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
    library(lib, character.only = TRUE)
  }
}

# Apply the function to each library
sapply(libraries, install_if_missing)
```

### Prerequisity Python

In the Python files you need to have the following functioning libraries
and to change the directory once you are inside the files.

```python
import pandas as pd
import os
import numpy as np
from IPython.display import display
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense
```

## Usage

Instructions and examples for using the project. Show code snippets or screenshots as necessary.


## Contact

Provide ways for users to get in touch with you.

- **Pietro Colombo** - [https://www.linkedin.com/in/pietro-colombo-62b295129/]

Project Link: "To be put on Archive")

## Acknowledgements

We thanks Jethro Browell (https://github.com/jbrowell) and Gabriel Dantas (https://www.gla.ac.uk/pgrs/gabrieldantasdeoliveirarolim/) for their insightfull contribution for starting the project.



