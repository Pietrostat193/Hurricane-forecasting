## Contains the python files for yearly predictions with LSTM


### Prerequisity Python

In the Python files you need to have the following functioning libraries
and to change the directory once you are inside the files.
I personally run the files on Google Colab, after having it  connected to my personal drive. So there is no need to set up a working environment 
on your personal laptop. See LSTM monthly predictions to see how to connect your colab with your drive.

```python
import pandas as pd
import os
import numpy as np
from IPython.display import display
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense
```
