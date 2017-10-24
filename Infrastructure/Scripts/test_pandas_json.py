import json
import pandas as pd
from pandas import DataFrame as df
from pandas.io.json import json_normalize

with open('Data/cio_sample_email.json') as data_file:
    data = json.load(data_file)

pd.DataFrame(data)

flat = json_normalize(data)

list(flat)
