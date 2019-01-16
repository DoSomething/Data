sms_model
==============================

This model aims to predict whether a user will respond to a broadcast. Below are instructions for creating the datasets required for training and test:
Ensure that you are in the main directory (sms_model) while running the following commands:

1. Create a virtual environment and install the requirements: `pip3 install -r requirements.txt`
2. Create the raw dataset: `python src/data/load_raw_data.py`
3. Process the raw dataset: `python src/data/make_dataset.py [input_file]`

   Input file is the raw datset and output file is where the processed dataset will be stored.
   
   Command is: `python src/data/make_dataset.py data/raw/gambit_sample.csv.gz`
   
   This command generates several supporting files (to data/processed) and several output files (to data/final), the latter include: the final cleaned dataset, and the train, validation, and test datasets for modeling to data/final
4. Train a model: `python src/models/train_model.py [train_file] [model_type (str of variable defined in src/models/models.py)]`

   The feature pipeline and model parameters used for training are specified in src/models/models.py. This writes out serialized models to the models/folder.
   
   Command is: `python src/models/train_model.py data/final/sms_data_train.csv.gz 'rf'`
5. Grid search: `python src/models/train_model.py [train_file] [model_type] --grid-search [string defined in src/models.py]`
6. Once grid search is complete, you can either update the model params in models.py or define a new model to be used and fit
   using step 4's instructions.
7. Make predictions: `python src/models/predict_model.py [validation/test_file] [model_type] --model-verison (optional int)`

   This prints out a report with the f1 score, segments for the best 3 trees, and generates images of the top 3 performing trees in reports/[model-type-version]/figures. The default model version is the most recently written model, but this can be set to earlier models using the `model-version` flag.


Project Organization
------------

    ├── LICENSE
    ├── README.md          <- The top-level README for developers using this project.
    ├── data
    │   ├── processed      <- Intermediate data that has been transformed.
    │   ├── final      	   <- The final, canonical data sets for modeling.
    │   └── raw            <- The original, immutable data dump.
    │
    ├── models             <- Trained and serialized models, model predictions, or model summaries
    │
    ├── notebooks          <- Jupyter notebooks. Naming convention is a number (for ordering),
    │                         and a short `-` delimited description, e.g.
    │                         `1-initial-data-exploration`.
    │
    ├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
    │   └── figures        <- Generated graphics and figures to be used in reporting
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`
    │
    ├── setup.py           <- makes project pip installable (pip install -e .) so src can be imported
    ├── src                <- Source code for use in this project.
    │   ├── __init__.py    <- Makes src a Python module
    │   │
    │   ├── data           <- Scripts to download or generate data
    │   │   └── make_dataset.py
    │   │
    │   ├── models         <- Scripts to train models and then use trained models to make
    │   │   │                 predictions
    │   │   ├── predict_model.py
    │   │   └── train_model.py
    │   │
    │   └── visualization  <- Scripts to create exploratory and results oriented visualizations
    │       └── visualize.py
    │
    └── tox.ini            <- tox file with settings for running tox; see tox.testrun.org


--------

<p><small>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</small></p>
