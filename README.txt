Notes by Alice for running the web app

1. Create a virtual environment and install flask and other packages needed in requirements.txt

When setting up the environment and app for the first time, run these commands:
conda create -n natalieenv python=3.7
conda activate natalieenv
pip install flask
pip install pandas
python app.py

To run the app in the future, you can just activate the environment first:
conda activate natalieenv
python app.py
You can now access the web app by going to http://127.0.0.1:8000
The consent form can be accessed at http://127.0.0.1:8000/consent


Counterbalancing:
If subject ID is odd: category learning, simple RL, size judgements
If subject ID is even: category learning, size judgements, simple RL


