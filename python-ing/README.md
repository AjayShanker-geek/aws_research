# Overview


# Sync AWS S3

To sync storage from `AWS S3` to `EC2` that holds data for the project. 

> [!WARNING]  
> Sync ONLY **Data** files

```bash
cd python-ing # IMPORTANT: change to the project directory
aws configure # allow access to AWS S3
aws s3 ls     # check if you can list the bucket
aws s3 cp s3://python-ing .
```


# Install Dependencies

```bash

sudo apt update
sudo apt install python3-venv python3-pip jupyter jupyter-notebook  -y
```

# Create Virtual Environment and Activate

```bash

python3 -m venv myenv
source myenv/bin/activate # important to activate

pip3 install -r requirements.txt
```


# Execute Jupyter without Notebook

```bash
jupyter nbconvert --to notebook --execute Assignment 1 NLP-.ipynb
```
```
