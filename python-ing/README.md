# Overview


# Sync AWS S3

To sync storage from `AWS S3` to `EC2` that holds data for the project. 

> [!WARNING]  
> Sync ONLY **Data** files

```bash
aws configure # allow access to AWS S3
aws s3 ls     # check if you can list the bucket
aws s3 sync <s3://bucket-name> </path/to/local/folder>
```


# Install Dependencies

```bash

sudo apt update
sudo apt install python3-venv python3-pip -y
```

# Create Virtual Environment and Activate

```bash
cd python-ing
python3 -m venv myenv
source myenv/bin/activate # important to activate

pip3 install -r requirements.txt
```



