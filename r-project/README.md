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

# Installation Dependencies

```bash


# Skip R from compiling from source
sudo apt update
sudo apt install -y r-cran-sf


# R 
R 
install.packages("...")

```

# Run Analysis

```bash
cd r-project
Rscript A0201011N.Rmd

```
