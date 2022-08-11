#!/bin/bash

R_EXE="Rscript.exe"
sql_dir=$(dirname $(realpath $0))

# Move into sub-directory
cd $sql_dir
rm -f *.db

cd ../..
# TODO: check if .Rproj file exists
proj_dir=$(pwd)

cd ../NFWP
# TODO: Check if .Rprof file exists
nfwp_dir=$(pwd)

# Bootstrap package environment
# This will ensure that package dependencies are set
# echo "Setting up dependencies via 'renv'"
# $R_EXE -e "if (!requireNamespace('renv')) install.packages('renv', repos = 'https://cran.rstudio.com')"
# $R_EXE -e "library(renv); renv::autoload()"

# Create/update the NFWP database
echo "Copying the NFWP database"
nfwp_db=$nfwp_dir/data/nfwp.db 
# rm $nfwp_db
# 
# states='Taraba Kebbi Niger'
# for state in $states
# do
#     $R_EXE src/start.R $state Services
# done


# Create the tables in the database
# Start by copying the database from the project
echo "Creating the app database with its tables"
cd $sql_dir
app_db=$sql_dir/data.db

cp $nfwp_db $app_db
sqlite3 $app_db < create.sql

# Populate the tables with the data
echo "Initializing the database"
"$R_EXE" initdb.R $nfwp_dir $app_db

# Create views
echo "Creating the database views"
sqlite3 $app_db < views.sql

echo "Installing the database"
mv $app_db $proj_dir/app

cd $proj_dir
