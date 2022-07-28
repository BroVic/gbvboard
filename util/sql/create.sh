#!/bin/bash

R_EXE="Rscript.exe"
sql_dir=$(dirname $(realpath $0))

# Move into the project directory
cd $sql_dir
rm -f *.db

cd ../..
proj_dir=$(pwd)

cd ../NFWP
nfwp_dir=$(pwd)

# Bootstrap package environment
# This will ensure that package dependencies are set
# echo "Setting up dependencies via 'renv'"
# $R_EXE -e "if (!requireNamespace('renv')) install.packages('renv', repos = 'https://cran.rstudio.com')"
# $R_EXE -e "library(renv); renv::autoload()"

# Create/update the NFWP database
# echo "Creating the NFWP database"
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
cd $nfwp_dir
"$R_EXE" $sql_dir/initdb.R . $app_db

# Create views
echo "Creating the database views"
cd $sql_dir
sqlite3 $app_db < views.sql

echo "Installing the database"
mv $app_db ../../app

cd $proj_dir
