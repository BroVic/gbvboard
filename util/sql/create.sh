#!/bin/bash

if [[ $1 == "help" ]]; then
  echo "./create.sh [init-only]"
  exit
fi

R_EXE="Rscript.exe"
sql_dir=$(dirname $(realpath $0))

# Move into sub-directory
cd $sql_dir
rm -f *.db

cd ../..
# TODO: check if .Rproj file exists
proj_dir=$(pwd)

app_dir=$proj_dir/app
app_db=$app_dir/data.db
rm -f $app_db

cd ../NFWP
# TODO: Check if .Rprof file exists
nfwp_dir=$(pwd)

# Bootstrap package environment
# This will ensure that package dependencies are set
# echo "Setting up dependencies via 'renv'"
# $R_EXE -e "if (!requireNamespace('renv')) install.packages('renv', repos = 'https://cran.rstudio.com')"
# $R_EXE -e "library(renv); renv::autoload()"

# Create/update the NFWP database
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
cp $nfwp_db $app_db
sqlite3 $app_db < create.sql

if [[ $1 == "init-only" ]]; then
  echo "Only table creation operation was carried out"
  exit
fi

# Populate the tables with the data
echo "Initializing the NFWP database"
"$R_EXE" initdb.R $nfwp_dir $app_db

cd $proj_dir
cd ../NEDC
# TODO: Check if .Rprof file exists
nedc_dir=$(pwd)
cd $sql_dir

echo "Populating the database from NEDC"
"$R_EXE" initdb.R $nedc_dir $app_db

# Create views
echo "Creating the database views"
sqlite3 $app_db < views.sql

cd $proj_dir
