#!/bin/bash

# The script assumes that the JohnsHopkins and Italy git repositories have already been cloned
# and that are respectively stored in JOHNS_HOPKINS_GIT_DIR and ITALY_GIT_DIR.
JOHNS_HOPKINS_GIT_DIR="../../JohnsHopkins"
ITALY_GIT_DIR="../../COVID-19-IT"
DATA_DIR=`pwd`

# Pull and copy data from Johns Hopkins git repository
echo "[INFO] - Synchronizing data from Johns Hopkins git repository"
cd $JOHNS_HOPKINS_GIT_DIR
echo "[INFO] - Pulling git repository"
git pull 
echo "[INFO] - Copying data"
cp -u csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv $DATA_DIR/Global_JohnsHopkins
cp -u csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv $DATA_DIR/Global_JohnsHopkins
cp -u csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv $DATA_DIR/Global_JohnsHopkins


# Pull and copy data from Italy git repository
echo "[INFO] - Synchronizing data from Italy git repository"
cd $DATA_DIR
cd $ITALY_GIT_DIR
echo "[INFO] - Pulling git repository"
git pull 
echo "[INFO] - Copying data"
cp -u dati-regioni/dpc-covid19-ita-regioni.csv $DATA_DIR/Italy
