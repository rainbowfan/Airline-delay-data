cp 1987.csv AirlineData.csv
for year in {1988..2008}
    do
      tail -n+2 $year.csv >> AirlineData.csv
    done