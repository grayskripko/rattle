.PHONY: all
all: 
	R --no-save --quiet < weather.R
	chmod go+r weather*.RData weather*.csv weather.arff weather_missing.csv
	cp locationsAUS.RData weather.RData weatherAUS.RData ../package/data/
	cp weather.csv ../package/inst/csv/
	cp weather.arff ../package/inst/arff/
	scp weatherAUS.csv weatherAUS.RData togaware.com:webapps/rattle/
