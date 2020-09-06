DEST = ~/public_html/covid19/
ifndef PRODUCTION
ifdef OFFLINE
FLAGS = OFFLINE
endif
else
FLAGS = PRODUCTION
endif

.PHONY : all site Rmarkdown push clean

ALLDATA = \
  NYT_data/us-counties.csv \
  JHU_data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv \
  JHU_data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv \
  JHU_data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv \
  JHU_data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv \
  JHU_data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv \
  src/lex/lexington-cases.csv

all: site

data/. :
	mkdir -p $@

RMDS := $(patsubst R/%.Rmd,%,$(wildcard R/*.Rmd))

Rmarkdown: $(patsubst %,src/_build/R/%.html,$(RMDS))

site: Rmarkdown
	rsync -rt --copy-links --exclude='*.jinja' --exclude='.*' --exclude='_*' src/ $(DEST)
	jinjagen -m jinjagenadd:$(FLAGS) --root src/  $(DEST)

data/agregados.csv:
	cd data && curl -O https://cnecovid.isciii.es/covid19/resources/agregados.csv

src/_build/R/%.md: R/%.Rmd $(ALLDATA) | src/_build/R/.
	Rscript --no-init-file --no-site-file make_knit.R $< $@

PANDOC_OPTS=--filter pandoc-citeproc --bibliography=covid19.bib -fmarkdown-implicit_figures

src/_build/R/%.html : src/_build/R/%.md
	pandoc $< --mathjax $(PANDOC_OPTS) -t html -o $@

$(DEST)/. :
	mkdir -p $@

src/_build/R/. :
	mkdir -p $@

clone :
	git clone git@github.com:CSSEGISandData/COVID-19.git JHU_data
	git clone git@github.com:nytimes/covid-19-data.git NYT_data
	git clone git@github.com:pcm-dpc/COVID-19.git ITA_data

pull :
	cd JHU_data && git pull
	cd NYT_data && git pull
	cd ITA_data && git pull

push :
	s3cmd sync $(DEST) s3://ref.castedo.com/covid19/

clean :
	rm -rf $(DEST)
	rm -rf src/_build/

.PRECIOUS : src/_build/R/%.md

