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
  data/datos_ccaas.csv \
  data/datos_provincias.csv \
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

# https://cnecovid.isciii.es/covid19/#documentaci%C3%B3n-y-datos
data/datos_ccaas.csv:
	cd data && curl -O https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv
data/datos_provincias.csv:
	cd data && curl -O https://cnecovid.isciii.es/covid19/resources/datos_provincias.csv

data/agregados.csv:
	cd data && curl -O https://cnecovid.isciii.es/covid19/resources/agregados.csv

src/_build/R/%.md: R/%.Rmd $(ALLDATA) | src/_build/R/.
	Rscript --no-init-file --no-site-file make_knit.R $< $@

PANDOC_OPTS=-fmarkdown-implicit_figures

src/_build/R/%.html : src/_build/R/%.md
	pandoc $< --mathjax $(PANDOC_OPTS) -t html -o $@

$(DEST)/. :
	mkdir -p $@

src/_build/R/. :
	mkdir -p $@

clone :
	git clone git@github.com:CSSEGISandData/COVID-19.git JHU_data
#	git clone git@github.com:pcm-dpc/COVID-19.git ITA_data

pull :
	cd JHU_data && git pull
#	cd ITA_data && git pull

push :
	s3cmd sync $(DEST) s3://ref.castedo.com/covid19/

clean :
	rm -rf $(DEST)
	rm -rf src/_build/

.PRECIOUS : src/_build/R/%.md

