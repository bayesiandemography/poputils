
.PHONY: all
all: ../data/booth_standard.rda \
     ../data/irn_fert.rda \
     ../data/nzl_mort.rda \
     ../data/nzl_mort_rvec.rda \
     ../data/west_lifetab.rda \
     documentation

../data/booth_standard.rda: booth_standard.R
	Rscript $^ $@

../data/irn_fert.rda: irn_fert/irn_fert.R \
  irn_fert/table_4.1.csv \
  irn_fert/table_4.2.csv
	Rscript $^ $@

../data/nzl_mort.rda: nzl_mort/nzl_mort.R \
  nzl_mort/DPE403905_20230924_095611_86.csv.gz \
  nzl_mort/VSD349204_20230924_095835_74.csv.gz
	Rscript $^ $@

../data/nzl_mort_rvec.rda: nzl_mort/nzl_mort_rvec.R \
  ../data/nzl_mort.rda
	Rscript $^ $@

../data/west_lifetab.rda: west_lifetab/west_lifetab.R
	Rscript $^ $@

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"

.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
