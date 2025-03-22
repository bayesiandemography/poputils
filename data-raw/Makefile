
.PHONY: all
all: ../data/iran_fertility.rda \
     ../data/nzmort.rda \
     ../data/nzmort_rvec.rda \
     ../data/west_lifetab.rda \
     documentation


../data/iran_fertility.rda: iran_fertility/iran_fertility.R \
  iran_fertility/table_4.1.csv \
  iran_fertility/table_4.2.csv
	Rscript $^ $@

../data/nzmort.rda: nzmort/nzmort.R \
  nzmort/DPE403905_20230924_095611_86.csv.gz \
  nzmort/VSD349204_20230924_095835_74.csv.gz
	Rscript $^ $@

../data/nzmort_rvec.rda: nzmort/nzmort_rvec.R \
  ../data/nzmort.rda
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
