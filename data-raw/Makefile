
.PHONY: all
all: ../data/nzmort.rda \
     ../data/nzmort_rvec.rda \
     ../data/west_mx.rda \
     ../data/west_qx.rda \
     documentation


../data/nzmort.rda: nzmort/nzmort.R \
  nzmort/DPE403905_20230924_095611_86.csv.gz \
  nzmort/VSD349204_20230924_095835_74.csv.gz
	Rscript $^ $@

../data/nzmort_rvec.rda: nzmort/nzmort_rvec.R \
  ../data/nzmort.rda
	Rscript $^ $@

../data/west_mx.rda: west_mx/west_mx.R
	Rscript $^ $@

../data/west_qx.rda: west_qx/west_qx.R
	Rscript $^ $@

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"

.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
