
.PHONY: all
all: ../data/nzmort.rda \
     ../data/west_mx.rda \
     documentation


../data/nzmort.rda: nzmort/nzmort.R \
  nzmort/DPE403905_20230924_095611_86.csv.gz \
  nzmort/VSD349204_20230924_095835_74.csv.gz
	Rscript $^ $@

../data/west_mx.rda: west_mx/west_mx.R
	Rscript $^ $@

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"

.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
