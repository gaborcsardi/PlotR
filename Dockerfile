FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

RUN installPackage

CMD ["Rscript", "inst/R_Code/startApplication.R"]
