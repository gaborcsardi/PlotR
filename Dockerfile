FROM inwt/r-shiny:4.0.1

RUN apt-get update && apt-get install -y libgsl-dev

ADD . .

RUN installPackage

CMD ["Rscript", "inst/R_Code/startApplication.R"]
