FROM jrnold/rstan

RUN R -e "install.packages(c('shiny','shinythemes','symmetry','metafor'))"
RUN R -e "install.packages('rmutil')"
RUN R -e "install.packages('spatstat')"
RUN R -e "install.packages('viridis')"
RUN R -e "install.packages('rhandsontable')"

COPY . /

EXPOSE 3838

CMD R -e 'shiny::runApp(port=3838, host = "0.0.0.0")'