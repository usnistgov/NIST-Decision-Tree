FROM rocker/shiny:4.1.1

RUN apt-get update -y
RUN apt-get install jags -y 

RUN R -e "install.packages(c('shiny','shinythemes','symmetry','metafor'))"
RUN R -e "install.packages('rmutil')"
RUN R -e "install.packages('spatstat')"
RUN R -e "install.packages('viridis')"
RUN R -e "install.packages('R2jags')"
RUN R -e "install.packages('rhandsontable')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('tinytex')"

COPY . /srv/shiny-server/

RUN chown -R 777 /srv/shiny-server

EXPOSE 3838
