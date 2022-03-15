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

WORKDIR /srv/shiny-server
RUN rm -r 01_hello 02_text 03_reactivity 04_mpg 05_sliders 06_tabsets 07_widgets 08_html 09_upload 10_download 11_timer
RUN chmod -R 755 /srv/shiny-server

EXPOSE 3838
