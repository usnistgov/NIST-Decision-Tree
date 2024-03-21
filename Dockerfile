FROM rocker/shiny:4.3.1

RUN apt-get update -y && apt-get install -y \
    jags \
    texlive-latex-recommended \
    texlive-fonts-recommended \
    texlive-latex-extra
    
WORKDIR /srv/shiny-server/

RUN R -e "install.packages('boot')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('extraDistr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('knitr')"
RUN R -e "install.packages('metafor')"
RUN R -e "install.packages('R2jags')"
RUN R -e "install.packages('rhandsontable')"
RUN R -e "install.packages('rmarkdown')"
RUN R -e "install.packages('rmutil')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinythemes')"
RUN R -e "install.packages('spatstat')"
RUN R -e "install.packages('symmetry')"
RUN R -e "install.packages('tinytex')"
RUN R -e "install.packages('viridis')"

RUN R -e "tinytex::install_tinytex(force=TRUE)"
RUN R -e "install.packages('jsonlite')"

COPY . /srv/shiny-server

RUN rm -r 01_hello 02_text 03_reactivity 04_mpg 05_sliders 06_tabsets 07_widgets 08_html 09_upload 10_download 11_timer
RUN chmod -R 775 /srv/shiny-server
RUN chgrp -R shiny /srv/shiny-server

EXPOSE 3838