FROM rocker/shiny:4.1.1

RUN apt-get update -y && apt-get install -y \
    jags \
    texlive-latex-recommended \
    texlive-fonts-recommended \
    texlive-latex-extra

RUN R -e "install.packages(c('shiny','ggplot2','devtools'))"
RUN R -e "install.packages('rmutil')"
RUN R -e "install.packages('rhandsontable')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('tinytex')"
RUN R -e "tinytex::install_tinytex()"
RUN R -e "devtools::install_github('usnistgov/NIST-Decision-Tree')"

COPY . /srv/shiny-server/

WORKDIR /srv/shiny-server
RUN rm -r 01_hello 02_text 03_reactivity 04_mpg 05_sliders 06_tabsets 07_widgets 08_html 09_upload 10_download 11_timer
RUN chmod -R 775 /srv/shiny-server
RUN chgrp -R shiny /srv/shiny-server

EXPOSE 3838
