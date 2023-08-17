FROM rocker/shiny:4.1.1

RUN apt-get update -y && apt-get install -y \
    jags \
    texlive-latex-recommended \
    texlive-fonts-recommended \
    texlive-latex-extra
    
#COPY renv.lock /srv/shiny-server
#COPY .Rprofile /srv/shiny-server
#COPY renv/ srv/shiny-server

WORKDIR /srv/shiny-server

RUN R -e "install.packages(c('shiny','shinythemes','symmetry','metafor'))"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('rmutil')"
RUN R -e "install.packages('spatstat')"
RUN R -e "install.packages('viridis')"
RUN R -e "install.packages('R2jags')"
RUN R -e "install.packages('rhandsontable')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('tinytex')"
RUN R -e "install.packages('rmarkdown')"
RUN R -e "install.packages('extraDistr')"
RUN R -e "install.packages('boot')"
RUN R -e "tinytex::install_tinytex()"
#RUN R -e "install.packages('renv')"
#RUN R -e "renv::restore(prompt=FALSE)"
#RUN R -e "tinytex::install_tinytex(force=TRUE)"
#RUN R -e "install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')"

COPY . /srv/shiny-server

RUN rm -r 01_hello 02_text 03_reactivity 04_mpg 05_sliders 06_tabsets 07_widgets 08_html 09_upload 10_download 11_timer
RUN chmod -R 775 /srv/shiny-server
RUN chgrp -R shiny /srv/shiny-server

EXPOSE 3838
