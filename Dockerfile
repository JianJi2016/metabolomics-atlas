FROM rocker/shiny-verse:latest
MAINTAINER Sajjan Singh Mehta "sajjan.s.mehta@gmail.com"

# Install system dependencies
RUN apt-get update && \
    apt-get install -y pandoc pandoc-citeproc libcurl4-gnutls-dev libcairo2-dev libxt-dev libssl-dev libssh2-1-dev && \
    apt-get install -y libudunits2-dev libgdal-dev

# Install R dependencies
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('readr', repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('sf', repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('tidyr', repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('tibble', repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('scales', repos='http://cran.rstudio.com/')" && \
    R -e "install.packages('ggpubr', repos='http://cran.rstudio.com/')"

# Remove demo Shiny project
RUN rm -rf /srv/shiny-server && mkdir /srv/shiny-server

# Set up Shiny project
COPY project.Rproj app.R brain.geojson demo_long.csv /srv/shiny-server/
COPY www /srv/shiny-server/www

# Change ownership of project root to our main user
RUN chown -R shiny:shiny /srv/shiny-server

# Set up log directory to avoid the LightSail error:
#   s6-mkdir: warning: unable to mkdir /var/run/s6: Permission denied
RUN mkdir -p /var/log/shiny-server
RUN chown shiny:shiny /var/log/shiny-server

# Open the served port
EXPOSE 3838