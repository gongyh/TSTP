# Install R version 3.5
FROM r-base:3.5.3

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-openssl-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev \
    libxml2-dev

# Download and install ShinyServer (latest version)
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# Install R packages that are required
RUN R -e "install.packages(c('shiny','shinyBS','shinydashboard','shinycssloaders','shinyjs','DT','formattable','data.table','devtools','dtwclust','doParallel','ggplot2','reshape2','dplyr'), repos='https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/

# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

# set user and privileges
RUN chmod a+x /usr/bin/shiny-server.sh
RUN chown -R shiny:shiny /srv/shiny-server
RUN chown -R shiny:shiny /var/lib/shiny-server

# Log to local volume
#VOLUME /srv/shiny-server/www

CMD ["/usr/bin/shiny-server.sh"]

