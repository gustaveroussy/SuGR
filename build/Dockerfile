FROM shiny-igr_image

MAINTAINER Yahia ADNANI "Yahia.ADNANI@gustaveroussy.fr"

RUN apt-get update && apt-get install -y \
    mysql-client \
    libmariadb-client-lgpl-dev \
    openssh-client \
    sshpass  

COPY setup.R /root/AppToRun

##lancer le script setup.R 
RUN R -e "source('/root/AppToRun/setup.R')"