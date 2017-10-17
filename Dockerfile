FROM ubuntu:17.04

MAINTAINER Kacper Sokol <ks1591@bristol.ac.uk>

ARG DEBIAN_FRONTEND=noninteractive

USER root
RUN apt-get update \
  && apt-get install -y software-properties-common \
  && apt-add-repository ppa:swi-prolog/stable \
  && apt-get update \
  && apt-get upgrade -y

RUN apt-get install -y \
  git \
  graphviz \
  imagemagick \
  nodejs-legacy \
  npm \
  swi-prolog
RUN npm install -g bower
# For development only:
RUN npm install -g clean-css-cli requirejs jsdoc

# Set environment variables
ENV SHELL /bin/bash
ENV SWISH_DIR /opt/swish
ENV SWISH_USER swisher
ENV SWISH_UID 1000
ENV HOME /home/$SWISH_USER
ENV SWISH_DATA $HOME/data
# To avoid `Warning: /usr/lib/swi-prolog/library/clp/clpfd.pl:...: Illegal multibyte Sequence`
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

# Create swisher user with UID=1000 and in the 'users' group \ -N -u $SWISH_UID
RUN useradd -m -s $SHELL $SWISH_USER \
  && mkdir -p $SWISH_DIR \
  && chown $SWISH_USER $SWISH_DIR \
  && mkdir -p $SWISH_DATA \
  && chown $SWISH_USER $SWISH_DATA
USER $SWISH_USER

RUN git clone --depth 1 https://github.com/SWI-Prolog/swish.git $SWISH_DIR

WORKDIR $SWISH_DIR
RUN git submodule update --init
RUN make packs

RUN bower install
RUN make src

# Configure container startup
WORKDIR $HOME
EXPOSE 3050
ENTRYPOINT swipl $SWISH_DIR/daemon.pl --port=3050 --interactive
