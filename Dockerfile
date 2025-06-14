ARG BASE_CONTAINER=jupyter/base-notebook:lab-3.2.4@sha256:438e87954dedd8b3c0088c89923ed29c9e14f139428c98d79c9ebabe55adc01d

# JupyterLab versions > 3.2.4 have this bug:
# https://github.com/jupyterlab/jupyterlab/issues/13383

FROM $BASE_CONTAINER
# https://hub.docker.com/r/jupyter/base-notebook/tags

LABEL maintainer="James Brock <jamesbrock@gmail.com>"

# Extra arguments to `stack build`. Used to build --fast, see Makefile.
ARG STACK_ARGS=

USER root

# The global snapshot package database will be here in the STACK_ROOT.
ENV STACK_ROOT=/opt/stack
RUN mkdir -p $STACK_ROOT
RUN fix-permissions $STACK_ROOT

# Install system dependencies
RUN apt-get update && apt-get install -yq --no-install-recommends \
        python3-pip \
        git \
        libtinfo-dev \
        libzmq3-dev \
        libcairo2-dev \
        libpango1.0-dev \
        libmagic-dev \
        libblas-dev \
        liblapack-dev \
        libffi-dev \
        libgmp-dev \
        gnupg \
        netbase \
# for ihaskell-graphviz
        graphviz \
# for ihaskell-gnuplot
        gnuplot-nox \
# for Stack download
        curl \
# Stack Debian/Ubuntu manual install dependencies
# https://docs.haskellstack.org/en/stable/install_and_upgrade/#linux-generic
        g++ \
        gcc \
        libc6-dev \
        libffi-dev \
        libgmp-dev \
        make \
        xz-utils \
        zlib1g-dev \
        git \
        gnupg \
        netbase \
# Need less for general maintenance
        less && \
# Clean up apt
    rm -rf /var/lib/apt/lists/*

# Stack Linux (generic) Manual download
# https://docs.haskellstack.org/en/stable/install_and_upgrade/#linux-generic
#
# So that we can control Stack version, we do manual install instead of
# automatic install:
#
#    curl -sSL https://get.haskellstack.org/ | sh
#
ARG STACK_VERSION="2.11.1"
ARG STACK_BINDIST="stack-${STACK_VERSION}-linux-x86_64"
RUN    cd /tmp \
    && curl -sSL --output ${STACK_BINDIST}.tar.gz https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/${STACK_BINDIST}.tar.gz \
    && tar zxf ${STACK_BINDIST}.tar.gz \
    && cp ${STACK_BINDIST}/stack /usr/bin/stack \
    && rm -rf ${STACK_BINDIST}.tar.gz ${STACK_BINDIST} \
    && stack --version

# Stack global non-project-specific config stack.config.yaml
# https://docs.haskellstack.org/en/stable/yaml_configuration/#non-project-specific-config
RUN mkdir -p /etc/stack
COPY stack.config.yaml /etc/stack/config.yaml
RUN fix-permissions /etc/stack

# Stack global project stack.yaml
# https://docs.haskellstack.org/en/stable/yaml_configuration/#yaml-configuration
RUN mkdir -p $STACK_ROOT/global-project
COPY global-project.stack.yaml $STACK_ROOT/global-project/stack.yaml
RUN    chown --recursive $NB_UID:users $STACK_ROOT/global-project \
    && fix-permissions $STACK_ROOT/global-project

# fix-permissions for /usr/local/share/jupyter so that we can install
# the IHaskell kernel there. Seems like the best place to install it, see
#      jupyter --paths
#      jupyter kernelspec list
RUN    mkdir -p /usr/local/share/jupyter \
    && fix-permissions /usr/local/share/jupyter \
    && mkdir -p /usr/local/share/jupyter/kernels \
    && fix-permissions /usr/local/share/jupyter/kernels

# Now make a bin directory for installing the ihaskell executable on
# the PATH. This /opt/bin is referenced by the stack non-project-specific
# config.
RUN    mkdir -p /opt/bin \
    && fix-permissions /opt/bin
ENV PATH ${PATH}:/opt/bin

# Specify a git branch for IHaskell (can be branch or tag).
# The resolver for all stack builds will be chosen from
# the IHaskell/stack.yaml in this commit.
# https://github.com/gibiansky/IHaskell/commits/master
# IHaskell 2022-12-19
ARG IHASKELL_COMMIT=7a8d728ad1e1c4a869657ab6899a8f58f6fb456c

# Specify a git branch for hvega
# https://github.com/DougBurke/hvega/commits/main
# hvega 2022-06-16
# hvega-0.12.0.3
# ihaskell-hvega-0.5.0.3
ARG HVEGA_COMMIT=2b453c230294b889564339853de02b0c1829a081

ARG IHASKELL_DISPLAY_COMMIT=c6741bb487cf7fcd1d7d2e01c8e1dccca8c7a8fa

ARG DATAFRAME_COMMIT=1264fd3a393615633754633ee87d292f83264e49

# Clone IHaskell and install ghc from the IHaskell resolver
RUN    cd /opt \
    && curl -L "https://github.com/gibiansky/IHaskell/tarball/$IHASKELL_COMMIT" | tar xzf - \
    && mv *IHaskell* IHaskell \
    && curl -L "https://github.com/DougBurke/hvega/tarball/$HVEGA_COMMIT" | tar xzf - \
    && mv *hvega* hvega \
    && curl -L "https://github.com/mchav/ihaskell-dataframe/tarball/$IHASKELL_DISPLAY_COMMIT" | tar xzf - \
    && mv *ihaskell-dataframe* ihaskell-dataframe \
    && curl -L "https://github.com/mchav/dataframe/tarball/$DATAFRAME_COMMIT" | tar xzf - \
    && mv *mchav-dataframe* dataframe \
    && fix-permissions /opt/IHaskell \
    && fix-permissions $STACK_ROOT \
    && fix-permissions /opt/hvega \
    && fix-permissions /opt/ihaskell-dataframe \
    && fix-permissions /opt/dataframe \
    && stack setup \
    && fix-permissions $STACK_ROOT \
# Clean 176MB
    && rm /opt/stack/programs/x86_64-linux/ghc*.tar.xz

# Build IHaskell
#
# Note that we are NOT in the /opt/IHaskell directory here, we are
# installing ihaskell via the paths given in /opt/stack/global-project/stack.yaml
RUN    stack build $STACK_ARGS ihaskell \
    && fix-permissions /opt/IHaskell \
    && fix-permissions $STACK_ROOT

# Install IHaskell.Display libraries
# https://github.com/gibiansky/IHaskell/tree/master/ihaskell-display
RUN    stack build $STACK_ARGS ihaskell-aeson \
    && stack build $STACK_ARGS ihaskell-blaze \
    && stack build $STACK_ARGS ihaskell-charts \
    && stack build $STACK_ARGS ihaskell-diagrams \
    && stack build $STACK_ARGS ihaskell-gnuplot \
    && stack build $STACK_ARGS ihaskell-graphviz \
    && stack build $STACK_ARGS ihaskell-hatex \
    && stack build $STACK_ARGS ihaskell-juicypixels \
#   && stack build $STACK_ARGS ihaskell-magic \
    && stack build $STACK_ARGS ihaskell-plot \
#   && stack build $STACK_ARGS ihaskell-rlangqq \
#   && stack build $STACK_ARGS ihaskell-static-canvas \
    && stack build $STACK_ARGS ihaskell-widgets \
    && stack build $STACK_ARGS hvega \
    && stack build $STACK_ARGS dataframe \
    && stack build $STACK_ARGS ihaskell-dataframe \
    && stack build $STACK_ARGS ihaskell-hvega \
    && fix-permissions $STACK_ROOT \
# Fix for https://github.com/IHaskell/ihaskell-notebook/issues/14#issuecomment-636334824
    && fix-permissions /opt/IHaskell \
    && fix-permissions /opt/hvega \
    && fix-permissions /opt/ihaskell-dataframe \
    && fix-permissions /opt/dataframe

# Cleanup
# Don't clean IHaskell/.stack-work, 7GB, this causes issue #5
#   && rm -rf $(find /opt/IHaskell -type d -name .stack-work) \
# Don't clean /opt/hvega
# We can't actually figure out anything to cleanup.

# Bug workaround for https://github.com/IHaskell/ihaskell-notebook/issues/9
RUN mkdir -p /home/jovyan/.local/share/jupyter/runtime \
    && fix-permissions /home/jovyan/.local \
    && fix-permissions /home/jovyan/.local/share \
    && fix-permissions /home/jovyan/.local/share/jupyter \
    && fix-permissions /home/jovyan/.local/share/jupyter/runtime

# Install system-level ghc using the ghc which was installed by stack
# using the IHaskell resolver.
RUN mkdir -p /opt/ghc && ln -s `stack path --compiler-bin` /opt/ghc/bin \
    && fix-permissions /opt/ghc
ENV PATH ${PATH}:/opt/ghc/bin

# Switch back to jovyan user
USER $NB_UID

RUN \
# Install the IHaskell kernel at /usr/local/share/jupyter/kernels, which is
# in `jupyter --paths` data:
       stack exec ihaskell -- install --stack --prefix=/usr/local

# # We don't need to install the ihaskell_labextension for JupyterLab syntax highlighting
# # https://github.com/IHaskell/IHaskell/issues/1238#issuecomment-907658217
#     && npm install -g typescript \
#     && cd /opt/IHaskell/jupyterlab-ihaskell \
#     && npm install \
#     && npm run build \
#     && jupyter labextension install . \
# # Cleanup
#     && npm cache clean --force \
#     && rm -rf /home/$NB_USER/.cache/yarn \
# # Clean jupyterlab-ihaskell/node_nodemodules, 86MB
#     && rm -rf /opt/IHaskell/jupyterlab-ihaskell/node_modules

RUN conda install --quiet --yes \
# ihaskell-widgets needs ipywidgets
# https://github.com/IHaskell/IHaskell/issues/1380
    'ipywidgets=7.7.1' && \
# ihaskell-hvega doesn't need an extension. https://github.com/jupyterlab/jupyter-renderers
#    'jupyterlab-vega3' && \
    conda clean --all -f -y && \
    fix-permissions "${CONDA_DIR}" && \
    fix-permissions "/home/${NB_USER}"

# Example IHaskell notebooks will be collected in this directory.
ARG EXAMPLES_PATH=/home/$NB_USER/ihaskell_examples

# Collect all the IHaskell example notebooks in EXAMPLES_PATH.
RUN    mkdir -p $EXAMPLES_PATH \
    && cd $EXAMPLES_PATH \
    && mkdir -p ihaskell \
    && cp --recursive /opt/IHaskell/notebooks/* ihaskell/ \
    && mkdir -p ihaskell-juicypixels \
    && cp /opt/IHaskell/ihaskell-display/ihaskell-juicypixels/*.ipynb ihaskell-juicypixels/ \
    && mkdir -p ihaskell-charts \
    && cp /opt/IHaskell/ihaskell-display/ihaskell-charts/*.ipynb ihaskell-charts/ \
    && mkdir -p ihaskell-diagrams \
    && cp /opt/IHaskell/ihaskell-display/ihaskell-diagrams/*.ipynb ihaskell-diagrams/ \
    && mkdir -p ihaskell-gnuplot \
    && cp /opt/IHaskell/ihaskell-display/ihaskell-gnuplot/*.ipynb ihaskell-gnuplot/ \
    && mkdir -p ihaskell-widgets \
    && cp --recursive /opt/IHaskell/ihaskell-display/ihaskell-widgets/Examples/* ihaskell-widgets/ \
    && mkdir -p ihaskell-hvega \
    && cp /opt/hvega/notebooks/*.ipynb ihaskell-hvega/ \
    && cp /opt/hvega/notebooks/*.tsv ihaskell-hvega/ \
    && mkdir -p ihaskell-plot \
    && cp /opt/IHaskell/ihaskell-display/ihaskell-plot/PlotExample.ipynb ihaskell-plot/ \
    && fix-permissions $EXAMPLES_PATH

# Enable this for debugging the kernel messages
# RUN conda install --quiet --yes \
#     'jupyterlab-kernelspy' && \
#     conda clean --all -f -y && \
#     fix-permissions "${CONDA_DIR}" && \
#     fix-permissions "/home/${NB_USER}"

