ARG BASE_CONTAINER=quay.io/jupyter/minimal-notebook:ubuntu-24.04

FROM $BASE_CONTAINER

# https://hub.docker.com/r/jupyter/base-notebook/tags

LABEL maintainer="Michael Chavinda <mschavinda@gmail.com>"

# Extra arguments to `cabal build`. Used to build with optimization flags.
ARG CABAL_ARGS=

USER root

# The global cabal configuration and package database will be here
ENV CABAL_DIR=/opt/cabal
ENV CABAL_CONFIG=${CABAL_DIR}/config
ENV LIBTORCH_HOME=/opt/libtorch
ENV LIBTORCH_CUDA_VERSION=cpu
RUN mkdir -p $CABAL_DIR
RUN touch $CABAL_CONFIG
RUN mkdir -p $LIBTORCH_HOME
RUN fix-permissions $CABAL_DIR
RUN fix-permissions $CABAL_CONFIG
RUN fix-permissions $LIBTORCH_HOME

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
        libsnappy-dev \
        libffi-dev \
        libgmp-dev \
        gnupg \
        netbase \
# for ihaskell-graphviz
        graphviz \
# for ihaskell-gnuplot
        gnuplot-nox \
# for ghcup and cabal
        curl \
        build-essential \
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
        zstd \
        libzstd-dev \
        libnuma-dev \
        libncurses-dev \
        zlib1g-dev liblapack-dev libblas-dev devscripts debhelper python3-pip cmake curl wget unzip git libtinfo6 libncurses-dev python3 python3-yaml \
# Need less for general maintenance
        less && \
# Clean up apt
    rm -rf /var/lib/apt/lists/*

# Install ghcup
# https://www.haskell.org/ghcup/install/
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.6.7
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=recommended
ENV GHCUP_INSTALL_BASE_PREFIX=/opt
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Add ghcup binaries to PATH
ENV PATH=/opt/.ghcup/bin:${PATH}

# Verify installations
RUN ghc --version && cabal --version

# Update cabal package list
RUN cabal update

# Create cabal config directory
RUN mkdir -p ${CABAL_DIR}

# Create global cabal.project
RUN mkdir -p /opt/cabal-project
COPY cabal.project /opt/cabal-project/cabal.project
COPY setup_torch.sh /opt/cabal-project/setup_torch.sh
RUN chmod +x /opt/cabal-project/setup_torch.sh
RUN cd /opt/cabal-project && /opt/cabal-project/setup_torch.sh
RUN chown --recursive $NB_UID:users /opt/cabal-project \
    && fix-permissions /opt/cabal-project

# fix-permissions for /usr/local/share/jupyter so that we can install
# the IHaskell kernel there. Seems like the best place to install it, see
#      jupyter --paths
#      jupyter kernelspec list
RUN mkdir -p /usr/local/share/jupyter \
    && fix-permissions /usr/local/share/jupyter \
    && mkdir -p /usr/local/share/jupyter/kernels \
    && fix-permissions /usr/local/share/jupyter/kernels

# Now make a bin directory for installing the ihaskell executable on
# the PATH.
RUN mkdir -p /opt/bin \
    && fix-permissions /opt/bin
ENV PATH=${PATH}:/opt/bin

ARG IHASKELL_COMMIT=dd67c78863586f33952bfe436c5f2b7e499dde8c

# Clone IHaskell and install ghc from the IHaskell resolver
RUN cd /opt && curl -L "https://github.com/gibiansky/IHaskell/tarball/$IHASKELL_COMMIT" | tar xzf -
RUN cd /opt && mv *IHaskell* IHaskell
RUN fix-permissions /opt/IHaskell

ARG IHASKELL_DISPLAY_COMMIT=e306d5ba296d09badad82358e1098d367daef0a1
RUN cd /opt && curl -L "https://github.com/mchav/ihaskell-dataframe/tarball/$IHASKELL_DISPLAY_COMMIT" | tar xzf - 
RUN cd /opt && mv *ihaskell-dataframe* ihaskell-dataframe 
RUN fix-permissions /opt/ihaskell-dataframe

ARG DATAFRAME_COMMIT=f12bd45ea57d8d79c7c4202fcacd0f9971ee9276
RUN cd /opt && curl  -L "https://github.com/mchav/dataframe/tarball/$DATAFRAME_COMMIT" | tar xzf - 
RUN cd /opt && mv *mchav-dataframe* dataframe
RUN cd /opt/dataframe && mv *dataframe-hasktorch* /opt/dataframe-hasktorch
RUN fix-permissions /opt/dataframe
RUN fix-permissions /opt/dataframe-hasktorch

ARG HVEGA_COMMIT=5e18d53b7748dc5e23c6cd6c38dc722f01e2dde6

RUN cd /opt/ \
    && curl -L "https://github.com/DougBurke/hvega/tarball/$HVEGA_COMMIT" | tar xzf - \
    && mv *hvega* hvega \
    && cd /opt/hvega && mv *ihaskell-hvega* /opt/ihaskell-hvega \
    && fix-permissions /opt/hvega \
    && fix-permissions /opt/ihaskell-hvega

RUN fix-permissions $CABAL_DIR

COPY config $CABAL_CONFIG

RUN cabal update

# Build IHaskell
# Note that we are using the cabal.project in /opt/cabal-project
RUN cd /opt/cabal-project \
    && fix-permissions /opt/IHaskell \
    && fix-permissions $LIBTORCH_HOME \
    && fix-permissions $CABAL_DIR

RUN cd /opt/cabal-project && \
    cabal install $CABAL_ARGS --lib \
    dataframe ihaskell-dataframe ihaskell \
    dataframe-hasktorch ihaskell-dataframe \
    regex-tdfa containers cassava statistics \
    monad-bayes time aeson hvega ihaskell-hvega

# Install IHaskell.Display libraries
# https://github.com/gibiansky/IHaskell/tree/master/ihaskell-display
# RUN cd /opt/cabal-project && cabal build $CABAL_ARGS ihaskell-aeson && cabal install --lib ihaskell-aeson
# RUN cd /opt/cabal-project && cabal build $CABAL_ARGS ihaskell-blaze && cabal install --lib ihaskell-blaze
# RUN cd /opt/cabal-project && cabal build $CABAL_ARGS ihaskell-gnuplot && cabal install --lib ihaskell-gnuplot
# RUN cd /opt/cabal-project && cabal build $CABAL_ARGS ihaskell-graphviz && cabal install --lib ihaskell-graphviz
# RUN cd /opt/cabal-project && cabal build $CABAL_ARGS ihaskell-hatex && cabal install --lib ihaskell-hatex
# RUN cd /opt/cabal-project && cabal build $CABAL_ARGS ihaskell-juicypixels && cabal install --lib ihaskell-juicypixels
# RUN cd /opt/cabal-project && cabal build $CABAL_ARGS ihaskell-widgets && cabal install --lib ihaskell-widgets

RUN fix-permissions $CABAL_DIR \
    && fix-permissions $LIBTORCH_HOME \
    && fix-permissions /opt/IHaskell \
    && fix-permissions /opt/ihaskell-dataframe \
    && fix-permissions /opt/dataframe-hasktorch \
    && fix-permissions /opt/dataframe

RUN cd /opt/cabal-project && cabal build $CABAL_ARGS ihaskell --force-reinstalls \
    && cabal install $CABAL_ARGS --lib dataframe ihaskell-dataframe hasktorch \
    ihaskell dataframe-hasktorch ihaskell-dataframe time ihaskell template-haskell \
    vector text containers array random unix directory regex-tdfa containers \
    cassava statistics monad-bayes aeson bytestring ghc-events hvega ihaskell-hvega \
    --force-reinstalls --install-method=copy --installdir=/opt/bin \
    && fix-permissions /opt/bin

# Install ihaskell binary to /opt/bin
RUN cd /opt/cabal-project \
    && cabal v2-install $CABAL_ARGS ihaskell --install-method=copy --installdir=/opt/bin --package-env=./opt/cabal/store/ghc-9.6.7 \
    && fix-permissions /opt/bin

# Bug workaround for https://github.com/IHaskell/ihaskell-notebook/issues/9
RUN mkdir -p /home/jovyan/.local/share/jupyter/runtime \
    && fix-permissions /home/jovyan/.local \
    && fix-permissions /home/jovyan/.local/share \
    && fix-permissions /home/jovyan/.local/share/jupyter \
    && fix-permissions /home/jovyan/.local/share/jupyter/runtime

# Add GHC to PATH
ENV PATH=${PATH}:/opt/.ghcup/ghc/9.6.7/bin

RUN rm -Rf /home/jovyan/.stack
RUN fix-permissions /opt/cabal-project
RUN fix-permissions "/home/${NB_USER}"

# (Re)install the IHaskell kernel spec after the env is in place
RUN ihaskell install --prefix=/usr/local

RUN fix-permissions "/home/${NB_USER}"
RUN fix-permissions "/usr/local/share/jupyter/kernels/haskell"

RUN mkdir -p /home/$NB_USER/examples
COPY ./app/Iris.ipynb /home/$NB_USER/examples
COPY ./app/California_Housing.ipynb /home/$NB_USER/examples
COPY ./app/performance_exploration.ipynb /home/$NB_USER/examples
COPY ./app/getting_started.ipynb /home/$NB_USER/
RUN mkdir -p /home/$NB_USER/examples/data
COPY ./data/fast.eventlog /home/$NB_USER/examples/data
COPY ./data/leaky.eventlog /home/$NB_USER/examples/data
RUN jupyter trust /home/$NB_USER/examples/California_Housing.ipynb
RUN jupyter trust /home/$NB_USER/examples/Iris.ipynb
RUN jupyter trust /home/$NB_USER/examples/performance_exploration.ipynb
RUN jupyter trust /home/$NB_USER/getting_started.ipynb
RUN fix-permissions "/home/${NB_USER}"

# Switch back to jovyan user
USER $NB_UID

RUN fix-permissions "/home/${NB_USER}"
RUN fix-permissions "/usr/local/share/jupyter/kernels/haskell"

RUN conda install --quiet --yes \
# ihaskell-widgets needs ipywidgets
# https://github.com/IHaskell/IHaskell/issues/1380
    'ipywidgets=8.1.8' && \
    conda clean --all -f -y && \
    fix-permissions "/home/${NB_USER}"

RUN cp /opt/dataframe/data/housing.csv /home/$NB_USER/examples/data
RUN cp /opt/dataframe/data/iris.parquet /home/$NB_USER/examples/data
RUN mkdir -p /home/$NB_USER/learning && \
    cd /home/$NB_USER/learning && \
    git clone https://github.com/mchav/applied-data-science-with-haskell

# Enable this for debugging the kernel messages
RUN conda install --quiet --yes \
    'jupyterlab-kernelspy' && \
    conda clean --all -f -y && \
    fix-permissions "/home/${NB_USER}"

RUN conda install -c conda-forge ipychart

USER root
COPY rc.hs /home/${NB_USER}/.ihaskell/rc.hs
RUN fix-permissions "/home/${NB_USER}/.ihaskell"
USER ${NB_USER}
