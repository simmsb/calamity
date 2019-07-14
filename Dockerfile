FROM fpco/stack-build:lts-13.27

ARG SRC_DIR=/build
WORKDIR ${SRC_DIR}

COPY stack.yaml ${SRC_DIR}
COPY calamity.cabal ${SRC_DIR}

RUN stack setup && stack exec -- ghc --version
RUN stack build --only-dependencies

COPY . ${SRC_DIR}
RUN stack build
