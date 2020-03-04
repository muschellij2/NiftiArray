FROM pennsive/r-env:base

# install anything not already in pennsive/r-env:base
RUN r -e "install.packages('BiocManager')"
RUN r -e "BiocManager::install(c('DelayedArray', 'HDF5Array', 'rhdf5', 'S4Vectors'))"
RUN r -e "devtools::install_github('muschellij2/NiftiArray', dependencies = FALSE)"

WORKDIR /src
COPY . .
ENTRYPOINT []
CMD bash
