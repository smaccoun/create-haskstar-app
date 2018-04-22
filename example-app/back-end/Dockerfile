FROM  fpco/stack-build:lts-9.4

# Copy everything to docker ecosystem
WORKDIR /opt/
RUN cabal update
COPY ./ ./

RUN stack setup
RUN stack build -j1

CMD stack exec api-exe

EXPOSE 8080
