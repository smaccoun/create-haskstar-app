FROM  fpco/stack-build:lts-9.4

# Copy everything to docker ecosystem
WORKDIR /app/
RUN cabal update
COPY ["./*.cabal","./stack.yaml", "./"]
COPY ./app  ./app
COPY ./src/ ./src

RUN stack setup
RUN stack build -j1

CMD stack exec api-exe

EXPOSE 8080
