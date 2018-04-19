FROM  haskell:8.0.2

# Copy everything to docker ecosystem
WORKDIR /app/
RUN cabal update
COPY ["./*.cabal","./stack.yaml", "./"]
COPY ./app  ./app
COPY ./src/ ./src

RUN stack setup
RUN stack build
RUN stack install
