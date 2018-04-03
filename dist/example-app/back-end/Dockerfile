FROM  smaccoun/haskstar-haskell:0.1

# Copy everything to docker ecosystem
WORKDIR /app/
COPY ["./*.cabal","./stack.yaml", "./"]
COPY ./app  ./app
COPY ./src/ ./src

RUN stack install --install-ghc -j1
RUN stack setup
RUN stack build -j1

CMD stack exec api-exe

EXPOSE 8080

