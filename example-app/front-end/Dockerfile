FROM node:9


WORKDIR /app/
COPY ./ ./
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev libnss3 netbase libnss-lwres libnss-mdns  
RUN ["yarn", "add", "global", "elm"]
RUN ["yarn", "build"]
RUN npm install -g serve

CMD serve -s ./build -p 3000

EXPOSE 3000
