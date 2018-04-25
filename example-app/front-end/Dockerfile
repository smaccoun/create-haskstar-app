FROM node:8

WORKDIR /app/
COPY ./build  ./

RUN npm install -g serve
CMD serve -s . -p 3000

EXPOSE 3000 3000
