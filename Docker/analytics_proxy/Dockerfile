FROM node:13.8.0-slim

WORKDIR /app

EXPOSE 80

COPY package.json \
     package-lock.json \
     ./

RUN npm install

COPY src ./src/

COPY LICENSE \
     config.js \
     ./

ENV APP__ENV_NAME "production"

CMD ["npm","run","start"]