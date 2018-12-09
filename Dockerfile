FROM node:10

WORKDIR /app

COPY ./package.json ./package-lock.json ./
RUN npm install

COPY ./elm.json ./webpack.config.js ./
COPY ./src ./src

ENTRYPOINT ["npm", "run", "build"]
