FROM pustic/budget-base:1

RUN mkdir /app1 

WORKDIR /app1

COPY . /app1

