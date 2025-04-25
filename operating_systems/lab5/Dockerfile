FROM ubuntu:22.04

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt install tzdata -y && apt install net-tools vim man file -y
RUN apt-get install -y build-essential
RUN yes| unminimize

WORKDIR /home/ubuntu

RUN mkdir -p src bin

COPY 2/catcher.c 2/sender.c ./src/

RUN gcc src/catcher.c -o bin/catcher
RUN gcc src/sender.c -o bin/sender

CMD ["/bin/bash"]
