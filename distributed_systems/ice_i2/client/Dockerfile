FROM ubuntu:22.04 AS build

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates wget gnupg \
    && wget -O /etc/apt/trusted.gpg.d/zeroc.asc \
        https://download.zeroc.com/GPG-KEY-zeroc-release-B6391CB2CFBA643D \
    && echo "deb https://download.zeroc.com/ice/3.7/ubuntu22.04 stable main" \
        > /etc/apt/sources.list.d/zeroc-ice.list \
    && apt-get update && apt-get install -y --no-install-recommends \
        zeroc-ice-all-dev \
        zeroc-ice-compilers \
        cmake \
        g++ \
        make \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src

COPY slice/ /src/slice/
RUN mkdir -p /src/generated/cpp \
    && slice2cpp --output-dir /src/generated/cpp -I/src/slice /src/slice/Counter.ice

COPY client/ /src/client/
RUN cmake -S /src/client -B /src/build \
    && cmake --build /src/build -j

FROM build AS runtime

WORKDIR /app
RUN cp /src/build/client /app/client

ENV ICE_HOST=server

ENTRYPOINT ["/app/client"]
