FROM ubuntu:22.04 AS runtime

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates wget gnupg \
    && wget -O /etc/apt/trusted.gpg.d/zeroc.asc \
        https://download.zeroc.com/GPG-KEY-zeroc-release-B6391CB2CFBA643D \
    && echo "deb https://download.zeroc.com/ice/3.7/ubuntu22.04 stable main" \
        > /etc/apt/sources.list.d/zeroc-ice.list \
    && apt-get update && apt-get install -y --no-install-recommends \
        zeroc-ice-slice \
        zeroc-ice-compilers \
        zeroc-ice-utils \
        python3-zeroc-ice \
        python3 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY slice/ /app/slice/
RUN mkdir -p /app/generated/python \
    && slice2py --output-dir /app/generated/python -I/app/slice /app/slice/Counter.ice

COPY server/ /app/server/

ENV PYTHONPATH=/app/generated/python:/app/server
ENV PYTHONUNBUFFERED=1

EXPOSE 10000
CMD ["python3", "-u", "/app/server/server.py", "--Ice.Config=/app/server/config.server"]
