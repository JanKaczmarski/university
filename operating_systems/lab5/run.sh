#!/bin/zsh

set -eo pipefail

IMAGE_TAG=bigJack213/ubuntu-with-c

docker build . -t $IMAGE_TAG

docker run -it -v $(pwd)/2/:/home/ubuntu/src $IMAGE_TAG  /bin/bash
