#!/bin/zsh

set -eo pipefail

IMAGE_TAG=bigJack213/operating-systems-lab8

docker build . -t $IMAGE_TAG

docker run -it -v $(pwd)/src/:/root/src $IMAGE_TAG  /bin/bash
