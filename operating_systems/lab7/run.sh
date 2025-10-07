#!/bin/zsh

set -eo pipefail

IMAGE_TAG=bigJack213/operating-systems-lab7

docker build . -t $IMAGE_TAG

docker run -it -v $(pwd)/src/:/home/ubuntu/src $IMAGE_TAG  /bin/bash
