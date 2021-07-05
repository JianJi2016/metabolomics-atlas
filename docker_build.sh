#!/bin/bash

IMAGE_NAME="metabolomics-atlas"
IMAGE_REGISTRY="eros.fiehnlab.ucdavis.edu/$IMAGE_NAME"

# Build docker image
docker build -t $IMAGE_NAME --rm=true . || exit 1

# Tag the docker container
ID=$(docker images --format="{{.Repository}} {{.ID}}" | grep $IMAGE_NAME | cut -d' ' -f2 | head -n 1)
docker tag $ID $IMAGE_NAME:latest
docker tag $ID $IMAGE_REGISTRY:latest

echo "Tagged $ID as $IMAGE_NAME:latest"
echo "Tagged $ID as $IMAGE_REGISTRY:latest"

# Push if requested
if [ "$1" == "push" ]; then
	echo "pushing $IMAGE to server"
	docker push $IMAGE_REGISTRY:latest
else
	echo "pushing disabled - use 'push' as argument to push to the docker registry"
fi