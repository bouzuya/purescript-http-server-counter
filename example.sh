#!/bin/bash -x

# create
id=$(curl \
  --data '{"name":"foo"}' \
  --request POST \
  --silent \
  'http://localhost:8080/counters' | jq -r '.id')

curl \
  --request GET \
  --silent \
  "http://localhost:8080/counters/${id}" | jq -r -S '.'

# count +1
curl \
  --request PATCH \
  --silent \
  "http://localhost:8080/counters/${id}" | jq -r -S '.'

# count +1
curl \
  --request PATCH \
  --silent \
  "http://localhost:8080/counters/${id}" | jq -r -S '.'

# count +1
curl \
  --request PATCH \
  --silent \
  "http://localhost:8080/counters/${id}" | jq -r -S '.'

# delete
curl \
  --request DELETE \
  --silent \
  "http://localhost:8080/counters/${id}"

curl \
  --request GET \
  --silent \
  "http://localhost:8080/counters/${id}" | jq -S '.'
