#!/usr/bin/env bash

mkdir -p db
mongod --dbpath db --nojournal --noprealloc --smallfiles
