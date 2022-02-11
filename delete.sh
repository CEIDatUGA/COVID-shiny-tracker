#!/bin/bash
cd ./data 
find . -type f -mtime +1 \
    -name '*[0-9][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]*' \
    -delete