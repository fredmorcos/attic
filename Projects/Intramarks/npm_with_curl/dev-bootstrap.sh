#!/bin/sh

function log {
    echo "DEV_BOOTSTRAP: $1" > /dev/stderr
}

function sep {
    log "----------"
}

function check_bin {
    res=$(which $1)
    if [[ $? -eq 0 ]]; then
	log "$1 found -> $res"
    else
	log "ERROR: $1 not found."
	exit 1
    fi
}

check_bin "d8"
check_bin "node"
check_bin "ruby"
check_bin "npm"
check_bin "mongod"
check_bin "mongo"
check_bin "curl"
check_bin "sha512sum"

sep

curl_cmd="curl -#"

if [[ -n $HTTP_PROXY ]]; then
    log "HTTP proxy is set: $HTTP_PROXY"
    curl_cmd="$curl_cmd --proxy $HTTP_PROXY"
elif [[ -n $http_proxy ]]; then
    log "HTTP proxy is set: $http_proxy"
    curl_cmd="$curl_cmd --proxy $http_proxy"
elif [[ -n $HTTPS_PROXY ]]; then
    log "HTTPS proxy is set: $HTTPS_PROXY"
    curl_cmd="$curl_cmd --proxy $HTTPS_PROXY"
elif [[ -n $https_proxy ]]; then
    log "HTTPS proxy is set: $https_proxy"
    curl_cmd="$curl_cmd --proxy $https_proxy"
else
    log "No proxy settings."
fi

mkdir -p _tmp
cd _tmp

sane=0

if [[ -r "all" ]]; then
    if [[ -r "all-cs" ]]; then
        sha512sum all > all-cs-tmp
	diff all-cs all-cs-tmp &>/dev/null
	if [[ $? -eq 0 ]]; then
	    log "Existing NPM package database is sane."
	    sane=1
	else
	    log "Existing NPM package database is NOT sane."
	    sane=0
	fi
    else
	log "Existing NPM package database checksum NOT found."
	sane=0
    fi
else
    log "Existing NPM package database NOT found."
    sane=0
fi

if [[ $sane -eq 0 ]]; then
    log "Downloading new NPM package database..."
    $curl_cmd -O "http://registry.npmjs.org/-/all"

    if [[ $? -eq 0 ]]; then
	log "Done."
    else
	log "Error occurred."
	exit 1
    fi

    log "Generating NPM package database checksum..."
    sha512sum all > all-cs
    if [[ $? -eq 0 ]]; then
	log "Done."
    else
	log "Error occurred."
	exit 1
    fi
else
    log "Skipping NPM package database download."
fi

sep

