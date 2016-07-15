#!/bin/bash
#
# Ask information from a Pengines/SWISH server from the shell.
#
# This program allows you to download query  results from a SWISH server
# as CSV data.

server=${SWISH_SERVER-http://localhost:3050}
srctext=
curlarg=
format=${SWISH_FORMAT-prolog}
program=$(basename $0)
output=csv

usage()
{
cat << _EOM_
Usage: $program "[--server=URL] [--format=rdf|prolog]" [--json-s|--json-html] file.pl ... projection query

Where

  - server is by default "$server".  Environment: SWISH_SERVER
  - format is by default "$format".  Environment: SWISH_FORMAT
  - output is by default csv.  Use --json-s or --json-html for JSON output, see 
  http://www.swi-prolog.org/pldoc/doc_for?object=pengines%3Aevent_to_json/4
  - file.pl ... are files saved in SWISH or URLs.  Zero or more files are allowed
  - projection is a comma-separated list of Prolog variables that define
    the CSV columns.
  - query is a Prolog goal using the variables from projection.

For example

  $program X 'X is 1<<100'
  X
  1267650600228229401496703205376

  $program factbook.pl Code,Country 'country(Country,Code)'
  Code,Country
  af,http://www4.wiwiss.fu-berlin.de/factbook/resource/Afghanistan
  ax,http://www4.wiwiss.fu-berlin.de/factbook/resource/Akrotiri
  ...
_EOM_
}

done=false
while [ $done = false ]; do
    case "$1" in
        --json-s)
	    output=json-s
	    shift
	    ;;
        --json-html)
	    output=json-html
	    shift
	    ;;
        --server=*)
            server=$(echo $1 | sed 's/.*=//')
            shift
            ;;
	--format=*)
	    format=$(echo $1 | sed 's/.*=//')
	    case "$format" in
	        rdf|prolog)
		    ;;
		*)
		    usage
		    exit 1
		    ;;
	    esac
            shift
            ;;
	https://*.pl|http://*.pl)
	    curlarg+=" -d src_url=$1"
	    shift
	    ;;
	*.pl)
            script=$(echo $1 | sed 's/.*=//')
	    srctext+=":- include('$script'). "
	    shift
	    ;;
	*)
	    done=true
	    ;;
    esac
done

vars="$1"
query="$2"

if [ -z "$vars" -o -z "$query" ]; then
  usage
  exit 1
fi

curl -s \
     -d ask="$query" \
     -d template="$format($vars)" \
     -d application="swish" \
     -d src_text="$srctext" \
     -d format="$output" \
     -d chunk=10 \
     -d solutions=all \
     $curlarg \
     $server/pengine/create
