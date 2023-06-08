rm -f RESPONSE
mkfifo RESPONSE

declare -g REQUEST_PATH=""
declare -gA REQUEST_HEADERS
declare -g REQUEST_BODY
function parseHttpRequest() {
  # Request method and path
  read line
  trline=`echo $line | tr -d '[\r\n]'`
  # Trim off the HTTP/1.1 protocol
  REQUEST_PATH=$(echo $trline | sed -E "s/([A-Z]+ .*) HTTP\/1.1/\1/")
 
  # Headers
  declare -gA REQUEST_HEADERS
  while read line; do
    trline=`echo $line | tr -d '[\r\n]'`
    if [[ -z "$trline" ]]; then
      break
    fi
    local header=$(echo $trline | sed -E "s/^([a-zA-Z\-]+): .*\$/\1/")
    local value=$(echo $trline | sed -E "s/^[a-zA-Z\-]+: (.*)\$/\1/")
    REQUEST_HEADERS+=( [$header]=$value )
  done

  # Request body
  if [[ -n ${REQUEST_HEADERS["Content-Length"]} ]]; then
    echo "READ"
    read -n ${REQUEST_HEADERS["Content-Length"]} REQUEST_BODY
  fi
  echo ${REQUEST_BODY}
}

function respond() {
  local length=$(($(echo $2 | wc --chars) - 1))
  echo -e "HTTP/1.1 $1\r\nContent-Length: $length\r\n\r\n$2" > RESPONSE
}
