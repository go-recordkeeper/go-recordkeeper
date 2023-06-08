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

function handleRequest() {
  parseHttpRequest
  echo $REQUEST_PATH
  for key in ${!REQUEST_HEADERS[@]}; do
    echo "$key: ${REQUEST_HEADERS[$key]}"
  done

  case $REQUEST_PATH in
    $(awk "/${AUTH_REGISTER_REGEX}/" <<< ${REQUEST_PATH})) handleAuthRegister ;;
    *) echo -e "HTTP/1.1 404\r\n\r\n" > RESPONSE
  esac
}

PORT=8000
echo "Listening on port $PORT"
while true; do
  cat RESPONSE | nc -lN $PORT | handleRequest
done
