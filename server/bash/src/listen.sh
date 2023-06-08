rm -f response
mkfifo response

declare -g REQUEST_PATH=""
declare -gA REQUEST_HEADERS
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
    if [ -z "$trline" ]; then
      break
    fi
    local header=$(echo $trline | sed -E "s/^([a-zA-Z\-]+): .*\$/\1/")
    local value=$(echo $trline | sed -E "s/^[a-zA-Z\-]+: (.*)\$/\1/")
    HEADERS+=( [$header]=$value )
  done

  # TODO request body
}

function handleRequest() {
  parseHttpRequest
  echo $REQUEST_PATH
  for key in ${!REQUEST_HEADERS[@]}; do
    echo "$key: ${REQUEST_HEADERS[$key]}"
  done

  case $REQUEST_PATH in
    $(awk "/${AUTH_REGISTER_REGEX}/" <<< ${REQUEST_PATH})) handleAuthRegister ;;
    *) echo "NO" ;; # TODO 404
  esac
  echo -e "HTTP/1.1 200\r\n\r\nlol" > response
}

PORT=8000
echo "Listening on port $PORT"
while true; do
  cat response | nc -lN $PORT | handleRequest
done
