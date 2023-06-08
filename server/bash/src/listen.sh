
function handleRequest() {
  parseHttpRequest
  echo $REQUEST_PATH
  for key in ${!REQUEST_HEADERS[@]}; do
    echo "$key: ${REQUEST_HEADERS[$key]}"
  done

  case $REQUEST_PATH in
    $(awk "/${AUTH_REGISTER_REGEX}/" <<< ${REQUEST_PATH})) handleAuthRegister ;;
    *) respond 404 ""
  esac
}

PORT=8000
echo "Listening on port $PORT"
while true; do
  cat RESPONSE | nc -lN $PORT | handleRequest
done
