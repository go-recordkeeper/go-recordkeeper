
# TODO merge this into getJsonField somehow
function requireJsonField() {
  local hasField=$(echo $2 | jq -e "has(\"$1\")")
  if [[ $hasField == "false" ]]; then
    echo "Missing field \"$1\""
    echo -e "HTTP/1.1 403\r\n\r\nMissing field \"$1\"" > RESPONSE
    exit 1
  fi
}

function getJsonField() {
  echo $(echo $2 | jq -r ".$1")
}
