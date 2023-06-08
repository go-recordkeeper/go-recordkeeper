
# TODO merge this into getJsonField somehow
function requireJsonField() {
  if [[ -z $(echo $2 | jq -e .) ]]; then
    respond 403 "Malformed input"
    exit 1
  fi
  local hasField=$(echo $2 | jq "has(\"$1\")")
  if [[ $hasField == "false" ]]; then
    echo "Missing field \"$1\""
    respond 403 "Missing field \"$1\""
    exit 1
  fi
}

function getJsonField() {
  echo $2 | jq -r ".$1"
}
