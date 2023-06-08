declare -g AUTH_REGISTER_REGEX='POST \/register\/'

function handleAuthRegister() {
  echo "Registering"
  requireJsonField "username" "$REQUEST_BODY"
  requireJsonField "email" "$REQUEST_BODY"
  requireJsonField "password" "$REQUEST_BODY"
  local username=$(getJsonField "username" "$REQUEST_BODY")
  local email=$(getJsonField "email" "$REQUEST_BODY")
  local password=$(getJsonField "password" "$REQUEST_BODY")
  echo "Username: $username, Email: $email, Password: $password"
  respond 200 lol
}
