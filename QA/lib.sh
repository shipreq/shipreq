# Set a global 'url' var
function eval_url {
  url="$1"
  [ -e "$url" ] && url="$(cat "$url" | sed '/^ *$/d' | head -1)"
}

function cd_gatling {
  cd "$(dirname "$0")/gatling"
}
