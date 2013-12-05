# Set a global 'url' var
function eval_url {
  url="$1"
  [ ! -e "$url" ] && [ -e "url-$url" ] && url="url-$url"
  [ ! -e "$url" ] && [ -e "$(dirname "$0")/$url" ] && url="$(dirname "$0")/$url"
  [ ! -e "$url" ] && [ -e "$(dirname "$0")/url-$url" ] && url="$(dirname "$0")/url-$url"
  [ -e "$url" ] && url="$(cat "$url" | sed '/^ *$/d' | head -1)"
}

function cd_gatling {
  cd "$(dirname "$0")/gatling"
}
