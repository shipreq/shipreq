url=https://shipreq.com

secret='secret=Hooquail2aehiey1viemiefaayengeiGhuch8Eishee3OHu4aiKieth3lieshaid'

envSyntax="(local|dev|prod)"
function envUrl {
  case "$1" in
    local) echo 'http://localhost:8080';;
    dev  ) echo 'https://localhost:14443';;
    prod ) echo 'https://shipreq.com';;
    *) echo "'$1' ∉ $envSyntax" >&2; exit 1
  esac
}

# Syntax: post <env> <urlPath> <params>
function post {
  env="$1"
  urlPath="$2"
  params="$3"

  cmd=(curl -s -o - -X POST)
  cmd+=(-w '\n\nhttp-code=%{http_code}, content-type=%{content_type}, time=%{time_total}\n')

  case "$1" in
    dev) cmd+=(-k);; # Ignore invalid HTTPS
  esac

  body="$secret"
  [ -n "$params" ] && body="$body&$params"
  cmd+=(-d "$body")

  cmd+=("$(envUrl "$1")$urlPath")

  echo "> ${cmd[@]}"
  echo
  "${cmd[@]}"
}
