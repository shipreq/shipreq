["#", "Step", "Remote", "Tabs", "Workers", "Network", "Browser"],
["=", "====", "======", "====", "=======", "=======", "======="],
(
  .

  # Condense drafts into a "worker:time:prov" format like "1.5:{1.3}"
  | (.. | select(.prov?)) |= ("\(.worker).\(.time)\(.prov | with_entries(select(.value>0)))" | gsub(":";".") | gsub("w";"") | gsub("{}";""))

  | .[]
  | [
    .no,
    .name,
    (.state.remote? // "-" | tostring),
    (.state.tabs?
      | with_entries(.value |= "\(.drafts? // [])\(if (.editRev? > 0) then "*" else "" end)")?
      // "-"
      | tostring
    ),
    (.state.workers?
      | with_entries(.value |= (.drafts? // "-"))?
      // "-"
      | tostring
    ),
    (.state.network?
      | ([ .[]
          | select(.drafts?)
          | "\(.type | sub(":.*";"")):\(.from)→\(.to):\(.drafts)\(if .edit.get? then "*" else "" end)" ]
          | sort
        )?
      // "-"
      | tostring
      | if . == "[]" then "-" else . end
    ),
    (.state.browsers?
      | with_entries(.value |= (with_entries(.value |= (.get? // "-"))))?
      // "-"
      | tostring
    )
  ]
)
| @tsv
| gsub("[\"\\\\]"; "")
| gsub(","; ", ")
