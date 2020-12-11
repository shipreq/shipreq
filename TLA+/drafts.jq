["#", "Step", "Remote", "Tabs", "Workers", "Network", "Browser"],
["=", "====", "======", "====", "=======", "=======", "======="],
(
  .

  # Condense drafts into a "worker:time:prov" format like "1.5:{1.3}"
  | (.. | select(.prov?)) |= (
      (.prov | with_entries(select(.value>0))) as $prov
      | (if .tombstone? then "d" else "" end) as $tomb
      | "\(.worker).\(.time)\($prov)\($tomb)"
      | gsub(":";".") | gsub("w";"") | gsub("{}";"")
    )

  | .[]
  | [
    .no,
    .name,
    (.state.remote.drafts? // "-" | tostring),
    (.state.tabs?
      | with_entries(
          if .value.status == "-" then
            "-"
          elif .value.status == "clean" then
            .value |= .tombstones
          else
            .value |= (
              (if (.editRev? > 0) then "+" else "" end) as $dirty
              | (if (.aborted?) then "A" else "" end) as $aborted
              | "\(.drafts? // [])\($dirty)\($aborted)"
            )
          end
        )?
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
          | "\(.type | sub(":.*";""))(\(.from)→\(.to)):\(.drafts)\(if .edit.get? then "+" else "" end)" ]
          | sort
        )?
      // "-"
      | tostring
      | gsub("Remote"; "R")
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
