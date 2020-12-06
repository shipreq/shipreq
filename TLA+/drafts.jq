["#", "Step", "Remote", "Tabs", "Workers"],
(
  .

  # Condense drafts into a "worker:time:prov" format like "w1.5:{w1.3}"
  | (.. | select(.prov?)) |= ("\(.worker).\(.time)\(.prov | with_entries(select(.value>0)))" | gsub("\"";"") | gsub(":";"."))

  | .[]
  | [
    .no,
    .name,
    (.state.remote? // "-" | tostring),
    (.state.tabs? | with_entries(.value |= (.draft?.get? // .drafts // []))? // "-" | tostring),
    (.state.workers? | with_entries(.value |= (.drafts? // "-"))? // "-" | tostring)
  ]
)
| @tsv
| gsub("\""; "")
| gsub(","; ", ")
