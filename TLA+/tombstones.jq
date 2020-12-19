["#", "Step", "Target", "Remote", "Nodes", "Network"],
["=", "====", "======", "======", "=====", "======="],
(
  .

  # Condense options
  | (.. | objects | select(has("isEmpty"))) |= (
    if .isEmpty then "-" else .get end
  )

  # Condense drafts
  | (.. | objects | select(has("live"))) |= (
    (if .live then "" else "!" end) as $tomb
    | "\(.rev)\($tomb)"
  )

  | .[]
  | [
    .no,
    .name,
    (.state.target? // "-" | tostring),
    (
      .state.nodes?.r?
      // "-"
      | tostring
      | gsub("[{}]"; "")
    ),
    (
      .state.nodes?
      | with_entries(select(.key != "r" and .value != "-"))?
      // "-"
      | tostring
      | gsub("[{}]"; "")
    ),
    (
      .state
      | if .network? then (
          [ .network[] | "\(.from)(\(.draft))\(.to)" ]
          | tostring
        ) else
          "-"
        end
    )
  ]
)
| @tsv
| gsub("[\"\\\\]"; "")
| gsub(","; ", ")
| if contains("\tEdit") then "\u001b[93m\(.)\u001b[0m" else . end
| if contains("\tKill") then "\u001b[94m\(.)\u001b[0m" else . end
