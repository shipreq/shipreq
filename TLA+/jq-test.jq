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

  # Condense node states
  | (.. | objects | select(has("awaiting"))) |= ( .
    | (.broadcast | if . == [] then "" else "←\(.)" end) as $broadcast
    | (.awaiting |  if . == [] then "" else "←\(.)" end) as $awaiting
    | "(\(.draft))\($broadcast)\($awaiting)"
  )

  | .[]
  | [
    .no,
    .name,
    (.state.target? // "-" | tostring),
    (
      .state.nodes.r?
      // "-"
      | tostring
      | gsub("[{}]"; "")
    ),
    (
      .state.nodes?
      | with_entries(
          select(.key != "r" and .value != "(-)")
        )?
      // "-"
      | tostring
      | gsub("[{}]"; "")
    ),
    (
      .state
      | if .network? then (
          [ .network[] |
            if .type == "send" then
              "\(.from)→\(.to)(\(.draft))"
            else
              "\(.to)←\(.from)(\(.draft))"
            end
          ]
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
