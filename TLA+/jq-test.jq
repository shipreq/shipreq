["#", "Step", "Clocks", "Nodes"],
["=", "====", "======", "====="],
(
  .

  | (.. | objects | select(has("rev"))) |= "\(.node)\(.rev)"
  | (.. | objects | select(has("prov"))) |= ("\(.key):\(.prov)" | sub(":\\[]";""))

  | .[]
  | [
    .no,
    .name,
    (.state.clocks? // "-" | tostring),
    (.state.nodes? // "-" | tostring)
  ]
)
| @tsv
| gsub("[\"\\\\]"; "")
| gsub(","; ", ")
| if contains("\tStart") then "\u001b[92m\(.)\u001b[0m" else . end
| if contains("\tEdit") then "\u001b[93m\(.)\u001b[0m" else . end
| if contains("\tMerge") then "\u001b[91m\(.)\u001b[0m" else . end
