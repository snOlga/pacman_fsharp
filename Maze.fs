module Maze

let WALL = "██"

let DOOR = "--"

let EMPT = "  "

let APPL = "🔸"

let BIG_APPL = "🔶"

let PLAYER_SYMBOL = "🟡"

let NPC_SYMBOL = "👻"

let MazeMatrix =
    [| [| WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL |] 
       [| WALL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; WALL |] 
       [| WALL; APPL; WALL; APPL; WALL; APPL; WALL; WALL; WALL; WALL; WALL; APPL; WALL; WALL; WALL; WALL; WALL; APPL; WALL; APPL; WALL; APPL; WALL |] 
       [| WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL |] 
       [| WALL; WALL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; WALL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; WALL; WALL |] 
       [| WALL; APPL; APPL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; APPL; APPL; WALL |] 
       [| WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; WALL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL |] 
       [| WALL; APPL; APPL; APPL; WALL; APPL; WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL; APPL; WALL; APPL; APPL; APPL; WALL |] 
       [| WALL; WALL; WALL; WALL; WALL; APPL; WALL; APPL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; APPL; WALL; APPL; WALL; WALL; WALL; WALL; WALL |] 
       [| WALL; BIG_APPL; APPL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; APPL; BIG_APPL; WALL |] 
       [| WALL; WALL; WALL; WALL; WALL; WALL; WALL; APPL; WALL; WALL; WALL; EMPT; WALL; WALL; WALL; APPL; WALL; WALL; WALL; WALL; WALL; WALL; WALL |] 
       [| EMPT; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL; EMPT; EMPT; EMPT; EMPT; EMPT; WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; EMPT |] 
       [| WALL; WALL; WALL; WALL; WALL; WALL; WALL; APPL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; APPL; WALL; WALL; WALL; WALL; WALL; WALL; WALL |] 
       [| WALL; BIG_APPL; APPL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; APPL; BIG_APPL; WALL |] 
       [| WALL; WALL; WALL; WALL; WALL; APPL; WALL; APPL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; APPL; WALL; APPL; WALL; WALL; WALL; WALL; WALL |] 
       [| WALL; APPL; APPL; APPL; WALL; APPL; WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL; APPL; WALL; APPL; APPL; APPL; WALL |] 
       [| WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; WALL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL |] 
       [| WALL; APPL; APPL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; APPL; APPL; WALL |] 
       [| WALL; WALL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; WALL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; APPL; WALL; WALL; WALL |] 
       [| WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL |] 
       [| WALL; APPL; WALL; APPL; WALL; APPL; WALL; WALL; WALL; WALL; WALL; APPL; WALL; WALL; WALL; WALL; WALL; APPL; WALL; APPL; WALL; APPL; WALL |] 
       [| WALL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; APPL; WALL; APPL; APPL; APPL; WALL |] 
       [| WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL; WALL |] |]