module Game
open System
open System.Threading

type Direction =
    | Left
    | Right
    | Up
    | Down
    | None

type Position = { X: int; Y: int; FutureDirection: Direction }

type State = 
    {
        Maze: string[][]
        PlayerPosition: Position
        Score: int
        Lifes: int
        IsRunning: bool
        NPCs: Position[]
    }

let initGame = 
    {
        Maze = Maze.MazeMatrix
        PlayerPosition = {X = 1; Y = 1; FutureDirection = Direction.None}
        Score = 0
        Lifes = 3
        IsRunning = true
        NPCs = [|{X = 11; Y = 11; FutureDirection = Direction.None};
                {X = 9; Y = 11; FutureDirection = Direction.None};
                {X = 12; Y = 11; FutureDirection = Direction.None}
                |]
    }

let rec willPrintNPC currentPosition npcs =
    match Array.length npcs with
    | 0 -> false
    | _ -> ((Array.head npcs).X = currentPosition.X && (Array.head npcs).Y = currentPosition.Y) || (willPrintNPC currentPosition (Array.tail npcs))

let printField (gameState:State) =
    for yMaze = 0 to (gameState.Maze.Length - 1) do
        let line = gameState.Maze[yMaze]
        for xMaze = 0 to (line.Length - 1) do
            let chr = line[xMaze]
            match (yMaze,xMaze) with
            | x when fst x = gameState.PlayerPosition.Y && snd x = gameState.PlayerPosition.X -> printf "%s" Maze.PLAYER_SYMBOL
            | x when (willPrintNPC { X=xMaze; Y=yMaze; FutureDirection = Direction.None } gameState.NPCs) -> printf "%s" Maze.NPC_SYMBOL
            | _ -> printf "%s" chr
        printfn ""

let printLifes lifes =
    for i = 1 to lifes do
        printf "❤️ "
    ()

let printGame(gameState:State) =
    printField gameState
    printfn "Your score: %A" gameState.Score
    printfn "Pacman's lifes: "
    printLifes gameState.Lifes

let getDirectionInput () =
    if not Console.KeyAvailable then
        Direction.None
    else
        match Console.ReadKey().Key with
        | ConsoleKey.UpArrow -> Direction.Up
        | ConsoleKey.DownArrow -> Direction.Down
        | ConsoleKey.LeftArrow -> Direction.Left
        | ConsoleKey.RightArrow -> Direction.Right
        | _ -> Direction.None

let moveSomeone position =
    match position.FutureDirection with
    | Direction.Up when position.Y > 0 && 
        Maze.MazeMatrix[position.Y-1][position.X] <> Maze.WALL -> 
        { position with Y = position.Y-1 } // inversion of y
    | Direction.Left when position.X > 0 && 
        Maze.MazeMatrix[position.Y][position.X-1] <> Maze.WALL -> 
        { position with X = position.X-1 }
    | Direction.Left when position.X = 0  -> 
        { position with X = Maze.MazeMatrix.Length-1 }
    | Direction.Down when position.Y < (Maze.MazeMatrix.Length - 1) && 
        Maze.MazeMatrix[position.Y+1][position.X] <> Maze.WALL -> 
        { position with Y = position.Y+1 }
    | Direction.Right when position.X < (Maze.MazeMatrix.Length - 1) && 
        Maze.MazeMatrix[position.Y][position.X+1] <> Maze.WALL -> 
        { position with X = position.X+1 }
    | Direction.Right when position.X = (Maze.MazeMatrix.Length - 1) -> 
        { position with X = 0 }
    | _ -> position

let checkEntranceForNPC position = 
    match position.FutureDirection with 
    | direcrion when (direcrion = Direction.Up || direcrion = Direction.Down) &&
        Maze.MazeMatrix[position.Y][position.X-1] <> Maze.WALL -> 
        moveSomeone { position with FutureDirection=Direction.Left }
    | direcrion when (direcrion = Direction.Up || direcrion = Direction.Down) &&
        Maze.MazeMatrix[position.Y][position.X+1] <> Maze.WALL -> 
        moveSomeone { position with FutureDirection=Direction.Right }
    | direcrion when (direcrion = Direction.Left || direcrion = Direction.Right) &&
        Maze.MazeMatrix[position.Y-1][position.X] <> Maze.WALL -> 
        moveSomeone { position with FutureDirection=Direction.Up }
    | direcrion when (direcrion = Direction.Left || direcrion = Direction.Right) &&
        Maze.MazeMatrix[position.Y+1][position.X] <> Maze.WALL -> 
        moveSomeone { position with FutureDirection=Direction.Down }
    | _ -> position

let moveNPC position =
    let moved = moveSomeone position
    if moved = position then
        let random = new Random()
        let futureDirection = 
            match random.Next(4) with
            | 0 -> Direction.Up
            | 1 -> Direction.Down
            | 2 -> Direction.Left
            | 3 -> Direction.Right
            | _ -> Direction.None
        moveSomeone { position with FutureDirection = futureDirection }
    else
        let canMoveToEntrance = checkEntranceForNPC { position with FutureDirection=moved.FutureDirection }
        if canMoveToEntrance = { position with FutureDirection=moved.FutureDirection } then
            moved
        else
            canMoveToEntrance

let moveNPCs (gameState:State) =
    Array.map moveNPC gameState.NPCs

let eatApple (gameState:State) =
    let playerX = gameState.PlayerPosition.X
    let playerY = gameState.PlayerPosition.Y
    if gameState.Maze[playerY][playerX] = Maze.APPL || gameState.Maze[playerY][playerX] = Maze.BIG_APPL then
        gameState.Maze |> Array.mapi (fun i line -> 
            if i = playerY then 
                line |> Array.mapi (fun j chr -> if j = playerX then Maze.EMPT else chr)
            else
                line)
    else
        gameState.Maze

let countScore (gameState:State) =
    let playerX = gameState.PlayerPosition.X
    let playerY = gameState.PlayerPosition.Y
    if gameState.Maze[playerY][playerX] = Maze.APPL then
        gameState.Score + 1
    elif gameState.Maze[playerY][playerX] = Maze.BIG_APPL then
        gameState.Score + 10
    else
        gameState.Score

let rec loseLife (gameState:State) =
    match Array.length gameState.NPCs with
    | 0 -> gameState.Lifes
    | _ ->
        if gameState.PlayerPosition.X = ((Array.head gameState.NPCs).X) && 
            gameState.PlayerPosition.Y = ((Array.head gameState.NPCs).Y) then
            (gameState.Lifes - 1)
        else
            loseLife {gameState with NPCs = Array.tail gameState.NPCs}

let checkLosing (gameState:State) =
    gameState.Lifes > 0
    
let printLose (gameState:State) =
    printField { gameState with Maze = ( gameState.Maze |> Array.map (fun line -> line |> Array.map (fun chr -> "❌")))}
    printfn "You losed!"

let restartGame (gameState:State) =
    printfn "Want to restart?"
    printfn "Press Y for yes, press N for quit game"
    match Console.ReadKey().Key with
        | ConsoleKey.Y -> initGame
        | ConsoleKey.N -> gameState
        | _ -> gameState

let rec run (gameState:State) =
    match gameState.IsRunning with
    | true ->
        Thread.Sleep(200)
        Console.Clear()
        printGame gameState
        let inputDirection = getDirectionInput ()
        let movedPlayer = moveSomeone { gameState.PlayerPosition with FutureDirection = inputDirection}
        run { Maze = eatApple gameState;
                PlayerPosition = movedPlayer; 
                Score = countScore gameState; 
                NPCs = moveNPCs gameState;
                Lifes = loseLife gameState;
                IsRunning = checkLosing gameState }
    | false -> 
        Console.Clear()
        printLose gameState
        let willRestart = restartGame gameState
        match willRestart.IsRunning with
        | false -> exit(0)
        | true -> run willRestart
        
