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
        NPC1Position: Position
    }

let initGame = 
    {
        Maze = Maze.MazeMatrix
        PlayerPosition = {X = 1; Y = 1; FutureDirection = Direction.None}
        Score = 0
        Lifes = 3
        IsRunning = true
        NPC1Position = {X = 11; Y = 9; FutureDirection = Direction.None}
    }

let printField (gameState:State) =
    for i = 0 to (gameState.Maze.Length - 1) do
        let line = gameState.Maze[i]
        for j = 0 to (line.Length - 1) do
            let chr = line[j]
            match (i,j) with
            | x when fst x = gameState.PlayerPosition.Y && snd x = gameState.PlayerPosition.X -> printf "%s" Maze.PLAYER_SYMBOL
            | x when fst x = gameState.NPC1Position.Y && snd x = gameState.NPC1Position.X -> printf "%s" Maze.NPC_SYMBOL
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
        moved

let eatApple (gameState:State) =
    let playerX = gameState.PlayerPosition.X
    let playerY = gameState.PlayerPosition.Y
    if gameState.Maze[playerY][playerX] = Maze.APPL then
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
    else
        gameState.Score

let loseLife (gameState:State) =
    if gameState.PlayerPosition.X = gameState.NPC1Position.X && gameState.PlayerPosition.Y = gameState.NPC1Position.Y then
        (gameState.Lifes - 1)
    else
        gameState.Lifes

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
        let movedNPC1 = moveNPC gameState.NPC1Position
        let mazeNoApple = eatApple gameState
        let countedScore = countScore gameState
        let newLifes = loseLife gameState
        let isRunning = checkLosing gameState
        run { Maze = mazeNoApple;
                PlayerPosition = movedPlayer; 
                Score = countedScore; 
                NPC1Position = movedNPC1;
                Lifes = newLifes;
                IsRunning = isRunning }
    | false -> 
        Console.Clear()
        printLose gameState
        let willRestart = restartGame gameState
        match willRestart.IsRunning with
        | false -> exit(0)
        | true -> run willRestart
        
