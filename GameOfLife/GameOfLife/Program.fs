// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System

type State = 
    | Alive
    | Dead

type Cell = 
    { state : State
      position : int * int
      neighbours : Cell list }

type console = System.Console

[<EntryPoint>]
let main argv = 


    let gameLoop state = 
        let stepCell (cell : Cell) = 
            let liveNeighbours = 
                cell.neighbours
                |> List.filter (fun c -> c.state = Alive)
                |> List.length
            match cell.state with
            | Dead -> 
                if liveNeighbours = 3 then { cell with state = Alive }
                else cell
            | Alive -> 
                if (liveNeighbours < 2 || liveNeighbours > 3) then { cell with state = Dead }
                else cell
        
        let withNeighbouringCells initialCells = 
            initialCells 
            |> List.map 
                   (fun row -> 
                   row 
                   |> List.map 
                          (fun cell -> 
                          let neighbourCoordinates (x, y) = 
                              seq { 
                                  for i in (x - 1)..(x + 1) do
                                      for j in (y - 1)..(y + 1) do
                                          if (i, j) <> (x, y) then yield (i, j)
                              }
                          { cell with neighbours = 
                                          initialCells
                                          |> List.collect (id)
                                          |> List.filter 
                                                 (fun c -> 
                                                 neighbourCoordinates (cell.position) 
                                                 |> Seq.exists (fun p -> p = c.position)) }))
        
        let iteration = withNeighbouringCells state |> List.map (fun row -> row |> List.map stepCell)
        
        iteration
    
    let initialConfig = 
        let pulsar = 
            let topHalf = 
                let allDead = 
                    [ for i in 1..15 -> Dead ]
                [ allDead
                  [ Dead; Dead; Dead; Alive; Alive; Alive; Dead; Dead; Dead; Alive; Alive; Alive; Dead; Dead; Dead ]
                  allDead
                  
                  [ Dead; Alive; Dead; Dead; Dead; Dead; Alive; Dead; Alive; Dead; Dead; Dead; Dead; Alive; Dead ]
                  
                  [ Dead; Alive; Dead; Dead; Dead; Dead; Alive; Dead; Alive; Dead; Dead; Dead; Dead; Alive; Dead ]
                  
                  [ Dead; Alive; Dead; Dead; Dead; Dead; Alive; Dead; Alive; Dead; Dead; Dead; Dead; Alive; Dead ]
                  [ Dead; Dead; Dead; Alive; Alive; Alive; Dead; Dead; Dead; Alive; Alive; Alive; Dead; Dead; Dead ] ]
            topHalf
            |> List.append 
                   [ [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; 
                       Dead ] ]
            |> List.append (topHalf |> List.rev)
        pulsar
    
    let initialCells = 
        initialConfig |> List.mapi (fun x row -> 
                             row |> List.mapi (fun y state -> 
                                        { state = state
                                          position = (x, y)
                                          neighbours = [] }))
    
    
    let numberOfIterations =
        if argv.Length > 0 
        then
            Int32.TryParse argv.[0]
        else
            (false, 0)


    let draw iteration =
        let writeline() = console.WriteLine()
        for row in iteration do
            for cell in row do
                console.Write(if cell.state = Alive then 'O'
                                else ' ')
            writeline()
        for _ in 1 .. 6 do writeline()

    let cachableIterations = 
        let cache = System.Collections.Generic.Dictionary<int,Cell list list>()
        cache.[0] <- initialCells

        (0, initialCells)
        |> Seq.unfold(fun (number, state) ->
            if fst numberOfIterations && number > snd numberOfIterations
            then None
            else
                Some(state, (number + 1, if cache.ContainsKey number then cache.[number] else gameLoop state))
        )

    if fst numberOfIterations
    then 
        cachableIterations |> Seq.last |> draw
        Console.ReadLine() |> ignore
    else 
        cachableIterations
        |> Seq.iter(fun iteration ->
            draw iteration
            System.Threading.Thread.Sleep 500
        )
    
    
    0 // return an integer exit code
