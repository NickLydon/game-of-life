// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
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
    let rec gameLoop state = 
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
        for row in iteration do
            for cell in row do
                console.Write(if cell.state = Alive then 'O'
                              else ' ')
            console.WriteLine("")
        console.WriteLine("")
        System.Threading.Thread.Sleep 500
        gameLoop iteration
    
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
    
    gameLoop initialCells
    0 // return an integer exit code
