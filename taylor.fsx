let eps = 1e-6
let a = 0.1
let b = 0.6
let pointCount = 10


let builtin x =
    (2.0 * x - 3.0) / ((x - 1.0) * (x - 1.0))


let rec taylorSum next n term sum x =
    if abs term < eps then
        sum, n
    else
        let sum' = sum + term
        let n', term' = next n term x
        taylorSum next n' term' sum' x


let dumbNext n _term x =
    let n' = n + 1
    let term' = -(float (n' + 3)) * (pown x n')
    n', term'

let dumbTaylor x =
    taylorSum dumbNext 0 (-(float 3) * (pown x 0)) 0.0 x


let smartNext n term x =
    let n' = n + 1
    let term' = term * x * (float (n + 4) / float (n + 3))
    n', term'

let smartTaylor x =
    taylorSum smartNext 0 (-3.0) 0.0 x


let gridPoints =
    [0 .. pointCount - 1]
    |> List.map (fun i -> a + float i * (b - a) / float (pointCount - 1))

let printHeader () =
    printfn "--------------------------------------------------------------------------------"
    printfn "|    x   |    Builtin    | Smart Taylor  | # terms |  Dumb Taylor  | # terms |"
    printfn "--------------------------------------------------------------------------------"

let printRow x =
    let builtinValue = builtin x
    let smartValue, smartTerms = smartTaylor x
    let dumbValue, dumbTerms = dumbTaylor x
    printfn "| %6.2f | %12.6f | %12.6f | %7d | %12.6f | %7d |"
        x builtinValue smartValue smartTerms dumbValue dumbTerms

let printTable () =
    printHeader ()
    gridPoints |> List.iter printRow
    printfn "--------------------------------------------------------------------------------"

printTable ()
