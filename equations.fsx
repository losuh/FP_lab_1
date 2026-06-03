let eps = 1e-6
let maxIterations = 10000


let dichotomy f left right =
    let rec loop a b iter =
        if iter >= maxIterations || abs (b - a) < eps then
            (a + b) / 2.0
        else
            let m = (a + b) / 2.0
            if f a * f m <= 0.0 then loop a m (iter + 1)
            else loop m b (iter + 1)

    if f left * f right > 0.0 then
        failwith "Для дихотомии на концах интервала функция должна иметь разные знаки"
    else
        loop left right 0

let iterations phi x0 =
    let rec loop x iter =
        let nextX = phi x
        if iter >= maxIterations || abs (nextX - x) < eps then nextX
        else loop nextX (iter + 1)

    loop x0 0


let newton f f' x0 =
    let phi x = x - f x / (f' x)
    iterations phi x0


let f14 x =
    tan (x / 2.0) - (1.0 / tan (x / 2.0)) + x

let f14' x =
    let c = cos (x / 2.0)
    let s = sin (x / 2.0)
    0.5 / (c * c) + 0.5 / (s * s) + 1.0

let g14 x =
    x - 0.2 * f14 x

let interval14 = (1.0, 2.0)
let x0_14 = 1.5


let f15 x =
    0.4 + atan (sqrt x) - x

let f15' x =
    1.0 / (2.0 * sqrt x * (1.0 + x)) - 1.0

let g15 x =
    0.4 + atan (sqrt x)

let interval15 = (1.0, 2.0)
let x0_15 = 1.5


let f16 x =
    3.0 * sin (sqrt x) + 0.35 * x - 3.8

let f16' x =
    3.0 * cos (sqrt x) / (2.0 * sqrt x) + 0.35

let g16 x =
    (3.8 - 3.0 * sin (sqrt x)) / 0.35

let interval16 = (2.0, 3.0)
let x0_16 = 2.5

type Equation =
    {
        Name: string
        F: float -> float
        F': float -> float
        G: float -> float
        Interval: float * float
        X0: float
    }

let equations =
    [
        { Name = "eq. 14"; F = f14; F' = f14'; G = g14; Interval = interval14; X0 = x0_14 }
        { Name = "eq. 15"; F = f15; F' = f15'; G = g15; Interval = interval15; X0 = x0_15 }
        { Name = "eq. 16"; F = f16; F' = f16'; G = g16; Interval = interval16; X0 = x0_16 }
    ]

let solve eq =
    let a, b = eq.Interval
    let rootDichotomy = dichotomy eq.F a b
    let rootIterations = iterations eq.G eq.X0
    let rootNewton = newton eq.F eq.F' eq.X0
    (rootDichotomy, rootIterations, rootNewton)

let printTable () =
    printfn "-------------------------------------------------------------------------------"
    printfn "|  Equation   |  Dichotomy  | Iterations  |    Newton    |"
    printfn "-------------------------------------------------------------------------------"
    equations
    |> List.iter (fun eq ->
        let d, i, n = solve eq
        printfn "| %11s | %12.6f | %12.6f | %12.6f |" eq.Name d i n)
    printfn "-------------------------------------------------------------------------------"

printTable ()
