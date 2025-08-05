module Nominacion


open DbContext
open TiposNTS



open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure



type [<Measure>] USD
type [<Measure>] GJ

type Punto = Punto of string
type Contrato = Contrato of string

type ContratoGas = {
    Contrato: Contrato
    PuntoRX: Punto
    CMD: float<GJ>
    Precio: float<USD/GJ>
}

type ContratoTransporte = {
    Contrato: Contrato
    PuntoRX: Punto
    PuntoEX: Punto
    CMD: float<GJ>
    Tarifa: float<USD/GJ>
}

type Entrega = {
    PuntoEX: Punto
    Entrega: float<GJ>
}


let fuel = 1.325 / 100.0



let completarContratosGas (contratosExistentes: ContratoGas list) : ContratoGas list =
    // Extraer todos los contratos y puntos posibles
    let contratosUnicos = contratosExistentes |> List.map (fun c -> c.Contrato) |> List.distinct
    let puntosUnicos = contratosExistentes |> List.map (fun c -> c.PuntoRX) |> List.distinct

    let existentesSet = contratosExistentes |> List.map (fun c -> c.Contrato, c.PuntoRX) |> Set.ofList

    let contratosFaltantes =
        [ for contrato in contratosUnicos do
            for punto in puntosUnicos do
                if not (Set.contains (contrato, punto) existentesSet) then
                    yield {
                        Contrato = contrato
                        PuntoRX = punto
                        CMD = 0.0<GJ>
                        Precio = 1.0<USD/GJ>
                    } ]

    contratosExistentes @ contratosFaltantes

    

let completarContratosTransporte (contratosExistentes: ContratoTransporte list) : ContratoTransporte list =
    // Extraer todos los contratos y puntos posibles
    let contratosUnicos = contratosExistentes |> List.map (fun c -> c.Contrato) |> List.distinct
    let puntosUnicosRx = contratosExistentes |> List.map (fun c -> c.PuntoRX) |> List.distinct
    let puntosUnicosEx = contratosExistentes |> List.map (fun c -> c.PuntoEX) |> List.distinct


    let existentesSet = contratosExistentes |> List.map (fun c -> c.Contrato, c.PuntoRX, c.PuntoEX) |> Set.ofList

    let contratosFaltantes =
        [ for contrato in contratosUnicos do
            for puntoRx in puntosUnicosRx do
                for puntoEx in puntosUnicosEx do
                if not (Set.contains (contrato, puntoRx, puntoEx) existentesSet) then
                    yield {
                        Contrato = contrato
                        PuntoRX = puntoRx
                        PuntoEX = puntoEx
                        CMD = 0.0<GJ>
                        Tarifa = 1.0<USD/GJ>
                    } ]

    contratosExistentes @ contratosFaltantes



// Declare the parameters for our model
let puntosRX = [Punto "V061"; Punto "V33"; Punto "V40"]
let puntosEX = [Punto "E01"; Punto "E02"; Punto "E03"]

let contratosGas = 
    [
        ({Contrato = Contrato "C11"; PuntoRX = Punto "V061"; CMD = 40.0<GJ>; Precio = 1.50<USD/GJ>});
        ({Contrato = Contrato "C12"; PuntoRX = Punto "V33"; CMD = 80.0<GJ>; Precio = 0.60<USD/GJ>});
        ({Contrato = Contrato "C13"; PuntoRX = Punto "V40"; CMD = 140.0<GJ>; Precio = 2.55<USD/GJ>})
        ({Contrato = Contrato "C21"; PuntoRX = Punto "V061"; CMD = 200.0<GJ>; Precio = 2.50<USD/GJ>});
        ({Contrato = Contrato "C22"; PuntoRX = Punto "V33"; CMD = 320.0<GJ>; Precio = 1.60<USD/GJ>});
        ({Contrato = Contrato "C23"; PuntoRX = Punto "V40"; CMD = 430.0<GJ>; Precio = 2.55<USD/GJ>})
    ]

let contratosGasCompleto = completarContratosGas contratosGas

let dContratosGas = contratosGasCompleto |> List.map (fun c -> (c.Contrato, c.PuntoRX), c) |> SMap2

let dCtoGasPuntoPrecio = contratosGasCompleto |> List.map (fun c -> (c.Contrato, c.PuntoRX), c.Precio) |> SMap2


// Transporte contracts

let contratosTransporte = 
    [
        ({Contrato = Contrato "T1"; PuntoRX = Punto "V061"; PuntoEX = Punto "E01"; CMD = 100.0<GJ>; Tarifa = 2.50<USD/GJ>});
        ({Contrato = Contrato "T1"; PuntoRX = Punto "V33"; PuntoEX = Punto "E02"; CMD = 160.0<GJ>; Tarifa = 2.60<USD/GJ>});
        ({Contrato = Contrato "T1"; PuntoRX = Punto "V40"; PuntoEX = Punto "E03"; CMD = 230.0<GJ>; Tarifa = 2.55<USD/GJ>});
        ({Contrato = Contrato "T2"; PuntoRX = Punto "V061"; PuntoEX = Punto "E01"; CMD = 100.0<GJ>; Tarifa = 1.50<USD/GJ>});
        ({Contrato = Contrato "T2"; PuntoRX = Punto "V33"; PuntoEX = Punto "E02"; CMD = 60.0<GJ>; Tarifa = 1.60<USD/GJ>});
        ({Contrato = Contrato "T2"; PuntoRX = Punto "V40"; PuntoEX = Punto "E03"; CMD = 130.0<GJ>; Tarifa = 1.55<USD/GJ>});
        ({Contrato = Contrato "T2S"; PuntoRX = Punto "V061"; PuntoEX = Punto "E03"; CMD = 500.0<GJ>; Tarifa = 15.50<USD/GJ>});
        ({Contrato = Contrato "T2S"; PuntoRX = Punto "V33"; PuntoEX = Punto "E03"; CMD = 560.0<GJ>; Tarifa = 15.60<USD/GJ>});
        ({Contrato = Contrato "T2S"; PuntoRX = Punto "V40"; PuntoEX = Punto "E03"; CMD = 550.0<GJ>; Tarifa = 15.55<USD/GJ>});

    ]


// completar contratos de transporte
let contratosTransporteCompleto = completarContratosTransporte contratosTransporte
let dCtoTteTarifa = contratosTransporteCompleto |> List.map (fun c -> (c.Contrato, c.PuntoRX, c.PuntoEX), c.Tarifa) |> SMap3

    
let dContratosTransporte = contratosTransporteCompleto |> List.map (fun c -> (c.Contrato, c.PuntoRX, c.PuntoEX), c.CMD) |> SMap3.ofList

let dCtoTtePrecio = contratosTransporteCompleto |> List.map (fun c -> c.Contrato, c.Tarifa) |> SMap


let entregas = 
    [
        { PuntoEX = Punto "E01"; Entrega = 100.0<GJ> }
        { PuntoEX = Punto "E02"; Entrega = 150.0<GJ> }
        { PuntoEX = Punto "E03"; Entrega = 500.0<GJ> }
    ]

let dEntregas = entregas |> List.map (fun c -> c.PuntoEX, c.Entrega) |> SMap


// Create Decisions for each puntoRX and puntoEX using a DecisionBuilder
// Turn the result into a `SMap2`   
// La nominacion de los contratos de gas por punto y contrato
let nomGas =        
    DecisionBuilder<GJ> "NomGas" {
        for (cto,pto) in dCtoGasPuntoPrecio.Keys  -> Continuous (0.<GJ>, dContratosGas.[cto, pto].CMD)
    } 
    |> SMap2.ofSeq

// La nominación de la entrega de transporte: por Contrato y PuntoEX
let nomTte =        
    DecisionBuilder<GJ> "NomTte" {
        for (cto, ptoRx, ptoEx) in dContratosTransporte.Keys   -> Continuous (0.<GJ>, dContratosTransporte.[cto, ptoRx, ptoEx])
    } |> SMap3.ofSeq


// Create the Linear Expression for the objective
let objectiveExpression = sum (dCtoGasPuntoPrecio .* nomGas) + sum (dCtoTteTarifa .* nomTte)


let objective = Objective.create "Costo Gas y Transp" Minimize objectiveExpression


// Agregar las restricciones


// Create a Constraints: La entrega de transporte debe ser igual a la entrega de gas
let entregaConstraint =
    ConstraintBuilder "Entrega" {
        for ptoEx in puntosEX ->
            sum(nomTte.[All, All, ptoEx]) == dEntregas.[ptoEx]
    }


// Considerar el Fuel en la restricción de gas
let gasConstraint =
    ConstraintBuilder "Gas" {
        for pto in puntosRX ->
            sum(nomTte.[All, pto, All]) == sum(nomGas.[All, pto]) *(1.0 - fuel)
    }


let model =
    Model.create objective
    |> Model.addConstraints entregaConstraint
    |> Model.addConstraints gasConstraint


let result = Solver.solve Settings.basic model

printfn "-- Result --"


// Match the result of the call to solve
// If the model could not be solved it will return a `Suboptimal` case with a message as to why
// If the model could be solved, it will print the value of the Objective Function and the
// values for the Decision Variables
match result with
| Optimal solution ->
    printfn "Objective Value: %f" (Objective.evaluate solution objective)

    let snomGas = Solution.getValues solution nomGas
    let snomTte = Solution.getValues solution nomTte

    for ctoG in  snomGas.Keys  do
       if snomGas.[ctoG] > 0.0<GJ>  then printfn "Item: %s\tValue: %f" (string ctoG) snomGas.[ctoG]

    printfn $"\nNominación Transporte:"

    for (ctoT, ptoRx, ptoEx) in  snomTte.Keys do
       if snomTte.[ctoT,ptoRx, ptoEx] > 0.0<GJ>  then printfn "Cto: %s\tPtoRx: %s\tPtoEx: %s\tValue: %f" (string ctoT) (string ptoRx)  (string ptoEx) snomTte.[ctoT,ptoRx, ptoEx]

| _ -> printfn $"Unable to solve. Error: %A{result}"





