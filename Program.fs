open Nominacion
open ContratosGas
open Entrega
open ContratoTransporte

let diaGas = new System.DateTime(2025, 07, 1)

let cg = cgVigente diaGas
cg |> Seq.iter (fun x -> printfn "%A" x)


let ed = entrega diaGas
ed |> Seq.iter (fun x -> printfn "%A" x)


let ct = contratosTransporte diaGas
ct |> Seq.iter (fun x -> printfn "%A" x)