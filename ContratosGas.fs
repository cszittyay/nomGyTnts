module ContratosGas

open DbContext
open TiposNTS




let idNaturgy =  763
// obtener los contratos de gas
let contratosGas = ctx.Dbo.TContratoGas |> Seq.toList


let cgVigente diaGas = 
    query {
        for cg in ctx.Dbo.TContratoGas do
        join cgd in ctx.Dbo.TContratoGasDetalle  on (cg.IdContratoGas = cgd.IdContratoGas)
        join pto in ctx.Dbo.TPunto on (cgd.IdPunto = pto.IdPunto)
        join cgCMD in ctx.Dbo.TContratoGasDetalleCmdDiaria on (cgd.IdContratoGasDetalle = cgCMD.IdContratoGasDetalle)
        
        where (cg.VigenciaDesde <= diaGas && cg.VigenciaHasta >= diaGas && 
               cg.IdComercializador = idNaturgy && 
               cgCMD.Dia  = diaGas  &&
               cgCMD.CantidadCmd > 0.0m)
        select { Contrato = Contrato cg.Nemonico ; PuntoRX = Punto pto.Codigo ;   CMD = float cgCMD.CantidadCmd * 1.0<GJ> ; Precio =  1.0<USD/GJ>  }
    }



// a los contratos de gas añadir las posibles inyecciones MAKEUP
let contratoMakup = { Contrato = Contrato "MAKEUP" ; PuntoRX = puntoMakeup ;   CMD = 999999.0<GJ> ; Precio =  100.0<USD/GJ>  }

let contratosGasConMakeup diaGas = 
    let contratos = cgVigente diaGas |> Seq.toList
    contratoMakup :: contratos