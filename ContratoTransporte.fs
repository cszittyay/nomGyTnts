module ContratoTransporte

open System
open DbContext
open TiposNTS


let ctoTte = ctx.Dbo.TContratoTransporte 
let cteDet = ctx.Dbo.TContratoTransporteDetalle
let punto = ctx.Dbo.TPunto


// Obtiene los contratos de transporte vigentes para un día dado

let contratosTransporte  diaGas = query{
                for cto in ctoTte do
                join ctd in cteDet on (cto.IdContratoTransporte = ctd.IdContratoTransporte)
                join ptr in punto on (ctd.IdPuntoRecepcion = ptr.IdPunto)
                join pte in punto on (ctd.IdPuntoEntrega = pte.IdPunto)
                where (cto.VigenciaDesde <= diaGas && cto.VigenciaHasta >= diaGas && 
                       cto.IdComercializador = idNaturgyServicios && 
                       cto.IdTransportista = idCengas &&
                       ctd.VigenciaDesde <= diaGas && ctd.VigenciaHasta >= diaGas &&
                       ctd.CantidadCmd.IsSome)
                select ({ Contrato = Contrato cto.Nemonico ; PuntoRX = Punto ptr.Codigo ; PuntoEX = Punto pte.Codigo ; CMD = float ctd.CantidadCmd.Value * 1.0<GJ> ; Tarifa = 1.0<USD/GJ> })
                }

// Obtener los puntos de entregas de los contratos de transporte
let puntosEX diaGas = contratosTransporte diaGas   |> Seq.filter(fun c -> c.CMD > 0.<GJ>) |> Seq.map (fun c -> c.PuntoEX)

// agregar el contrato de Transporte para Makeup
let contratosTteMakeup  (diaGas:DateTime) = puntosEX diaGas |> Seq.map (fun ptoEx -> { Contrato = Contrato "MAKEUP-TTE"; PuntoRX = puntoMakeup ; PuntoEX = ptoEx ; CMD = 999999.0<GJ> ; Tarifa = 100.0<USD/GJ> })