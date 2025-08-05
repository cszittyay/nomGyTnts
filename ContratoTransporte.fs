module ContratoTransporte

open DbContext
open TiposNTS


let ctoTte = ctx.Dbo.TContratoTransporte 
let cteDet = ctx.Dbo.TContratoTransporteDetalle
let punto = ctx.Dbo.TPunto


let contratosTransporte  diaGas = query{
                for cto in ctoTte do
                join ctd in cteDet on (cto.IdContratoTransporte = ctd.IdContratoTransporte)
                join ptr in punto on (ctd.IdPuntoRecepcion = ptr.IdPunto)
                join pte in punto on (ctd.IdPuntoEntrega = pte.IdPunto)
                where (cto.VigenciaDesde <= diaGas && cto.VigenciaHasta >= diaGas && 
                       cto.IdComercializador = 763 && 
                       ctd.VigenciaDesde <= diaGas && ctd.VigenciaHasta >= diaGas &&
                       ctd.CantidadCmd.IsSome)
                select ({ Contrato = Contrato cto.Nemonico ; PuntoRX = Punto ptr.Codigo ; PuntoEX = Punto pte.Codigo ; CMD = float ctd.CantidadCmd.Value * 1.0<GJ> ; Tarifa = 1.0<USD/GJ> })
                }