module Entrega

open DbContext
open TiposNTS


let entrega diaGas = query { 
    for de in ctx.Dbo.TDemandaEstimadaPunto do
    join pt in ctx.Dbo.TPunto on (de.IdPunto = pt.IdPunto)
    where (de.Fecha = diaGas)
    select ({ PuntoEX = Punto pt.Codigo ; Entrega = float de.CantidadGj * 1.0<GJ> })
}