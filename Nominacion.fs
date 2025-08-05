module Nominacion


open DbContext
open TiposNTS

//let getNominacion diaGas =   
//                        query {
//                            for nom in ctx.Dbo.TNominacion do
//                            join ctoT in ctx.Dbo.TContratoTransporteDetalle on (nom.IdContratoTransporteDetalle.Value = ctoT.IdContratoTransporteDetalle)
//                            join ptr in ctx.Dbo.TPunto on (ctoT.IdPuntoRecepcion = ptr.IdPunto)
//                            join pte in ctx.Dbo.TPunto on (ctoT.IdPuntoEntrega = pte.IdPunto)

//                            where (nom.Fecha = diaGas)
//                            select ({ DiaGas = nom.Fecha ; PtoRx = Punto ptr.Codigo ; PtoEx = Punto pte.Codigo ; NominadoRx = float nom.NominadoGj.Value; NominadoEx = 0.})
//                        }



// Decisione
// Nominación gas

