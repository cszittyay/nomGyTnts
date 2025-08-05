module TiposNTS


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


