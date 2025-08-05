module DbContext

open FSharp.Data.Sql

[<Literal>]
let connectionString = "Server=LAPTOP-9EH9CLGC\MSSQLSERVER03;Database=PRD_DegasMX_Trading;Trusted_Connection=True;TrustServerCertificate=True;"

[<Literal>]
let useOptionTypes = FSharp.Data.Sql.Common.NullableColumnType.OPTION

type sqlNTS = SqlDataProvider<Common.DatabaseProviderTypes.MSSQLSERVER, connectionString, UseOptionTypes = useOptionTypes>

let ctx = sqlNTS.GetDataContext()


let nominaciones = ctx.Dbo.TNominacion |> Seq.toList