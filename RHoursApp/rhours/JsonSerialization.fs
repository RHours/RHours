module JsonSerialization

open Json
open System
open System.Collections
open System.Reflection
open Microsoft.FSharp.Reflection

type private ForReflection =
    class end

type SkipSerializationAttribute() =
    inherit Attribute()

let private SkipProperty (p: PropertyInfo) =
    (p.GetCustomAttributes(typeof<SkipSerializationAttribute>) |> Seq.length) <> 0

let rec SerializeEnumerable (data: obj) : obj =
    let enumerable = data :?> IEnumerable

    let seqEnumerable = 
        seq {
            for x in enumerable do
                yield (Serialize x)
        }

    (seqEnumerable |> Seq.toArray<obj>) :> obj

and SerializeRecord (data: obj) : obj =
    let dataType = data.GetType()
    let dataProperties = dataType.GetProperties() |> Array.toList
    let fieldList = (FSharpValue.GetRecordFields(data)) |> Array.toList

    let mutable i = 0
    let jsonList = 
        fieldList
            |> List.choose
                (
                    fun x -> 
                        let p = dataProperties.[i]
                        i <- i + 1
                        if SkipProperty p then
                            None
                        else
                            Some
                                {
                                    JsonPair.Label = p.Name;
                                    JsonPair.Value = (Serialize x);
                                }
                )
    jsonList :> obj

and SerializeUnion (data: obj) : obj =
    let dataType = data.GetType()
    let (info, caseData) = FSharpValue.GetUnionFields(data, dataType)

    let typePair = 
        {
            JsonPair.Label = "Type";
            JsonPair.Value = info.Name
        }

    let caseJson = SerializeEnumerable caseData
    let itemsPair = 
        {
            JsonPair.Label = "Items";
            JsonPair.Value = caseJson;
        }

    ([ typePair; itemsPair; ]) :> obj

and Serialize (data: obj) : obj =
    let dataType = data.GetType()
    match data with
    | :? string
    | :? int
    | :? float
    | :? bool -> data
    | :? DateTime as x -> (x.ToString("o")) :> obj
    | :? Decimal as x -> float(x) :> obj
    | _ when dataType = typeof<byte array> ->
        (Convert.ToBase64String(data :?> byte array)) :> obj
    | _ -> 
        if FSharpType.IsRecord(dataType) then
            SerializeRecord data
        else
            if ((typeof<System.Collections.IEnumerable>).IsAssignableFrom(dataType)) then
                SerializeEnumerable data
            else
                if FSharpType.IsUnion(dataType) then
                    SerializeUnion data
                else
                    if dataType.IsPrimitive then
                        (data.ToString()) :> obj
                    else
                        failwith "Unable to serialize data"

let private IsList (typeOfT: Type) =
    if typeOfT.IsGenericType then
        let genericTypeOfT = typeOfT.GetGenericTypeDefinition()
        let genericTypeOfList = (typeof<obj list>).GetGenericTypeDefinition()
        if (genericTypeOfT = genericTypeOfList) && (typeOfT.GenericTypeArguments.Length = 1) then
            Some(typeOfT.GenericTypeArguments.[0])
        else
            None
    else
        None

let MakeList<'T> (values: obj array) : 'T list = 
    let typedSequence =
        seq {
            for v in values do
                yield (v :?> 'T)
        }
    typedSequence |> Seq.toList<'T>

let private GetMethod (t: Type) (name: string) =
    let typeOfForReflection = typeof<ForReflection>
    let moduleType = typeOfForReflection.DeclaringType
    let method = moduleType.GetMethod(name);
    method.MakeGenericMethod(t)

let rec Deserialize<'T> (json: obj) : 'T =
    let typeOfT = typeof<'T>   

    match typeOfT with
    | t when t = typeof<string> -> json :?> 'T
    | t when t = typeof<int> -> json :?> 'T
    | t when t = typeof<float> -> json :?> 'T
    | t when t = typeof<bool> -> json :?> 'T
    | t when t = typeof<DateTime> -> (DateTime.Parse(json :?> string) :> obj) :?> 'T
    | t when t = typeof<Decimal> -> ((Convert.ToDecimal(json)) :> obj) :?> 'T
    | t when t = typeof<byte array> -> ((Convert.FromBase64String(json :?> string)) :> obj) :?> 'T
    | _ -> 
        if FSharpType.IsRecord(typeOfT) then
            DeserializeRecord<'T> (json :?> JsonObject)
        else
            match IsList(typeOfT) with
            | Some(listType) -> 
                    DeserializeList (json :?> JsonArray) listType
            | None -> 
                if FSharpType.IsUnion(typeOfT) then
                    DeserializeUnion<'T> (json :?> JsonObject)
                else 
                    failwith "Can't deserialize"

and DeserializeUnion<'T> (jsonObj: JsonObject) : 'T =
    match jsonObj with
    | [ { Label= "Type"; Value= unionTypeName; };
        { Label= "Items"; Value= itemsJsonValue; };
      ] -> 
        // 'T is a union
        let typeOfT = typeof<'T>
        let unionCases = FSharpType.GetUnionCases(typeOfT)
        let thisCase = unionCases |> Array.find (fun uc -> uc.Name = (unionTypeName :?> string))
        let fields = thisCase.GetFields()
        let itemsJsonArray = itemsJsonValue :?> JsonArray

        let DeserializeField (json: obj) (field: PropertyInfo) : obj =
            let desMethod = GetMethod (field.PropertyType) "Deserialize"
            desMethod.Invoke(null, [| json |])
            
        let args = Array.map2 DeserializeField itemsJsonArray fields

        let unionObj = FSharpValue.MakeUnion(thisCase, args)
        unionObj :?> 'T

    | _ -> failwith "Bad union format"

and DeserializeList<'T> (jsonArray : JsonArray) (listType: Type) : 'T =    
    let desMethod = GetMethod listType "Deserialize"
    let makeListMethod = GetMethod listType "MakeList"
   
    let desArray = jsonArray |> Array.map (fun x -> desMethod.Invoke(null, [| x |] ))
    let desList = makeListMethod.Invoke(null, [| desArray |])
    (desList :?> 'T)

and DeserializeRecord<'T> (jsonObj : JsonObject) : 'T =
    let typeOfT = typeof<'T>
    let recordFields = FSharpType.GetRecordFields(typeOfT)  // PropertyInfo

    let recordData = Array.create (recordFields.Length) null
    let mutable i = 0

    let DeserializeProperty (jsonPair: JsonPair) =  // returns deserialized object as obj
        while SkipProperty (recordFields.[i]) do
            i <- i + 1
        
        let p = recordFields.[i]
        if not (SkipProperty p) then
            if p.Name = jsonPair.Label then
                let desMethod = GetMethod (p.PropertyType) "Deserialize"
                recordData.[i] <- desMethod.Invoke(null, [| jsonPair.Value |])
                i <- i + 1
            else
                failwith "Bad order of data."

    jsonObj |> List.iter DeserializeProperty
    let recordObj = FSharpValue.MakeRecord(typeOfT, recordData)
    (recordObj :?> 'T)

