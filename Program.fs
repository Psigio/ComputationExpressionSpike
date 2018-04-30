open System
open System.IO
open System.Text

module WebReqModule =
    type KVP = {
        key: string
        value: string
    }
    type WebReqContext = {
        guid: Guid
        parameters: KVP list
    }
    
    let defaultWebReqContext = { 
        guid = Guid.Empty
        parameters = []
    }

    type WebReqData =
        | Context of WebReqContext
        | StreamData of byte array

    type WebReqBuilder() =
        member __.UpdateWithGuid requestContext guid =
            {requestContext with guid = guid}
            
        member __.Yield (_: Unit): WebReqData = 
            // Generate the default request context (provide a new Guid in case the user doesn't provide one)
            Context(__.UpdateWithGuid defaultWebReqContext (Guid.NewGuid()))

        [<CustomOperation("guid")>]
        // Allows the user to replace the default Guid
        member __.Guid (requestContext, newGuid) =             
            match requestContext with
                | Context ctx ->
                    Context(__.UpdateWithGuid ctx newGuid)
                | _ -> failwith "Attempt to set Guid after Stream generated"

        [<CustomOperation("parameter")>] 
        // Allows the user to add a parameter to the request
        member __.Parameter (requestContext, parameterName, parameterValue) =    
          match requestContext with
                | Context ctx ->
                    Context({ctx with parameters = {key = parameterName; value = parameterValue} :: ctx.parameters})
                | _ -> failwith "Attempt to set Parameter after Stream generated"       

        [<CustomOperation("generate")>] 
        // Allows the user to generate the request stream in UTF8
        member __.Generate requestContext =
            match requestContext with
                | Context ctx ->
                    let formDataBoundary = String.Format("----{0:N}", ctx.guid)
                    let generateContent key value =
                        String.Format("--{0}\r\nContent-Disposition: form-data; name=\"{1}\"\r\n\r\n{2}\r\n", formDataBoundary, key, value)
                    let writeToStream (target: Stream) (rawString: String) = 
                        let bytes = Encoding.UTF8.GetBytes(rawString)
                        target.Write(bytes, 0, bytes.Length)
                    use ms = new MemoryStream()
                    // Write out parameters
                    for p in ctx.parameters do
                        let raw = generateContent p.key p.value
                        writeToStream ms raw
                    // And footer
                    let footer = String.Format("--{0}--\r\n", formDataBoundary)       
                    writeToStream ms footer
                    // Rewind and convert to array
                    ms.Seek(0L, SeekOrigin.Begin) |> ignore
                    StreamData(ms.ToArray())
                | _ -> failwith "Attempt to generate Stream data more than once"

    let webreq = WebReqBuilder()

    let testIt =
        let reqGuid = Guid.NewGuid()
        let test = webreq {
            guid reqGuid
            parameter "id" "abc-123"
            parameter "test_value" "def-456"
            generate
        }
        match test with 
            | StreamData s ->
                printfn "Generated data %A" s
            | _ -> failwith "Not generated"
        ()


[<EntryPoint>]
let main _ =
    try
        WebReqModule.testIt
        0 // return an integer exit code
    with
    | ex -> 
        printfn "Failed: %s" ex.Message
        // Return -ve exit code
        -1
    

