let sendQuery = q =>
  Fetch.(
    fetchWithInit(
      "https://pokeql.com/v1",
      RequestInit.make(
        ~method_=Post,
        ~body=
          Js.Dict.fromList([
            ("query", Js.Json.string(q##query)),
            ("variables", q##variables),
          ])
          |> Js.Json.object_
          |> Js.Json.stringify
          |> BodyInit.make,
        ~headers=
          HeadersInit.makeWithArray([|
            ("content-type", "application/json"),
          |]),
        (),
      ),
    )
    |> Js.Promise.then_(resp =>
         if (Response.ok(resp)) {
           Response.json(resp)
           |> Js.Promise.then_(data =>
                switch (Js.Json.decodeObject(data)) {
                | Some(obj) =>
                  let parse = q##parse;
                  Js.Dict.unsafeGet(obj, "data")
                  ->parse
                  ->Belt.Result.Ok
                  ->Js.Promise.resolve;
                | None =>
                  Belt.Result.Error("Response is not an object")
                  ->Js.Promise.resolve
                }
              );
         } else {
           Belt.Result.Error("Request failed: " ++ Response.statusText(resp))
           ->Js.Promise.resolve;
         }
       )
  );
