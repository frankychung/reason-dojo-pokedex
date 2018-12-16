open Belt;

module Query = [%graphql
  {|
query getPokemon($id: Int) {
  pokemon: Pokemon(filter: {id: $id}) {
    edges {
      node {
        id
        englishName
        identifier
      }
    }
  }
}
|}
];

type pokemon = {
  identifier: string,
  id: int,
  englishName: string,
};

let decode = response => {
  let edges = [%get_in response##pokemon#??edges];
  let pokemons =
    switch (edges) {
    | Some(edges) =>
      edges
      ->Array.map(edge => {
          let identifier = [%get_in edge#??node#??identifier];
          let id = [%get_in edge#??node#??id];
          let englishName = [%get_in edge#??node#??englishName];
          identifier->Option.flatMap(identifier =>
            id->Option.flatMap(id =>
              englishName->Option.flatMap(englishName =>
                Some({identifier, id: int_of_string(id), englishName})
              )
            )
          );
        })
      ->List.fromArray
      ->List.keep(Option.isSome)
      ->List.map(Option.getExn)
    | None => []
    };
  switch (pokemons) {
  | [pokemon] => Result.Ok(pokemon)
  | _ => Result.Error("bad data")
  };
};

type state =
  | Loading
  | Error(string)
  | Data(pokemon);

type action =
  | LoadData(pokemon)
  | SetError(string);

let component = ReasonReact.reducerComponent("Pokemon");

let make = (~id, _children) => {
  ...component,
  initialState: () => Loading,
  reducer: (action, _state) => {
    switch (action) {
    | LoadData(pokemon) => ReasonReact.Update(Data(pokemon))
    | SetError(msg) => ReasonReact.Update(Error(msg))
    };
  },
  didMount: self => {
    Query.make(~id, ())
    |> Api.sendQuery
    |> Js.Promise.then_(r => {
         switch (r) {
         | Result.Ok(response) =>
           switch (decode(response)) {
           | Result.Ok(data) => self.send(LoadData(data))
           | Result.Error(error) => self.send(SetError(error))
           }
         | Result.Error(err) => self.send(SetError(err))
         };
         Js.Promise.resolve();
       })
    |> ignore;
  },
  render: self => {
    switch (self.state) {
    | Loading => <div> {ReasonReact.string("Loading...")} </div>
    | Error(msg) => <div> {ReasonReact.string(msg)} </div>
    | Data(pokemon) =>
      <div>
        {ReasonReact.string(pokemon.englishName)}
        <div> <Link to_="/"> ...{ReasonReact.string("Back")} </Link> </div>
      </div>
    };
  },
};
