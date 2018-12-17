open Belt;

module Query = [%graphql
  {|
query getPokemon($id: Int) {
  pokemon: PokemonSpecies(filter: {id: $id} ){
    edges {
      node {
        pokemons {
          id
          englishName
          sprites {
            normal {
              male {
                front
              }
            }
          }
        }
      }
    }
  }
}
|}
];

type pokemon = {
  id: int,
  englishName: string,
};

let filter = (id, pokemon) => {
  let id2 = Option.map([%get_in pokemon#??id], int_of_string);
  Option.eq(Some(id), id2, (==));
};

let decode = (id, response) => {
  let edges = [%get_in response##pokemon#??edges];
  let pokemons =
    switch (edges) {
    | Some(edges) =>
      edges
      ->Array.map(edge => {
          let pokemons = [%get_in edge#??node#??pokemons];
          switch (pokemons) {
          | Some(pokemons) =>
            let pokemons = Array.keep(pokemons, filter(id));
            switch (pokemons) {
            | [|pokemon|] =>
              let pid = Option.map([%get_in pokemon#??id], int_of_string);
              let englishName = [%get_in pokemon#??englishName];
              pid->Option.flatMap(id =>
                englishName->Option.flatMap(englishName =>
                  Some({id, englishName})
                )
              );
            | _ => None
            };
          | None => None
          };
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
           switch (decode(id, response)) {
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
