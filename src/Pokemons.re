open Belt;

module Query = [%graphql
  {|
query getPokemons($first: Int, $after: String, $search: String) {
  pokemon: Pokemon(first:$first, after:$after, filter: {identifierLike: $search}) {
    totalCount
    pageInfo {
      endCursor
    }
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

type data = {
  totalCount: int,
  endCursor: string,
  pokemons: list(pokemon),
};

type state =
  | Loading
  | Error(string)
  | Data(data);

type action =
  | LoadData(data)
  | SetError(string);

let component = ReasonReact.reducerComponent("PokemonList");

let decode = response => {
  let totalCount = [%get_in response##pokemon#??totalCount];
  let endCursor = [%get_in response##pokemon#?pageInfo#??endCursor];
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
  totalCount
  ->Option.flatMap(totalCount =>
      endCursor->Option.flatMap(endCursor =>
        Some({totalCount, endCursor, pokemons})
      )
    )
  ->(
      fun
      | Some(data) => Result.Ok(data)
      | None => Result.Error("bad data")
    );
};

let loadMore = (endCursor, self) => {
    Js.log(endCursor);
    Query.make(~after=endCursor, ())
      -> Api.sendQuery
      |> Js.Promise.then_(
        result => {
          switch result {
            | Result.Ok(response) => self.ReasonReact.send(SetError("Ok"))
            | Result.Error(error) => self.ReasonReact.send(SetError(error))
          }
          Js.Promise.resolve()
        }
      )
      |> ignore
};
  
let make = _children => {
  ...component,
  initialState: () => Loading,
  reducer: (action, _state) => {
    switch (action) {
    | LoadData(data) => ReasonReact.Update(Data(data))
    | SetError(msg) => ReasonReact.Update(Error(msg))
    };
  },
  didMount: self => {
    Query.make(~first=50, ())
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
  render: self =>
    switch (self.state) {
    | Loading => <div> {ReasonReact.string("Loading...")} </div>
    | Error(msg) => <div> {ReasonReact.string(msg)} </div>
    | Data(data) =>
      let pokemons =
        data.pokemons
        ->List.map(pokemon =>
            <div key={string_of_int(pokemon.id)}>
              <Link to_={"/" ++ pokemon.identifier}>
                ...{ReasonReact.string(pokemon.englishName)}
              </Link>
            </div>
          )
        ->List.toArray;
      <div>
        <div> {ReasonReact.array(pokemons)} </div>
        <button onClick={_ => self.handle(loadMore, data.endCursor)}> {ReasonReact.string("Load More")}</button>
      </div>;
    },
};
