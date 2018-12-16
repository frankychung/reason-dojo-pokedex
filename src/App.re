type id = int;

type state =
  | List
  | Single(id);

type action =
  | ShowList
  | ShowSingle(id);

let component = ReasonReact.reducerComponent("App");

let fonts = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'";

let style =
  ReactDOMRe.Style.make(
    ~fontFamily=fonts,
    ~marginTop="24px",
    ~margin="auto",
    ~width="800px",
    (),
  );

let make = _children => {
  ...component,
  initialState: () => {
    switch (ReasonReact.Router.dangerouslyGetInitialUrl().path) {
    | [id] => Single(int_of_string(id))
    | _ => List
    };
  },
  reducer: (action, _state) => {
    switch (action) {
    | ShowList => ReasonReact.Update(List)
    | ShowSingle(id) => ReasonReact.Update(Single(id))
    };
  },
  didMount: self => {
    let watcherID =
      ReasonReact.Router.watchUrl(url =>
        switch (url.path) {
        | [id] => self.send(ShowSingle(int_of_string(id)))
        | _ => self.send(ShowList)
        }
      );
    self.onUnmount(() => ReasonReact.Router.unwatchUrl(watcherID));
  },
  render: self => {
    let content =
      switch (self.state) {
      | List => <Pokemons />
      | Single(id) => <Pokemon id />
      };
    <div style> <div> {ReasonReact.string("Pokedex!")} </div> content </div>;
  },
};
