type identifier = string;

type state =
  | List
  | Single(identifier);

type action =
  | ShowList
  | ShowSingle(identifier);

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
    | [identifier] => Single(identifier)
    | _ => List
    };
  },
  reducer: (action, _state) => {
    switch (action) {
    | ShowList => ReasonReact.Update(List)
    | ShowSingle(identifier) => ReasonReact.Update(Single(identifier))
    };
  },
  didMount: self => {
    let watcherID =
      ReasonReact.Router.watchUrl(url =>
        switch (url.path) {
        | [identifier] => self.send(ShowSingle(identifier))
        | _ => self.send(ShowList)
        }
      );
    self.onUnmount(() => ReasonReact.Router.unwatchUrl(watcherID));
  },
  render: self => {
    let content =
      switch (self.state) {
      | List => <Pokemons />
      | Single(identifier) => <Pokemon identifier />
      };
    <div style> <div> {ReasonReact.string("Pokedex!")} </div> content </div>;
  },
};
