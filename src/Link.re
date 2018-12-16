let component = ReasonReact.statelessComponent("Link");

let make = (~to_, children) => {
  ...component,
  render: _self => {
    <a
      style={ReactDOMRe.Style.make(~cursor="pointer", ~borderBottom="1px solid black", ())}
      onClick={_ => ReasonReact.Router.push(to_)}>
      children
    </a>;
  },
};
