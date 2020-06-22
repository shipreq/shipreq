'use strict';

const Root = document.getElementById('root');

async function logClipboardContent() {

  // const perms = await navigator.permissions.query({name: "clipboard-read"});
  // console.log("Perms: ", perms);
  // if (perms.state == "granted" || perms.state == "prompt") {
  //   const data = await navigator.clipboard.read();
  //   console.log("navigator.clipboard.read: ", data)
  // }

  const data = await clipboard.read();
  console.log("clipboard.read: ", data)
  // for (let i=0; i<data.items.length; i++) {
  //   console.log(`  data.items[${i}].type = ${data.items[i].type}`)
  // }
}

class Comp extends React.Component {

  onClick() {
    logClipboardContent()
  }

  render() {
    return React.createElement("button", {
      onClick: this.onClick,
      style: {fontSize: "200%", padding: "4em"},
    }, "Read clipboard and log to console")
  }
}

ReactDOM.render(React.createElement(Comp, null), Root)
