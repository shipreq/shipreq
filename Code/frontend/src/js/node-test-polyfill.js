if (typeof(document.execCommand) === "undefined") {
  document.execCommand = () => {}
}

window.alert = (a) => {console.log(`[window.alert] ${JSON.stringify(a)}`)}

require("es6-symbol/implement")
