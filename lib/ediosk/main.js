var ui = require("ediosk/edi-ui");
var emacs = require("ediosk/emacs-talker");
var tabs = require("tabs");

function openUI() {
  var talker = new emacs.EmacsTalker("http://127.0.0.1:8080/");

  var pageContent = "<div id='root'></div>";
  tabs.open({
    url: "data:text/html," + pageContent,
    onOpen: function(tab) {
      // since it's a data URL it should already be fully loaded...
      manifest(tab.contentDocument, talker);
    }
  });
}

function manifest(doc, talker) {
  var emitter = ui.wy.wrapElement(doc.getElementById("root"));
  var container = emitter.emit({type: "talker", obj: talker});
};

exports.main = function(options, callbacks) {
  openUI();
};
