var ui = require("ediosk/edi-ui");
var emacs = require("ediosk/emacs-talker");
var tabs = require("tabs");
var self = require("self");
var protovis = require("protovis");


function openUI() {
  var talker = new emacs.EmacsTalker("http://127.0.0.1:8080/");

  var pageContent = "<html><head><title>ediosk</title>" +
                      "<body><div id='root'></div></body></html>";

  tabs.open({
    url: "data:text/html," + pageContent,
    onOpen: function(tab) {
      manifest(tab.contentDocument, talker);
    }
  });
}

function manifest(doc, talker) {
  console.log("manifesting...");
  var emitter = ui.wy.wrapElement(doc.getElementById("root"));
  talker.pv = protovis.gimmeAProtovisForDoc(doc);
  var container = emitter.emit({type: "talker", obj: talker});
};

exports.main = function(options, callbacks) {
  openUI();
};
