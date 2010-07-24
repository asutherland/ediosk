/**
 * Load protovis into a sandbox that has the globals it expects and expose all
 *  this via CommonJS.  This is based on our code from the jetpack prototype to
 *  do this for protovis which was heavily derived from the Jetpack prototype's
 *  jQuery sandboxy thing.
 * We do this at all because we want to have protovis operating in the same
 *  security context as us while not caring what security context the document
 *  is operating in.  Honestly, I'd be fine with the page/document having
 *  chrome privileges but I want it to be a firefox tab and it really likes to
 *  make the browser have content privileges.
 **/

var sm = require("securable-module");
var self = require("self");

/**
 * Create a new protovis sandbox instance that loves the given document.
 */
exports.gimmeAProtovisForDoc = function(aDoc) {
  var loader = new sm.Loader({
    // no loading!
    rootPaths: [],
    defaultPrincipal: "system",
    globals: {
      document: aDoc,
      window: aDoc.defaultView,
      exports: {},
    }
  });
  var sourceCode = self.data.load("protovis-3.3.js");
  // and make sure to export pv to us...
  sourceCode += "pv;";
  var pv = loader.runScript({
    filename: "protovis-3.3.js",
    contents: sourceCode,
  });
  console.log("pv", pv);
  return pv;
};
