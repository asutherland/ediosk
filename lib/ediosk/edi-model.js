
/**
 * Our data-model.  Because emacs has some odd terminology we attempt to define
 *  very specific class names that avoid sounding like the emacs terms while
 *  also being somewhat intuitive.
 **/

/**
 * Capture the essence of a text-editor buffer.  emacs calls them buffers too.
 */
function Buffer(aTextEditor, aName, aFilename, aMode) {
  this._editor = aTextEditor;
  this.name = aName;
  this.filename = aFilename;
  this.mode = aMode;
}
exports.Buffer = Buffer;
Buffer.prototype = {
  get selectedIn() {
    var selectedIn = null;
    for (var viewportId in this._editor.viewportsById) {
      var viewport = this._editor.viewportsById[viewportId];
      if (viewport.bufferName == this.name) {
        if (!selectedIn)
          selectedIn = [];
        selectedIn.push(viewport);
      }
    }
    return selectedIn;
  }
};

/**
 * Viewports show buffers.  emacs calls them windows, a web browser would call
 *  them frames.
 */
function Viewport(aId) {
  this.id = aId;
  this.edges = [null, null, null, null];
}
exports.Viewport = Viewport;
Viewport.prototype = {

};

/**
 * ViewportClusters contain one or more viewports (which show buffers).  A UI
 *  would call this a window, emacs would call this a frame.
 */
function ViewportCluster(aId) {
  this.is = aId;
  this.left = this.top = this.width = this.height = null;
  this.viewports = [];
}
exports.ViewportCluster = ViewportCluster;
ViewportCluster.prototype = {

};

/**
 * The text editor has a list of buffers and viewport clusters.
 */
function TextEditor() {
  this.buffersByName = {};
  this.clustersById = {};
  this.viewportsById = {};
}
TextEditor.prototype = {
  get viewports() {
    var ports = [];
    for (var key in this.viewportsById) {
      ports.push(this.viewportsById[key]);
    }
    return ports;
  }
};
exports.TextEditor = TextEditor;
