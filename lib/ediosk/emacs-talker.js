var request = require("request");
var model = require("ediosk/edi-model");

/**
 * Speaks a somewhat ad-hoc JSON and gets-with-side-effects dialect with a web
 *  server embedded in the emacs instance.
 */
function EmacsTalker(aBaseURL) {
  this.baseUrl = aBaseURL;
  this.editor = new model.TextEditor();
  this.listener = null;

  this.pendingBuffers = this.pendingFrames = false;
}
exports.EmacsTalker = EmacsTalker;
EmacsTalker.prototype = {
  toString: function() {
    return "[EmacsTalker:" + this.baseUrl + "]";
  },
  selectViewport: function(aViewport) {
    var dis = this;
    var req = request.Request({
      url: this.baseUrl + "select-window",
      content: {
        window: aViewport.id,
      },
      onComplete: function() {
        dis.update();
      },
    });
    req.get();
  },
  showBufferInViewport: function(aBuffer, aViewport) {
    var dis = this;
    var req = request.Request({
      url: this.baseUrl + "show-buffer-in-window",
      content: {
        buffer: aBuffer.name,
        window: aViewport.id,
      },
      onComplete: function() {
        dis.update();
      },
    });
    req.get();
  },
  update: function() {
    this.pendingBuffers = this.pendingFrames = true;
    var dis = this;

    this._reqBuffers = request.Request({
      url: this.baseUrl + "list-buffers-json",
      onComplete: function() {
        dis._gotBuffers();
      },
    });
    this._reqFrames = request.Request({
      url: this.baseUrl + "list-frames-json",
      onComplete: function() {
        dis._gotFrames();
      },
    });

    this._reqBuffers.get();
    this._reqFrames.get();
  },
  _gotBuffers: function() {
    this.pendingBuffers = false;
    if (!this.pendingFrames)
      this._updated();
  },
  _gotFrames: function() {
    this.pendingFrames = false;
    if (!this.pendingBuffers)
      this._updated();
  },
  _updated: function() {
    var editor = this.editor;
    var buffers = this._reqBuffers.response.json.buffers, obufs = {};
    var addbufs = [], modbufs = [], delbufs = [];
    for (var iBuf = 0; iBuf < buffers.length; iBuf++) {
      var dBuf = buffers[iBuf], eBuf;

      if (dBuf.name in editor.buffersByName) {
        eBuf = editor.buffersByName[dBuf.name];
        if (eBuf.filename != dBuf.filename ||
            eBuf.mode != dBuf.mode) {
          eBuf.filename = dBuf.filename;
          eBuf.mode = dBuf.mode;
          modbufs.push(eBuf);
        }
      }
      else {
        eBuf = new model.Buffer(dBuf.name, dBuf.filename, dBuf.mode);
        addbufs.push(eBuf);
      }
      obufs[dBuf.name] = eBuf;
    }
    // check for deleted buffers...
    for (var bufName in editor.buffersByName) {
      if (!(bufName in obufs)) {
        delbufs.push(editor.buffersByName[obufs]);
      }
    }
    editor.buffersByName = obufs;

    var frames = this._reqFrames.response.json.frames, oclusters = {},
        oports = {};
    var addclusters = [], delclusters = [], addports = [], delports = [];
    for (var iFrame = 0; iFrame < frames.length; iFrame++) {
      var frame = frames[iFrame];
      var cluster;

      if (frame.id in editor.clustersById) {
        cluster = editor.clustersById[frame.id];
      }
      else {
        cluster = new model.ViewportCluster(frame.id);
        addclusters.push(cluster);
      }
      cluster.left = frame.left;
      cluster.top = frame.top;
      cluster.width = frame.width;
      cluster.height = frame.height;
      oclusters[frame.id] = cluster;


      var windows = frame.windows, localports = [];
      for (var iWin = 0; iWin < windows.length; iWin++) {
        var win = windows[iWin];
        var viewport;

        if (win.id in editor.viewportsById) {
          viewport = editor.viewportsById[win.id];
        }
        else {
          viewport = new model.Viewport(win.id);
          addports.push(viewport);
        }
        viewport.parent = cluster;
        viewport.edges = win.pixelEdges;
        oports[win.id] = viewport;
        localports.push(viewport);
      }
      cluster.viewports = localports;
    }
    // check for deleted clusters
    for (var clustId in editor.clustersById) {
      if (!(clustId in oclusters))
        delclusters.push(editor.clustersById[clustId]);
    }
    editor.clustersById = oclusters;

    // check for deleted viewports
    for (var portId in editor.viewportsById) {
      if (!(portId in oports))
        delports.push(editor.viewportsById[portId]);
    }
    editor.viewportsById = oports;

    if (this.listener) {
      this.listener.editorUpdated({
        buffersAdded: addbufs,
        buffersModified: modbufs,
        buffersRemoved: delbufs,
        clustersAdded: addclusters,
        clustersRemoved: delclusters,
        viewportsAdded: addports,
        viewportsRemoved: delports,
      });
    }
  },
};
