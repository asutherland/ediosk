var request = require("request");
var model = require("ediosk/edi-model");

/**
 * Allocate consecutive id's that can be deallocated and then later reused.
 *  Used for distinctive but stable coloring without assuming anything about
 *  emacs.
 */
function StickyIdMaker() {
  this.highId = 0;
  this.spare = [];
}
StickyIdMaker.prototype = {
  alloc: function() {
    if (this.spare.length)
      return this.spare.pop();
    return this.highId++;
  },
  dealloc: function(val) {
    this.spare.push(val);
  }
};

function emacsTimeToEpochSecs(aEmacsTime) {
  if (!aEmacsTime)
    return null;
  return aEmacsTime[0] * 65536 + aEmacsTime[1];
}

/**
 * Speaks a somewhat ad-hoc JSON and gets-with-side-effects dialect with a web
 *  server embedded in the emacs instance.
 */
function EmacsTalker(aBaseURL) {
  this.baseUrl = aBaseURL;
  this.editor = new model.TextEditor();
  this.listener = null;

  this.pendingBuffers = this.pendingFrames = false;

  this.viewportStickyIdMaker = new StickyIdMaker();
}
exports.EmacsTalker = EmacsTalker;
EmacsTalker.prototype = {
  toString: function() {
    return "[EmacsTalker:" + this.baseUrl + "]";
  },
  /**
   * Make the given viewport the active one.
   */
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
  /**
   * Show the given buffer in the given viewport.
   */
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
    // -- buffers
    var bufferResponse = this._reqBuffers.response.json;

    editor.lastNow = emacsTimeToEpochSecs(bufferResponse.now);

    var buffers = bufferResponse.buffers, obufs = {};
    var addbufs = [], modbufs = [], delbufs = [];
    for (var iBuf = 0; iBuf < buffers.length; iBuf++) {
      var dBuf = buffers[iBuf], eBuf;

      // exists?
      if (dBuf.name in editor.buffersByName) {
        eBuf = editor.buffersByName[dBuf.name];
        // delta check and update
        if (eBuf.filename != dBuf.filename ||
            eBuf.mode != dBuf.mode ||
            eBuf.modified != Boolean(dBuf.modified) ||
            eBuf.displayTime != emacsTimeToEpochSecs(dBuf.displayTime)) {
          eBuf.filename = dBuf.filename;
          eBuf.mode = dBuf.mode;
          eBuf.modified = Boolean(dBuf.modified);
          eBuf.displayTime = emacsTimeToEpochSecs(dBuf.displayTime);
          modbufs.push(eBuf);
        }
      }
      // new!
      else {
        eBuf = new model.Buffer(this.editor,
                                dBuf.name, dBuf.filename, dBuf.mode);
        eBuf.modified = Boolean(dBuf.modified);
        eBuf.displayTime = emacsTimeToEpochSecs(dBuf.displayTime);
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
    // -- clusters/frames
    for (var iFrame = 0; iFrame < frames.length; iFrame++) {
      var frame = frames[iFrame];
      var cluster;

      // exists?
      if (frame.id in editor.clustersById) {
        cluster = editor.clustersById[frame.id];
      }
      // new!
      else {
        cluster = new model.ViewportCluster(frame.id);
        addclusters.push(cluster);
      }
      cluster.left = frame.left;
      cluster.top = frame.top;
      cluster.width = frame.width;
      cluster.height = frame.height;
      cluster.selected = frame.selected;
      oclusters[frame.id] = cluster;

      // - viewports/windows
      var windows = frame.windows, localports = [];
      for (var iWin = 0; iWin < windows.length; iWin++) {
        var win = windows[iWin];
        var viewport;

        if (win.id in editor.viewportsById) {
          viewport = editor.viewportsById[win.id];
        }
        else {
          viewport = new model.Viewport(win.id);
          viewport.stickyId = this.viewportStickyIdMaker.alloc();
          addports.push(viewport);
        }
        viewport.parent = cluster;
        viewport.edges = win.pixelEdges;
        viewport.bufferName = win.buffer;
        viewport.selected = win.selected;
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
      if (!(portId in oports)) {
        var delport = editor.viewportsById[portId];
        this.viewportStickyIdMaker.dealloc(delport.stickyId);
        delports.push(editor.viewportsById[portId]);
      }
    }
    editor.viewportsById = oports;

    // -- notify listeners
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
