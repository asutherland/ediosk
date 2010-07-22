var wmsy = require("wmsy/wmsy");
var wy = exports.wy = new wmsy.WmsyDomain({id: "edi-ui",
                                           domain: "ediosk",
                                           clickToFocus: true});
var Categorizer = require("ediosk/categorizer").Categorizer;

////////////////////////////////////////////////////////////////////////////////
//// UI Top Level

wy.defineWidget({
  name: "root-talker",
  focus: wy.focus.domain.vertical("groups"),
  constraint: {
    type: "talker",
  },
  structure: {
    editor: wy.widget({type: "editor"}, "editor"),
    groups: wy.horizList({type: "buffer-group"}, wy.NONE),
  },
  impl: {
    preInit: function() {
      this.categorizer = new Categorizer();
      this.obj.listener = this;
      this.obj.update();
    },
    editorUpdated: function(what) {
      this.categorizer.chewAdds(what.buffersAdded);
      this.categorizer.chewRemoves(what.buffersRemoved);
      this.groups_set(this.categorizer.groups);
    },
  },
  receive: {
    selectBuffer: function(aBuffer) {
      this.obj.selectBuffer(aBuffer);
    },
  },
  style: {
    root: [
      "font-family: sans-serif;",
    ],
  }
});

wy.defineWidget({
  name: "root-editor",
  constraint: {
    type: "editor",
  },
  structure: {
  },
});


////////////////////////////////////////////////////////////////////////////////
//// Buffer Groups

wy.defineWidget({
  name: "buffer-group",
  focus: wy.focus.container.vertical("buffers"),
  constraint: {
    type: "buffer-group",
  },
  structure: {
    label: wy.bind("name"),
    subLabel: wy.bind("subName"),
    buffers: wy.vertList({type: "buffer"}, "buffers"),
  },
  style: {
    root: [
      "display: inline-block;",
      "float: left;",
      "padding: 4px;",
      "border: 1px solid black;",
      "margin: 4px;",
      "min-width: 6em;",
    ],
    label: [
      "display: block;",
      "font-size: 2em;",
      "font-weight: bold;",
    ],
    subLabel: [
      "display: block;",
      "font-size: 0.5em;",
      "color: gray;",
    ],
  }
});


////////////////////////////////////////////////////////////////////////////////
//// Buffers

wy.defineWidget({
  name: "buffer",
  focus: wy.focus.item,
  constraint: {
    type: "buffer",
  },
  emit: ["selectBuffer"],
  structure: {
    name: wy.bind("name"),
  },
  events: {
    root: {
      click: function() {
        this.emit_selectBuffer(this.obj);
      }
    }
  },
  style: {
    root: [
    ],
    name: [
      "font-size: 1.5em;",
    ],
  }
});

