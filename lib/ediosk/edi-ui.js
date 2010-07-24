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
    // visualizes the editor's state...
    editor: wy.widget({type: "editor"}, "editor"),
    // the groups are populated dynamically by the categorizer that we hook up
    groups: wy.horizList({type: "buffer-group"}, wy.NONE),
  },
  // let our kids know about our protovis instance
  provideContext: {
    pv: "pv",
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
      this.update();
    },
  },
  receive: {
    selectViewport: function(aViewport) {
      console.log("selecting viewport", aViewport);
      this.selectedViewport = aViewport;
      this.obj.selectViewport(aViewport);
    },
    selectBuffer: function(aBuffer) {
      this.obj.showBufferInViewport(aBuffer, this.selectedViewport);
    },
  },
  style: {
    root: [
      "font-family: sans-serif;",
    ],
  }
});

/**
 * Visualize the layout of the editor windows...
 */
wy.defineWidget({
  name: "root-editor",
  constraint: {
    type: "editor",
  },
  structure: {
  },
  emit: ["selectViewport"],
  impl: {
    preInit: function() {
      // the root provides us with a protovis for this document
      var pv = this.pv = this.__context.pv;

      var WIDTH = this.WIDTH = 320,
          HEIGHT = this.HEIGHT = 240;

      var vis = this.vis = new pv.Panel()
        .margin(2)
        .width(WIDTH)
        .height(HEIGHT)
        .canvas(this.domNode);

      // Scale screen coordinates down to our output coordinates.  Domain/range
      //  are computed in our update function.
      var xScale = this.xScale = pv.Scale.linear();
      var yScale = this.yScale = pv.Scale.linear();

      var fillColors = pv.colors("#aec7e8", "#ffbb78", "#98df8a", "#ff9896",
                                 "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7",
                                 "#dbdb8d", "#9edae5");

      var dis = this;
      var boxVis = this.boxVis = vis.add(pv.Bar)
        .data([]) // dummy out until we get the update
        .left(function (d) {
           return xScale(d.edges[0] + d.parent.left);
          })
        .top(function (d) {
           return yScale(d.edges[1] + d.parent.top);
          })
        .width(function (d) {
           return xScale(d.edges[2]) - xScale(d.edges[0]);
          })
        .height(function (d) {
           return yScale(d.edges[3]) - yScale(d.edges[1]);
          })
        .fillStyle(fillColors.by(function () {
                                   return this.index;
                                 }))
        .strokeStyle(pv.color("black"))
        .lineWidth(function (d) {
            return d.selected ? 4 : 1;
          })
        .event("click", function(viewport) {
            dis.emit_selectViewport(viewport);
          });
    },
    update: function() {
      this.__update();
      var viewports = this.obj.viewports;
      if (!viewports.length)
        return;
      console.log("viewports", viewports);
      // We need to use min/max explicitly because domain() uses 'blah
      //  instanceof Array' to test for Array-ness which does not work with
      //  sandboxes; each sandbox gets its own Array instance so code messing
      //  with Array.prototype does not affect other sandboxes.
      this.xScale.domain(
        this.pv.min(viewports, function(d) {
          return d.edges[0] + d.parent.left;
        }),
        this.pv.max(viewports, function(d) {
          return d.edges[2] + d.parent.left;
        }));
      this.xScale.range(0, this.WIDTH-1);
      this.yScale.domain(
        this.pv.min(viewports, function(d) {
          return d.edges[1] + d.parent.top;
        }),
        this.pv.max(viewports, function(d) {
          return d.edges[3] + d.parent.top;
        }));
      this.yScale.range(0, this.HEIGHT-1);
      this.boxVis.data(viewports);
      this.vis.render();
    }
  },
});


////////////////////////////////////////////////////////////////////////////////
//// Buffer Groups

wy.defineWidget({
  name: "buffer-group",
  doc: "Buffer group container; can collapse.",
  focus: wy.focus.container.vertical("buffers"),
  constraint: {
    type: "buffer-group",
  },
  structure: {
    label: wy.bind("name"),
    subLabel: wy.bind("subName"),
    buffers: wy.vertList({type: "buffer"}, "buffers"),
  },
  events: {
    label: {
      command: function() {
        if (this.domNode.hasAttribute("collapsed"))
          this.domNode.removeAttribute("collapsed");
        else
          this.domNode.setAttribute("collapsed", "");
      }
    }
  },
  style: {
    root: {
      _: [
        "display: inline-block;",
        "float: left;",
        "padding: 4px;",
        "border: 1px solid black;",
        "margin: 4px;",
        "min-width: 6em;",
      ],
      "[collapsed]": {
        label: [
          "color: gray;",
        ],
        buffers: [
          "display: none;"
        ]
      }
    },
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

