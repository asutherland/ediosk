
function BufferGroup(aId, aName, aSubName) {
  this.id = aId;
  this.name = aName;
  this.subName = aSubName;
  this.buffers = [];
}
BufferGroup.prototype = {

};

/**
 * Category strategy path.
 */
function CatStratPath(aAttrName) {
  this.attr = aAttrName;
}
CatStratPath.prototype = {
  chew: function(aBuffer) {
    var path = aBuffer[this.attr];
    // this is either things like dired or implementation details like *buffa*
    if (path == null) {
      // nuke *blah* into oblivion.
      if (aBuffer.name[0] == "*" || aBuffer.name[0] == " ")
        return [null, "Blah", "emacs junk"];
      console.log("name", aBuffer.name, "single char", aBuffer.name[0]);
      return ["*nofile*", "(other)", "no backing file"];
    }
    var idxLastSlash = path.lastIndexOf("/");
    if (idxLastSlash == -1)
      return ["/", "Wha?", "weird"];
    var dir = path.substring(0, idxLastSlash);
    var filename = path.substring(idxLastSlash + 1);
    idxLastSlash = dir.lastIndexOf("/");
    var lastdir, remainder;
    if (idxLastSlash >= 0) {
      lastdir = dir.substring(idxLastSlash + 1);
      remainder = dir.substring(0, idxLastSlash);
    }
    else {
      lastdir = dir;
      remainder = "";
    }
    remainder = remainder.replace("/home/", "~");
    return [dir, lastdir, remainder];
  }
};

/**
 * Cluster/categorize the buffers into something useful.  Right now all
 *  categorization happens on the basis of filename.
 */
function Categorizer() {
  this.groups = [];
  this.groupsById = {};
  this.strategy = new CatStratPath("filename");
}
exports.Categorizer = Categorizer;
Categorizer.prototype = {
  chewAdds: function(buffers) {
    for (var iBuf = 0; iBuf < buffers.length; iBuf++) {
      var buffer = buffers[iBuf];

      var tupey = this.strategy.chew(buffer);
      var groupId = tupey[0], groupName = tupey[1], groupSubName = tupey[2];
      // null means throw the buffer away
      if (groupId == null)
        continue;

      var group;
      if (groupId in this.groupsById) {
        group = this.groupsById[groupId];
      }
      else {
        group = new BufferGroup(groupId, groupName, groupSubName);
        this.groups.push(group);
        this.groupsById[groupId] = group;
      }
      group.buffers.push(buffer);
    }
  },
  chewRemoves: function(buffers) {
    for (var iBuf = 0; iBuf < buffers.length; iBuf++) {
      var buffer = buffers[iBuf];

      var tupey = this.strategy.chew(buffer);
      var groupId = tupey[0], groupName = tupey[1], groupSubName = tupey[2];
      // null means throw the buffer away
      if (groupId == null)
        continue;

      var group;
      group = this.groupsById[groupId];
      group.buffers.splice(group.buffers.indexOf(buffer), 1);
      if (group.buffers.length == 0) {
        delete this.groupsById[groupId];
        this.groups.splice(this.groups.indexOf(group), 1);
      }
    }
  },
};
