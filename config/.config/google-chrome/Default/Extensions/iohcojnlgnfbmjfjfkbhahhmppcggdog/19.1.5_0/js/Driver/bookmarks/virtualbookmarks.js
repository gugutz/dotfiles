/**
 * virtual bookmarks interface
 * it imlements some chrome.bookmarks methods:
 * - removeTree
 * - create
 * - udpate
 * - move
 */

var virtualBookmarks = {
  _tree: null,
  _needPersist: false,
  _roots: null,
  // indices
  _itemsById: {},
  init: function(roots) {
    this._roots = roots;
    var self = this;
    try {
      this._tree = JSON.parse(localStorage.virtual_bookmarks);
    }
    catch(ex) {
      this._tree = [];
    }
    // check all roots should be found in tree
    roots.forEach(function(rootId) {
      var found = false;
      self._tree.forEach(function(item) {
        if(item.global_id == rootId) {
          found = true;
        }
      });
      if(!found) {
        self._tree.push({
          global_id: rootId,
          parent_id: null,
          title: rootId,
          children: []
        });
      }
    });
    // build indices
    self._itemsById = {};
    this._walkEach(function(item) {
      self._itemsById[item.global_id] = item;
    });
    // persistency interval
    setInterval(function() {
      if(self._needPersist) {
        self._needPersist = false;
        localStorage.virtual_bookmarks = JSON.stringify(self._tree);
      }
    }, 1000);
  },

  _schedulePersist: function() {
    this._needPersist = true;
  },

  _walkChildren: function(arr, cb) {
    for(var i = 0; i != arr.length; i++) {
      var item = arr[i];
      cb(item);
      if(item.children) {
        this._walkChildren(item.children, cb);
      }
    }
  },

  // walk through each item in tree
  _walkEach: function(cb) {
    this._walkChildren(this._tree, cb);
  },

  _removeFromIndices: function(item) {
    delete this._itemsById[item.global_id];
  },

  _addToIndices: function(item) {
    this._itemsById[item.global_id] = item;
  },

  removeTree: function(id) {
    var item = this._itemsById[id];
    var self = this;
    if(!item) {
      return;
    }
    var parent = this._itemsById[item.parent_id];
    if(!parent) {
      return;
    }
    var index = parent.children.indexOf(item);
    if(index >= 0) {
      parent.children.splice(index, 1);
    }
    this._removeFromIndices(item);
    if(item.children) {
      // remove children from index
      this._walkChildren(item.children, function(c) {
        self._removeFromIndices(c);
      });
    }
    this._schedulePersist();
  },
  /**
   * @param {Object} item {
   *   {String} global_id
   *   {String} parent_id
   *   {String} title
   *   {String} url
   * } 
   */
  create: function(item) {
    var parent = this._itemsById[item.parent_id];
    if(!parent || !parent.children) {
      return;
    }
    if(!item.url) {
      item.children = [];
    }
    parent.children.push(item);
    this._addToIndices(item);
    this._schedulePersist();
  },
  /**
   * @param {Object} details {
   *   {String} title
   *   {String} url
   * } 
   */
  update: function(id, details) {
    var item = this._itemsById[id];
    if(!item) {
      return;
    }
    for(var k in details) {
      item[k] = details[k];
    }
    this._schedulePersist();
  },
  /**
   * @param {Object} destination {
   *   {String} [parent_id] - global_id
   *   {Number} [index]
   * } 
   */
  move: function(id, destination) {
    var item = this._itemsById[id];
    if(!item) {
      return;
    }
    destination.index = destination.index || 0;
    // get parents
    var oldParent = this._itemsById[item.parent_id];
    var newParent = null;
    if(destination.parent_id) {
      newParent = this._itemsById[destination.parent_id];
      if(!newParent) {
        return;
      }
    }
    else {
      newParent = oldParent;
    }
    // remove from old parent
    var index = oldParent.children.indexOf(item);
    if(index < 0) {
      console.log(item, oldParent);
      throw new Error("Virtual bookmarks database corrupted");
    }
    oldParent.children.splice(index, 1);
    newParent.children.splice(destination.index, 0, item);
    item.parent_id = newParent.global_id;
    this._schedulePersist();
  },
  getSubTree: function(id) {
    return this._itemsById[id];
  },
  // create/update/move in one method
  save: function(item) {
    if(this._itemsById[item.global_id]) {
      this.move(item.global_id, {
        parent_id: item.parent_id,
        index: item.index
      });
      this.update(item.global_id, item);
    }
    else {
      this.create(item);
    }
  },
  // remove all bookmarks, and reinit roots
  empty: function() {
    delete localStorage.virtual_bookmarks;
    this.init(this._roots);
  },
  removeNotInList: function(ids) {
    var self = this;
    var idsHash = {};
    ids.forEach(function(id) {
      idsHash[id] = true;
    });
    Object.keys(this._itemsById).forEach(function(id) {
      if(!idsHash[id]) {
        self.removeTree(id);
      }
    });
  },
  getChildrenFlat: function(global_id) {
    if(!this._itemsById[global_id] || !this._itemsById[global_id].children) {
      return [];
    }
    var children = [];
    this._walkChildren(this._itemsById[global_id].children, function(c) {
      children.push(c);
    });
    return children;
  }
};

virtualBookmarks.init([
  "unsorted"
]);