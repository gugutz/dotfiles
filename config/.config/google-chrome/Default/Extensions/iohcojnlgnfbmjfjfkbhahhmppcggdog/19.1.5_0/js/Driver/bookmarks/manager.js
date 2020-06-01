// visible globally
CHROME_MENU_ID = null;
CHROME_TOOLBAR_ID = null;
CHROME_UNSORTED_ID = null; // for opera only

// proxy for chrome bookmarks api, to count write requests
var chromeBookmarks = new function(){
  var LOG_MINUTES_COUNT = 10;
  var self = this;

  var actions = [
    "removeTree",
    "create",
    "update",
    "move",
    "getTree",
    "getSubTree"
  ];

  actions.forEach(function( action ){

    self[action] = function(){
      var sentArguments = arguments;
      chrome.bookmarks[action].apply( chrome.bookmarks, sentArguments );
    };

  });

}();

var BookmarksManager = function() {

  var self = this;

  function createGuidsForAll( callback, progressCallback ){

    bookmarksManager.getAllBookmarks( true, function( bookmarks ){
      var processedCount = 0;
      bookmarks.forEach( function(bookmark) {
        var guid = bookmarksDatabase.getGuid(bookmark.id);
        processedCount++;
        if( progressCallback ) {
          progressCallback(processedCount, bookmarks.length);
        }
        if(!guid){
          bookmarksDatabase.setGuid(bookmark.id, null);
        }
      });
      callback();
    } );

  }

  function getGlobalId(id){
    if( id == CHROME_TOOLBAR_ID ){
      return "toolbar";
    }
    if( id == CHROME_MENU_ID ){
      return "menu";
    }
    return bookmarksDatabase.getGuid(id);
  }

  function refreshBookmarksRootIds(){

    chrome.bookmarks.getChildren("0", function(children) {
      if(children.length >= 2) {
        if(children[1].title.toLowerCase() == "bookmarks bar") {
          CHROME_MENU_ID = children[0].id;
          CHROME_TOOLBAR_ID = children[1].id;
        }
        else {
          CHROME_MENU_ID = children[1].id;
          CHROME_TOOLBAR_ID = children[0].id;
        }
      }
    });

  }

  function getId( guid, params ) {
    if( guid == "toolbar" ){
      return CHROME_TOOLBAR_ID;
    }
    if( guid == "menu" ){
      return CHROME_MENU_ID;
    }
    return bookmarksDatabase.getId(guid);
  }

  function remove( id, callback, params ){

    params = params || {};

    var via = getInterface( params.via );

    bookmarkExists(id, function( exists ){
      fvdSynchronizer.Utils.Async.chain([
        function( chainCallback ){
          if( exists ){
            bookmarksListeners.disable();
            via.removeTree( id, function(){
              bookmarksListeners.enable();
              chainCallback();
            } );
          }
          else{
            chainCallback();
          }
        },
        function(){
          bookmarksDatabase.removeGuid(id);
          callback();
        }
      ]);
    });
  }

  function prepareBookmarksTree( tree, onlyIds, callback ){

    var result = [];
    if( onlyIds ) {
      result.push({
        id: tree.id
      });
    }
    else{
      var data = {
        id: tree.id,
        title: tree.title,
        index: tree.index
      };

      if( tree.url ){
        data.type = "bookmark";
        data.url = tree.url;
      }
      else{
        data.type = "folder";
      }
      data.date = tree.dateAdded;
      var guid = getGlobalId(tree.id);
      data.global_id = guid;
      data.parent_id = getGlobalId(tree.parentId);
      result.push( data );
    }
    fvdSynchronizer.Utils.Async.chain( [

      function( chainCallback ){

        if( !tree.children || tree.children.length === 0 ){
          chainCallback();
        }
        else{

          fvdSynchronizer.Utils.Async.arrayProcess( tree.children, function( subTree, arrayProcessCallback ){

            prepareBookmarksTree( subTree, onlyIds, function( subResult ){

              result = result.concat( subResult );
              arrayProcessCallback();

            } );

          }, function(){
            chainCallback();
          } );

        }

      },

      function(){
        callback( result );
      }

    ] );

  }

  function bookmarkExists( id, callback ){
    if( !id ){
      return callback( false );
    }
    chrome.bookmarks.get( id, function( bookmarks ){
      if( bookmarks && bookmarks.length > 0 ){
        callback( true, bookmarks[0] );
      }
      else{
        callback( false );
      }
    } );
  }


  function countChildren( id, callback ){

    if( !id ){
      return callback( 0 );
    }

    chrome.bookmarks.getChildren( id.toString(), function( sub ){
      if( !sub ){
        console.log( "Fail get childrent count for ", id );
        return callback( 0 );
      }

      callback( sub.length );
    } );

  }

  function getActiveIndex( activeIndex, parentId, callback ){

    countChildren( parentId, function( count ){

      if( activeIndex >= count ){
        activeIndex = count - 1;
      }

      if( activeIndex < 0 ){
        activeIndex = 0;
      }

      callback( activeIndex );

    } );

  }

  function getInterface( via ){
    return chromeBookmarks;
  }

  this.getGlobalId = getGlobalId;

  this.isRootFolder = function( globalId ){

    if( bookmarksDatabase.systemGuids().indexOf( globalId ) != -1 ){
      return true;
    }

    return false;

  };

  this.changeGlobalId = function( oldGuid, newGuid){
    var id = getId(oldGuid);
    if( id ){
      bookmarksDatabase.setGuid(id, newGuid);
    }
  };

  this.getAllParents = function( globalId, callback ){

    var parents = [];

    fvdSynchronizer.Utils.Async.cc( function( ccCallback ){

      self.getParentId( globalId, function( _globalId ){

        if( !_globalId ){
          // maybe error?
          console.log( "ASSERTION: GlobalId of parent is Null! Check this situation!" );
          return callback( parents );
        }

        if( self.isRootFolder(_globalId) ){
          callback( parents );
          return;
        }

        parents.push( _globalId );
        globalId = _globalId;

        ccCallback();

      } );


    } );


  };

  this.getParentId = function( globalId, callback ){
    this.getBookmarkById( globalId, function( bookmark ){
      if( !bookmark ){
        callback( null );
      }
      else{
        callback( bookmark.parent_id );
      }
    } );
  };

  this.getRootParentIdByBookmarkId = function(id, cb) {
    var currId = id;
    fvdSynchronizer.Utils.Async.cc(function(next) {
      chrome.bookmarks.get(currId, function(res) {
        if(!res.length) {
          return cb(null);
        }
        var b = res[0];
        if(!b.parentId || b.parentId === "0") {
          // is root
          return cb(getGlobalId(b.id));
        }
        currId = b.parentId;
        next();
      });
    });
  };

  this.getAllGuids = function(){
    return bookmarksDatabase.getAllGuids();
  };

  this.removeAllBookmarks = function( callback, params, progressCallback ){
    var countRemoved = 0;
    this.getAllBookmarks( false, function( bookmarks ){
      fvdSynchronizer.Utils.Async.arrayProcess( bookmarks, function( bookmark, arrayProcessCallback ){
        countRemoved++;
        if( progressCallback ){
          progressCallback( countRemoved, bookmarks.length );
        }
        self.remove( bookmark.global_id, arrayProcessCallback, params, bookmark.id );
      }, function(){
        callback();
      } );
    } );
  };

  this.getGlobalId = getGlobalId;

  this.getFolderContents = function( guid, callback ){
    var id = getId(guid);
    if( !id ) {
      callback( [] );
    }
    else {
      chrome.bookmarks.getSubTree( id, function( tree ){
        if(!tree.length){
          callback( [] );
        }
        else{
          prepareBookmarksTree( tree[0], false, function( r ){
            r.splice(0,1);
            callback( r );
          } );
        }
      } );
    }
  };

  this.getMaxId = function( callback ){

    var maxId = 0;

    function processTree( node ){

      var id = parseInt(node.id);

      if( id > maxId ){
        maxId = id;
      }

      if( node.children && node.children.length > 0 ){

        for( var i = 0; i != node.children.length; i++ ){
          processTree( node.children[i] );
        }

      }

    }

    chromeBookmarks.getTree( function( tree ){
      processTree( tree[0] );
      callback( maxId );
    } );

  };

  this.getAllBookmarks = function( onlyIds, callback ){

    if( typeof onlyIds == "function" ){
      callback = onlyIds;
      onlyIds = false;
    }

    chrome.bookmarks.getTree( function( tree ){

      var result = [];

      tree = tree[0];

      fvdSynchronizer.Utils.Async.arrayProcess( tree.children, function( rootDir, arrayProcessCallback ){
        var rootGuid = getGlobalId(rootDir.id);
        if(!self.isRootFolder(rootGuid)) {
          return arrayProcessCallback();
        }
        prepareBookmarksTree( rootDir, onlyIds, function( r ){
          r.splice(0,1);
          result = result.concat( r );
          arrayProcessCallback();
        } );
      }, function(){

        // remove system guids

        var removeItems = [];

        result.forEach( function( item ){

          if( self.isRootFolder( item.global_id ) ){
            removeItems.push( item );
          }

        } );

        removeItems.forEach( function( item ){

          var index = result.indexOf( item );
          if( index != -1 ){
            result.splice( index, 1 );
          }

        } );
        callback( result );

      } );

    } );

  };

  this.getBookmarkById = function( globalId, callback ){

    var id = null;

    fvdSynchronizer.Utils.Async.chain( [

      function( chainCallback ){
        var _id = getId(globalId);
        if( !_id ){
          callback( null );
          return;
        }
        id = _id;
        chainCallback();
      },

      function(){

        chrome.bookmarks.get( id, function( bookmark ){

          if( !bookmark || bookmark.length == 0 ){
            callback( null );
            return;
          }

          bookmark = bookmark[0];

          prepareBookmarksTree( bookmark, false, function( preparedBookmarks ){

            if( !preparedBookmarks || preparedBookmarks.length == 0 ){
              callback( null );
            }
            else{
              var bmk = preparedBookmarks[0];
              callback( bmk );
            }

          } );

        } );



      }

    ] );

  };

  this.remove = function( globalId, callback, params, bookmarkId ){

    fvdSynchronizer.Utils.Async.chain([

      function( chainCallback ){

        if( bookmarkId ){
          chainCallback();
        }
        else{
          var id = getId(globalId);
          if( !id ){
            callback();
            return;
          }
          bookmarkId = id;
          chainCallback();
        }

      },

      function(){
        remove( bookmarkId, callback, params );
      }

    ]);


  };

  this.save = function( bookmark, callback, params ){

    params = params || {};

    var _origCallback = callback;
    _log.profile.start("save");
    callback = function(){
      _log.profile.end("save");
      _origCallback.apply(window, arguments);
    };

    var via = getInterface( params.via );

    var parentId = null;
    var id = null;
    var exists = null;
    var currentBookmark = null; // current bookmark stored, if exists
    var newBookmark = null;

    if( bookmark.url ){

      if( !fvdSynchronizer.Utils.containsAllAscii(bookmark.url) ){
        // need prepare url with punycode
        bookmark.url = new URI(bookmark.url).normalizeHostname().toString();//"http://wrong-url.yyy";
      }

    }

    fvdSynchronizer.Utils.Async.chain( [

      function( chainCallback ) {
        _log.profile.start("save:getId");
        var _parentId = getId(bookmark.parent_id, params);
        if(_parentId === null) {
          // fail to resolve parent, maybe is unsorted bookmark
          // or DB is corrupted
          // ignore
          if(virtualBookmarks.getSubTree(bookmark.parent_id) || virtualBookmarks.getSubTree(bookmark.global_id)) {
            // bookmark can be saved in virtual bookmarks
            virtualBookmarks.save(bookmark);
          }
          return callback(null);
        }
        _log.profile.end("save:getId");
        parentId = _parentId;
        _log.profile.start("save:getId");
        var _id = getId(bookmark.global_id, params);
        _log.profile.end("save:getId");
        id = _id;
        chainCallback();
      },

      function( chainCallback ){
        if( !id ){
          exists = false;
          chainCallback();
        }
        else{
          _log.profile.start("save:bookmarkExists");
          bookmarkExists( id, function( _exists, _currentBookmark ){
            _log.profile.end("save:bookmarkExists");
            exists = _exists;
            currentBookmark = _currentBookmark;
            chainCallback();
          } );
        }
      },

      function(chainCallback) {
        // getting parent
        countChildren(parentId, function(countChildren) {
          var maxIndex = countChildren;
          // check is bookmark.index note more than max index
          if(bookmark.index > maxIndex) {
            bookmark.index = maxIndex;
          }
          if(bookmark.index < 0) {
            bookmark.index = 0;
          }
          chainCallback();
        });
      },

      function( chainCallback ){
        if( id && exists ) {

          fvdSynchronizer.Utils.Async.chain([

            function( chainCallback2 ) {
              var needUpdate = false;
              var fields = {};
              if(bookmark.type == "folder") {
                if( currentBookmark.title != bookmark.title ) {
                  needUpdate = true;
                  fields = {
                    title: bookmark.title
                  };
                }
              }
              else if(bookmark.type == "bookmark") {
                if( !fvdSynchronizer.Utils.isIdenticalUrls( currentBookmark.url, bookmark.url ) ||
                    currentBookmark.title != bookmark.title ) {
                  needUpdate = true;
                  fields = {
                    title: bookmark.title,
                    url: bookmark.url
                  };
                }
              }

              if(needUpdate) {
                _log.profile.start("save:update");
                via.update( id, fields, function(){
                  _log.profile.end("save:update");
                  chainCallback2();
                } );
              }
              else {
                chainCallback2();
              }

            },

            function( chainCallback2 ){

              var activeIndex = bookmark.index;
              if( activeIndex == currentBookmark.index && parentId == currentBookmark.parentId ){
                return chainCallback2();
              }
              _log.profile.start("save:move");
              via.move( id, {
                parentId: parentId,
                index: activeIndex
              }, function( r ){
                _log.profile.end("save:move");
                if( !r ){
                  return callback(null, false);
                }

                chainCallback2();

              } );

            },

            function(){
              chainCallback();
            }

          ]);

        }
        else{
          // try to save
          var saveFields = {};
          if( bookmark.type == "folder" ){
            saveFields = {
              parentId: parentId,
              index: bookmark.index,
              title: bookmark.title
            };
          }
          else if( bookmark.type == "bookmark" ){
            saveFields = {
              parentId: parentId,
              index: bookmark.index,
              title: bookmark.title,
              url: bookmark.url
            };
          }
          fvdSynchronizer.Utils.Async.chain([
            function( chainCallback2 ){
              _log.profile.start("save:create");
              via.create(saveFields, function( _newBookmark ){
                _log.profile.end("save:create");
                if( _newBookmark ){
                  bookmarksDatabase.setGuid(_newBookmark.id, bookmark.global_id);
                  newBookmark = _newBookmark;
                }
                chainCallback2();
              });
            },

            function() {
              chainCallback();
            }

          ]);


        }

      },

      function(){

        if( newBookmark ){
          id = newBookmark.id;
        }

        var result = false;

        if( exists ){
          result = true;
        }
        else if( newBookmark ){
          result = true;
        }

        callback( id, result );
      }

    ] );


  };

  this.bookmarkExistsByRealId = function( id, callback ){
    return bookmarkExists( id, callback );
  };


  this.saveList = function( bookmarks, callback, params, oneItemSaveCallback ){
    params = params || {};
    var folders = {};
    // really stored global ids
    var globalIdsStored = {};
    // items that really stored
    var itemsStored = [];
    // items that not stored in db
    var itemsNotStored = [];
    /*
    // add extra bookmarks to save(for example if moving from unsorted folder)
    function _addToSaveList(bs) {
      for(var i = 0; i != bs.length; i++) {
        var b = bs[i];
        if(!global_ids[b.global_id]) {
          global_ids[b.global_id] = true;
          bookmarks.push(b);
        }
      }
    }
    */
    function _getDepth(b) {
      if(folders[b.parent_id]) {
        return 1 + _getDepth(folders[b.parent_id]);
      }
      return 0;
    }
    bookmarks.forEach( function( bookmark, index ){
      if( bookmark.type == "folder" ){
        folders[bookmark.global_id] = bookmark;
      }
    } );
    bookmarks.forEach( function( bookmark, index ){
      bookmark._depth = _getDepth(bookmark);
    } );
    bookmarks.sort(function(b1, b2) {
      var a = b1._depth, b = b2._depth;
      if(b1._depth == b2._depth) {
        a = b1.index;
        b = b2.index;
      }
      return a-b;
    });

    function getParent( bookmark ){
      if(!bookmark.parent_id){
        return null;
      }
      if(folders[bookmark.parent_id]) {
        return folders[bookmark.parent_id];
      }
      return null;
    }

    var countSaved = 0;

    function save( bookmark, callback, params ) {
      fvdSynchronizer.Utils.Async.chain([

        function( chainCallback ){
          var parent = getParent( bookmark );
          if( parent ){
            save( parent, function(){
              chainCallback();
            } );
          }
          else{
            chainCallback();
          }
        },

        function() {
          self.save( bookmark, function( id, result ){
            if(folders[bookmark.global_id]) {
              delete folders[bookmark.global_id];
            }
            countSaved++;
            if( oneItemSaveCallback ){
              oneItemSaveCallback();
            }
            callback( id, result );
          }, params );
        }

      ]);

    }

    fvdSynchronizer.Utils.Async.chain( [
      function( chainCallback ){
        bookmarksListeners.disable();
        fvdSynchronizer.Utils.Async.arrayProcess( bookmarks, function( bookmark, arrayProcessCallback ){
          save( bookmark, function( id, result ){
            if(id && result) {
              itemsStored.push(bookmark);
              globalIdsStored[bookmark.global_id] = true;
            }
            else {
              itemsNotStored.push(bookmark);
            }
            arrayProcessCallback();
          }, params );
        }, function(){
          bookmarksListeners.enable();
          chainCallback();
        } );
      },
      function(chainCallback) {
        var needStoreAdditional = [];
        itemsStored.forEach(function(bookmark) {
          if(virtualBookmarks.getSubTree(bookmark.global_id)) {
            if(bookmark.type == "folder") {
              // get all children
              var children = virtualBookmarks.getChildrenFlat(bookmark.global_id);
              for(var i = 0; i != children.length; i++) {
                var b = children[i];
                if(!globalIdsStored[b.global_id]) {
                  needStoreAdditional.push(b);
                }
              }
            }
            virtualBookmarks.removeTree(bookmark.global_id);
          }
        });
        if(!needStoreAdditional.length) {
          console.log("NOthing additional to store");
          return chainCallback();
        }
        console.log("!!!Store additional: ", needStoreAdditional);
        self.saveList(needStoreAdditional, function() {
          chainCallback();
        });
      },
      function(chainCallback) {
        console.log("Items not stored is: ", itemsNotStored);
        if(!itemsNotStored.length) {
          return chainCallback();
        }
        // maybe need to move from main tree to virtual tree, check it
        fvdSynchronizer.Utils.Async.arrayProcess(itemsNotStored, function(bookmark, apNext) {
          // if parent of this bookmark in virtual tree, need to move bookmark to it
          // and remove from main tree
          if(!virtualBookmarks.getSubTree(bookmark.parent_id)) {
            return apNext();
          }
          var id = getId(bookmark.global_id);
          if(!id) {
            return apNext();
          }
          var bookmarksToSave = [bookmark];
          fvdSynchronizer.Utils.Async.chain([
            function(next) {
              if(bookmark.type != "folder") {
                return next();
              }
              // get all children from main tree
              self.getFolderContents(bookmark.global_id, function(subs) {
                for(var i = 0; i != subs.length; i++) {
                  bookmarksToSave.push(subs[i]);
                }
                next();
              });
            },
            function(next) {
              console.log("SAVEEE TO UNSORTED", bookmarksToSave);
              for(var i = 0; i != bookmarksToSave.length; i++) {
                virtualBookmarks.save(bookmarksToSave[i]);
              }
              next();
            },
            function() {
              self.remove(bookmark.global_id, function() {
                apNext();
              });
            }
          ]);
        }, function() {
          chainCallback();
        });
      },
      function(){
        callback( true );
      }
    ] );
  };

  this.createGuidsForAll = function( callback, progressCallback ){
    createGuidsForAll( callback, progressCallback );
  };

  this.setListeners = function(){
    bookmarksListeners.init();
  };

  refreshBookmarksRootIds();

};



var bookmarksManager = new BookmarksManager();