var bookmarksListeners = {
  enabled: true,
  _needPrepareRemoved: false,
  _prepareRemovedInterval: null,
  _nowRemovedBookmarksPreparing: false,
  _prepareRemovedIntervalStart: function() {
    var self = this;
    if(this._prepareRemovedInterval) {
      return;
    }
    setInterval( function(){
      if( self._needPrepareRemoved && !fvdSynchronizer.Driver.Bookmarks.isSyncNow() ){
        self._needPrepareRemoved = false;
        self._prepareRemovedBookmarks(function(){
        });
      }
    }, 500 );
  },
  _setRemovedBookmarksPreparingNowState: function(state) {
    console.log("Prepare removed state ", state);
    this._nowRemovedBookmarksPreparing = state;
    fvdSynchronizer.Observer.fireEvent( "prepareRemovedBookmarksStateChanged" );
  },
  /*
   * Because chrome fires only one event if folder with bookmarks removed recursively
   * we need way to check which bookmarks are removed to preserve clean list of id - globalId associations
   * and to remove these bookmarks from server
   */
  _prepareRemovedBookmarks: function( callback ) {
    var self = this;
    this._setRemovedBookmarksPreparingNowState(true);
    var totalRemovedCount = 0,
        bookmarksIdsHash = {};
    fvdSynchronizer.Utils.Async.chain([
      function(next) {
        // make bookmarks hash
        bookmarksManager.getAllBookmarks(true, function(ids) {
          ids.forEach(function(id) {
            bookmarksIdsHash[id.id] = true;
          });
          console.log("hash", bookmarksIdsHash);
          next();
        });
      },
      function() {
        var ids = bookmarksDatabase.getAllIds();
        ids.forEach( function(id) {
          if( !bookmarksIdsHash[id] ) {
            var globalId = bookmarksManager.getGlobalId(id);
            if( globalId ){
              if( !bookmarksManager.isRootFolder( globalId ) ) {
                totalRemovedCount++;
                fvdSynchronizer.Driver.Bookmarks.syncData( [ "removedIds" ], globalId, false );
              }
            }
            bookmarksDatabase.removeGuid(id);
          }
        });
        self._setRemovedBookmarksPreparingNowState( false );
        console.log("removed count ", totalRemovedCount);
        fvdSynchronizer.Server.Sync.syncStateRefresh();
        if( callback ){
          callback();
        }
      }
    ]);
  },
  enable: function() {
    this.enabled = true;
  },
  disable: function() {
    this.enabled = false;
  },
  init: function() {
    this._prepareRemovedIntervalStart();
    var self = this;
    chrome.bookmarks.onChanged.addListener(function( id ){
      if(!self.enabled) {
        return;
      }
      fvdSynchronizer.Utils.Async.chain( [
        function() {
          fvdSynchronizer.Driver.Bookmarks.Backup.setHasChanges();
          var globalId = bookmarksManager.getGlobalId(id);
          if(bookmarksManager.isRootFolder(globalId)) {
            return;
          }
          if(globalId) {
            fvdSynchronizer.Driver.Bookmarks.syncData( [ "changedIds" ], globalId );
          }
        }
      ] );
    });

    chrome.bookmarks.onCreated.addListener(function( id ) {
      if(!self.enabled) {
        return;
      }
      fvdSynchronizer.Utils.Async.chain( [
        function() {
          fvdSynchronizer.Driver.Bookmarks.Backup.setHasChanges();
          var globalId = bookmarksDatabase.setGuid(id, null);
          if( globalId ){
            fvdSynchronizer.Driver.Bookmarks.syncData( [ "changedIds", "newIds" ], globalId );
          }
        }

      ] );

    });

    chrome.bookmarks.onMoved.addListener(function( id, moveInfo ){
      if(!self.enabled) {
        return;
      }
      fvdSynchronizer.Utils.Async.chain( [
        function(){
          fvdSynchronizer.Driver.Bookmarks.Backup.setHasChanges();
          var globalId = bookmarksManager.getGlobalId(id);
          if( bookmarksManager.isRootFolder( globalId ) ){
            return;
          }
          if( globalId ){
            fvdSynchronizer.Driver.Bookmarks.syncData( [ "reorderInside" ], moveInfo.parentId );
            if(moveInfo.oldParentId != moveInfo.parentId) {
              fvdSynchronizer.Driver.Bookmarks.syncData( [ "reorderInside" ], moveInfo.oldParentId );
              fvdSynchronizer.Driver.Bookmarks.syncData( [ "changedIds" ], globalId );
            }
          }
        }

      ] );

    });

    chrome.bookmarks.onRemoved.addListener(function( id ){
      if(!self.enabled) {
        return;
      }
      fvdSynchronizer.Driver.Bookmarks.Backup.setHasChanges();
      self._needPrepareRemoved = true;
    });

  }
};