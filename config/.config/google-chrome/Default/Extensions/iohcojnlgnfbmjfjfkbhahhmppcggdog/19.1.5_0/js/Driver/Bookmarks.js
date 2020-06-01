(function(){

  if (window == chrome.extension.getBackgroundPage()) {
    /***********************************/
    /** Main Bookmarks Driver Interface*/

    var Bookmarks = function(){
      var initialActionsProcess = false;
      var currentProcessAborted = false;
      var syncNow = false;
      var initialActionsProgress = {
        total: 0,
        current: 0
      };
      var mainSyncState = "nosync";
      var mainSyncProgress = {};
      var changeMainSyncStateListeners = [];
      var self = this;
      this.ignoreSyncProgress = false; // ignore sync stats displaying
      this.__defineGetter__("lastSyncLog", function(){
        return _log;
      });
      this.__defineSetter__("lastSyncLog", function(v){
        console.info("Set last sync log");
        _log = v;
      });

      window.addEventListener( "load", function(){

        initialActionsProcess = true;

        bookmarksDatabase.connect( function() {

          initialActions(function(){
            initialActionsProcess = false;

            fvdSynchronizer.Observer.fireEvent( "bookmarksInitializationCompleted" );

            bookmarksManager.setListeners();
          });

        } );


      }, false );

      var _initToSyncData = {
        removedIds: [],
        newIds: [],
        changedIds: []
      };

      function initialActions( callback ){

        console.log("Start initial process");

        fvdSynchronizer.Utils.Async.chain( [

          function( chainCallback ){
            // check integrity
            // remove from db not exists bookmarks guids
            console.log("1. Check intergrity");
            var ids = bookmarksDatabase.getAllIds();
            var _processed = 0;
            if(!ids.length){
              return chainCallback();
            }
            function _end(){
              if( _processed == ids.length ){
                chainCallback();
              }
            }
            ids.forEach(function( id ){
              bookmarksManager.bookmarkExistsByRealId( id, function( exists ){
                _processed++;
                if( !exists ){
                  bookmarksDatabase.removeGuid(id);
                }
                _end();
              } );
            });
          },

          function( chainCallback ){

            console.log("2. Create guids if not created");

            initialActionsProgress.current = 0;
            initialActionsProgress.total = 0;

            bookmarksManager.createGuidsForAll( function(){

              console.log("Create guids for all finished!");

              chainCallback();
            }, function( current, total ){

              initialActionsProgress.current = current;
              initialActionsProgress.total = total;

            } );

          },

          function(){

            console.log("Initial process finished");

            callback();
          }

        ] );

      }

      function saveListIfFailTryPlugin( bookmarks, callback, params ){

        params = params || {};

        var cloneBookmarks = fvdSynchronizer.Utils.objectClone( bookmarks );

        bookmarksManager.saveList( bookmarks, function( result ){
          callback( result );
        }, params );

      }


      function startSync(){
        syncNow = true;

        fvdSynchronizer.Observer.fireEvent( "syncStart", ["Bookmarks"] );

        fvdSynchronizer.Server.Sync.syncStateRefresh();
      }

      function endSync(){
        syncNow = false;

        fvdSynchronizer.Observer.fireEvent( "syncEnd", ["Bookmarks"] );

        fvdSynchronizer.Server.Sync.syncStateRefresh();
      }

      /* abort current process */

      function checkCurrentProcessAborted( callback ){

        if( currentProcessAborted ){
          callback( fvdSynchronizer.Errors.ERROR_SYNC_USER_ABORTED );
          return true;
        }

        return false;

      }

      /* transactions */

      function startTransaction( callback ){

        callback();

      }

      function commitTransaction( transId, callback ){

        callback();

      }

      function rollbackTransaction( transId, callback ){

        bookmarksDatabase.rollbackMassGuidChange();
        callback();

      }

      /* requests to server */

      // getting list of bookmarks

      function normalizeServerBookmark( bookmark ){
        if( bookmark.type == "bookmark" ){
          if( !bookmark.url ){
            bookmark.url = "http://nourl";
          }
          else{
            if(bookmark.url.indexOf( "javascript:" ) !== -1){
              if( bookmark.url.indexOf( "http://javascript:" ) === 0 ){
                  bookmark.url = String(bookmark.url).replace('http://', '');
              }                    
            }else{
              if( bookmark.url.indexOf( "[" ) != -1 || bookmark.url.indexOf( "]" ) != -1 ){
                // square brackets only allowed if used in host(for ipv6 addresses)
                var removeBrackets = false,
                    match1 = bookmark.url.match(/\[/g),
                    match2 = bookmark.url.match(/\]/g);
                if( match1 && match1.length != 1 || match2 && match2.length != 1 ){
                  removeBrackets = true;
                }
                else if( bookmark.url.indexOf( "http://[" ) == -1 ){
                  removeBrackets = true;
                }

                if( removeBrackets ){

                  //bookmark.url = bookmark.url.replace( /\[/g, "" )
                  //               .replace( /\]/g, "" );

                  bookmark.url = "http://incorrecturl";

                }

              }
            }
          }
        }
      }

      function getBookmarksList( params, callback ){

        var message = params;
        message.action = "list_bookmarks";

        makeRequest( message, function( error, response ){

          if( error != 0 ){
            callback( error );
          }
          else{

            // order bookmarks by index
            response.bookmarks.sort( function( a, b ){
              return  a.index - b.index;
            } );

            // need prepare bookmarks

            response.bookmarks.forEach( function( bookmark ){

              normalizeServerBookmark( bookmark );

            } );

            callback( error, response );

          }

        } );

      }

      function putBookmarksList( bookmarks, callback, additional ){

        var message = {
          action: "put_bookmarks",
          bookmarks: bookmarks
        };

        additional = additional || {};

        for( var k in additional ){
          message[k] = additional[k];
        }

        makeRequest( message, callback );

      }

      function removeAllServerData( callback ){
        // remove only menu and toolbar bookmarks, because chrome doesn't works with unsorted folder
        var message = {
          action: "remove_bookmarks",
          bookmarkIds: [
            "menu",
            "toolbar"
          ]
        };

        makeRequest( message, callback );
      }

      function removeBookmarksFromServer( ids, callback ){

        var message = {
          action: "remove_bookmarks",
          bookmarkIds: ids
        };

        makeRequest( message, callback );

      }

      function hasUpdates( callback ){

        var lastUpdateTime = getLastUpdateTime();

        var message = {
          action: "found_bookmarks",
          lastUpdateTime: lastUpdateTime
        };



        makeRequest( message, function( error, data ){

          if( error != 0 ){
            callback( error );
          }
          else{
            callback( 0, {
              found: data.found,
              lastUpdateTime: lastUpdateTime
            } );
          }

        } );

      }

      /**
       * upload changed bookmarks/folders to server
       * @param {Array} [exclude] - array of guids to ignore uploading
       */
      function uploadChangesToServer( params, callback ){
        if(typeof params == "function") {
          callback = params;
          params = {};
        }
        console.log("upload params", params);
        params = params || {};
        params.exclude = params.exclude || [];
        var changeData = getToSyncData();
        if(params.exclude && params.exclude.length) {
          params.exclude.forEach(function(guid) {
            for(var k in changeData) {
              var d = changeData[k],
                  index = d.indexOf(guid);
              if(index >= 0) {
                d.splice(index, 1);
              }
            }
          });
        }
        fvdSynchronizer.Utils.Async.chain( [
          function( chainCallback ){
            // first remove data
            if( changeData.removedIds.length > 0 ){
              removeBookmarksFromServer( changeData.removedIds, function( error, data ){
                if( error != 0 ){
                  callback( error );
                }
                else{
                  setLastUpdateTime( data.lastUpdateTime );
                  chainCallback();
                }
              } );
            }
            else{
              chainCallback();
            }
          },

          function( chainCallback ){
            //upload data
            var toServerList = [];
            // store here which type of data we need upload to server
            // such as "full" or "position", key is bookmark global_id
            var uploadChanges = {};
            changeData.changedIds.forEach(function(globalId) {
              uploadChanges[globalId] = "full";
            });
            fvdSynchronizer.Utils.Async.chain([
              function(next) {
                if(changeData.reorderInside && changeData.reorderInside.length) {
                  // need to get index info about bookmarks in folders which ids in reorderInside
                  // array
                  fvdSynchronizer.Utils.Async.arrayProcess(changeData.reorderInside, function(id, apc) {
                    chrome.bookmarks.getSubTree(id, function(bookmarks) {
                      if(!bookmarks || !bookmarks[0] || !bookmarks[0].children) {
                        return apc();
                      }
                      bookmarks[0].children.forEach(function(b) {
                        var global_id = bookmarksManager.getGlobalId(b.id);
                        // check if guid is ignored
                        if(params.exclude.indexOf(global_id) != -1) {
                          return;
                        }
                        if(!uploadChanges[global_id]) {
                          uploadChanges[global_id] = "index";
                        }
                      });
                      apc();
                    });
                  }, function() {
                    next();
                  });
                }
                else {
                  next();
                }
              },
              function() {
                var global_ids = Object.keys(uploadChanges);
                if( global_ids.length > 0 ){
                  fvdSynchronizer.Utils.Async.arrayProcess( global_ids, function( global_id, arrayProcessCallback ){
                    var saveInfo = uploadChanges[global_id];
                    bookmarksManager.getBookmarkById( global_id, function( bookmark ){
                      if( bookmark ){
                        if(saveInfo == "full") {
                          toServerList.push( bookmark );
                        }
                        else if(saveInfo == "index") {
                          toServerList.push({
                            global_id: bookmark.global_id,
                            index: bookmark.index
                          });
                        }
                      }
                      arrayProcessCallback();
                    } );
                  }, function(){
                    if( toServerList.length > 0 ){
                      putBookmarksList( toServerList, function( error, data ){
                        if(error){
                          callback( error, {
                            count: toServerList.length
                          } );
                        }
                        else{
                          setLastUpdateTime( data.lastUpdateTime );
                          chainCallback();
                        }
                      } );
                    }
                    else{
                      chainCallback();
                    }

                  } );
                }
                else{
                  chainCallback();
                }
              }
            ]);
          },

          function(){

            callback( 0 );

          }

        ] );

      }

      function startMainSync( callback ){
        var updateInfo = null,
        // array of guids
            storedBookmarksFromServer = [];

        //console.log( "Start main sync process-----------" );

        function emptyMainSyncProgress(){
          mainSyncProgress = JSON.parse( JSON.stringify({
          }) );
        }

        emptyMainSyncProgress();

        fvdSynchronizer.Utils.Async.chain([

          function( chainCallback ){

            // check need backup
            if( !_b(fvdSynchronizer.Prefs.get("bookmarks.archived")) ){

              mainSyncProgress.backuping = true;

              fvdSynchronizer.Driver.Bookmarks.Backup.make(function(){
                fvdSynchronizer.Prefs.set("bookmarks.archived", true);
                mainSyncProgress.backuping = false;
                chainCallback();
              });

            }
            else{
              chainCallback();
            }

          },

          function( chainCallback ){

            if( checkCurrentProcessAborted( callback ) ){
              return;
            }

            console.log( "1. Check For Updates\n" );

            hasUpdates( function( error, data ){

              if( error != 0 ){
                callback( error );
              }
              else{
                updateInfo = data;
                chainCallback();
              }

            } );

          },
          function( chainCallback ){
            if( checkCurrentProcessAborted( callback ) ){
              return;
            }
            console.log( "3. Apply server updates\n" );
            if( updateInfo.found ) {
              applyServerUpdates( {
                lastUpdateTime: updateInfo.lastUpdateTime
              }, function( error, res ){
                if( error != 0 ) {
                  callback( error );
                }
                else {
                  if(res && res.storedGuids) {
                    storedBookmarksFromServer = res.storedGuids;
                  }
                  chainCallback();
                }

              } );
            }
            else{
              chainCallback();
            }
          },
          function( chainCallback ){
            if( checkCurrentProcessAborted( callback ) ){
              return;
            }
            console.log( "2. Upload changes to server\n" );
            uploadChangesToServer( {
              exclude: storedBookmarksFromServer
            }, function( error, _additional ){
              if( error != 0 ){
                callback( error, _additional );
              }
              else{
                chainCallback();
              }
            } );
          },

          function(){


            clearToSyncData();

            callback( 0 );
          }

        ]);

      }

      function overwriteServerData( callback ){

        fvdSynchronizer.Utils.Async.chain( [

          function(chainCallback) {
            removeAllServerData(function(error) {
              if(error) {
                return callback(error);
              }
              chainCallback();
            });
          },

          function( chainCallback ){

            bookmarksManager.getAllBookmarks(function(clientBookmarks){

              putBookmarksList( clientBookmarks, function( error, data ){

                if( error != 0 ){
                  callback( error, {
                    count: clientBookmarks.length
                  } );
                }
                else{

                  setLastUpdateTime( data.lastUpdateTime );

                  chainCallback();
                }

              } );

            });

          },

          function(){
            clearToSyncData();

            callback( 0 );
          }

        ] );

      }

      function overwriteLocalData( callback ){

        var _origCallback = callback;
        var clientBookmarks = [];
        callback = function(){
          log.debug("End overwrite proccess");
          _origCallback.apply(window, arguments);
        };

        self.lastSyncLog = new Log();
        var log = self.lastSyncLog;
        log.debug("Start overwrite proccess");

        var interfaceParams = {
          via: "chrome"
        };

        currentProcessAborted = false;

        bookmarksDatabase.startMassGuidsChange();
        fvdSynchronizer.Utils.Async.chain( [

          function( chainCallback ){

            // get current bookmarks count to remove
            log.profile.start("bookmarksManager.getAllBookmarks");
            bookmarksManager.getAllBookmarks( null, function( bookmarks ){
              log.profile.end("bookmarksManager.getAllBookmarks");
              clientBookmarks = bookmarks;
              chainCallback();
            } );

          },

          function( chainCallback ){

            if( checkCurrentProcessAborted( callback ) ){
              return;
            }

            log.profile.start("getBookmarksList");
            getBookmarksList( {}, function( error, data ){
              log.profile.end("getBookmarksList");

              if( error ){
                callback( error );
              }
              else{

                fvdSynchronizer.Utils.Async.chain([
                  function(next) {
                    // need to remove bookmarks on client which not exists on server
                    var serverGuids = {};
                    var toRemove = [];
                    var i = 0;
                    if(data.bookmarks) {
                      for(i = 0; i != data.bookmarks.length; i++) {
                        serverGuids[data.bookmarks[i].global_id] = true;
                      }
                    }
                    for(i = 0; i != clientBookmarks.length; i++) {
                      var clentBookmark = clientBookmarks[i];
                      if(!serverGuids[clentBookmark.global_id]) {
                        toRemove.push(clentBookmark);
                      }
                    }
                    fvdSynchronizer.Utils.Async.arrayProcess(toRemove, function(clientBookmark, apNext) {
                      bookmarksManager.remove(clientBookmark.global_id, function() {
                        apNext();
                      }, {}, clientBookmark.id);
                    }, function() {
                      // cleanup guids
                      try {
                        var allGuids = BookmarksGuidsStorage.getAllGuids();
                        for( var i = 0; i != allGuids.length; i++ ){
                          if( allGuids[i] != "unsorted" ) {
                            if(!serverGuids[allGuids[i]]) {
                              BookmarksGuidsStorage.removeGuid( allGuids[i] );
                            }
                          }
                        }
                      }
                      catch(ex) {
                        console.error("Fail process guids remove", ex);
                      }
                      next();
                    });
                  },
                  function() {
                    virtualBookmarks.empty();
                    saveListIfFailTryPlugin( data.bookmarks, function( result, newInterfaceParams ){

                      if( !result ){
                        //self.displayChromeQuotaExceedMessage();
                        return callback( fvdSynchronizer.Errors.ERROR_CHROME_QUOTA_EXCEED );
                      }

                      try{
                        setLastUpdateTime( data.lastUpdateTime );
                      }
                      catch( ex ){

                      }

                      if( newInterfaceParams ){
                        interfaceParams.via = newInterfaceParams.via;
                      }

                      chainCallback();

                    }, interfaceParams );

                  }

                ]);

              }

            } );

          },

          function(){
            bookmarksDatabase.applyMassGuidsChange();
            callback( 0 );
          }

        ] );

      }

      function mergeLocalAndServerData( callback ){

        currentProcessAborted = false;

        var translateBookmarksGuids = {}; // if client change guids this object will contain [clientGuid] => [serverGuid] data

        var serverBookmarks = [];
        var clientBookmarks = [];
        var toServerBookmarks = [];
        var toClientBookmarks = [];
        var toClientBookmarksGlobalIds = {}; // assoc list for fast access

        // assoc list by parent and it children
        var parentsClientAssoc = {};
        var parentsServerAssoc = {};

        var serverLastUpdateTime = null;

        var foundCollisionsFor = {}; // global_ids that collised

        var storeToClientInterfaceParams = {
          via: "chrome"
        };

        var collisions = [];

        function getItemIn( globalId, list ){
          var index = -1;
          for( var i = 0; i != list.length; i++ ){

            if( list[i].global_id == globalId ){
              index = i;
              break;
            }

          }

          return index;
        }

        bookmarksDatabase.startMassGuidsChange();
        fvdSynchronizer.Utils.Async.chain( [

          function( chainCallback ){

            //console.log( "M1" );

            if( checkCurrentProcessAborted(callback) ){
              return;
            }

            // get server bookmarks

            getBookmarksList({}, function( error, data ){

              if( error == 0 ){
                serverBookmarks = data;

                serverLastUpdateTime = serverBookmarks.lastUpdateTime;
                serverBookmarks = serverBookmarks.bookmarks;

                for( var i = 0; i != serverBookmarks.length; i++ ){
                  var b = serverBookmarks[i];
                  if( b.parent_id ){
                    if( !parentsServerAssoc[ b.parent_id ] ){
                      parentsServerAssoc[ b.parent_id ] = [];
                    }

                    parentsServerAssoc[ b.parent_id ].push( b );
                  }
                }

                chainCallback();
              }
              else{
                callback( error );
              }

            });

          },

          function( chainCallback ){

            //console.log( "M2" );

            if( checkCurrentProcessAborted(callback) ){
              return;
            }

            // get client bookmarks

            bookmarksManager.getAllBookmarks( function( _bookmarks ){
              clientBookmarks = _bookmarks;



              for( var i = 0; i != clientBookmarks.length; i++ ){
                var b = clientBookmarks[i];
                if( b.parent_id ){
                  if( !parentsClientAssoc[ b.parent_id ] ){
                    parentsClientAssoc[ b.parent_id ] = [];
                  }

                  parentsClientAssoc[ b.parent_id ].push( b );
                }
              }

              chainCallback();
            } );

          },


          function( chainCallback ){

            console.log("Merge step: get collisions");

            //console.log( "M3" );

            if( checkCurrentProcessAborted(callback) ){
              return;
            }

            // first get global_id intersections
            for( var i = 0; i != clientBookmarks.length; i++ ){
              var index = -1;
              if( (index = getItemIn( clientBookmarks[i].global_id, serverBookmarks )) != -1 ){
                collisions.push( {
                  clientItem: clientBookmarks[i],
                  serverItem: serverBookmarks[index]
                } );
                foundCollisionsFor[clientBookmarks[i].global_id] = true;
              }
            }

            // collision search

            var _giwpb = new SimpleBench( false );
            var _total = new SimpleBench();

            function getItemsWithParent( data, parentIds ){

              _giwpb.start();

              if( parentIds.length == 2 && parentIds[0] == parentIds[1] ){
                parentIds = [parentIds[0]];
              }

              var result = [];

              var searchIn = {};

              if( data == serverBookmarks ){
                searchIn = parentsServerAssoc;
              }
              else if( data == clientBookmarks ){
                searchIn = parentsClientAssoc;
              }

              if( parentIds.length == 1 ){
                var parentId = parentIds[0];
                if( parentId in searchIn ){
                    result = searchIn[parentId];
                }
              }
              else{
                parentIds.forEach(function( parentId ){

                  if( parentId in searchIn ){
                      result = result.concat( searchIn[parentId] );
                  }

                });
              }

              _giwpb.inc();

              return result;

            }

            function processItems( parentIds ){

              var clientItems = getItemsWithParent( clientBookmarks, parentIds );
              var serverItems = getItemsWithParent( serverBookmarks, parentIds );

              function getCollisedServerItem( clientItem ){

                // collision for folder is equal title params
                // collision for bookmark is equal url params
                // collision for both types if they have similiar global_id

                var collisedServerItem = null;

                for( var i = 0; i != serverItems.length; i++ ){

                  var serverItem = serverItems[i];

                  if( foundCollisionsFor[serverItem.global_id] ){
                    // this server item already collised with another client bookmark
                    continue;
                  }

                  if( clientItem.type == serverItem.type ){

                    if( clientItem.type == "folder" ){

                      if( clientItem.title == serverItem.title  ){
                        collisedServerItem = serverItem;
                        break;
                      }

                    }
                    else if( clientItem.type == "bookmark" ){

                      //console.log( "COMP", clientItem, serverItem );

                      if( fvdSynchronizer.Utils.isIdenticalUrls( clientItem.url, serverItem.url )  ){
                        collisedServerItem = serverItem;
                        break;
                      }

                    }

                    if( clientItem.global_id == serverItem.global_id ){
                      collisedServerItem = serverItem;
                      break;
                    }

                  }

                }

                return collisedServerItem;

              }


              clientItems.forEach(function( clientItem ){

                var collisedServerItem = getCollisedServerItem( clientItem );

                if( collisedServerItem ){

                  if( !( clientItem.global_id in foundCollisionsFor ) && !( collisedServerItem.global_id in foundCollisionsFor ) ){

                    foundCollisionsFor[clientItem.global_id] = true;
                    foundCollisionsFor[collisedServerItem.global_id] = true;

                    collisions.push({
                      clientItem: clientItem,
                      serverItem: collisedServerItem
                    });

                    if( clientItem.type == "folder" ){
                      processItems( [ clientItem.global_id, collisedServerItem.global_id ] );
                    }

                  }

                }

              });

            }

            // start process every root folder
            processItems(["menu"]);
            processItems(["toolbar"]);

            _giwpb.end("Get items with parent duration");
            _total.end( "Total duration" );

            chainCallback();

          },

          function( chainCallback ){



            //console.log( "M4" );

            // all collision will be resolved to server bookmark data

            console.log("Merge step: resolve collisions");

            var _cforeach = new SimpleBench(false);

            collisions.forEach( function( collision ){

              _cforeach.start();

              // remove data from client bookmarks

              var index = getItemIn( collision.clientItem.global_id, clientBookmarks );

              if( index != -1 ){
                clientBookmarks.splice( index, 1 );
              }

              // remove data from server bookmarks

              var index = getItemIn( collision.serverItem.global_id, serverBookmarks );

              if( index != -1 ){
                serverBookmarks.splice( index, 1 );
              }

              _cforeach.inc();

            } );

            _cforeach.end( "collisions.forEach" );

            // fill to server bookmarks
            clientBookmarks.forEach( function( clientBookmark ){

              if( getItemIn( clientBookmark.global_id, serverBookmarks ) == -1 ){
                toServerBookmarks.push( clientBookmark );
              }

            } );

            // fill to client bookmarks
            serverBookmarks.forEach( function( serverBookmark ){

              if( getItemIn( serverBookmark.global_id, clientBookmarks ) == -1 ){
                if( !( serverBookmark.global_id in toClientBookmarksGlobalIds ) ){
                  toClientBookmarks.push( serverBookmark );
                  toClientBookmarksGlobalIds[ serverBookmark.global_id ] = true;
                }
              }

            } );

            chainCallback();

          },

          function( chainCallback ){

            //console.log( "M5" );

            console.log( "TO SERVER", toServerBookmarks.length );

            console.log( "TO CLIENT", toClientBookmarks.length );

            chainCallback();

          },

          function( chainCallback ){
            if( checkCurrentProcessAborted(callback) ){
              return;
            }
            // apply changes on local
            // save collisions first
            console.log( "Collisions ", collisions );

            console.log("Merge step: resolve collisions 2");

            var _firstarrayprocess = new SimpleBench(false);

            collisions.forEach(function(collision) {
              _firstarrayprocess.inc();
              _firstarrayprocess.start();
              // save server item, e.g. server priority
              if( !( collision.serverItem.global_id in toClientBookmarksGlobalIds ) ){
                toClientBookmarks.push( collision.serverItem );
                toClientBookmarksGlobalIds[collision.serverItem.global_id] = true;
              }
              if( collision.serverItem.global_id != collision.clientItem.global_id ){
                translateBookmarksGuids[ collision.clientItem.global_id ] = collision.serverItem.global_id;
                // change global id to server global id
                bookmarksManager.changeGlobalId( collision.clientItem.global_id, collision.serverItem.global_id);
              }
            });

            _firstarrayprocess.end("First arrayProcess");
            // save new bookmarks
            console.log("Merge step: store bookmarks interface:", storeToClientInterfaceParams, toClientBookmarks.length);
            saveListIfFailTryPlugin( toClientBookmarks, function( result, newInterfaceParams ){
              if( !result ){
                return callback( fvdSynchronizer.Errors.ERROR_CHROME_QUOTA_EXCEED );
              }

              if( newInterfaceParams ){
                storeToClientInterfaceParams.via = newInterfaceParams.via;
              }

              chainCallback();
            }, storeToClientInterfaceParams );
          },

          function( chainCallback ){

            console.log("Merge step: translate ids");

            // before store bookmarks on server need to translate parent_id's if changed to server ids
            for( var i = 0; i != toServerBookmarks.length; i++ ){
               var toServerBookmark = toServerBookmarks[i];

               // parents global ids
               if( translateBookmarksGuids[toServerBookmark.parent_id] ){
                toServerBookmark.parent_id = translateBookmarksGuids[toServerBookmark.parent_id];
               }

               // self bookmarks global ids
               if( translateBookmarksGuids[toServerBookmark.global_id] ){
                toServerBookmark.global_id = translateBookmarksGuids[toServerBookmark.global_id];
               }
            }

            chainCallback();

          },

          function( chainCallback ){

            //console.log( "M7" );

            console.log("Merge step: upload data to server");

            if( checkCurrentProcessAborted(callback) ){
              return;
            }

            if( toServerBookmarks.length > 0 ){

              putBookmarksList( toServerBookmarks, function( error, data ){

                if( error != 0 ){
                  callback( error, {
                    count: toServerBookmarks.length
                  } );
                }
                else{
                  serverLastUpdateTime = data.lastUpdateTime;

                  chainCallback();
                }

              } );

            }
            else{
              chainCallback();
            }

          },

          function(){
            bookmarksDatabase.applyMassGuidsChange();
            try{
              if( serverLastUpdateTime ){
                setLastUpdateTime( serverLastUpdateTime );
              }
            }
            catch( ex ){
            }
            callback( 0 );
          }

        ] );

      }

      /**
       * download changes from server and apply it
       * @return {
       *   {Array} storedGuids - list of guids
       * }
       */
      function applyServerUpdates( params, callback ) {

        var allGuids = [];
        var toRemove = [];

        var interfaceParams = {
          via: "chrome"
        };

        getBookmarksList( params,
          function( error, data ){

            if( error != 0 ){
              callback( error );
            }
            else{
              var changeData = getToSyncData(),
                  requestTree = [],
                  storedGuids = [];

              bookmarksDatabase.startMassGuidsChange();
              allGuids = bookmarksManager.getAllGuids();
              // remove bookmarks
              allGuids.forEach( function( clientGuid ) {
                if( data.bookmarkIds.indexOf( clientGuid ) == -1 ){
                  if( changeData.newIds.indexOf( clientGuid ) == -1 ){
                    toRemove.push( clientGuid );
                  }
                }
              } );
              virtualBookmarks.removeNotInList(data.bookmarkIds);

              fvdSynchronizer.Utils.Async.chain( [
                function( chainCallback ){
                  fvdSynchronizer.Utils.Async.arrayProcess( toRemove, function( clientId, arrayProcessCallback ){
                    bookmarksManager.remove( clientId, function(){
                      arrayProcessCallback();
                    }, interfaceParams );
                  }, function(){
                    chainCallback();
                  } );
                },
                function(chainCallback) {
                  if(!changeData.removedIds || !changeData.removedIds.length) {
                    return chainCallback();
                  }
                  data.bookmarks.forEach(function(b) {
                    // checking for folders which removed on client and updated on server
                    if(changeData.removedIds.indexOf(b.global_id) != -1) {
                      removeSyncData(["removedIds"], b.global_id);
                      if(b.type == "folder") {
                        requestTree.push(b.global_id);
                      }
                    }
                  });
                  if(!requestTree.length) {
                    return chainCallback();
                  }
                  // restore bookmarks tree for removed folders on client
                  fvdSynchronizer.Utils.Async.arrayProcess(requestTree, function(parent_id, next) {
                    getBookmarksList({
                      parent_id: parent_id
                    }, function(err, res) {
                      if(err) {
                        return cb(err);
                      }
                      if(!res.bookmarks || !res.bookmarks.length) {
                        return next();
                      }
                      for(var i = 0; i != res.bookmarks.length; i++) {
                        var b = res.bookmarks[i];
                        if(b.global_id == parent_id) {
                          continue;
                        }
                        if(changeData.removedIds.indexOf(b.global_id) != -1) {
                          removeSyncData(["removedIds"], b.global_id);
                        }
                        data.bookmarks.push(b);
                      }
                      next();
                    });
                  }, function() {
                    chainCallback();
                  });
                },

                function( chainCallback ){
                  saveListIfFailTryPlugin( data.bookmarks, function(result, newInterfaceParams){
                    if( !result ){
                      //self.displayChromeQuotaExceedMessage();
                      return callback( fvdSynchronizer.Errors.ERROR_CHROME_QUOTA_EXCEED );
                    }
                    data.bookmarks.forEach(function(b) {
                      storedGuids.push(b.global_id);
                    });

                    setLastUpdateTime( data.lastUpdateTime );

                    if( newInterfaceParams ){
                      interfaceParams.via = newInterfaceParams.via;
                    }

                    chainCallback();


                  }, interfaceParams );

                },

                function( chainCallback ){
                  bookmarksDatabase.applyMassGuidsChange();
                  callback( 0, {
                    storedGuids: storedGuids
                  } );
                }

              ] );

            }

          }
        );

      }



      /* lock */

      function acquireSyncLock( callback ){


        //console.log("+++");
        fvdSynchronizer.Server.Sync.acquireSyncLock( callback );

      }

      function releaseSyncLock( callback ){

        //console.log("---");
        fvdSynchronizer.Server.Sync.releaseSyncLock( callback );

      }

      function acquireSyncLockCallback( errorCallback, successCallback ){

        acquireSyncLock( function( error ){

          if( error != 0 ){

            errorCallback( error );
          }
          else{

            successCallback();
          }

        } );


      }

      /* global request */

          function makeRequest(data, callback){

            fvdSynchronizer.Server.Sync.authorizedMessage( data, callback );

          }



      /* last update time */

      function restoreLastUpdateTime(){
        var prev = fvdSynchronizer.Prefs.get( "bookmarks.last_update_time.prev" );

        if( prev ){
          fvdSynchronizer.Prefs.set( "bookmarks.last_update_time", prev );
        }
      }

      function setLastUpdateTime( time ){

        var old = fvdSynchronizer.Prefs.get( "bookmarks.last_update_time" );

        if( old ){
          fvdSynchronizer.Prefs.set( "bookmarks.last_update_time.prev", old );
        }

        fvdSynchronizer.Prefs.set( "bookmarks.last_update_time", time );

      }

      function getLastUpdateTime(){
        var time = fvdSynchronizer.Prefs.get("bookmarks.last_update_time");
        // if last update time is set to the empty string it can make negative effects on the sync
        if(time === "") {
          time = "0";
        }
        return time;
      }

      /* TO sync data */

          function getToSyncData(){

              var result = null;

              try {
          var str = fvdSynchronizer.Prefs.get("bookmarks.to_sync_data");

          if( str != "{}" ){
            result = JSON.parse(str);
          }
              }
              catch (ex) {
              }

              if (!result) {
                  result = fvdSynchronizer.Utils.objectClone( _initToSyncData );
              }
        else{
          for( var k in _initToSyncData ){
            if( !result[k] ){
              result[ k ] = _initToSyncData[ k ];
            }
          }
        }

              return result;

          }

          function setToSyncData(toSyncData, refreshButton){
              fvdSynchronizer.Prefs.set("bookmarks.to_sync_data", JSON.stringify(toSyncData));

        if(typeof refreshButton == "undefined"){
          refreshButton = true;
        }

        if( refreshButton ){
          fvdSynchronizer.Server.Sync.syncStateRefresh();
        }
        }

        function clearToSyncData(){
            setToSyncData(_initToSyncData);
        }

        function syncData(category, data, refreshButton) {

          if(refreshButton == "undefined"){
            refreshButton = true;
          }

          if (typeof category == "string") {
            category = [category];
          }

          if (!data) {
            return;
          }

          var toSyncData = getToSyncData();

          category.forEach(function(cat){
            if( !toSyncData[cat] ){
              toSyncData[cat] = [];
            }
            try{
              if (toSyncData[cat].indexOf(data) == -1 && data != null) {
                toSyncData[cat].push(data);
              }
            }
            catch( ex ){
              console.log( "Fail add to sync data to ", cat  );
            }
          });
          setToSyncData(toSyncData, refreshButton);
       }

      function removeSyncData(category, data){

        if( typeof category == "string" ){
          category = [category];
        }

        if( !data ){
          return;
        }

        var toSyncData = getToSyncData();

        category.forEach(function( cat ){
          if( !toSyncData[cat] ){
            toSyncData[cat] = [];
          }

          var index = toSyncData[cat].indexOf( data );

          if( index != -1 ){
            toSyncData[cat].splice( index, 1 );
          }
        });

        setToSyncData( toSyncData );

      }

      /* call main sync state listeners */

      function removeChangeMainSyncStateListener( listener ){

        var index = changeMainSyncStateListeners.indexOf( listener );

        if( index != -1 ){

          changeMainSyncStateListeners.splice( index, 1 );
        }

      }

      function callChangeMainSyncStateListeners(){

        var listenersToRemove = [];

        changeMainSyncStateListeners.forEach(function( listener ){
          try{
            listener();
          }
          catch( ex ){
            listenersToRemove.push( listener );
          }
        });

        listenersToRemove.forEach(function( listener ){
          removeChangeMainSyncStateListener( listener );
        });


      }

      // public

      this.getInitialActionsProgress = function(){
        return initialActionsProgress;
      };

      this.initializationInProgress = function(){
        return initialActionsProcess;
      };

      this.restoreBeforeManualSyncState = function( callback ){
        restoreLastUpdateTime();
        bookmarksDatabase.rollbackMassGuidChange();
        callback();
      };

      this.isSyncNow = function(){
        return syncNow || this.getMainSyncState() == "sync";
      };

      this.setSyncNow = function( state ){
        if( state ){
          startSync();
        }
        else{
          endSync();
        }
      };

      this.isAllowed = function(cb, ignoreInitialActions){
        var allowed = true;

        fvdSynchronizer.Utils.Async.chain([
          function(next){
            if(ignoreInitialActions) {
              return next();
            }
            if(initialActionsProcess){
              return cb(false);
            }
            next();
          },
          function(next) {
            // bookmarks sync is disabled for Opera now
            /*if(navigator.userAgent.indexOf("OPR/") !== -1) {
              return cb(false);
            }*/
            next();
          },
         function(){
           cb(true);
         }
        ]);
      };

      this.setFirstSyncAfter = function(after){
          fvdSynchronizer.Prefs.set("bookmarks.first_sync_after", after);
      };

      this.getFirstSyncAfter = function(){
        return fvdSynchronizer.Prefs.get("bookmarks.first_sync_after");
      };

      this.supportsLargeSync = function() {
        // FIXME I don't know if chrome will supports this constant in future releases
        return chrome.bookmarks.MAX_SUSTAINED_WRITE_OPERATIONS_PER_MINUTE >= 1000000;
      };

      this.abortCurrentSync = function(){
        currentProcessAborted = true;
      };

      this.removeSyncData = function() {
        removeSyncData.apply( self, arguments );
      };

      this.syncData = function(){
        syncData.apply( self, arguments );
      };

      this.hasToSyncData = function(){

        var data = getToSyncData();

        var has = false;

        for( var k in data ){

          if( data[k].length > 0 ){
            has = true;
            break;
          }

        }

        return has;

      };

      this.getMainSyncState = function(){
        return mainSyncState;
      };

      this.setMainSyncState = function( state ){
        mainSyncState = state;

        chrome.extension.sendRequest( {
          subject: "mainSyncStateChange"
        } );

        callChangeMainSyncStateListeners();

        fvdSynchronizer.Server.Sync.syncStateRefresh();
      };

      this.addChangeMainSyncStateListener = function( callback ){

        changeMainSyncStateListeners.push( callback );

      };

      this.getMainSyncProgress = function(){
        return mainSyncProgress;
      };

      /* syncing */

      this.mergeLocalAndServerData = function( callback ){

        acquireSyncLockCallback( callback, function(){

          currentProcessAborted = false;

          startSync();

          startTransaction( function( transId ){

            mergeLocalAndServerData( function( error, _additional ){

              _additional = _additional || {};

              endSync();

              if( error == fvdSynchronizer.Errors.ERROR_COUNT_ITEMS_QUOTA_EXCEED ){
                self.displayQuotaExceedMessage( _additional.count );
              }

              fvdSynchronizer.Utils.Async.chain( [
                function( chainCallback ){

                  if( error == 0 ){
                    commitTransaction( transId, chainCallback );

                    self.setFirstSyncAfter( "none" );
                  }
                  else{
                    rollbackTransaction( transId, chainCallback );
                  }

                },
                function(){

                  if( error == 0 ){
                    clearToSyncData();
                  }

                  releaseSyncLock(function(){
                    callback( error );
                  });

                }
              ] );




            } );

          } );

        } );

      };

      this.overwriteLocalData = function( callback ){


        acquireSyncLockCallback( callback, function(){

          currentProcessAborted = false;

          startSync();

          startTransaction( function( transId ){

            overwriteLocalData( function( error ){

              endSync();

              fvdSynchronizer.Utils.Async.chain( [
                function( chainCallback ){

                  if( error == 0 ){
                    commitTransaction( transId, chainCallback );

                    self.setFirstSyncAfter( "none" );
                  }
                  else{
                    rollbackTransaction( transId, chainCallback );
                  }

                },
                function(){

                  if( error == 0 ){
                    clearToSyncData();
                  }

                  releaseSyncLock(function(){
                    callback( error );
                  });

                }
              ] );




            } );

          } );

        } );

      };

      this.overwriteServerData = function( callback ){

        acquireSyncLockCallback( callback, function(){

          currentProcessAborted = false;

          startSync();

          startTransaction( function( transId ){

            overwriteServerData( function( error, _additional ){

              if( error == fvdSynchronizer.Errors.ERROR_COUNT_ITEMS_QUOTA_EXCEED ){
                self.displayQuotaExceedMessage( _additional.count );
              }

              endSync();

              fvdSynchronizer.Utils.Async.chain( [
                function( chainCallback ){

                  if( error == 0 ){
                    commitTransaction( transId, chainCallback );

                    self.setFirstSyncAfter( "none" );
                  }
                  else{
                    rollbackTransaction( transId, chainCallback );
                  }

                },
                function(){

                  if( error == 0 ){
                    clearToSyncData();
                  }

                  releaseSyncLock(function(){
                    callback( error );
                  });

                }
              ] );




            } );

          } );

        } );

      };

      this.startAutoSync = function( params, callback ){

        if( syncNow || initialActionsProcess ){
          console.log("initial action or sync now");

          callback( fvdSynchronizer.Errors.ERROR_DRIVER_BUSY );
          return;
        }

        if( self.getFirstSyncAfter() != "" && self.getFirstSyncAfter() != "none" ){
          console.log( "Need initial sync after: " + self.getFirstSyncAfter() );

          callback( fvdSynchronizer.Errors.ERROR_DRIVER_BUSY );
          return;
        }

        self.ignoreSyncProgress = true;

        function _end( error ){
          self.ignoreSyncProgress = false;

          callback( error );
        }

        acquireSyncLock( function( error ){

          if( error != 0 ) {
            return _end( error );
          }
          else {

            startSync();
            self.setMainSyncState( "sync" );

            startMainSync( function( error ){

              releaseSyncLock( function(  ){

                console.log("end sync");

                self.setMainSyncState( "nosync" );
                endSync();

                _end( error );

              });

            });

          }

        } );

      };

      this.startMainSync = function(){

        acquireSyncLock(function( error ){

          if( error != 0 ){

            chrome.extension.sendRequest( {
              subject: "syncError",
              error: error
            } );

            self.setMainSyncState( "nosync" );

          }
          else{

            currentProcessAborted = false;

            startSync();

            self.setMainSyncState( "sync" );

            startTransaction( function( transId ){

              startMainSync( function( error, _additional ){

                if( error ){

                  if( error == fvdSynchronizer.Errors.ERROR_COUNT_ITEMS_QUOTA_EXCEED ){
                    self.displayQuotaExceedMessage( _additional.count );
                  }
                  else if( error != fvdSynchronizer.Errors.ERROR_SYNC_USER_ABORTED ){
                    //self.displayQuotaExceedMessage();
                  }

                }

                fvdSynchronizer.Utils.Async.chain( [
                  function( chainCallback ){

                    if( error == 0 ){
                      commitTransaction( transId, chainCallback );
                    }
                    else{
                      rollbackTransaction( transId, chainCallback );
                    }

                  },
                  function(){

                    if( error == 0 ){
                      clearToSyncData();
                    }

                    releaseSyncLock(function(){

                      self.setMainSyncState( "nosync" );
                      endSync();
                      //callback( error );
                    });

                  }
                ] );


              } );

            } );

          }

        } );

      };

      this.displayChromeQuotaExceedMessage = function(){

        fvdSynchronizer.Dialogs.alertWindow(_("dlg_alert_sync_failed_chrome_quota_exceed_title"), _("dlg_alert_sync_failed_chrome_quota_exceed_text"));

      };

      this.displayQuotaExceedMessage = function( countItemsToSave ){

        //fvdSynchronizer.Dialogs.alertWindow( _("dlg_alert_sync_quota_exceed_title"), _("dlg_alert_sync_quota_exceed_text") +
        //   ", <a target='blank' href='https://everhelper.me/client/?l=/components/MainOptions/quota'>"+_("dlg_alert_sync_quota_exceed_fix_it")+"</a>" );

        fvdSynchronizer.Dialogs.typedWindow( "syncQuotaExceed", {
          query: {
            count: countItemsToSave
          }
        } );

      };

      this.totalItemsCount = function( callback ){

        console.log("Bookmarks: Requested total items count");

        bookmarksManager.getAllBookmarks(function(clientBookmarks){

          console.log("Bookmarks: Total Items counted: ", clientBookmarks.length);

          if( clientBookmarks instanceof Array ){
            return callback( clientBookmarks.length );
          }

          callback( 0 );

        });

      };

      this.canSync = function(){
        return true;
      };

      this.canSyncAndReaction = function(){
        return true;
      };

      this.Backup = new function(){

        var selfBackup = this;

        const BOOKMARKS_DUMP_FILE_NAME = "bookmarks.backups.json";
        const AUTOBACKUP_CHECK_INTERVAL = 60 * 1000; //60 * 1000 * 60 * 24;
        const MAX_BACKUPS = 30;

        var _ignoreBackupChecker = false;
        var restoreBackupProgress = {
          current: 0,
          max: 0,
          type: "remove"
        };


        window.addEventListener("load", function(){

          setInterval(function(){

            if( _ignoreBackupChecker || !enabled() ){
              return;
            }

            if( fvdSynchronizer.Server.Sync.isSyncNow() ){
              return;
            }

            var now = new Date().getTime();

            var lastCheckTime = 0;
            lastCheckTime = parseInt( fvdSynchronizer.Prefs.get( "bookmarks.backup.last_check_time" ) );


            if( isNaN(lastCheckTime) || (now - lastCheckTime >= AUTOBACKUP_CHECK_INTERVAL) ){

              var hasChanges = _b( fvdSynchronizer.Prefs.get( "bookmarks.backup.has_changes" ) );

              if( isNaN(lastCheckTime) && hasChanges || lastCheckTime ){
                fvdSynchronizer.Prefs.set( "bookmarks.backup.last_check_time", now );
              }

              if( hasChanges ){
                fvdSynchronizer.Prefs.set( "bookmarks.backup.has_changes", false );
                selfBackup.make(function(){});
              }

            }

          }, 1000);

        });

        setTimeout(function(){

          // some delayed prepatations

          if( _b(fvdSynchronizer.Prefs.get("bookmarks.backup.need_set_change_on_start")) ){
            selfBackup.setHasChanges();
            fvdSynchronizer.Prefs.set("bookmarks.backup.need_set_change_on_start", false);
          }

        }, 1000);

        function enabled(){
          return  _b( fvdSynchronizer.Prefs.get( "autobackup.enabled" ) );
        }

        this.setHasChangesOnStart = function(){
          fvdSynchronizer.Prefs.set("bookmarks.backup.need_set_change_on_start", true);
        };

        this.setHasChanges = function(){

          fvdSynchronizer.Prefs.set( "bookmarks.backup.has_changes", true );

        };

        this.setNotHasChanges = function(){

          fvdSynchronizer.Prefs.set( "bookmarks.backup.has_changes", false );

        };

        this.getRestoreProgress = function(){
          return restoreBackupProgress;
        };

        this.removeOldBackups = function( callback ){

          selfBackup.listBackups(function( backups ){
            if(!(backups instanceof Array)) {
              backups = [];
            }

            backups.sort(function( a, b ){
              return a.time - b.time;
            });

            fvdSynchronizer.Utils.Async.cc( function( ccCallback ){

              if( backups.length <= MAX_BACKUPS ){
                return callback();
              }

              var backup = backups.shift();

              fvdSynchronizer.FileSystem.removeDir( "backups/bookmarks/" + backup.dir, function(){
                ccCallback();
              } );

            } );

          });

        };

        this.listBackups = function( callback ){

          var result = [];

          fvdSynchronizer.FileSystem.dirContents( "backups/bookmarks", function( dirs ){

            if( !dirs ){
              return callback( [] );
            }

            fvdSynchronizer.Utils.Async.arrayProcess( dirs, function( dir, apc ){

              // read info file
              fvdSynchronizer.FileSystem.read( "backups/bookmarks/" + dir + "/info.json", function(r, contents) {
                if(!r) {
                  console.error("Fail read file " + "backups/bookmarks/" + dir + "/info.json, ignore", r);
                  return apc();
                }

                try{
                  var info = JSON.parse( contents );
                  info.dir = dir;
                  result.push( info );
                }
                catch( ex ){

                }
                finally{
                  apc();
                }
              } );
            }, function(){

              callback( result );

            } );

          } );

        };

        this.make = function( callback ){

          fvdSynchronizer.Prefs.set( "bookmarks_backup_date", new Date().getTime() );

          var bookmarks = [];

          var nowDate = new Date();
          var backupDir = nowDate.getFullYear()+""+nowDate.getMonth()+""+nowDate.getDay()+""+nowDate.getHours()+nowDate.getMinutes()+""+nowDate.getSeconds();

          var dir = "backups/bookmarks/" + backupDir;

          fvdSynchronizer.Utils.Async.chain([

            function( chainCallback ){

              bookmarksManager.getAllBookmarks(false, function( _bookmarks ){

                bookmarks = _bookmarks;

                chainCallback();

              });

            },

            function( chainCallback ){

              fvdSynchronizer.FileSystem.makeDir( dir, function(){
                chainCallback();
              } );

            },

            function( chainCallback ){

              fvdSynchronizer.FileSystem.write( dir + "/data.json", JSON.stringify( bookmarks ), function( result ){
                if( !result ){
                  return callback( result );
                }

                chainCallback();
              } );

            },

            function( chainCallback ){

              var info = {
                time: nowDate.getTime(),
                count: bookmarks.length
              };

              fvdSynchronizer.FileSystem.write( dir + "/info.json", JSON.stringify( info ), function( result ){
                if( !result ){
                  return callback( result );
                }

                chainCallback();
              } );

            },

            function( chainCallback ){

              selfBackup.removeOldBackups( function(){
                chainCallback();
              } );

            },

            function(){

              fvdSynchronizer.Observer.fireEvent( "bookmarksBackupMaked" );

              callback( true );

            }

          ]);



        };

        this.restore = function( dir, callback ){

          _ignoreBackupChecker = true;

          fvdSynchronizer.Driver.Bookmarks.setSyncNow( true );

          this._restore( dir, function( result ){
            fvdSynchronizer.Driver.Bookmarks.setSyncNow( false );

            selfBackup.setNotHasChanges();
            _ignoreBackupChecker = false;
            callback( result );
          } );

        };

        this._restore = function( dir, callback ){

          restoreBackupProgress.current = 0;

          console.log( "Restore from", "backups/bookmarks/" + dir + "/data.json" );

          fvdSynchronizer.FileSystem.read( "backups/bookmarks/" + dir + "/data.json", function( result, data ){

            var bookmarks = [];

            try{
              bookmarks = JSON.parse( data );
            }
            catch( ex ){
              return callback( false );
            }

            var interfaceParams = {
              via: "chrome"
            };

            var countOperations = 0;
            var currentBookmarksCount = bookmarksDatabase.getCountStoredGuids();
            countOperations = currentBookmarksCount + bookmarks.length;
            console.log("To restore need to make ", countOperations);

            fvdSynchronizer.Utils.Async.chain([

              function(){

                try{

                  restoreBackupProgress.type = "remove";
                  restoreBackupProgress.current = 0;

                  bookmarksManager.removeAllBookmarks(function(){

                    restoreBackupProgress.type = "create";
                    restoreBackupProgress.current = 0;
                    restoreBackupProgress.max = bookmarks.length;


                    //bookmarks.forEach(function( b ){
                    //  fvdSynchronizer.Driver.Bookmarks.syncData( [ "changedIds", "newIds" ], b.global_id, false );
                    //});
                    //
                    //fvdSynchronizer.Server.Sync.syncStateRefresh();

                    console.log("Start save bookmarks list: ", bookmarks.length);

                    bookmarksManager.saveList( bookmarks, function(  ){

                      fvdSynchronizer.Driver.Bookmarks.setFirstSyncAfter( "register" );

                      console.log("Restore interface:", interfaceParams);
                      var start = new Date().getTime()/1000;

                      callback( true );
                    }, interfaceParams, function(){

                      restoreBackupProgress.current++;

                    } );

                  }, interfaceParams, function( aCurrent, aMax ){
                    restoreBackupProgress.current = aCurrent;
                    restoreBackupProgress.max = aMax;
                  });

                }
                catch(e){
                  callback( false );
                }

              }

            ]);

          } );

        };

        this.hasBackup = function( callback ){

          selfBackup.listBackups( function( backups ){

            callback( backups.length > 0 );

          } );

        };

        this.countBackuped = function( callback ){

          fvdSynchronizer.FileSystem.read( BOOKMARKS_DUMP_FILE_NAME, function( result, data ){

            try{

              var data = JSON.parse( data );

              callback( data.count );

            }
            catch(e){

              console.log(e);

              callback( null );
            }

          } );

        };

      };


    };

    fvdSynchronizer.Driver.Bookmarks = new Bookmarks();


  }
  else{

    fvdSynchronizer.Driver.Bookmarks = chrome.extension.getBackgroundPage().fvdSynchronizer.Driver.Bookmarks;

  }

})();
