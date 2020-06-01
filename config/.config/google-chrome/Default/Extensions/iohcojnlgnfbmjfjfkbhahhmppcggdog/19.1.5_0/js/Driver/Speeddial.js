(function(){

  if (window == chrome.extension.getBackgroundPage()) {

      var Errors = fvdSynchronizer.Errors;

      var Speeddial = function(){

      var UPLOAD_PREVIEW_FILENAME = "preview.png";

      var changeMainSyncStateListeners = [];
      var fvdSpeedDialName = "Speed Dial [FVD] - New Tab Page, 3D, Sync...";
      var fvdSpeedDialId = "llaficoajjainaijghjlofdfmbjpebpa";

      var isSyncNow = false;

      var port = null;
      var allow = false;
      // version of api which speed dial supports
      var apiVersion = 0;

      var abortCurrentSync = false;
      var currentSyncType
      var currentSyncId

      var self = this;

      var _initToSyncData = {
        dials: [],
        groups: [],
        newDials: [], // here only just created dials
        newGroups: [], // here only just created groups
        deleteDials: [],
        deleteGroups: [],
        specialActions: [] // special actions, such as merge one group and etc.
      };

      var _pendingSDRequests = []; /* here saves pending requests to speeddial with their callbacks */
      var _pendingRequestLastId = 0; // id of last pending request
      const PENDING_REQUESTS_TIMEOUT = 1000 * 5 * 60; // max life time of pending requests

      var mainSyncState = "nosync";
      var mainSyncProgress = {};


      this.ignoreSyncProgress = false; // ignore sync stats displaying

      this.displayQuotaExceedMessage = function( countItemsToSave ){

        fvdSynchronizer.Dialogs.typedWindow( "syncQuotaExceed", {
          query: {
            count: countItemsToSave,
            category: "dials_count"
          }
        } );

      };

      this.addChangeMainSyncStateListener = function( callback ){

        changeMainSyncStateListeners.push( callback );

      };

      this.getMainSyncState = function(){
        return mainSyncState;
      };

      this.getMainSyncProgress = function(){
        return mainSyncProgress;
      };

      this.startAutoSync = function( params, callback ){

        if( isSyncNow ){
          callback( fvdSynchronizer.Errors.ERROR_DRIVER_BUSY );
          return;
        }

        if( self.getFirstSyncAfter() != "" && self.getFirstSyncAfter() != "none" ){
          console.log( "Need initial sync after: " + self.getFirstSyncAfter() );

          callback( fvdSynchronizer.Errors.ERROR_DRIVER_BUSY );
          return;
        }

        this.ignoreSyncProgress = true;

        fvdSynchronizer.Utils.Async.chain([
          function(next) {
            self.isAllowed(function(allowed) {
              if(allowed) {
                next();
              }
              else {
                callback(fvdSynchronizer.Errors.ERROR_NEED_ADDITIONAL_SOFTWARE);
              }
            });
          },
          function() {
            self.startMainSync({type: 'auto'}, function(error) {
              self.ignoreSyncProgress = false;
              callback(error);
            });
          }
        ]);
      };

      this.startMainSync = function(options, callback) {
        if(typeof options === 'function') {
          callback = options
          options = {}
        }
        options = options || {}
        options.type = options.type || 'main'
        trackSync(options.type)

        callback = callback || function(){};

        function nullMainSyncProgress(){
          mainSyncProgress = {
            action: "upload_changes", // upload_changes, download_changes, special_actions
            state: "",
            successStates: [],
            dialsCount: 0,
            groupsCount: 0,
            specialActionsCount: 0,
            specialActionsCurrent: 0,
          };
        }

        nullMainSyncProgress();

        setMainSyncState("sync");


        acquireSyncLock( function( error ){
          if( error !== 0 ){

            chrome.extension.sendRequest( {
              subject: "syncError",
              error: error
            } );

            setMainSyncState( "nosync" );

            callback( error );

          }
          else{

            var syncError = 0;
            var transId = 0;
            var hasUpdates = false;

            var toSyncData = getToSyncData();

            var specialActions = toSyncData.specialActions;

            fvdSynchronizer.Utils.Async.chain( [

              function( chainCallback ){
                startTransaction( function( id ){
                  transId = id;
                  chainCallback();
                } );
              },

              function( chainCallback ){
                nullMainSyncProgress();
                if( fvdSynchronizer.Prefs.get("speeddial.first_sync_after") != "none" ){
                  mainSyncProgress.action = "upload_changes";
                  fvdSynchronizer.Driver.Speeddial.mergeLocalAndServerData( function( error ){
                    setMainSyncState( "nosync" );
                    fvdSynchronizer.Prefs.set("speeddial.first_sync_after", "none");
                    if( error ){
                      syncError = error;
                    }
                    chainCallback();
                  }, function( state, progress ){
                    fvdSynchronizer.Utils.objectsMerge( mainSyncProgress, progress );

                    if( mainSyncProgress.state ){
                      mainSyncProgress.successStates.push( mainSyncProgress.state );
                    }

                    mainSyncProgress.state = state;

                    setMainSyncState( "sync" );

                  } );

                }
                else{
                  mainSyncProgress.action = "upload_changes";
                  fvdSynchronizer.Driver.Speeddial.hasUpdates( function( error, has, updateInfo ){
                    if(error) {
                      syncError = error;
                    }
                    hasUpdates = has;
                    fvdSynchronizer.Utils.Async.chain([
                      function(chainCallback) {
                        if(syncError) {
                          return chainCallback();
                        }
                        nullMainSyncProgress();
                        mainSyncProgress.action = "download_changes";
                        setMainSyncState( "sync" );
                        var lastUpdateTime = Math.max( updateInfo.lastUpdateTimeDials, updateInfo.lastUpdateTimeGroups );

                        mainSyncProgress.successStates = [];
                        mainSyncProgress.dialsCount = 0;
                        mainSyncProgress.groupsCount = 0;

                        self.applyServerUpdates( updateInfo.localLastUpdateTimeGroups,
                                                 updateInfo.localLastUpdateTimeDials, function( error ){
                          if( error ){
                            syncError = error;
                          }
                          chainCallback();
                        }, function( state, progress ){
                          fvdSynchronizer.Utils.objectsMerge( mainSyncProgress, progress );

                          if( mainSyncProgress.state ){
                            mainSyncProgress.successStates.push( mainSyncProgress.state );
                          }

                          mainSyncProgress.state = state;

                          setMainSyncState( "sync" );
                        } );
                      },
                      function(){
                        // update to sync data again
                        toSyncData = getToSyncData();
                        // do sync
                        self.uploadUpdates( function( error, _result ){
                          _result = _result || {};

                          if( error ){

                            if( error == fvdSynchronizer.Errors.ERROR_COUNT_ITEMS_QUOTA_EXCEED ){
                              self.displayQuotaExceedMessage( _result.count );
                            }

                          }
                          if( has && error === 0 ){
                            releaseSyncLock(function(){
                              setMainSyncState( "nosync" );
                            });
                            chainCallback();
                          }
                          else{
                            releaseSyncLock(function(){
                              setMainSyncState( "nosync" );
                            });
                            if( error ){
                              syncError = error;
                            }
                            chainCallback();
                          }
                        }, function( state, progress ){

                          fvdSynchronizer.Utils.objectsMerge( mainSyncProgress, progress );

                          //console.log( "gr", mainSyncProgress.groupsCount );

                          if( mainSyncProgress.state ){
                            mainSyncProgress.successStates.push( mainSyncProgress.state );
                          }

                          mainSyncProgress.state = state;

                          setMainSyncState( "sync" );

                        } );


                      }
                    ]);

                  } );
                }
              },

              function( chainCallback ){
                // process special actions

                mainSyncProgress.action = "special_actions";

                mainSyncProgress.specialActionsCount = specialActions.length;

                fvdSynchronizer.Utils.Async.arrayProcess( specialActions, function( specialAction, arrayProcessCallback ){

                  nullMainSyncProgress();

                  mainSyncProgress.specialActionsCurrent++;

                  if( specialAction.indexOf( "merge_group" ) == 0 ){

                    hasUpdates = true;

                    var params = specialAction.split( ":" );

                    var groupId = params[1];
                    var groupGlobalId = params[2];

                    mergeLocalAndServerData( {

                      groupId: groupId,
                      groupGlobalId: groupGlobalId

                    },function( error ){

                      setMainSyncState( "nosync" );

                      if( error ){
                        syncError = error;
                      }

                      arrayProcessCallback();

                    }, function( state, progress ){

                      fvdSynchronizer.Utils.objectsMerge( mainSyncProgress, progress );

                      mainSyncProgress.groupsCount = 1;

                      if( mainSyncProgress.state ){
                        mainSyncProgress.successStates.push( mainSyncProgress.state );
                      }

                      mainSyncProgress.state = state;

                      setMainSyncState( "sync" );

                    } );

                  }

                }, function(){

                  chainCallback();

                } );

              },


              function( chainCallback ){
                if( syncError != 0 ) {
                  if( syncError == fvdSynchronizer.Errors.ERROR_COUNT_ITEMS_QUOTA_EXCEED ){
                    //fvdSynchronizer.Dialogs.alertWindow( _("dlg_alert_sync_quota_exceed_title"), _("dlg_alert_sync_quota_exceed_text") );
                  }
                  else if( syncError != fvdSynchronizer.Errors.ERROR_SYNC_USER_ABORTED ){
                    fvdSynchronizer.Dialogs.alertWindow( _("dlg_alert_sync_failed_title"), _("dlg_alert_sync_failed_text") );
                  }

                  rollbackTransaction( transId, chainCallback );

                }
                else{

                  clearToSyncData();
                  commitTransaction( transId, chainCallback );
                }
              },
              function(){

                if( hasUpdates ){
                  messageToSD({
                    action: "rebuild"
                  });
                }
                callback( syncError );
              }

            ] );
          }
        } );
      };

      /*
       * First sync after is used for make first speed dial sync  on specified scenario( if login - select between three sync variants, if register
       * upload all dials to server )
       */
      this.setFirstSyncAfter = function(after){
        fvdSynchronizer.Prefs.set("speeddial.first_sync_after", after);
      };

      this.getFirstSyncAfter = function(){
        return fvdSynchronizer.Prefs.get("speeddial.first_sync_after");
      };

      this.isAllowed = function(cb){
        return cb(allow);
      };

      this.abortCurrentSync = function(){
        abortCurrentSync = true;
      };

      this.gMessageToSD = function( message, callback ){ // Task #2009
        messageToSD(message, callback);
      };

      this.mergeLocalAndServerData = function( callback, stateCallback ){
        trackSync('merge')
        acquireSyncLockCallback( callback, function(){

          abortCurrentSync = false;

          clearToSyncData();

          startSync();

          startTransaction( function( transId ){

            mergeLocalAndServerData( {}, function( error, _result ){

              _result = _result || {};

              endSync();

              if( error ){

                if( error == fvdSynchronizer.Errors.ERROR_COUNT_ITEMS_QUOTA_EXCEED ){
                  self.displayQuotaExceedMessage( _result.count );
                }

              }

              fvdSynchronizer.Utils.Async.chain( [
                function( chainCallback ){

                  if( error == 0 ){
                    commitTransaction( transId, chainCallback );
                    fvdSynchronizer.Prefs.set("speeddial.first_sync_after", "none");
                  }
                  else{
                    rollbackTransaction( transId, chainCallback );
                  }

                },
                function(){

                  messageToSD({
                    action: "rebuild"
                  });

                  releaseSyncLock(function(){
                    callback( error );
                  });

                }
              ] );
            }, stateCallback );

          } );

        } );

      };

      this.overwriteLocalData = function( callback, stateCallback ){
        trackSync('overwrite-local')
        acquireSyncLockCallback( callback, function(){

          clearToSyncData();

          abortCurrentSync = false;

          startSync();

          var transId = 0;
          var syncError = 0;

          fvdSynchronizer.Utils.Async.chain( [

            function( chainCallback ){

              startTransaction( function( id ){
                transId = id;
                chainCallback();
              } );

            },

            function( chainCallback ){

              messageToSD({
                action: "clearGroupsAndDials"
              }, function(){
                chainCallback();
              });

            },

            function( chainCallback ){
              mergeLocalAndServerData({}, function( error ) {
                endSync();

                syncError = error;
                chainCallback();
              }, stateCallback);
            },

            function( chainCallback ){

              if( syncError == 0 ){
                commitTransaction( transId, chainCallback );
                fvdSynchronizer.Prefs.set("speeddial.first_sync_after", "none");
              }
              else{
                rollbackTransaction( transId, chainCallback );
              }

            },

            function(){

              messageToSD({
                action: "rebuild"
              });

              if( syncError == 0 ){
                fvdSynchronizer.Driver.Speeddial.Backup.setHasChanges();
              }

              releaseSyncLock(function(){
                callback( syncError );
              });

            }
          ] );

        } );
      };

      this.overwriteServerData = function( callback, stateCallback ){
        trackSync('overwrite-server')
        acquireSyncLockCallback( callback, function(){

          clearToSyncData();

          abortCurrentSync = false;

          startSync();

          fvdSynchronizer.Utils.Async.chain( [

            function( chainCallback ){

              putAllDataOnServer( function( error, _result ){

                _result = _result || {};

                endSync();

                if( error ){

                  if( error == fvdSynchronizer.Errors.ERROR_COUNT_ITEMS_QUOTA_EXCEED ){
                    self.displayQuotaExceedMessage( _result.count );
                  }

                }

                if( error == 0 ){
                  fvdSynchronizer.Prefs.set("speeddial.first_sync_after", "none");
                }

                releaseSyncLock( function(){
                  callback( error );
                } );

              }, stateCallback, {
                overwrite: true
              } );

            }
          ] );

        } );
      };

      this.hasUpdates = function( callback ){
        startSync();

        hasUpdates( function(){

          endSync();

          callback.apply( window, arguments );

        } );
      };

      this.uploadUpdates = function( callback, stateCallback ){
        startSync();
        uploadUpdates( function( error, _result ){
          endSync();
          callback( error, _result );
        }, stateCallback );

      };

      this.applyServerUpdates = function( groupsLastUpdateTime, dialsLastUpdateTime, callback, stateCallback ) {
        startSync();
        applyServerUpdates( groupsLastUpdateTime, dialsLastUpdateTime, function( error ){
          endSync();
          callback( error );
        }, stateCallback );
      };

      this.connectToSpeeddial = function(){
        connectToSpeeddial();
      };

      this.hasToSyncData = function(){

        var toSyncData = getToSyncData();

        var has = false;

        Object.keys(toSyncData).forEach(function(key) {
          if( toSyncData[key].length > 0 ){
            has = true;
          }
        });

        return has;

      };

      /* update sync states */

      this.isSyncNow = function(){
        return isSyncNow || this.getMainSyncState() == "sync";
      };

      this.setSyncNow = function( state ){
        if( state ){
          startSync();
        }
        else{
          endSync();
        }
      };


      /* call main sync state listeners */

      function removeChangeMainSyncStateListener( listener ){

        var index = changeMainSyncStateListeners.indexOf( listener );

        if( index != -1 ){

          changeMainSyncStateListeners.splice( index, 1 );
        }

      }

      function callChangeMainSyncStateListeners() {
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

      function startSync(){
        isSyncNow = true;
        fvdSynchronizer.Server.Sync.syncStateRefresh();
        fvdSynchronizer.Observer.fireEvent( "syncStart", ["SpeedDial"] );

        messageToSD({
          action: "syncStartNotification"
        });
      }

      function endSync(){
        isSyncNow = false;
        fvdSynchronizer.Observer.fireEvent( "syncEnd", ["SpeedDial"] );
        fvdSynchronizer.Server.Sync.syncStateRefresh();

        messageToSD({
          action: "syncEndNotification"
        });
      }

      function checkSyncAbort(){
        return abortCurrentSync;
      }

      function trackSync(type) {
        currentSyncType = type
        currentSyncId = new Date().toUTCString()
      }

      function getTrackingData() {
        return {
          _sync_type: currentSyncType,
          _sync_id: currentSyncId
        }
      }

      /* Messaging with server */

      function makeRequest(data, callback){
        Object.assign(data, getTrackingData())
        fvdSynchronizer.Server.Sync.authorizedMessage( data, callback );
      }

      /* transactions */

      function startTransaction( callback ){
        messageToSD( {action: "startTransaction"}, function( response ){
          callback( response.transId );
        } );
      }

      function commitTransaction( transId, callback ){
        messageToSD( {action: "commitTransaction", transId: transId}, callback );
      }

      function rollbackTransaction( transId, callback ){
        messageToSD( {action: "rollbackTransaction", transId: transId}, callback );
      }

      /* to sync data functions */

      function getToSyncData() {
        var result = null;

        try {
          var str = fvdSynchronizer.Prefs.get("speeddial.to_sync_data");

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

      function setToSyncData(toSyncData) {
        fvdSynchronizer.Prefs.set("speeddial.to_sync_data", JSON.stringify(toSyncData));

        messageToSD( {
          action: "toSyncDataChanged",
          toSyncData: toSyncData
        } );

        fvdSynchronizer.Server.Sync.syncStateRefresh();
      }

      function clearToSyncData() {
        setToSyncData(_initToSyncData);
      }

      /* Pending requests to speeddial */

      function nextPendingRequestId(){
        _pendingRequestLastId++;
        return _pendingRequestLastId;
      }

      function addPendingRequest( callback ){
        var id = nextPendingRequestId();

        _pendingSDRequests.push( {
          id: id,
          callback: callback,
          time: new Date().getTime()
        } );

        return id;
      }

      function getPendingRequest(id) {
        var request = null;

        for( var i = 0; i != _pendingSDRequests.length; i++ ){
          if( id == _pendingSDRequests[i].id ){
            request = _pendingSDRequests[i];
            break;
          }
        }

        return request;
      }

      function removePendingRequestById(id) {
        var index = null;
        for( var i = 0; i != _pendingSDRequests.length; i++ ){
          if( _pendingSDRequests[i] == id ){
            index = i;
            break;
          }
        }

        if( index !== null ){
          _pendingSDRequests.splice( index, 1 );
        }
      }

      function clearTimedoutPendingRequests() {
        var toRemoveIds = [];

        var now = new Date().getTime();

        _pendingSDRequests.forEach( function( request ){

          if( now - request.time >= PENDING_REQUESTS_TIMEOUT ){
            toRemoveIds.push( request.id );
          }

        } );

        toRemoveIds.forEach(function( id ){
          removePendingRequestById( id );
        });
      }

      /* mapping from server dials to local dials and local dials -> server dials */

      function mapDialsLocalToServer( localDials, extra ){
        extra = extra || {};

        var serverDials = [];

        var mapThumbSourceType = {
          "screen": "url",
          "url": "force_url",
          "local_file": "local_file"
        };

        localDials.forEach( function(localDial) {

          var serverDial = {
            url: localDial.url,
            title: localDial.title ? localDial.title : localDial.auto_title,
            thumb_source_type: mapThumbSourceType[localDial.thumb_source_type],
            position: localDial.position,
            hand_changed: localDial.title ? 1 : 0,
            thumb_width: localDial.thumb_width,
            thumb_height: localDial.thumb_height,
            thumb_url: localDial.thumb_url == null ? "" : localDial.thumb_url,
            global_id: localDial.global_id,
            group_global_id: localDial.group_global_id,
            local_file_type: localDial.get_screen_method == "manual" ? "manual_crop" : "custom",
            update_interval: localDial.update_interval
          };
          var isManuallyCroppedPreview = localDial.get_screen_method == "manual" && localDial.thumb_source_type == "screen";
          if( localDial.thumb_source_type == "local_file" || isManuallyCroppedPreview ){
            if(typeof localDial.need_sync_screen == "undefined" || localDial.need_sync_screen || extra.forceStorePic) {
              try {
                serverDial._previewContent = localDial.thumb;
              }
              catch( ex ){

              }
            }
          }
          if(isManuallyCroppedPreview) {
            //for manually cropped preview, thumb_source_type should equal "local_file", not "url"
            serverDial.thumb_source_type = "local_file";
          }

          serverDials.push( serverDial );

        } );

        return serverDials;
      }

      function mapDialsServerToLocal(serverDials) {

        var localDials = [];

        var mapThumbSourceType = {
          "screen": "screen",
          "url": "screen",
          "force_url": "url",
          "local_file": "local_file"
        };

        serverDials.forEach(function(serverDial) {
          var localDial = {
            url: serverDial.url,
            title: serverDial.hand_changed == 1 ? serverDial.title : "",
            auto_title: serverDial.hand_changed == 0 ? serverDial.title : "",
            thumb_source_type: mapThumbSourceType[serverDial.thumb_source_type],
            position: serverDial.position,
            thumb_width: serverDial.thumb_width,
            thumb_height: serverDial.thumb_height,
            thumb_url: serverDial.thumb_url,
            global_id: serverDial.global_id,
            group_global_id: serverDial.group_global_id,
            last_change_by: serverDial.last_change_by,
            update_interval: serverDial.update_interval
          };

          var getScreenMethod = "auto";
          // setting "screen" for manual crop is not a good idea, really need to set "local_file"
          // use "screen" for backward compatibility
          if( (localDial.thumb_source_type == "screen" || localDial.thumb_source_type == "local_file") &&
            serverDial.local_file_type == "manual_crop" ) {
            localDial.thumb_source_type = "screen";
            getScreenMethod = "manual";
          }

          localDial.get_screen_method = getScreenMethod;

          if( serverDial._previewContent ){
            localDial.thumb = "data:image/png;base64," + serverDial._previewContent;
          }
          else if( serverDial._previewUrl ){
            localDial._previewUrl = serverDial._previewUrl;
          }

          localDials.push( localDial );
        } );

        return localDials;
      }

      /* Syncing */

      /*
       *
       * Download changed data from server and saves it locally
       *
       * */

      function applyServerUpdates( groupsLastUpdateTime, dialsLastUpdateTime, callback, stateCallback ){
        // get data from server

        var anyChangesMaked = false; // sign of new data updated/created

        function changeState( state, data ){
          if(stateCallback) {
            stateCallback(state, data);
          }
        }
        var toSyncData = getToSyncData();
        var needFixGroupsPositions = false; // if have groups updates need fix positions
        var needFixDialsPositionsGroupsIds = []; // if have dials updates need fix it's positions

        var groupsDownloadCount = 0; // include all affected groups, maybe changed, updated, or removed
        var dialsDownloadCount = 0;  // include all affected dials, maybe changed, updated, or removed

        fvdSynchronizer.Utils.Async.chain([
          function( chainCallback ) {
            if(checkSyncAbort()) {
              callback( Errors.ERROR_SYNC_USER_ABORTED );
              return;
            }

            changeState( "syncGroups" );

            // get groups and save if new found

            var currentLastUpdateTime = groupsLastUpdateTime || getGroupsLastUpdateTime();

            getGroupsFromServer( currentLastUpdateTime, function( error, result ){

              if( error != 0 ){
                callback( error );
              }
              else{
                var groups, lastUpdateTime;
                try{
                  groups = result.groups;
                  lastUpdateTime = result.lastUpdateTime;
                }
                catch( ex ){
                  callback( Errors.ERROR_SERVER_RESPONSE_MALFORMED );
                  return;
                }

                groupsDownloadCount += groups.length;

                if(groups.length > 0) {
                  anyChangesMaked = true;
                  needFixGroupsPositions = true;
                }

                fvdSynchronizer.Utils.Async.chain([
                  function(chainCallback2) {
                    // check conflicted groups positions
                    if(!toSyncData.groups.length) {
                      return chainCallback2();
                    }
                    var positionsServer = {};
                    groups.forEach(function(group) {
                      positionsServer[group.position] = group.global_id;
                    });
                    messageToSD({
                      action: "queryGroups",
                      where: ["`global_id` IN ('" + toSyncData.groups.join("','") + "')"]
                    }, function(response) {
                      // compare local changed groups positions and server positions
                      // if we have positions conflict, need to upload the server group again with its new position
                      if(response.list) {
                        response.list.forEach(function(clientGroup) {
                          var conflictServerGlobalId = positionsServer[clientGroup.position];
                          if(conflictServerGlobalId) {
                            console.log("Got a conflict on position", clientGroup.position);
                            syncData("groups", conflictServerGlobalId);
                          }
                        });
                      }
                      chainCallback2();
                    });
                  },
                  function(chainCallback2) {
                    fvdSynchronizer.Utils.Async.arrayProcess(groups, function(group, arrayProcessCallback) {
                      messageToSD({
                        action: "saveGroup",
                        group: group
                      }, function(){
                        arrayProcessCallback();
                      });
                    }, function() {
                      if(currentLastUpdateTime != lastUpdateTime) {
                        setGroupsLastUpdateTime(lastUpdateTime);
                      }
                      chainCallback2();
                    });
                  },
                  function(chainCallback2) {
                    if( result.groupIds && result.groupIds.length > 0 ) {
                      // remove groups which is not in list
                      anyChangesMaked = true;
                      // don't remove new groups
                      result.groupIds = result.groupIds.concat(toSyncData.newGroups);
                      var serverGroupIds = result.groupIds;
                      messageToSD({
                        action: "removeGroupsNotInList",
                        groupsIds: result.groupIds
                      }, function( result ) {
                        var countRemovedGroups = result.removed;

                        if( countRemovedGroups > 0 ) {
                          groupsDownloadCount += countRemovedGroups;
                          needFixGroupsPositions = true;
                        }
                        // remove from changed groups if removed on server
                        if(toSyncData.groups && toSyncData.groups.length) {
                          toSyncData.groups.forEach(function(global_id) {
                            if(serverGroupIds.indexOf(global_id) == -1) {
                              removeSyncData(["groups"], global_id);
                            }
                          });
                        }
                        chainCallback2();
                      });

                    }
                    else{
                      chainCallback2();
                    }

                  },
                  function(){
                    chainCallback();
                  }
                ]);
              }
            } );
          },
          function(chainCallback) {
            // get dials and save if new found

            if( checkSyncAbort() ){
              return;
            }

            changeState( "syncDials", {
              groupsCount: groupsDownloadCount
            } );

            //dump( "Start update dials\n" );

            var currentLastUpdateTime = dialsLastUpdateTime || getDialsLastUpdateTime();

            getDialsFromServer( {
              lastUpdateTime: currentLastUpdateTime
            }, function( error, result ){
              if( error != 0 ){
                callback( error );
              }
              else{

                try{
                  var dials = result.dials;
                  var lastUpdateTime = result.lastUpdateTime;
                }
                catch( ex ){
                  callback( Errors.ERROR_SERVER_RESPONSE_MALFORMED );
                  return;
                }

                //dump( "Found new dials: " + dials.length + "\n" );

                dialsDownloadCount += dials.length;

                if( dials.length > 0 ){
                  anyChangesMaked = true;
                }

                fvdSynchronizer.Utils.Async.chain([
                  function( chainCallback2 ){

                    fvdSynchronizer.Utils.Async.arrayProcess( dials, function( dial, arrayProcessCallback ){

                      messageToSD({
                        action: "saveDial",
                        dial: dial
                      }, function( result ){

                        var saveInfo = result.saveInfo;

                        if( saveInfo ){

                          if( saveInfo.move ){
                            needFixDialsPositionsGroupsIds.push( saveInfo.move.from );
                            needFixDialsPositionsGroupsIds.push( saveInfo.move.to );
                          }
                          else{
                            needFixDialsPositionsGroupsIds.push( saveInfo.group_id );
                          }

                        }

                        arrayProcessCallback();

                      });


                    }, function(){
                      if( currentLastUpdateTime != lastUpdateTime ){
                        setDialsLastUpdateTime( lastUpdateTime );
                      }

                      chainCallback2();
                    } );

                  },
                  function(chainCallback2){

                    if( result.dialIds){
                      anyChangesMaked = true;
                      // don't remove new dials
                      result.dialIds = result.dialIds.concat(toSyncData.newDials);
                      messageToSD( {
                        action: "removeDialsNotInList",
                        dialsIds: result.dialIds
                      }, function( response ){

                        var removeInfo = response.removeInfo;

                        if( removeInfo.count > 0 ){
                          dialsDownloadCount += removeInfo.count;
                          needFixDialsPositionsGroupsIds = needFixDialsPositionsGroupsIds.concat( removeInfo.removedFromGroups );
                        }

                        chainCallback2();

                      } );


                    }
                    else{
                      chainCallback2();
                    }
                  },
                  function(){
                    chainCallback();
                  }
                ]);
              }
            } );
          },
          function(chainCallback) {
            changeState("applyChanges", {
              dialsCount: dialsDownloadCount
            });

            // fix groups positions if need
            if(needFixGroupsPositions) {
              messageToSD({
                action: "fixGroupsPositions"
              }, function(){
                chainCallback();
              });
            }
            else{
              chainCallback();
            }
          },
          function(chainCallback) {
            // fix dials position
            if( needFixDialsPositionsGroupsIds.length > 0 ){
              needFixDialsPositionsGroupsIds = fvdSynchronizer.Utils.arrayUnique( needFixDialsPositionsGroupsIds );

              fvdSynchronizer.Utils.Async.arrayProcess( needFixDialsPositionsGroupsIds, function( groupId, arrayProcessCallback ){

                if( groupId ){
                  messageToSD({
                    action: "fixDialsPositions",
                    groupId: groupId
                  }, function(){
                    arrayProcessCallback();
                  });
                }
                else{
                  arrayProcessCallback();
                }

              }, function(){
                chainCallback();
              } );

            }
            else{
              chainCallback();
            }
          },

          function(){
            if( dialsDownloadCount > 0 || groupsDownloadCount > 0 ){
              fvdSynchronizer.Driver.Speeddial.Backup.setHasChanges()
            }

            callback(0, anyChangesMaked);
          }
        ]);
      }

      function removeDialsFromServer( dialIds, callback ){
        var request = {
          action: "remove_dials",
          dialIds: dialIds
        };

        makeRequest( request, function( error, data ){

          if( error == 0 ){
            setDialsLastUpdateTime( data.lastUpdateTime );
          }
          callback( error );

        } );
      }

      function removeGroupsFromServer( groupIds, callback ){
        var request = {
          action: "remove_groups",
          groupIds: groupIds
        };

        makeRequest( request, function( error, data ){

          if( error == 0 ){
            setGroupsLastUpdateTime( data.lastUpdateTime );
          }
          callback( error );

        } );
      }


      /*
       *
       * Upload updated dials and groups data to server
       *
       */
      function uploadUpdates(callback, stateCallback) {
        var toSyncData = getToSyncData();

        var groupsUploadedCount = 0;
        var dialsUploadedCount = 0;
        var serverMissedGroups = []

        function changeState( state, countObjectsSynced ){
          if( stateCallback ){
            stateCallback( state, countObjectsSynced );
          }
        }

        fvdSynchronizer.Utils.Async.chain([
          function( chainCallback ){
            changeState( "syncGroups" );
            // check need remove groups from server
            if(toSyncData.deleteGroups.length > 0) {
              groupsUploadedCount += toSyncData.deleteGroups.length;

              removeGroupsFromServer(toSyncData.deleteGroups, function(error) {
                if(error) {
                  return callback(error);
                }
                toSyncData.deleteGroups = [];
                chainCallback();
              });
            }
            else{
              chainCallback();
            }
          },

          function(chainCallback) {
            // check need sync groups with server
            if(toSyncData.groups.length > 0) {
              messageToSD({
                action: "queryGroups",
                where: ["`global_id` IN ('"+toSyncData.groups.join("','")+"')"]
              }, function(response) {
                var groups = response.list;

                groupsUploadedCount += groups.length;

                toSyncData.groups = [];
                if(!groups.length) {
                  return chainCallback();
                }
                putGroupsListOnserver( groups, function( error, data ){
                  if( error ){
                    return callback( error, {
                      count: groups.length
                    } );
                  }
                  chainCallback();
                } );
              });
            }
            else{
              chainCallback();
            }
          },
          function(chainCallback) {
            changeState( "syncDials", {
              groupsCount: groupsUploadedCount
            } );

            // check need remove dials from server
            if( toSyncData.deleteDials.length > 0 ){

              dialsUploadedCount += toSyncData.deleteDials.length;

              removeDialsFromServer( toSyncData.deleteDials, function( error ){
                if( error ){
                  return callback( error );
                }

                toSyncData.deleteDials = [];
                chainCallback();
              } );
            }
            else{
              chainCallback();
            }
          },
          function(chainCallback) {
            // timeout for UI
            setTimeout(function() {
              chainCallback();
            }, 500);
          },
          function(chainCallback) {
            // check need sync dials with server
            if(toSyncData.dials.length > 0) {
              queryDials({
                where: ["`dials`.`global_id` IN ('"+toSyncData.dials.join("','")+"')"],
                fetchThumb: false
              }, function(response) {

                var dials = response.list;

                dialsUploadedCount += dials.length;

                toSyncData.dials = [];

                putDialsListOnserver(dials, function(error, data) {
                  if(error) {
                    return callback(error, {
                      count: dials.length
                    });
                  }
                  if(data.serverMissedGroups && data.serverMissedGroups.length) {
                    serverMissedGroups = data.serverMissedGroups
                  }
                  chainCallback()
                });
              });
            }
            else{
              chainCallback();
            }
          },
          function(chainCallback) {
            if(!serverMissedGroups && serverMissedGroups.length) {
              return chainCallback()
            }
            // we are here because in the result of put_dials server said that it doesn't have groups for some of the dials
            // it is a fix for it, we just upload all groups on the server.
            // However this doesn't fix the main issue why the groups are not uploaded on the server, perhaps it was because
            // of the race condition in syncData when the groups were added during the main sync process. Now that is fixed.
            console.log("Reupload all groups because server has missed:", serverMissedGroups)
            messageToSD({
              action: "queryGroups"
            }, function(response) {
              var groups = response.list;
              if(!groups.length) {
                return chainCallback();
              }
              putGroupsListOnserver(groups, function(error, data) {
                // skip if error happens
                chainCallback();
              });
            });
          },
          function( chainCallback ){
            changeState( "applyChanges", {
              dialsCount: dialsUploadedCount
            } );

            chainCallback();
          },
          function( chainCallback ){
            // timeout for UI
            setTimeout( function(){
              chainCallback();
            }, 500 );
          },
          function(){
            callback( 0 );
          }
        ]);
      }

      function hasUpdates(callback) {
        var request = {
          action: "found_changes",
          // because server wants value > 0 we need check last udpate time, and set 1, to prevent error
          lastUpdateTimeDials: getDialsLastUpdateTime() || 1,
          lastUpdateTimeGroups: getGroupsLastUpdateTime() || 1
        };
        makeRequest(request, function(error, response){
          if (!error) {
            callback(error, response.foundChanges, {
              lastUpdateTimeDials: response.lastUpdateTimeDials,
              lastUpdateTimeGroups: response.lastUpdateTimeGroups,
              localLastUpdateTimeDials: getDialsLastUpdateTime(),
              localLastUpdateTimeGroups: getGroupsLastUpdateTime()
            });
          }
          else {
            callback(error);
          }
        });
      }

      // dials
      function getDialsFromServer(params, callback) {
        var params = params || {};

        var request = {
          action: "list_dials"
        };

        for( var k in params ){
          request[k] = params[k];
        }

        makeRequest(request, function(error, data) {
          if(error == 0) {
            data.dials = mapDialsServerToLocal( data.dials );
          }
          callback( error, data );
        });
      }

      function putDialsListOnserver(dials, callback, extra) {
        extra = extra || {};
        // if server says that it doesn't have groups for some of the dials we send in this function
        // this array will be filled with ids of these groups
        var serverMissedGroups = []

        // prepare dial data and remove not needed fields

        dials.forEach(function( dial ){
          delete dial.rowid;
          delete dial.deny;
          delete dial.group_id;
          delete dial.clicks;
          delete dial.status;
          delete dial.restore_previous_thumb;
          delete dial.ignore_restore_previous_thumb;
          delete dial.thumb_update_date;
          delete dial.thumb_src;
        });

        var preDials = mapDialsLocalToServer(dials, extra);

        var sendData = {
          action: "put_dials",
          dials: preDials
        };

        var uploadError = 0;

        var resetSyncManualPreviewDialsIds = [];
        var putDialsServerResponse

        fvdSynchronizer.Utils.Async.chain([
          function(chainCallback) {
            fvdSynchronizer.Utils.Async.arrayProcess(preDials, function(dial, apCallback) {
              if(dial._previewContent && typeof dial._previewContent === "string") {
                fvdSynchronizer.Utils.Async.chain([
                  function(next) {
                    if(dial._previewContent.indexOf("filesystem:") === 0) {
                      console.log("Need to resolve filesystem thumb");
                      queryDialThumb(dial.global_id, function(thumb) {
                        console.log("Received thumb");
                        dial._previewContent = thumb;
                        next();
                      });
                    }
                    else {
                      console.log("No need to query thumb, seems it already in needed format");
                      next();
                    }
                  },
                  function() {
                    if(!dial._previewContent) {
                      console.info('Fail to receive thumb, received empty:', dial._previewContent);
                      return apCallback();
                    }
                    var blob = fvdSynchronizer.Utils.dataURItoBlob(dial._previewContent);
                    fvdSynchronizer.Server.Sync.preUploadFile({
                      blob: blob,
                      name: UPLOAD_PREVIEW_FILENAME
                    }, function( error, data ){
                      if(error) {
                        return callback(error);
                      }
                      if(!data || !data.files) {
                        return callback( fvdSynchronizer.Errors.ERROR_STORAGE_ENGINE_RETURNS_ERROR );
                      }
                      var filesMap = data.files;
                      if(!filesMap.file) {
                        return callback( fvdSynchronizer.Errors.ERROR_STORAGE_ENGINE_RETURNS_ERROR );
                      }
                      dial._tempfilename = filesMap.file;
                      delete dial._previewContent;

                      resetSyncManualPreviewDialsIds.push( dial.global_id );

                      apCallback();
                    });
                  }
                ]);
              }
              else{
                apCallback();
              }
            }, function(){
              chainCallback();
            } );
          },

          function( chainCallback ){
            makeRequest(sendData, function(error, result) {
              if( error == 0 ){
                setDialsLastUpdateTime( result.lastUpdateTime );
              }
              putDialsServerResponse = result
              uploadError = error;

              chainCallback();
            });
          },
          function(chainCallback) {
            if(
              !putDialsServerResponse.extra ||
              !putDialsServerResponse.extra.notFoundGroups ||
              !Array.isArray(putDialsServerResponse.extra.notFoundGroups) ||
              !putDialsServerResponse.extra.notFoundGroups.length
            ) {
              return chainCallback()
            }
            // server notices us that some dials were created, but groups for them didn't exist on the server
            let nfGroupsGlobalIds = putDialsServerResponse.extra.notFoundGroups.map(group => group.global_id)
            serverMissedGroups = nfGroupsGlobalIds
            // don't cause any slowdown on sync process, log asynchronously
            setTimeout(function() {
              let toSend = {}
              fvdSynchronizer.Utils.Async.chain([
                next => {
                  messageToSD({
                    action: "countItems"
                  }, res => {
                    toSend.count = res
                    next()
                  })
                },
                next => {
                  messageToSD({
                    action: "queryGroups"
                  }, function(response) {
                    groups = response.list
                    toSend.groups = {
                      notFoundGroups: nfGroupsGlobalIds,
                      localGroups: groups
                    }
                    next()
                  })
                },
                () => {
                  // report not found groups to the server
                  let xhr = new XMLHttpRequest()
                  xhr.open('POST', 'https://everhelper.me/spec/nogroups_put_dials_dbg.php')
                  xhr.send(JSON.stringify(toSend))
                }
              ])
            }, 0)
            chainCallback()
          },

          function( chainCallback ){
            if( resetSyncManualPreviewDialsIds.length != 0 ){
              messageToSD( {
                action: "updateMass",
                globalIds: resetSyncManualPreviewDialsIds,
                data: {
                  need_sync_screen: 0
                }
              }, function(){
                chainCallback();
              } );
            }
            else{
              chainCallback();
            }
          },

          function(){
            callback(uploadError, {serverMissedGroups});
          }

        ]);
      }

      // groups
      function getGroupsFromServer( params, callback ){
        var lastUpdateTime = null;

        if( typeof params == "object" && params ){
          lastUpdateTime = params.lastUpdateTime;
        }
        else{
          lastUpdateTime = params;
          params = {};
        }

        var request = {
          action: "list_groups"
        };

        for( var k in params ){
          request[k] = params[k];
        }

        if( lastUpdateTime ){
          request.lastUpdateTime = lastUpdateTime;
        }

        makeRequest( request, callback );
      }


      function putGroupsListOnserver( groups, callback ){
        groups.forEach(function( group ){
          delete group.rowid;
        });

        var sendData = {
          action: "put_groups",
          groups: groups
        };

        makeRequest(sendData, function( error, result ){
          if( error == 0 ){
            setGroupsLastUpdateTime( result.lastUpdateTime );
          }

          callback( error );
        });
      }

      function mergeLocalAndServerData(params, callback, stateCallback){
        params = params || {};

        // try to get groups list for update, and groups to save on server
        var newGroups = [];
        var toServerGroups = [];
        var collisedGroups = [];
        var collisedGroupsTranslateGlobalIds = {}; // key is old global_id, value is new global_id
        var newDials = [];
        var toServerDials = [];
        var collisedDials = []; // dials collised by urls, client dials will be replaced with server dials with such urls and such groups
        var dialsToReplaceIds = []; // list of client dials to replace with server dials
        var dialsToOverrideIds = []; // list of server dials to override collisions
        var clientGroups = [];
        var serverGroups = [];
        var clientDials = [];
        var serverDials = [];

        var lastUpdateGroups = null;
        var lastUpdateDials = null;

        //dump( "\n----------------\n\n" );

        var syncedDialsCount = 0;
        var syncedGroupsCount = 0;

        var localDatabaseAffected = false;

        if( params.groupGlobalId ){
          collisedGroupsTranslateGlobalIds[ params.groupGlobalId ] = params.groupGlobalId;
        }

        function changeState(newState, data){
          if (stateCallback) {
            stateCallback(newState, data);
          }
        }

        var needFixGroups = []; // groups where dials need to fix positionss

        var transId = null;

        fvdSynchronizer.Utils.Async.chain([

        function(chainCallback){
          changeState("syncGroups");

          if( params.groupId ){
            // no need to get groups list, sync only one group dials
            chainCallback();
          }
          else{
            messageToSD({
              action: "queryGroups",
              where: ["`global_id` IS NOT NULL"],
              transId: transId
            }, function( response ){

                clientGroups = response.list;


                chainCallback();

            });
          }
        },
        function(chainCallback) {
          if (params.groupId) {
            // no need to get groups list, sync only one group dials
            chainCallback();
          }
          else{
            getGroupsFromServer(null, function(error, serverResponse){
              if (error != 0) {
                callback(error);
              }
              else {
                try {
                  serverGroups = serverResponse.groups;
                  lastUpdateGroups = serverResponse.lastUpdateTime;
                }
                catch (ex) {
                  console.log("EX: " + ex + "\n");
                  callback(Errors.ERROR_SERVER_RESPONSE_MALFORMED);
                  return;
                }
                chainCallback();
              }
            });
          }
        },
        function(chainCallback) {
          if (checkSyncAbort()) {
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }

          // compare server groups with client groups
          function isGroupInList(groupName, list){
            for (var i = 0; i != list.length; i++) {
              if (list[i].name == groupName) {
                return list[i];
              }
            }
            return null;
          }

          // toServerGroups filling
          clientGroups.forEach(function(clientGroup) {
            var collisedGroup = isGroupInList(clientGroup.name, serverGroups);

            if (!collisedGroup) {
              toServerGroups.push(clientGroup);
            }
            else {
              collisedGroupsTranslateGlobalIds[clientGroup.global_id] = collisedGroup.global_id;

              collisedGroups.push({
                clientGroup: clientGroup,
                serverGroup: collisedGroup
              });
            }
          });

          // newGroups filling
          serverGroups.forEach(function(serverGroup){
            if (!isGroupInList(serverGroup.name, clientGroups)) {
              newGroups.push(serverGroup);
            }
          });

          chainCallback();
        },
        function(chainCallback) {
          if (checkSyncAbort()) {
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }

          syncedGroupsCount = newGroups.length + toServerGroups.length;

          var where = ["`dials`.`global_id` IS NOT NULL"];
          if(params.groupId) {
            where.push( "`group_id` = " + params.groupId );
          }

          queryDials({
            where: where,
            transId: transId,
            fetchThumb: false
          }, function( response ){
            clientDials = response.list;
            chainCallback();
          });
        },
        function(chainCallback) {
          if (checkSyncAbort()) {
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }

          changeState("syncDials", {
            groupsCount: syncedGroupsCount
          });

          var getDialsParams = {};

          if( params.groupGlobalId ){
            getDialsParams.groupId = params.groupGlobalId;
          }

          getDialsFromServer(getDialsParams, function(error, serverResponse){
            if (error != 0) {
              callback(error);
            }
            else {
              try {
                serverDials = serverResponse.dials;
                lastUpdateDials = serverResponse.lastUpdateTime;
              }
              catch (ex) {
                console.log(ex);
                callback(Errors.ERROR_SERVER_RESPONSE_MALFORMED);
                return;
              }
              chainCallback();
            }
          });
        },
        function(chainCallback) {
          if (checkSyncAbort()) {
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }
          function clientDialInList(dial, list) {
            for (var i = 0; i != list.length; i++) {
              if ( fvdSynchronizer.Utils.isUrlsEqual(list[i].url, dial.url) ) {
                var groupGlobalId = collisedGroupsTranslateGlobalIds[dial.group_global_id];
                if (groupGlobalId == list[i].group_global_id && dialsToOverrideIds.indexOf(list[i].global_id) == -1) {
                  dialsToOverrideIds.push(list[i].global_id);
                  return list[i];
                }
              }
            }
            return null;
          }

          clientDials.forEach(function(clientDial) {
            var collisedDial = clientDialInList(clientDial, serverDials);
            if (collisedDial) {
              if(dialsToReplaceIds.indexOf(clientDial.global_id) == -1) {
                  dialsToReplaceIds.push(clientDial.global_id);
                  collisedDials.push({
                    clientDial: clientDial,
                    serverDial: collisedDial
                  });
              }
              else {
                collisedDial = null;
              }
            }
            if (!collisedDial) {
              toServerDials.push(clientDial);
            }
          });

          serverDials.forEach(function(serverDial){
            if (dialsToOverrideIds.indexOf(serverDial.global_id) == -1) {
              newDials.push(serverDial);
            }
          });

          chainCallback();
        },
        function(chainCallback) {
          if (checkSyncAbort()) {
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }

          syncedDialsCount = newDials.length + toServerDials.length;

          changeState("applyChanges", {
            dialsCount: syncedDialsCount
          });

          // update collised groups

          fvdSynchronizer.Utils.Async.arrayProcess(collisedGroups, function(collision, arrayProcessCallback) {
            messageToSD({
              action: "mergeUpdateCollisedGroup",
              clientGroup: collision.clientGroup,
              serverGroup: collision.serverGroup,
              transId: transId
            }, function(){
                arrayProcessCallback();
            });
          }, function(){
            chainCallback();
          });
        },
        function(chainCallback) {
          if (checkSyncAbort()) {
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }

          // create new groups
          fvdSynchronizer.Utils.Async.arrayProcess(newGroups, function(newGroup, arrayProcessCallback) {
            messageToSD({
              action: "saveGroup",
              group: newGroup,
              transId: transId
            }, function() {
              arrayProcessCallback();
            });
          }, function(){
            chainCallback();
          });
        },
        function(chainCallback) {
          if (checkSyncAbort()) {
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }

          // save groups to server
          if (toServerGroups && toServerGroups.length) {
            putGroupsListOnserver(toServerGroups, function( error ){
              if(error) {
                return callback( error, {
                  count: toServerGroups.length
                } );
              }
              chainCallback();
            });
          }
          else {
            chainCallback();
          }
        },
        function(chainCallback) {
          console.log("Store collised dials");

          if (checkSyncAbort()) {
            console.log("sync aborted")
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }

          console.log("sync is not aborted")

          // update collised dials
          console.log("start each of collissions")
          fvdSynchronizer.Utils.Async.arrayProcess(collisedDials, function(collision, arrayProcessCallback){
            console.log("Process collision", collision)
            messageToSD({
              action: "mergeUpdateCollisedDial",
              clientDial: collision.clientDial,
              serverDial: collision.serverDial,
              transId: transId
            }, function(response) {
              console.log("got a response from SD", response)
              if( response.saveInfo ) {

                if( response.saveInfo.move ){
                  needFixGroups.push( response.saveInfo.move.from );
                  needFixGroups.push( response.saveInfo.move.to );
                }
                else{
                  needFixGroups.push( response.saveInfo.group_id );
                }

              }

              arrayProcessCallback();
            });

          }, function(){
            chainCallback();
          });
        },
        function(chainCallback){
          console.log("Store new dials");

          if (checkSyncAbort()) {
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }

          // add new dials
          fvdSynchronizer.Utils.Async.arrayProcess(newDials, function(newDial, arrayProcessCallback){
            console.log("Save dial", newDial);
            messageToSD( {
              action: "saveDial",
              dial: newDial,
              transId: transId
            }, function(response) {
              console.log("saved");

              if( response.saveInfo ){
                if( response.saveInfo.move ){
                  needFixGroups.push( response.saveInfo.move.from );
                  needFixGroups.push( response.saveInfo.move.to );
                }
                else{
                  needFixGroups.push( response.saveInfo.group_id );
                }
              }
              arrayProcessCallback();
            });
          }, function(){
            console.log("All dials saved!")
            chainCallback();
          });
        },
        function(chainCallback) {
          console.log("Put dials on server");

          if (checkSyncAbort()) {
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }

          // put dials on server
          if (toServerDials.length > 0) {
            console.log("need to put", toServerDials.length)
            // before put dials to server need to
            // replace old group global ids to new if changed

            for( var i = 0; i != toServerDials.length; i++ ){
              var d = toServerDials[i];
              var groupGlobalId = d.group_global_id;

              if( collisedGroupsTranslateGlobalIds[groupGlobalId] ) {
                d.group_global_id = collisedGroupsTranslateGlobalIds[groupGlobalId];
              }
            }
            console.log("Start put")
            putDialsListOnserver(toServerDials, function(error) {
              console.log("put finished", error)
              if (error != 0) {
                callback(error, {
                  count: toServerDials.length
                });
              }
              else {
                chainCallback();
              }
            }, {
              forceStorePic: true
            });
          }
          else {
            chainCallback();
          }
        },
        function( chainCallback ){
          // fix groups positions
          console.log("send fix positions message to SD");
          messageToSD({
            action: "fixGroupsPositions",
            transId: transId
          }, function(response) {
            console.log("got a response from sd about fix positions", response)
            chainCallback();
          });
        },
        function( chainCallback ) {
          console.log("fixing groups step")
          // fix dials positions
          // it's kind of a bug when migration I think, in some dials field group_id has string value "undefined"
          // fix it by directly filtering such values
          needFixGroups = needFixGroups.filter(function(v) {
            if(!v || v == "undefined") {
              return false;
            }
            return true;
          });

          needFixGroups = fvdSynchronizer.Utils.arrayUnique( needFixGroups );
          needFixGroups = fvdSynchronizer.Utils.arrayFilter( needFixGroups );
          console.log("process fix groups")
          fvdSynchronizer.Utils.Async.arrayProcess( needFixGroups, function( groupId, arrayProcessCallback ){
            console.log("process fix", groupId)
            messageToSD({
              action: "fixDialsPositions",
              groupId: groupId,
              transId: transId
            }, function(response){
              console.log("got a response from sd", response);
              arrayProcessCallback();
            });
          }, function(){
            chainCallback();
          } );
        },
        function() {
          localDatabaseAffected = newDials.length || newGroups.length || collisedGroups.length || collisedDials.length;

          if (checkSyncAbort()) {
            callback( Errors.ERROR_SYNC_USER_ABORTED );
            return;
          }

          // save update times
          setDialsLastUpdateTime(lastUpdateDials);
          setGroupsLastUpdateTime(lastUpdateGroups);

          // all done
          if( localDatabaseAffected ){
            fvdSynchronizer.Driver.Speeddial.Backup.setHasChanges();
          }

          callback(0);
        }

        ]);

      }

      /*
       * Remove all data from server
       */

      function clearServerData(callback) {
        var sendData = {
          action: "remove_all"
        };
        makeRequest(sendData, function( error ){
          callback( error );
        });
      }

      function acquireSyncLockCallback( errorCallback, successCallback ){
        acquireSyncLock( function( error ){
          if(error != 0) {
            errorCallback( error );
          }
          else{
            successCallback();
          }
        });
      }

      function putAllDataOnServer(callback, stateCallback, params) {
        params = params || {};

        function changeState( newState, data ){
          if( stateCallback ){
            stateCallback( newState, data );
          }
        }

        var uploadedDialsCount = 0;
        var uploadedGroupsCount = 0;

        var groups = [];
        var dials  = [];

        fvdSynchronizer.Utils.Async.chain([
          function(chainCallback) {
            changeState( "syncGroups" );

            messageToSD({
              action: "queryGroups",
              where: ["`global_id` IS NOT NULL"]
            }, function( response ){
              groups = response.list;
              chainCallback();
            });
          },
          function(chainCallback) {
            // put dials
            changeState( "syncDials", {
              groupsCount: uploadedGroupsCount
            } );

            queryDials({
              where: ["`dials`.`global_id` IS NOT NULL"],
              fetchThumb: false
            }, function( response ){
              dials = response.list;
              chainCallback();
            });
          },
          function(chainCallback) {
            if(params.overwrite) {
              fvdSynchronizer.Server.Sync.getQuotaByType("dials_count", "", function(error, max) {
                if(error) {
                  return callback( error );
                }

                if(max && (groups.length + dials.length > max)) {
                  return callback( fvdSynchronizer.Errors.ERROR_COUNT_ITEMS_QUOTA_EXCEED, {
                    count: groups.length + dials.length
                  } );
                }

                clearServerData(function(){
                  chainCallback();
                });
              } );
            }
            else{
              chainCallback();
            }
          },
          function(chainCallback) {
            putGroupsListOnserver(groups, function(error) {
              uploadedGroupsCount = groups.length;

              if( error != 0 ){
                callback( error, {
                  count: groups.length
                } );
              }
              else{
                chainCallback();
              }
            });
          },

          function(chainCallback) {
            putDialsListOnserver(dials, function(error) {
              uploadedDialsCount = dials.length;

              if( error != 0 ){
                callback( error, {
                  count: dials.length
                } );
              }
              else{
                chainCallback();
              }
            }, {
              forceStorePic: true
            } );
          },

          function() {
            changeState( "applyChanges", {
              dialsCount: uploadedDialsCount
            } );

            callback( 0 );
          }
        ]);

      }

      /* sync lock */

      function acquireSyncLock(callback) {
        fvdSynchronizer.Server.Sync.acquireSyncLock(getTrackingData(), callback);

      }

      function releaseSyncLock(callback) {
        fvdSynchronizer.Server.Sync.releaseSyncLock(getTrackingData(), callback);

      }

      /* last update time */

      function setDialsLastUpdateTime(time){
          fvdSynchronizer.Prefs.set("speeddial.last_dials_update", time);
      }

      function getDialsLastUpdateTime(){
          return fvdSynchronizer.Prefs.get("speeddial.last_dials_update");
      }

      function setGroupsLastUpdateTime(time){
          fvdSynchronizer.Prefs.set("speeddial.last_groups_update", time);
      }

      function getGroupsLastUpdateTime(time){
          return fvdSynchronizer.Prefs.get("speeddial.last_groups_update");
      }

      function getTemporaryStoredSyncData() {
        let tempData = localStorage['speeddial.temporary_sync_data']
        if(!tempData) {
          tempData = []
        }
        else {
          try {
            tempData = JSON.parse(tempData)
          }
          catch(ex) {
            tempData = []
          }
        }
        return tempData
      }

      function storeSyncDataTemporary(category, data) {
        let tempData = getTemporaryStoredSyncData()
        tempData.push({
          category,
          data
        })
        console.log("saving syncData temporary", category, data)
        localStorage['speeddial.temporary_sync_data'] = JSON.stringify(tempData)
      }

      this.restoreTemporaryStoredSyncData = function() {
        let tempData = getTemporaryStoredSyncData()
        for(let record of tempData) {
          console.log("Restoring temporary stored syncData", record.category, record.data)
          syncDataAndStore(record.category, record.data)
        }
        localStorage['speeddial.temporary_sync_data'] = '[]'
      }

      /* messaging with speeddial */

      function syncData(category, data) {
        // need to store item in different location if sync is in progress
        // because at the end of the sync this data can be overwritten because sync empties toSyncData at the end
        if(self.isSyncNow()) {
          storeSyncDataTemporary(category, data)
        }
        else {
          syncDataAndStore(category, data)
        }
      }

      function syncDataAndStore(category, data){
        if (typeof category == "string") {
          category = [category];
        }

        if (!data) {
          return;
        }

        var toSyncData = getToSyncData();

        category.forEach(function(cat){
          if(!toSyncData[cat]) {
            toSyncData[cat] = [];
          }
          try {
            if(toSyncData[cat].indexOf(data) == -1 && data != null) {
              toSyncData[cat].push(data);
            }
          }
          catch( ex ){
            console.log( "Fail add to sync data to ", cat  );
          }
        });

        fvdSynchronizer.Driver.Speeddial.Backup.setHasChanges();

        setToSyncData(toSyncData);
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

      function queryDialThumb(globalId, cb) {
        messageToSD({
          action: "queryDialThumb",
          global_id: globalId
        }, function(r) {
          cb(r.thumb);
        });
      }

      function queryDials(message, callback) {
        message.action = "queryDials";
        var originalWhere = null;
        if(message.where) {
          originalWhere = message.where;
        }
        // get 10 dials per IPC request
        message.limit = 10;
        var result = {
          list: []
        };
        var lastId = 0;
        fvdSynchronizer.Utils.Async.cc(function(next) {
          var where = [];
          if(originalWhere) {
            where = fvdSynchronizer.Utils.objectClone(originalWhere);
          }
          where.push("dials.rowid > " + lastId);
          console.log("RUN", where);
          message.where = where;
          messageToSD(message, function(r) {
            var list = r.list;
            console.log("got sub request dials", list.length);
            for(var i = 0; i != list.length; i++) {
              var dial = list[i];
              result.list.push(dial);
              if(dial.rowid > lastId) {
                lastId = dial.rowid;
              }
            }
            if(list.length < message.limit) {
              // no more dials in the speeddial, return
              callback(result);
            }
            else {
              // perhaps the speeddial has more dials to return, go to next request
              next();
            }
          });
        });
      }

      function uploadTempData(data, cb) {
        var dataStr = JSON.stringify(data);
        // each upload chunk size is 10mb
        var chunkSize = 10 * 1024 * 1024;
        var chunks = fvdSynchronizer.Utils.splitString(dataStr, chunkSize);
        var tempDataId = fvdSynchronizer.Utils.generateGUID();
        var chunksStored = 0;
        fvdSynchronizer.Utils.Async.arrayProcess(chunks, function(chunk, next) {
          console.log("Save chunk", chunksStored, chunks.length);
          messageToSD({
            action: "saveTempData",
            data: {
              id: tempDataId,
              value: chunk
            }
          }, function() {
            chunksStored++;
            next();
          });
        }, function() {
          cb(null, tempDataId);
        });
      }

      function messageToSD(message, callback) {
        connectToSpeeddial();

        if( callback ){
          var requestId = addPendingRequest( callback );
          message.requestId = requestId;
        }

        port.postMessage(message);
      }

      function _setAllow(value, apiv) {
        allow = value;
        apiVersion = apiv;
        fvdSynchronizer.Observer.fireEvent( "syncDriverAllowedStateChanged", [
          "SpeedDial"
        ] );
      }

      function portMessageProcess(message) {
        if(!message.action) {
          console.log("Fail process", message, "Action not found");
        }

        switch (message.action) {
          case "startSync":
            if(isSyncNow) {
              if(message.requestId) {
                port.postMessage({
                  action: "_response",
                  requestId: message.requestId,
                  state: "sdSyncActive"
                });
              }
              return;
            }

            if(fvdSynchronizer.Server.Sync.isSyncNow()) {
              if(message.requestId) {
                port.postMessage( {
                  action: "_response",
                  requestId: message.requestId,
                  state: "syncActive"
                } );
              }

              return;
            }

            switch(message.type) {
              case "main":
                fvdSynchronizer.Server.Sync.activityState(function(state) {
                  if(state != "logged") {
                    chrome.tabs.create({
                      url: chrome.extension.getURL( "options.html#login" ),
                      active: true
                    });
                  }
                  else if( self.getFirstSyncAfter() == "none" ){
                    self.startMainSync();
                  }
                  else{
                    chrome.tabs.create({
                      url: chrome.extension.getURL( "options.html#sync" ),
                      active: true
                    });
                  }

                  if(message.requestId) {
                    port.postMessage( {
                      action: "_response",
                      requestId: message.requestId,
                      state: "ok"
                    } );
                  }
                });
              break;
            }

          break;

          case "restoreBackupProgress":
            self.Backup.setRestoreBackupProgress( message.current, message.max );
          break;

          case "needInitialSync":
            fvdSynchronizer.Prefs.set("speeddial.first_sync_after", "install");
          break;

          case "deactivate":
            console.log( "SpeedDial request deactivation" );
            _setAllow( false );
          break;

          case "activate":
            console.log("SpeedDial driver activated");
            message.apiv = message.apiv || 1;
            _setAllow(true, message.apiv);

            chrome.extension.sendRequest({
              subject: "speedDialConnected"
            });
          break;


          case "hasToSyncData":
            var has = self.hasToSyncData();

            if( message.requestId ){

              port.postMessage( {
                action: "_response",
                requestId: message.requestId,
                has: has
              } );

            }
          break;

          case "addDataToSync":
            syncData(message.category, message.data);

            if(message.requestId) {
              port.postMessage( {
                action: "_response",
                requestId: message.requestId
              } );
            }
          break;

          case "removeSyncData":
            removeSyncData(message.category, message.data);

            if(message.requestId) {
              port.postMessage( {
                action: "_response",
                requestId: message.requestId
              } );
            }
          break;

          case "importFinished":

            fvdSynchronizer.Driver.Speeddial.setFirstSyncAfter( "register" );
            fvdSynchronizer.Driver.Speeddial.Backup.setHasChanges();

          break;
                

          case "getAccountInfo": // #2126
            if( !message.requestId ) return;
            var result = {};
            
            console.info('getAccountInfo', message);
            
            fvdSynchronizer.Utils.Async.chain([
              function( chainCallback ){
                fvdSynchronizer.Server.Sync.activityState( function( aState ){
                  console.info('aState', aState);
                  
                  if(aState !== 'logged'){
                    result.auth = false;
                  }else{
                    result.auth = true;
                  }

                  chainCallback();
                });
              },
              function( chainCallback ){
                if(result.auth){
                  fvdSynchronizer.Server.Sync.userInfo(function(err, info) {
                    console.info('info', info, err);
                    result.user = info || {};
                    chainCallback();
                  });
                }else{
                  chainCallback();
                }
              },
              function( ){
                port.postMessage( {
                  action: "_response",
                  requestId: message.requestId,
                  info: result
                } );
              }
            ]);
          break;

          case "_response":

            // if it's response to request. check pending request
            if( message.requestId ){

              var request = getPendingRequest( message.requestId );
              if( request ){

                removePendingRequestById( request.id );
                request.callback( message );

              }


            }

          break;
        }

      }

      function connectToSpeeddial(params) {

        params = params || {};

        if( port != null ){
          return;
        }

        getSpeedDialId(function( id ){

          if( !id ){
            return;
          }

          try{

            port = chrome.extension.connect(id, {
              name: "synchronizer"
            });

            port.onMessage.addListener(portMessageProcess);

            port.onDisconnect.addListener(function(){

              if( params.disconnectCallback ){
                params.disconnectCallback();
              }

              _setAllow(false);
              port = null;

            });
          }
          catch( ex ){
          }
        });
      }

      function getSpeedDialId(callback) {
        chrome.management.getAll(function( results ){
          var id = null;
          results.forEach(function( extension ){
            if( extension.enabled &&
              (extension.name == fvdSpeedDialName || extension.id == fvdSpeedDialId) ){
              id = extension.id;
            }
          });
          callback( id );
        });
      }

      function setMainSyncState(state) {
        mainSyncState = state;

        chrome.extension.sendRequest( {
          subject: "mainSyncStateChange"
        } );

        callChangeMainSyncStateListeners();

        if( state == "sync" ){
          fvdSynchronizer.Observer.fireEvent( "syncStart", ["SpeedDial"] );
        }
        else if( state == "nosync" ){
          fvdSynchronizer.Observer.fireEvent( "syncEnd", ["SpeedDial"] );
        }

        fvdSynchronizer.Server.Sync.syncStateRefresh();
      }

      this.totalItemsCount = function( callback ){
        messageToSD({
          action: "countItems"
        }, function( response ){
          callback( response.dials + response.groups );
        });
      };

      this.Backup = new function() {

        var selfBackup = this;

        const AUTOBACKUP_CHECK_INTERVAL = 60 * 1000;//60 * 1000 * 60 * 24;
        const MAX_BACKUPS = 30;

        var _ignoreBackupChecker = false;

        var restoreBackupProgress = {
          current: 0,
          max: 0
        };

        setInterval(function() {

          if( _ignoreBackupChecker || !enabled() ){
            return;
          }

          if( fvdSynchronizer.Server.Sync.isSyncNow() ){
            return;
          }

          var now = new Date().getTime();

          var lastCheckTime = 0;
          lastCheckTime = parseInt( fvdSynchronizer.Prefs.get( "sd.backup.last_check_time" ) );


          if( isNaN(lastCheckTime) || (now - lastCheckTime >= AUTOBACKUP_CHECK_INTERVAL) ){

            var hasChanges = _b( fvdSynchronizer.Prefs.get( "sd.backup.has_changes" ) );

            if( isNaN(lastCheckTime) && hasChanges || lastCheckTime ){
              fvdSynchronizer.Prefs.set( "sd.backup.last_check_time", now );
            }

            if( hasChanges ){
              fvdSynchronizer.Prefs.set( "sd.backup.has_changes", false );
              selfBackup.make(function(){});
            }

          }

        }, 1000);

        function enabled(){
          return  _b( fvdSynchronizer.Prefs.get( "autobackup.enabled" ) );
        }

        this.getRestoreProgress = function(){
          return restoreBackupProgress;
        };

        this.setRestoreBackupProgress = function( current, max ){
          restoreBackupProgress.current = current;
          restoreBackupProgress.max = max;
        };

        this.setHasChanges = function(){

          fvdSynchronizer.Prefs.set( "sd.backup.has_changes", true );

        };

        this.setNotHasChanges = function(){

          fvdSynchronizer.Prefs.set( "sd.backup.has_changes", false );

        };

        this.removeOldBackups = function( callback ){

          selfBackup.listBackups(function( backups ){

            backups.sort(function( a, b ){
              return a.time - b.time;
            });

            fvdSynchronizer.Utils.Async.cc( function( ccCallback ){

              if( backups.length <= MAX_BACKUPS ){
                return callback();
              }

              var backup = backups.shift();

              fvdSynchronizer.FileSystem.removeDir( "backups/sd/" + backup.dir, function(){
                ccCallback();
              } );

            } );

          });

        };

        this.listBackups = function( callback ){

          var result = [];

          fvdSynchronizer.FileSystem.dirContents( "backups/sd", function( dirs ){

            console.log( "SpeedDial backups: ", dirs );

            if( !dirs ){
              return callback( [] );
            }

            fvdSynchronizer.Utils.Async.arrayProcess( dirs, function( dir, apc ){

              // read info file
              fvdSynchronizer.FileSystem.read( "backups/sd/" + dir + "/info.json", function( r, contents ){
                if(!r){
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
              } )

            }, function(){

              callback( result );

            } );

          } );

        };


        this.make = function(callback) {
          var bookmarks = [];

          var nowDate = new Date();
          var backupDir = nowDate.getFullYear()+""+nowDate.getMonth()+""+nowDate.getDay()+""+nowDate.getHours()+nowDate.getMinutes()+""+nowDate.getSeconds();

          var dir = "backups/sd/" + backupDir;

          var dials = [];
          var groups = [];

          fvdSynchronizer.Utils.Async.chain([

            function( chainCallback ){

              messageToSD( {
                action: "queryGroups"
              }, function( message ){

                groups = message.list;
                chainCallback();

              } );

            },

            function( chainCallback ){

              queryDials( {
                fetchOnlyCustomThumb: true
              }, function( message ){
                dials = message.list;
                dials.forEach(function( dial ){
                  delete dial.group_global_id;
                });
                chainCallback();
              } );

            },

            function( chainCallback ){

              fvdSynchronizer.FileSystem.makeDir( dir, function(){
                chainCallback();
              } );

            },

            function( chainCallback ){

              var data = {
                dials: dials,
                groups: groups
              };

              fvdSynchronizer.FileSystem.write( dir + "/data.json", JSON.stringify( data ), function( result ){
                if( !result ){
                  return callback( result );
                }

                chainCallback();
              } );

            },

            function( chainCallback ){

              var info = {
                time: nowDate.getTime(),
                countDials: dials.length,
                countGroups: groups.length
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

              fvdSynchronizer.Observer.fireEvent( "sdBackupMaked" );

              callback( true );

            }

          ]);
        };

        this.restore = function( dir, callback ){
          _ignoreBackupChecker = true;

          fvdSynchronizer.Driver.Speeddial.setSyncNow( true );

          this._restore( dir, function( r ){
            fvdSynchronizer.Driver.Speeddial.setSyncNow( false );

            selfBackup.setNotHasChanges();
            _ignoreBackupChecker = false;
            callback( r );
          } );
        };

        this._restore = function( dir, callback ){

          console.log( "Restore from", "backups/sd/" + dir + "/data.json" );

          restoreBackupProgress.current = 0;

          fvdSynchronizer.FileSystem.read( "backups/sd/" + dir + "/data.json", function( result, data ){

            var bookmarks = [];

            try{
              data = JSON.parse( data );
            }
            catch( ex ){
              return callback( false );
            }

            fvdSynchronizer.Utils.Async.chain([

              function(chainCallback) {
                if(apiVersion < 2) {
                  console.info("speed dial doesn't support the chunked requests, crashes can happen on a large dataset")
                  return chainCallback();
                }
                uploadTempData(data, function(err, tempDataId) {
                  data = tempDataId;
                  chainCallback();
                });
              },

              function(chainCallback) {
                messageToSD({
                  action: "restoreBackup",
                  data: data
                }, function() {
                  chainCallback();
                });
              },

              function() {
                messageToSD({
                  action: "rebuild"
                }, function(){
                });

                fvdSynchronizer.Driver.Speeddial.setFirstSyncAfter( "register" );

                callback();
              }
            ]);

          } );

        };

        this.hasBackup = function( callback ){
          selfBackup.listBackups( function( backups ){
            callback( backups.length > 0 );
          } );
        };
      };

      window.addEventListener( "load", function() {
        connectToSpeeddial();

        setTimeout( function(){
          connectToSpeeddial();
        }, 5000 );

        var HelloWaiter = function( id, callback ){

          function req( _callback ){
            chrome.extension.sendMessage(id, {
              action: "sayHello"
            }, function( response ){

              _callback( response );

            });
          }

          var tryNum = 0
          fvdSynchronizer.Utils.Async.cc( function( ccCallback ){

            if( tryNum > 20 ){
              return;
            }

            tryNum++;

            req( function( response ){

              if( response == "hello" ){
                callback();
              }
              else{
                setTimeout(function(){
                  ccCallback();
                }, 500);
              }

            } );

          } );


        };

        function installCallback( ext ){
          getSpeedDialId( function( id ){

            if( id ){

              if( id == ext.id ){

                new HelloWaiter( ext.id, function(){
                  connectToSpeeddial();
                } );

              }

            }

          }) ;
        }

        chrome.management.onEnabled.addListener(installCallback);
        chrome.management.onInstalled.addListener(installCallback);

      }, false );

      // clear timed out pending requests
      setInterval( function(){
        clearTimedoutPendingRequests();
      }, PENDING_REQUESTS_TIMEOUT );

     };

    fvdSynchronizer.Driver.Speeddial = new Speeddial();

    // restore temporary stored syncData in case we had addon failure during sync and it wasn't restored at the sync end
    fvdSynchronizer.Driver.Speeddial.restoreTemporaryStoredSyncData()
    fvdSynchronizer.Observer.registerCallback("syncEnd", function onSyncEnd(driverName) {
      if(driverName === 'SpeedDial' && !fvdSynchronizer.Driver.Speeddial.isSyncNow()) {
        fvdSynchronizer.Driver.Speeddial.restoreTemporaryStoredSyncData()
      }
    })
  }
  else {
    fvdSynchronizer.Driver.Speeddial = chrome.extension.getBackgroundPage().fvdSynchronizer.Driver.Speeddial;
  }
})();