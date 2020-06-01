(function(){

  this.Background = new function(){

    window.addEventListener( "load", function(){

      // refresh setting tabs
      chrome.tabs.query({
        url: chrome.extension.getURL("options.html")
      }, function( tabs ){

        tabs.forEach(function( tab ){
          chrome.tabs.reload( tab.id );
        });

      });

      if( fvdSynchronizer.Utils.isVersionChanged() ){
        // reset display chrome sync message
        fvdSynchronizer.Prefs.set( "dont_display_ds_chromesync_message", false );
      }

      fvdSynchronizer.Localizer.localizeCurrentPage();

      function mainSyncChangeListener() {
      }

      // listen driver change state
      for (var driverName in fvdSynchronizer.Driver) {

        if(fvdSynchronizer.Driver[driverName].addChangeMainSyncStateListener){

          fvdSynchronizer.Driver[driverName].addChangeMainSyncStateListener( mainSyncChangeListener );

        }
      }

      function setSyncAfterLogin() {
        for (var driverName in fvdSynchronizer.Driver) {
          if(fvdSynchronizer.Driver[driverName].setFirstSyncAfter){
            fvdSynchronizer.Driver[driverName].setFirstSyncAfter( "login" );
          }
        }
      }

      function processRegisterEvent() {
        // user has been registered
        // upload data of all drivers to the server
        var drivers = Object.keys(fvdSynchronizer.Driver);
        fvdSynchronizer.Utils.Async.arrayProcess(drivers, function(driverName, next) {
          var driver = fvdSynchronizer.Driver[driverName];
          driver.isAllowed(function(allowed) {
            if(!allowed) {
              return next();
            }
            fvdSynchronizer.Utils.getOptionsPagesOpenedTabsIds(function(err, tabsIds) {
              if(!tabsIds || !tabsIds.length) {
                driver.overwriteServerData(function() {
                  next();
                });
              }
              else {
                chrome.tabs.sendMessage(tabsIds[0], {
                  action: "runSync",
                  driver: driverName,
                  type: "overwriteServerData"
                }, function() {
                  next();
                });
              }
            });
          });
        });
      }

      fvdSynchronizer.Observer.registerCallback("event:login", setSyncAfterLogin);
      fvdSynchronizer.Observer.registerCallback("event:register", processRegisterEvent);
      fvdSynchronizer.Observer.registerCallback("event:logout", setSyncAfterLogin);

      fvdSynchronizer.Observer.registerCallback( "event:openURL", function( data ){

        window.open( data.url );

      } );

      fvdSynchronizer.Server.Sync.getAuthState( function( error, authorized ){

        if( !authorized ){
          // user not authorized - be sure that auth cookie is removed
          /* // Task #1985
          chrome.cookies.remove({
            url: fvdSynchronizer.Server.Sync.getAdminUrl(),
            name: fvdSynchronizer.Server.Sync.getAuthCookieName()
          });
          */
          fvdSynchronizer.Server.Sync.removeAuthCookie();
        }

      } );

    }, false );

  };

}).apply( fvdSynchronizer );
