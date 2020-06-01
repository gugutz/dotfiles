(function(){

  chrome.tabs.query({
    url: chrome.extension.getURL( "/options.html" )
  }, function( tabs ){

    if( tabs.length > 0 ){
      var tab = tabs[0];
      chrome.tabs.getCurrent(function( currentTab ){

        if( currentTab.id != tab.id ){
          chrome.tabs.update( tab.id, {
            active: true
          } );
          chrome.tabs.remove( currentTab.id );
        }

      });

    }

  });

  this.Options = new function() {

    var self = this;

    const MIN_PASSWORD_LENGTH = 8;

    var Errors = fvdSynchronizer.Errors;

    // checks if sync in progress and refreshes log out buttons
    function refreshSyncNowState(){

      // dont allow user log out if sync in progress

      var syncNow = fvdSynchronizer.Server.Sync.isSyncNow();

      try{
        var logOutButton = document.getElementById("buttonLogout");

        if( syncNow ){
          logOutButton.setAttribute("disabled", true);
        }
        else{
          logOutButton.removeAttribute("disabled");
        }
      }
      catch( ex ){

      }

      var buttons = document.querySelectorAll( ".manualSyncTabContent button" );

      for( var i = 0; i != buttons.length; i++ ){
        var button = buttons[i];

        if( syncNow ){
          button.setAttribute("disabled", true);
        }
        else{
          button.removeAttribute("disabled");
        }
      }

      buttons = document.querySelectorAll("#manual_bookmarks button");
      for( var i = 0; i != buttons.length; i++ ){
        if( fvdSynchronizer.Driver.Bookmarks.canSync() ){
          buttons[i].setAttribute( "can", 1 );
        }
        else{
          buttons[i].setAttribute( "can", 0 );
        }
      }

      document.getElementById( "accountFrameContainer" ).setAttribute( "syncing", syncNow ? 1 : 0 );

      refreshBackupsCanRestoreState();

    }

    function setFieldError( fieldId ){
      document.getElementById(fieldId).className += " errorField";
    }

    function removeFieldError( fieldId ){
      document.getElementById(fieldId).className = document.getElementById(fieldId).className.replace("errorField", "");
    }

    function setErrorText( id, text ){
      if( text == "" ){
        text = "&nbsp;";
      }
      document.getElementById( id ).innerHTML = text;
    }



    function makeBookmarksBackupIfNeed( callback ){
      if( !_b(fvdSynchronizer.Prefs.get("bookmarks.archived")) ){

        self.bookmarksBackupMake(function(){
          callback();
        });

      }
      else{
        callback();
      }
    }

    function refreshBackupsCanRestoreState(){

      fvdSynchronizer.Server.Sync.getDriversList().forEach(function( driver ){

        var buttons = document.querySelectorAll("#backupContent_" + driver + " button");

        if( fvdSynchronizer.Driver[ driver ].isSyncNow() ){
          for( var i = 0; i != buttons.length; i++ ){
            buttons[i].setAttribute( "disabled", true );
          }
        }
        else{
          for( var i = 0; i != buttons.length; i++ ){
            buttons[i].removeAttribute( "disabled" );
          }
        }

      });


    }


    function refreshTabsVisibility(){

      var state = null;

      fvdSynchronizer.Utils.Async.chain([

        function( chainCallback ){
          fvdSynchronizer.Server.Sync.activityState( function( aState ){
            state = aState;
            chainCallback();
          } );

        },
        function(){

          var tabHeadSync = document.getElementById("tabHeadSync");
          var tabHeadSyncBookmarks = document.getElementById("tabHeadSyncBookmarks");
          var tabSdBackups = document.getElementById("tabSdBackups");
          var sdBackups = document.getElementById("sdBackups");
          console.info('state', state)
          if( state == "logged" ){
            fvdSynchronizer.Driver.Speeddial.isAllowed(function(allowed){
              console.info('isAllowed', allowed)
              if( allowed ){
                tabHeadSync.style.display = "";
                sdBackups.removeAttribute( "disabled" );

                //tabSdBackups.style.display = "";
              }
              else{
                sdBackups.setAttribute( "disabled", 1 );

                tabHeadSync.style.display = "none";
                sdBackups.style.display = "none";
              }
            });

            fvdSynchronizer.Driver.Bookmarks.isAllowed(function(allowed){
              if(allowed) {
                tabHeadSyncBookmarks.style.display = "";
              }
              else{
                tabHeadSyncBookmarks.style.display = "none";
              }
            }, true);
          }
          else if( state == "not_logged" ){

            tabHeadSync.style.display = "none";
            tabHeadSyncBookmarks.style.display = "none";

          }

          if(
            typeof fvdSynchronizer.Customs == "object"
            && typeof fvdSynchronizer.Customs.ui == "object"
          ){
            console.info('Refresh getGlobalState')
            fvdSynchronizer.Customs.ui.getGlobalState();
          }

          //document.querySelector(".tabsHead").style.width = 106 * fvdSynchronizer.Controls.Tabs.tabs[0].countActiveTabs() + "px";
          //fvdSynchronizer.Controls.Tabs.tabs[0].refreshActiveTab();
        }


      ]);

    }



    this.loadAccountFrame = function() {
      var url = fvdSynchronizer.Server.Sync.getAdminUrl();

      url += "/stats?addon=chrome_sync";

      var accountFrame = document.getElementById( "accountFrame" );
      var accountFrameContainer = document.getElementById( "accountFrameContainer" );

      function _loadListener() {
        accountFrame.removeEventListener( "load", _loadListener, false );
        accountFrame.setAttribute("data-loaded", 1);
        accountFrameContainer.removeAttribute( "loading" );
      }

      accountFrameContainer.setAttribute( "loading", 1 );
      accountFrame.addEventListener( "load", _loadListener, false );

      accountFrame.setAttribute( "src", url );

    };

    this.refreshActivityState = function(){
      var state = null;

      fvdSynchronizer.Utils.Async.chain([

        function( chainCallback ){

          fvdSynchronizer.Server.Sync.activityState( function( aState ){
            state = aState;
            chainCallback();
          } );

        },

        function(){
          var tabHeadSync = document.getElementById("tabHeadSync");
          var tabHeadSyncBookmarks = document.getElementById("tabHeadSyncBookmarks");

          fvdSynchronizer.Controls.Tabs.addInitCallback(function(){
            refreshTabsVisibility();
          });

          if( state == "logged" ){

            //document.getElementById( "tabHeadAccount" ).removeAttribute("alert");

          }
          else if( state == "not_logged" ){
            try{
              fvdSynchronizer.Controls.Tabs.tabs[0].setTab(0);
            }
            catch( ex ){

            }


            //document.getElementById( "tabHeadAccount" ).setAttribute("alert", 1);
          }

        }

      ]);

      //setBackupFrameSrc(); // Task #2097
    };

    this.setAccountAction = function(action) {
      function setAction() {
        action = action.toLowerCase();
        accountFrame.contentWindow.postMessage({
          a: "select-form",
          form: action
        }, "*");
      }

      var accountFrame = document.getElementById("accountFrame");
      if(!accountFrame.getAttribute("data-loaded")) {
        accountFrame.addEventListener("load", function _loadListener() {
          accountFrame.removeEventListener("load", _loadListener, false);
          setAction();
        }, false);
        return;
      }
      setAction();
      /*
      var actions = document.querySelectorAll( ".accountAction" );
      for( var i = 0; i != actions.length; i++ ){
        actions[i].setAttribute( "hidden", true );
      }

      var buttons = document.querySelectorAll( ".accountActionButton" );
      for( var i = 0; i != buttons.length; i++ ){
        buttons[i].setAttribute( "active", 0 );
      }*/
    };


    this.logout = function(){

      fvdSynchronizer.Server.Sync.setActivityState( "not_logged" );

    };


    this.sdMergeLocalAndServerData = function(){

      fvdSynchronizer.Dialogs.syncProgress( "sdMergeLocalAndServerData", function(){
        self.refreshInitialSync();
      } );

    };

    this.sdOverwriteServerData = function(options, cb) {
      cb = cb || function() {};
      options = options || {};
      fvdSynchronizer.Utils.Async.chain([
        function(next) {
          if(options.ignoreConfirm) {
            return next();
          }
          fvdSynchronizer.Dialogs.confirm(_("dlg_speeddial_confirm_overwrite_server_data_title"), _("dlg_speeddial_confirm_overwrite_server_data_text"), function(result) {
            if(!result) {
              return;
            }
            next();
          });
        },
        function() {
          var dlg = fvdSynchronizer.Dialogs.syncProgress("sdOverwriteServerData", function() {
            self.refreshInitialSync();
            if(options.autoClose) {
              dlg.close();
            }
            cb();
          });
        }
      ]);
    };

    this.sdOverwriteLocalData = function() {
      fvdSynchronizer.Dialogs.syncProgress("sdOverwriteLocalData", function() {
        self.refreshInitialSync();
      });
    };

    this.bookmarksMergeLocalAndServerData = function(){

      fvdSynchronizer.Utils.Async.chain([

        function( chainCallback ){
          makeBookmarksBackupIfNeed( chainCallback );
        },

        function(){


          fvdSynchronizer.Dialogs.syncProgressSimple( "Bookmarks", "mergeLocalAndServerData", function(){
            self.refreshInitialSync();
          } );


        }

      ]);



    };

    this.bookmarksOverwriteServerData = function(options, cb) {
      cb = cb || function() {};
      options = options || {};
      fvdSynchronizer.Utils.Async.chain([
        function(next) {
          if(options.ignoreConfirm) {
            return next();
          }
          fvdSynchronizer.Dialogs.confirm(
            _("dlg_bookmarks_confirm_overwrite_server_data_title"),
            _("dlg_bookmarks_confirm_overwrite_server_data_text"), function(result) {
            if(!result) {
              return;
            }
            next();
          });
        },
        function() {
          fvdSynchronizer.Utils.Async.chain([
            function(chainCallback) {
              makeBookmarksBackupIfNeed(chainCallback);
            },
            function() {
              var dlg = fvdSynchronizer.Dialogs.syncProgressSimple("Bookmarks", "overwriteServerData", function() {
                self.refreshInitialSync();
                if(options.autoClose) {
                  dlg.close();
                }
                cb();
              });
            }
          ]);
        }
      ]);
    };

    this.bookmarksOverwriteLocalData = function(){

      fvdSynchronizer.Utils.Async.chain([

        function( chainCallback ){
          makeBookmarksBackupIfNeed( chainCallback );
        },

        function(){

          fvdSynchronizer.Dialogs.syncProgressSimple( "Bookmarks", "overwriteLocalData", function(){
            self.refreshInitialSync();
          } );

        }

      ]);

    };

    this.bookmarksBackupMake = function( callback ){
      fvdSynchronizer.Dialogs.syncProgressSimple( "Bookmarks", "makeBackup", function(){

        fvdSynchronizer.Prefs.set( "bookmarks.archived", true );
        self.refreshBookmarksBackupState();

        if( callback ){
          callback();
        }

      } );
    };

    this.sdBackupMake = function( callback ){

      fvdSynchronizer.Dialogs.syncProgressSimple( "Speeddial", "makeBackup", function(){

        if( callback ){
          callback();
        }

      } );

    };

    this.bookmarksBackupRestore = function( dir, callback ){
      fvdSynchronizer.Dialogs.syncProgressSimple( "Bookmarks", "restoreBackup", function(){

        self.refreshBookmarksBackupState();

        self.refreshInitialSync();

        if( callback ){
          callback();
        }
      }, {
        dir: dir
      } );
    };

    this.sdBackupRestore = function( dir, callback ){
      fvdSynchronizer.Dialogs.syncProgressSimple( "Speeddial", "restoreBackup", function(){
        self.refreshBookmarksBackupState();

        self.refreshInitialSync();

        if( callback ){
          callback();
        }
      }, {
        dir: dir
      } );
    };

    this.bookmarksBuildBackupsListing = function(){

      var container = document.querySelector( "#backupContent_Bookmarks" );
      var tabPanel = document.querySelector( "#bookmarksBackups" );

      while( container.firstChild ){
        container.removeChild( container.firstChild );
      }

      fvdSynchronizer.Driver.Bookmarks.Backup.listBackups(function( backups ){

        backups.sort(function( a, b ){
          return b.time - a.time;
        });

        backups.forEach( function( backup, index ){

          var tr = document.createElement("tr");

          tr.className = "content";

          var tdDate = document.createElement( "td" );
          var tdCount = document.createElement( "td" );
          var tdRestore = document.createElement( "td" );

          tdDate.className = "restore_date";
          tdCount.className = "restore_items";
          tdRestore.className = "restore_action";

          var button = document.createElement( "button" );
          button.className = "fvdButton";
          button.textContent = _("options_backups_restore");

          button.addEventListener( "click", function(){

            fvdSynchronizer.Dialogs.confirm( _("confirm_really_restore_bookmarks_title"), _("confirm_really_restore_bookmarks_text"), function(r){

              if( r ){
                self.bookmarksBackupRestore( backup.dir );
              }

            } );

          }, false );

          tdDate.textContent = (index + 1) + ") " + new Date( parseInt(backup.time) ).toLocaleString();
          tdCount.textContent = backup.count;
          tdRestore.appendChild( button );

          tr.appendChild( tdDate );
          tr.appendChild( tdCount );
          tr.appendChild( tdRestore );

          container.appendChild( tr );

        } );

        var tr = document.createElement("tr");
        var tdDate = document.createElement( "td" );
        var tdCount = document.createElement( "td" );
        var tdRestore = document.createElement( "td" );
        tdDate.className = "restore_date";
        tdCount.className = "restore_items";
        tdRestore.className = "restore_action";
        tr.appendChild( tdDate );
        tr.appendChild( tdCount );
        tr.appendChild( tdRestore );

        container.appendChild( tr );

        if( backups.length == 0 ){
          tabPanel.setAttribute( "empty", 1 );
        }
        else{
          tabPanel.removeAttribute( "empty" );
        }

        refreshBackupsCanRestoreState();

      });


    };

    this.sdBuildBackupsListing = function(){

      var container = document.querySelector( "#backupContent_Speeddial" );
      var tabPanel = document.querySelector( "#sdBackups" );

      while( container.firstChild ){
        container.removeChild( container.firstChild );
      }

      fvdSynchronizer.Driver.Speeddial.Backup.listBackups(function( backups ){

        if( !backups ){
          backups = [];
        }

        backups.sort(function( a, b ){
          return b.time - a.time;
        });

        backups.forEach( function( backup, index ){

          var tr = document.createElement("tr");

          tr.className = "content";

          var tdDate = document.createElement( "td" );
          var tdCountGroups = document.createElement( "td" );
          var tdCountDials = document.createElement( "td" );
          var tdRestore = document.createElement( "td" );

          tdDate.className = "restore_date";
          tdCountGroups.className = "restore_groups";
          tdCountDials.className = "restore_dials";
          tdRestore.className = "restore_action";

          var button = document.createElement( "button" );
          button.className = "fvdButton";
          button.textContent = _("options_backups_restore");;

          button.addEventListener( "click", function(){

            fvdSynchronizer.Dialogs.confirm( _("confirm_really_restore_sd_title"), _("confirm_really_restore_sd_text"), function(r){

              if( r ){
                self.sdBackupRestore( backup.dir );
              }

            } );

          }, false );

          tdDate.textContent = (index + 1) + ") " + new Date( parseInt(backup.time) ).toLocaleString();
          tdCountGroups.textContent = backup.countGroups;
          tdCountDials.textContent = backup.countDials;
          tdRestore.appendChild( button );

          tr.appendChild( tdDate );
          tr.appendChild( tdCountGroups );
          tr.appendChild( tdCountDials );
          tr.appendChild( tdRestore );

          container.appendChild( tr );

        } );

        var tr = document.createElement("tr");
        var tdDate = document.createElement( "td" );
        var tdCountGroups = document.createElement( "td" );
        var tdCountDials = document.createElement( "td" );
        var tdRestore = document.createElement( "td" );
        tdDate.className = "restore_date";
        tdCountGroups.className = "restore_groups";
        tdCountDials.className = "restore_dials";
        tdRestore.className = "restore_action";
        tr.appendChild( tdDate );
        tr.appendChild( tdCountGroups );
        tr.appendChild( tdCountDials );
        tr.appendChild( tdRestore );

        container.appendChild( tr );

        if( backups.length == 0 ){
          tabPanel.setAttribute( "empty", 1 );
        }
        else{
          tabPanel.removeAttribute( "empty" );
        }

        refreshBackupsCanRestoreState();

      });

    };

    this.closePage = function(){

      fvdSynchronizer.Utils.getActiveTab( function( tab ){
        chrome.tabs.remove( tab.id );
      } );

    };

    this.refreshInitialSync = function(){

      var text = document.getElementById("initSyncDesc");
      var manualSyncCompleted = document.getElementById( "manualSyncCompleted" );

      if( fvdSynchronizer.Prefs.get("speeddial.first_sync_after") != "none" ){
        document.getElementById( "tabHeadSync" ).setAttribute("alert", 1);
        text.removeAttribute( "hidden" );
      }
      else{
        document.getElementById( "tabHeadSync" ).removeAttribute("alert");
        text.setAttribute( "hidden", true );
      }
      text.removeAttribute( "hidden" ); // Show message

      text = document.getElementById("initSyncDescBookmarks");

      if( !manualSyncCompleted.hasAttribute("hidden") ){
        text.setAttribute( "hidden", true );
      }
      else{
        if( fvdSynchronizer.Driver.Bookmarks.getFirstSyncAfter() != "none" ){
          document.getElementById( "tabHeadSyncBookmarks" ).setAttribute("alert", 1);
          text.removeAttribute( "hidden" );
        }
        else{
          document.getElementById( "tabHeadSyncBookmarks" ).removeAttribute("alert");
          text.setAttribute( "hidden", true );
        }
      }
      text.removeAttribute( "hidden" ); // Show message
    };

    this.refreshBookmarksBackupState = function(){

      fvdSynchronizer.Driver.Bookmarks.Backup.hasBackup( function( has ){

        //if( has ){
        if( true ){
          document.getElementById("backupBookmarksContainer").removeAttribute("hidden");
          /*
          fvdSynchronizer.Driver.Bookmarks.Backup.countBackuped( function( count ){
            document.getElementById("bookmarksCountBackuped").textContent = count;
          } );
          */
          var lastDate = fvdSynchronizer.Prefs.get( "bookmarks_backup_date" );
          if( lastDate ){
            try{
              var lastDate = new Date( parseInt( lastDate ) );
              document.getElementById( "bookmarksLastBackupDate" ).textContent = " ( "+lastDate.toLocaleString()+" )";
            }
            catch( ex ){
            }
          }
        }
        else{
          document.getElementById("backupBookmarksContainer").setAttribute("hidden", true);
        }

      } );

    };

    this.setTab = function( tab ){ // Task #2000

      document.location.hash = "#" + tab;

      let main_tab = String(tab).split('_').shift();

      if( typeof window[main_tab + "_open"] !== "function" ){ // Task #2090
        main_tab = "account";
      }

      fvdSynchronizer.Prefs.set( "last_settings_tab_index", tab )

      window[main_tab + "_open"](tab);

    };

    function highlightTabsInit(){

      setInterval(function(){

        var tabs = document.querySelectorAll( ".tab[alert=\"1\"]" );

        for( var i = 0; i != tabs.length; i++ ){
          var tab = tabs[i];

          if( tab.hasAttribute( "unhighligh" ) ){
            tab.removeAttribute( "unhighligh" );
          }
          else{
            tab.setAttribute( "unhighligh", 1 );
          }
        }

      }, 800);

    }

    function refreshTabsByHash() {
      if(document.location.hash == "#register") {
        self.setAccountAction('Register');
      }
      else if( document.location.hash == "#sync" ) {
        fvdSynchronizer.Controls.Tabs.addInitCallback(function(){
          self.setTab("speeddial");
        });
      }
      else if( document.location.hash == "#sync-bookmarks" ) {
        fvdSynchronizer.Controls.Tabs.addInitCallback(function(){
          self.setTab("bookmarks");
        });
      }
      else{
        return false;
      }
      return true;
    }

    function refreshManualBookmarksSyncCompletedMessage(){

      var message = document.getElementById("manualSyncCompleted");

      message.setAttribute("hidden", true);

      self.refreshInitialSync();

    }

    document.addEventListener( "DOMContentLoaded", function() {
      // check need show get pro message
      fvdSynchronizer.Server.Sync.userInfo(function(err, info) {
        var show = true;
        if(!err) {
          show = !info.premium.active;
        }
        if(show) {
          var els = document.querySelectorAll(".get-pro-center-button");
          els = Array.prototype.slice.call(els);
          els.forEach(function(el) {
            el.style.display = "block";
            setTimeout(function() {
              el.setAttribute("appear", 1);
            }, 0);
          });
        }
      });

      document.querySelector("#bookmarks-older-chrome-message .inner a").addEventListener("click", function() {
        chrome.tabs.create({
          url: "chrome://chrome/",
          active: true
        });
        return false;
      }, false);

      if(!fvdSynchronizer.Driver.Bookmarks.supportsLargeSync()) {
        document.querySelector("#bookmarks-older-chrome-message").removeAttribute("hidden");
      }

      document.getElementById("buttonpremiumForShareButton").addEventListener("click", function() {
        fvdSynchronizer.Utils.Opener.newTab(chrome.runtime.getURL("premiumforshare.html"));
      }, false);

      fvdSynchronizer.PremiumForShare.canDisplay(function(can) {
        if(can) {
          var topButtons = document.getElementById("topButtons");
          var btn = document.getElementById("premiumForShareButton");
          topButtons.setAttribute("with-premium-for-share", 1);
          setTimeout(function() {
            btn.style.opacity = 1;
          }, 0);
        }
      });

      // init autosync checkboxes
      var cbs = document.querySelectorAll(".enableAutoSync input");
      for( var i = 0; i != cbs.length; i++ ){
        var cb = cbs[i];
        var driver = cb.getAttribute("driver");

        cb.checked = _b( fvdSynchronizer.Prefs.get(driver + ".enable_autosync") );

        (function( cb, driver ){

          cb.addEventListener("click", function(){

            setTimeout(function(){

              fvdSynchronizer.Prefs.set(driver + ".enable_autosync", cb.checked);

            }, 0);

          }, false);

        })( cb, driver );
      }

      highlightTabsInit();

      refreshManualBookmarksSyncCompletedMessage();

      var activityState = null;

      fvdSynchronizer.Utils.Async.chain([

        function( chainCallback ){

          fvdSynchronizer.Server.Sync.activityState( function( state ){
            activityState = state;
            chainCallback();
          } );

        },

        function(){

          fvdSynchronizer.Controls.Tabs.onChange.addListener(function( container, num ){

            if( container.getAttribute("id") == "mainTabs" ){
              fvdSynchronizer.Prefs.set( "last_settings_tab_index", num );
            }

          });

          // build backups listings
          self.bookmarksBuildBackupsListing();
          self.sdBuildBackupsListing();

          document.getElementById("enableAutoBackup").checked = _b( fvdSynchronizer.Prefs.get("autobackup.enabled") );

          var toolTipElems = document.querySelectorAll("[tooltip]");
          for( var i = 0; i != toolTipElems.length; i++ ){

            (function( el ){

              el.addEventListener( "click", function( event ){

                fvdSynchronizer.ToolTip.display( el, document.getElementById( el.getAttribute("tooltip") ).innerHTML, event );

              }, false );

            })( toolTipElems[i] );

          }

          refreshSyncNowState();

          fvdSynchronizer.Localizer.localizeCurrentPage();

          self.refreshActivityState();
          self.setAccountAction( "Login" );
          self.refreshInitialSync();
          self.refreshBookmarksBackupState();
          
          if( refreshTabsByHash() ){ // Task #2090
            console.info('refreshTabsByHash');
          }
          
          if( activityState == "logged" ){

            fvdSynchronizer.Controls.Tabs.addInitCallback(function(){
              self.setTab( fvdSynchronizer.Prefs.get( "last_settings_tab_index" ) );
            });
            
          }
          else{
            self.setTab( "account" );
          }

          var isBookmarksInitialization = fvdSynchronizer.Driver.Bookmarks.initializationInProgress();

          if( isBookmarksInitialization ){
            document.getElementById( "bookmarksSyncTypes" ).setAttribute("initialization", "1");

            function _bintervalFunc(){

              var p = fvdSynchronizer.Driver.Bookmarks.getInitialActionsProgress();
              document.getElementById("bookmarksInitializationProgress").textContent = p.current + " / " + p.total;

            }

            var initBookmarksProgressInterval = setInterval(_bintervalFunc, 1000);
            _bintervalFunc();
          }

          fvdSynchronizer.Observer.registerCallback( "bookmarksInitializationCompleted", function(){
            isBookmarksInitialization = false;
            document.getElementById( "bookmarksSyncTypes" ).removeAttribute("initialization");
            if(initBookmarksProgressInterval){
              clearInterval( initBookmarksProgressInterval );
            }

            self.loadAccountFrame();
          } );

          // set events
          var events = [
            {
              event: "click",
              elem: "buttonMergeLocalAndServerData",
              callback: function(){
                fvdSynchronizer.Options.sdMergeLocalAndServerData();
              }
            },
            {
              event: "click",
              elem: "buttonOverwriteServerData",
              callback: function(){
                fvdSynchronizer.Options.sdOverwriteServerData();
              }
            },
            {
              event: "click",
              elem: "buttonOverwriteLocalData",
              callback: function(){
                fvdSynchronizer.Options.sdOverwriteLocalData();
              }
            },
            {
              event: "click",
              elem: "buttonMergeLocalAndServerDataBookmarks",
              callback: function(){
                if( !isBookmarksInitialization ){
                  fvdSynchronizer.Options.bookmarksMergeLocalAndServerData();
                }
              }
            }
            ,
            {
              event: "click",
              elem: "buttonOverwriteServerDataBookmarks",
              callback: function(){
                if (!isBookmarksInitialization) {
                  fvdSynchronizer.Options.bookmarksOverwriteServerData();
                }
              }
            }
            ,
            {
              event: "click",
              elem: "buttonOverwriteLocalDataBookmarks",
              callback: function(){
                if (!isBookmarksInitialization) {
                  fvdSynchronizer.Options.bookmarksOverwriteLocalData();
                }
              }
            },
            {
              event: "click",
              elem: "buttonBookmarksBackupRestore",
              callback: function(){

                self.setTab( "backups_local" );

              }
            },
            {
              event: "click",
              elem: "buttonBookmarksBackupMake",
              callback: function(){
                if (!isBookmarksInitialization) {
                  fvdSynchronizer.Options.bookmarksBackupMake();
                }

              }
            },
            {
              event: "click",
              elem: "buttonSDBackupRestore",
              callback: function(){

                self.setTab( "backups_local" );

              }
            },
            {
              event: "click",
              elem: "buttonSDBackupMake",
              callback: function(){
                if (!isBookmarksInitialization) {
                  fvdSynchronizer.Options.sdBackupMake();
                }

              }
            },
            {
              event: "click",
              elem: "enableAutoBackup",
              callback: function(event){

                setTimeout(function(){

                  fvdSynchronizer.Prefs.set( "autobackup.enabled", event.target.checked );

                }, 0);

              }
            },
            {
              event: "click",
              elem: "backupsOnlineAccessLink",
              callback: function( event ){
                fvdSynchronizer.Server.Sync.navigateToAdminPanelOptions("backups");
                event.preventDefault();
              }
            },

          ];

          events.forEach( function( event ){
            let element = document.getElementById( event.elem )
            if (element) {
              element.addEventListener( event.event, event.callback, false );
            }
          } );

          var els = document.getElementsByClassName( "buttonCloseBottom" );

          for( var i = 0; i != els.length; i++ ){

            els[i].addEventListener( "click", function(){

              fvdSynchronizer.Options.closePage();

            }, false );

          }


          // help messages

          var containers = document.querySelectorAll("[helpmessageto]");

          for( var i = 0; i != containers.length; i++ ){

            (function( container ){

              var messageContainer = document.getElementById( container.getAttribute( "helpmessageto" ) );

              var items = container.querySelectorAll( "button" );

              for( var j = 0; j != items.length; j++ ){

                (function( item ){

                  item.setAttribute( "_title", item.getAttribute("title") );
                  item.removeAttribute( "title" );

                  item.parentNode.addEventListener( "mouseover", function(){
                    messageContainer.textContent = item.getAttribute("_title");
                  } );

                  item.parentNode.addEventListener( "mouseout", function(){
                    messageContainer.textContent = messageContainer.getAttribute( "defaulttitle" );
                  } );

                })( items[j] );

              }

              messageContainer.textContent = messageContainer.getAttribute( "defaulttitle" );

            })( containers[i] );

          }

          fvdSynchronizer.Controls.Tabs.addInitCallback(function(){
            refreshTabsVisibility();
          });

          self.loadAccountFrame();

        }

      ]);

    }, false );

    document.addEventListener( "keypress", function( event ){

      var accountAction = null;

      var elements = document.getElementsByClassName( "accountAction" );

      for( var i = 0; i != elements.length; i++ ){
        if( !elements[i].hasAttribute("hidden") ){
          accountAction = elements[i];
          break;
        }
      }

      if( event.keyCode == 13 ){
        if( accountAction ){

          if( accountAction.id == "accountActionLogin" ){

            document.getElementById("btnLogin").click();

          }

        }
      }

    }, false );

    var _observerStruct = {

      syncQuotaExceed: function(){

        var msg = document.querySelector( ".syncProgressDialog .syncQuotaExceed" );
        if( msg ){
          msg.removeAttribute( "hidden" );
        }

      },

      syncEnd: function(){

        refreshSyncNowState();
        self.loadAccountFrame();

      },

      syncStart: function(){

        refreshSyncNowState();

      },

      syncDriverAllowedStateChanged: function(){

        fvdSynchronizer.Controls.Tabs.addInitCallback(function(){
          refreshTabsVisibility();
          self.loadAccountFrame();
        });

      },

      bookmarksBackupMaked: function(){

        self.bookmarksBuildBackupsListing();

      },

      sdBackupMaked: function(){
        self.sdBuildBackupsListing();
      },

      "event:logout": function(){
        self.refreshActivityState();
        self.loadAccountFrame();
      },

      "event:login": function(){
        self.refreshActivityState();
        self.loadAccountFrame();
        self.refreshInitialSync();
      },

      bookmarksTimeoutRefresh: function(){
        refreshSyncNowState();
      }

    };

    fvdSynchronizer.Observer.registerStruct( _observerStruct );

    window.addEventListener( "unload", function(){
      fvdSynchronizer.Observer.unRegisterStruct( _observerStruct );
    }, false );

    chrome.extension.onMessage.addListener(function(r, sender, sendResponse) {
      if(r.action) {
        if(r.action == "options:refreshTabs") {
          refreshTabsByHash();
        }
        else if(r.action === "runSync") {
          var methods = {
            "Speeddial_overwriteServerData": "sdOverwriteServerData",
            "Bookmarks_overwriteServerData": "bookmarksOverwriteServerData",
          };
          var method = methods[r.driver + "_" + r.type];
          if(method) {
            fvdSynchronizer.Options[method]({
              ignoreConfirm: true,
              autoClose: true
            }, function() {
              sendResponse();
            });
            return true;
          }
        }
      }
    } );

    chrome.extension.onRequest.addListener( function( r ){

      if( r.subject == "changeActivityState" ){
        self.refreshActivityState();
      }

    } );
  };

}).apply( fvdSynchronizer );
