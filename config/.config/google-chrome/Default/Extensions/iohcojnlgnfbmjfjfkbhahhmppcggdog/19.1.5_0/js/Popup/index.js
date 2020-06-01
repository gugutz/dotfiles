(function(){

    this.Popup = new function(){

        const DISPLAY_RATE_US_AFTER = 3600 * 24 * 1;

    		var observerHolder = fvdSynchronizer.Observer.createHolder();
    		window.addEventListener("unload", function(){
    			observerHolder.cleanup();
    		});

        var self = this;

        function refreshManualSyncCompletedMessage(){

            var message = document.getElementById("manualSyncCompleted");

            message.setAttribute("hidden", true);

        }

        // checks if sync in progress and refreshes log out buttons
        function refreshSyncNowState(){

            // dont allow user log out if sync in progress

            var logOutButton = document.getElementById("buttonLogout");

            if (fvdSynchronizer.Server.Sync.isSyncNow()) {
                logOutButton.setAttribute("disabled", true);
            }
            else {
                logOutButton.removeAttribute("disabled");
            }

        }

        this.setActiveContent = function(id){

            var contents = document.getElementsByClassName("popupContent");
            for (var i = 0; i != contents.length; i++) {
                contents[i].setAttribute("hidden", true);
            }

            document.getElementById(id).removeAttribute("hidden");

        };

        this.openOptions = function(hash){
            var url = chrome.extension.getURL("options.html");

            chrome.tabs.query({
                url: url
            }, function(tabs){

                if (hash) {
                    url += hash;
                }

                if (tabs.length > 0) {

                    var updateData = {
                        active: true
                    };

                    if (tabs[0].url != url) {
                        updateData.url = url;
                    }

                    chrome.tabs.update(tabs[0].id, updateData);

					if( updateData.url ){
						chrome.tabs.sendMessage(tabs[0].id, {
							action: "options:refreshTabs"
						});
					}

                }
                else {

                    if (hash) {
                        url += hash;
                    }

                    chrome.tabs.create({
                        url: url,
                        active: true
                    });

                }

            });

        };

        // Bookmarks sync

        this.syncBookmarks = function(){

            if (fvdSynchronizer.Driver.Bookmarks.canSyncAndReaction) {
                if (!fvdSynchronizer.Driver.Bookmarks.canSyncAndReaction()) {
                    return;
                }
            }

            if (fvdSynchronizer.Prefs.get("bookmarks.first_sync_after") == "none") {
                document.getElementById("syncCancelButtonBookmarks").removeAttribute("loading");
                fvdSynchronizer.Driver.Bookmarks.startMainSync();
            }
            else {
                this.openOptions("#sync-bookmarks");
            }

        };

        this.abortBookmarksSync = function(){

            fvdSynchronizer.Driver.Bookmarks.abortCurrentSync();
            document.getElementById("syncCancelButtonBookmarks").setAttribute("loading", "1");

        };

        // Speeddial sync

        this.syncSpeedDial = function(){

            if (fvdSynchronizer.Prefs.get("speeddial.first_sync_after") == "none") {
                document.getElementById("syncSDCancelButton").removeAttribute("loading");

                fvdSynchronizer.Driver.Speeddial.startMainSync();
            }
            else {
                this.openOptions("#sync");
            }

        };

        this.abortSpeedDialSync = function(){

            fvdSynchronizer.Driver.Speeddial.abortCurrentSync();
            document.getElementById("syncSDCancelButton").setAttribute("loading", "1");

        };

        this.refreshSyncState_Speeddial = function(){

            var states = ["syncGroups", "syncDials", "applyChanges"];

            var progress = fvdSynchronizer.Driver.Speeddial.getMainSyncProgress();

            if (!progress.state) {
                //	progress.state = "syncGroups";
            }

            console.log(progress.action, progress.state, progress);

            if (progress.state) {
                document.getElementById("speedDialProgressState_" + progress.state).setAttribute("state", "progress");
            }

            progress.successStates.forEach(function(state){
                document.getElementById("speedDialProgressState_" + state).setAttribute("state", "success");
            });

            states.forEach(function(state){

                if (progress.successStates.indexOf(state) == -1 && state != progress.state) {
                    document.getElementById("speedDialProgressState_" + state).removeAttribute("state");
                }

            });


            document.getElementById("syncProgress_syncedCountGroups").textContent = progress.groupsCount;
            document.getElementById("syncProgress_syncedCountDials").textContent = progress.dialsCount;

            var textContainer = document.querySelector("#syncingContent_Speeddial .textDesc");

            var text = _("popup_speeddial_sync_action_" + progress.action);

            if (progress.action == "special_actions") {

                text = text.replace("%progress%", progress.specialActionsCurrent + "/" + progress.specialActionsCount);

            }

            textContainer.textContent = text;

        };

        function refreshDriversSyncing(){

            var speedDial = document.querySelector(".driver.speedDial");
            var bookmarks = document.querySelector(".driver.bookmarks");
      			var bookmarksButton = bookmarks.querySelector( ".fvdButton" );
      			var sdButton = speedDial.querySelector( "#buttonSyncSpeedDial" );
            console.log("Books", fvdSynchronizer.Driver.Bookmarks.isSyncNow());

            if (fvdSynchronizer.Driver.Speeddial.isSyncNow()) {
                speedDial.setAttribute("syncing", 1);
                sdButton.setAttribute("can", 0);
            }
            else {
                speedDial.removeAttribute("syncing", 1);
                sdButton.setAttribute("can", 1);
            }

            if (fvdSynchronizer.Driver.Bookmarks.isSyncNow()) {
                bookmarks.setAttribute("syncing", 1);
            }
            else {
                bookmarks.removeAttribute("syncing", 1);
            }

      			if( fvdSynchronizer.Driver.Bookmarks.canSync() || fvdSynchronizer.Driver.Bookmarks.isSyncNow() ){
      				bookmarksButton.setAttribute("can", 1);
      			}
      			else{
      				bookmarksButton.setAttribute("can", 0);
      			}


        }

        function initSyncDriversList(){

            // Speeddial

            var speedDial = document.querySelector(".driver.speedDial");

            fvdSynchronizer.Driver.Speeddial.isAllowed(function(allowed){
              if(allowed){
                speedDial.setAttribute("active", 1);
              }
            });

            var bookmarks = document.querySelector(".driver.bookmarks");

            fvdSynchronizer.Driver.Bookmarks.isAllowed(function(allowed){
              if (allowed) {
                bookmarks.setAttribute("active", 1);
              }
              else {
                if (fvdSynchronizer.Driver.Bookmarks.initializationInProgress()) {
                    bookmarks.setAttribute("initialization", 1);

                    function _bintervalFunc(){

                        var p = fvdSynchronizer.Driver.Bookmarks.getInitialActionsProgress();
                        document.getElementById("bookmarksInitializationProgress").textContent = p.current + " / " + p.total;

                    }

                    var initBookmarksProgressInterval = setInterval(_bintervalFunc, 1000);
                    _bintervalFunc();
                }

                observerHolder.registerCallback("bookmarksInitializationCompleted", function(){
                    if (initBookmarksProgressInterval) {
                        clearInterval(initBookmarksProgressInterval);
                    }

                    bookmarks.setAttribute("active", 1);
                    bookmarks.removeAttribute("initialization");

                });
              }
            });

            observerHolder.registerCallback("syncEnd", function(){
              refreshDriversSyncing();
            });

            refreshDriversSyncing();

        }


        function refreshMainSyncState(){

            //console.log( "State ", Math.random(), fvdSynchronizer.Driver.Speeddial.getMainSyncState(), fvdSynchronizer.Driver.Speeddial.getMainSyncProgress() );

            var hasSync = false;

            for (var driverName in fvdSynchronizer.Driver) {

                if (!fvdSynchronizer.Driver[driverName].getMainSyncState) {
                    continue;
                }

                var state = fvdSynchronizer.Driver[driverName].getMainSyncState();

                if (state == "sync" && !fvdSynchronizer.Driver[driverName].ignoreSyncProgress) {

                    self.setActiveContent("syncingContent_" + driverName);
                    hasSync = true;

                    if (self["refreshSyncState_" + driverName]) {
                        self["refreshSyncState_" + driverName]();
                    }
                    else {
                        console.log("NF", "refreshSyncState_" + driverName);
                    }

                }
            }

            var message = document.getElementById("manualSyncCompleted");

            if (!hasSync) {
                self.setActiveContent("loggedContent");

            }
            else {

                message.setAttribute("hidden", true);

            }



            /*

             var cancelButton = document.getElementById("syncCancelButton");



             cancelButton.onclick = function(){

             fvdSynchronizer.Driver.Speeddial.abortCurrentSync = true;

             cancelButton.setAttribute("loading", 1);

             }*/

        }

        function mainSyncChangeListener(){
            refreshMainSyncState();
        }



        chrome.extension.onRequest.addListener(function(request){

            if (request.subject == "speedDialConnected") {
                initSyncDriversList();
            }
            else
                if (request.subject == "mainSyncStateChange") {
                //refreshMainSyncState();
                }
                else
                    if (request.subject == "syncError") {

                        if (fvdSynchronizer.Errors.ERROR_ALREADY_LOCKED == request.error) {
                            alert(_("popup_alert_sync_already_active"));
                        }

                    }

        });

        document.addEventListener("DOMContentLoaded", function(){
          if(!fvdSynchronizer.Driver.Bookmarks.supportsLargeSync()) {
            document.querySelector("#bookmarks-older-chrome-message").removeAttribute("hidden");
            document.querySelector("#bookmarks-older-chrome-message a").addEventListener("click", function(e) {
              e.preventDefault();
              window.open(chrome.runtime.getURL("/options.html#bookmarks"));
              return false;
            }, false);
          }

          document.querySelector("#warning-unsorted a").addEventListener("click", function(e) {
            var locale = chrome.i18n.getMessage("extension_locale");
            chrome.tabs.create({
              url: "https://everhelper.me/info/sync-unsorted.php?locale="+encodeURIComponent(locale),
              active: true
            });
            e.preventDefault();
          }, false);

            var activityState = null;


            fvdSynchronizer.Utils.Async.chain(
			[
				function(chainCallback){

	                fvdSynchronizer.Server.Sync.activityState(function(state){
	                    activityState = state;
	                    chainCallback();
	                });

	            },
				function(){
	                if (fvdSynchronizer.Utils.getSecondsCountAfterInstall() > DISPLAY_RATE_US_AFTER) {
	                    document.getElementById("buttonRateUs").style.display = "";
	                }

	                refreshSyncNowState();
	                refreshManualSyncCompletedMessage();

	                fvdSynchronizer.Driver.Speeddial.isAllowed(function(allowed){
	                  if(allowed){
	                    fvdSynchronizer.Driver.Speeddial.connectToSpeeddial();
	                  }
	                });

	                fvdSynchronizer.Localizer.localizeCurrentPage();

	                if (activityState != "logged") {

	                    document.getElementById("buttonLogout").setAttribute("hidden", true);
	                    document.getElementById("buttonReportBug").setAttribute("hidden", true);

	                    var mustLoggedMessage = document.getElementById("syncMustLoginMessage");
	                    mustLoggedMessage.removeAttribute("hidden");
	                    mustLoggedMessage.querySelector("a.login").setAttribute("href", chrome.extension.getURL("options.html#login"));
	                    mustLoggedMessage.querySelector("a.register").setAttribute("href", chrome.extension.getURL("options.html#register"));

	                }
	                else {
	                    var loggedContent = document.getElementById("loggedContent");
	                    loggedContent.removeAttribute("hidden");

	                    initSyncDriversList();

	                    refreshMainSyncState();

	                    //self.setActiveContent( "syncingContent" );
	                }

	                for (var driverName in fvdSynchronizer.Driver) {

	                    if (fvdSynchronizer.Driver[driverName].addChangeMainSyncStateListener) {

	                        fvdSynchronizer.Driver[driverName].addChangeMainSyncStateListener(mainSyncChangeListener);

	                    }
	                }

	                // set listeners

	                document.getElementById("buttonSyncSpeedDial").addEventListener("click", function(){
	                    fvdSynchronizer.Popup.syncSpeedDial();
	                }, false);

	                document.getElementById("buttonSyncBookmarks").addEventListener("click", function(){
	                    fvdSynchronizer.Popup.syncBookmarks();
	                }, false);

	                document.getElementById("syncCancelButton").addEventListener("click", function(){
	                    fvdSynchronizer.Options.restore();
	                }, false);

	                document.getElementById("syncSDCancelButton").addEventListener("click", function(){
	                    fvdSynchronizer.Popup.abortSpeedDialSync();
	                }, false);

	                document.getElementById("syncCancelButtonBookmarks").addEventListener("click", function(){
	                    fvdSynchronizer.Popup.abortBookmarksSync();
	                }, false);

	                document.getElementById("buttonOpenOptions").addEventListener("click", function(){
	                    fvdSynchronizer.Popup.openOptions();
	                }, false);

	                document.getElementById("buttonReportBug").addEventListener("click", function(){

						/*
	                    fvdSynchronizer.Server.Sync.loginInAdminPanel(function(result){

	                        chrome.tabs.create({
	                            url: fvdSynchronizer.Server.Sync.getAdminUrl() + "/?l=/components/MainOptions/feedback",
	                            active: true
	                        });

	                    });
	                    */

                        chrome.tabs.create({
                            url: "http://fvdmedia.userecho.com/list/21212-everhelper/?category=4907",
                            active: true
                        });



	                }, false);

	                document.getElementById("buttonLogout").addEventListener("click", function(){

	                    if (!document.getElementById("buttonLogout").hasAttribute("disabled")) {
	                        fvdSynchronizer.Server.Sync.setActivityState("not_logged");
	                        window.close();
	                    }

	                }, false);

	                document.querySelector(".bottomMenu .goToEverhelper").addEventListener("click", function(){

	                    var toUrl = fvdSynchronizer.Server.Sync.getAdminUrl() + "/";

	                    fvdSynchronizer.Server.Sync.loginInAdminPanel(function(result){

	                        chrome.tabs.create({
	                            url: toUrl,
	                            active: true
	                        });

	                    });

	                }, false);


	                observerHolder.registerCallback("syncEnd", function(){

	                    refreshSyncNowState();


	                });

	                observerHolder.registerCallback("syncStart", function(){
	                    refreshSyncNowState();
	                });
	                observerHolder.registerCallback("bookmarksTimeoutRefresh", function(){
	                    refreshDriversSyncing();
	                });

	                observerHolder.registerCallback("manualSyncSuccess", function(){

	                    refreshManualSyncCompletedMessage();

	                });


	                document.querySelector(".fvdButton.getPro").addEventListener("click", function() {
                    chrome.tabs.create({
                      url: "https://everhelper.me/everhelperplans.php",
                      active: true
                    });
                  });
                  fvdSynchronizer.Server.Sync.userInfo(function(err, info){
                    if(!err) {
                      if(!info.premium.active) {
                        var btn = document.querySelector(".fvdButton.getPro");
                        btn.style.display = "";
                        setTimeout(function() {
                          btn.setAttribute("appear", 1);
                        }, 0);
                      }
                    }
                  });

	                setTimeout(function(){
	                    fvdSynchronizer.Dialogs.fireOpenedAlerts();
	                }, 10);

	            }

			]);

        }, false);

    };

}).apply(fvdSynchronizer);
