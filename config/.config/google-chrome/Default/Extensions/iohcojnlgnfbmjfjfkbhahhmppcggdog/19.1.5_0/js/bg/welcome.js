(function () {
    // setup
    var WELCOME_URL = "https://nimbus.everhelper.me/app-update/misc.php?v={VERSION}",
        BADGE_TEXT = "new!";
    // end setup

    var LAST_VERSION_KEY = "welcomemod:lastversion",
        SHOULD_SHOW_WELCOME_KEY = "welcomemode:show_welcome",
        restoreData = null;

    chrome.runtime.onInstalled.addListener(function (info) {
        if (info.reason == "update") {
            localStorage[SHOULD_SHOW_WELCOME_KEY] = "1";
        } else
        if (info.reason == "install") {
            openWelcomeUrl();
        }
    });

    var WELCOME_URL_EN = "https://www.everhelper.me/eversyncmanual.php",
        WELCOME_URL_RU = "https://www.everhelper.me/eversyncmanualru.php";



    function openWelcomeUrl() {
        chrome.i18n.getAcceptLanguages(function (languages) {
            let URL;

            if (languages.indexOf("ru") != -1) {
                URL = WELCOME_URL_RU;
            } else {
                URL = WELCOME_URL_EN;
            }

            chrome.tabs.create({
                url: URL,
                active: true
            });
        });
    }

    /*
  function getCurrentVersion(){
    return chrome.runtime.getManifest().version;
  }
  
  function getWelcomeUrl(){
    return WELCOME_URL.replace("{VERSION}", getCurrentVersion());
  }
  
  function restoreBrowserAction(){
    console.log("Restore data is", restoreData);
       
    var badgeText = restoreData.badgeText ? restoreData.badgeText : "";
    if(badgeText == BADGE_TEXT){
      badgeText = "";
    }

    chrome.browserAction.setBadgeText({
      text: badgeText
    });
      
    if(restoreData.popup){            
      chrome.browserAction.setPopup({
        popup: restoreData.popup
      });      
    }
    
    restoreData = null; 
  }
  
  function needShowWelcome(){      
    if(!localStorage[LAST_VERSION_KEY]){
      localStorage[LAST_VERSION_KEY] = getCurrentVersion();
      //localStorage[SHOULD_SHOW_WELCOME_KEY] = "1";
    }  
    else{
      var currentVersion = getCurrentVersion();
      if(localStorage[LAST_VERSION_KEY] != currentVersion){
        localStorage[SHOULD_SHOW_WELCOME_KEY] = "1";
        localStorage[LAST_VERSION_KEY] = currentVersion;
      }
    }     
    
    return localStorage[SHOULD_SHOW_WELCOME_KEY] === "1";
  }  
  
  
  setInterval(function(){
    if(needShowWelcome() && !restoreData){
      restoreData = {};
      chrome.browserAction.getBadgeText({}, function(text){
        restoreData.badgeText = text;
        
        chrome.browserAction.setBadgeText({
          text: BADGE_TEXT
        });
        
        chrome.browserAction.getPopup({}, function(popupName){
          if(popupName){
            popupName = popupName.replace(/chrome-extension:\/\/[^\/]+/, ""); 
            restoreData.popup = popupName;
            chrome.browserAction.setPopup({
              popup: ""
            }); 
          }          
        });
      });
    }
  }, 1000);
  
  chrome.browserAction.onClicked.addListener(function(tab){
    if(needShowWelcome()){
      tab._ignore = true;
      localStorage[SHOULD_SHOW_WELCOME_KEY] = "0";
      restoreBrowserAction();
      
      window.open(getWelcomeUrl());
    }
  });  
    */
})();
