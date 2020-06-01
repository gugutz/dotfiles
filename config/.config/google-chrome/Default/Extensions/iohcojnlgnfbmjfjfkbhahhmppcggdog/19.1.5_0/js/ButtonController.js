(function(){
  
  var ButtonController = function() {
    
    function refreshButton() {
      
      fvdSynchronizer.Server.Sync.activityState(function(activityState) {
        var image = "/images/icons/24x24_nosync.png";
        if(activityState === "logged") {
          image = "/images/icons/24x24.png";
          switch(fvdSynchronizer.Server.Sync.syncState()){
            case "sync":
              image = "/images/icons/24x24_sync.png";
            break;
            case "hasDataToSync":
              
            break;
          }
        }
        chrome.browserAction.setIcon( {
          path: image
        } );
      });

    }
    /*
    chrome.extension.onRequest.addListener(function( request ){
      
      if( request.subject == "changeSyncState" ){
        refreshButton();
      }
      
    });
    */
    
    this.refreshButton = function(){
      refreshButton();
    };
    
    window.addEventListener( "load", function(){
      
      refreshButton();

    }, false );
    
  };

  fvdSynchronizer.Observer.registerCallback("event:login", function() {
    fvdSynchronizer.ButtonController.refreshButton();
  });
  fvdSynchronizer.Observer.registerCallback("event:logout", function() {
    fvdSynchronizer.ButtonController.refreshButton();
  });
  
  this.ButtonController = new ButtonController();
  
}).apply( fvdSynchronizer );