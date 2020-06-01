fvdSynchronizer.ContentScriptsController = new function(){

	chrome.extension.onMessage.addListener(function( msg, sender, callback ){

		if( msg.action ){

			if( msg.action.indexOf("event:") === 0 ) {
        var timeout = 0;
        if(msg.action == "event:login") {
          // wait for cookie handling
          timeout = 500;
        }
        setTimeout(function() {
          fvdSynchronizer.Observer.fireEvent( msg.action, [ msg.data ] );
        }, timeout);
				return;
			}

			switch( msg.action ){

				case "getCurrentUsage":

					var result = {};

					fvdSynchronizer.Utils.Async.arrayProcess( fvdSynchronizer.Server.Sync.getDriversList(), function( driverName, apc ){

						var d = fvdSynchronizer.Driver[ driverName ];

						if(!d.isAllowed){
              result[ driverName ] = -1;
              return apc();
						}
						d.isAllowed(function(allowed){
						  if(allowed){
						    d.totalItemsCount( function( count ){
                  result[ driverName ] = count;
                  apc();
                } );
						  }
						  else{
                result[ driverName ] = -1;
                return apc();
						  }
						});

					}, function(){

						callback( result );

					} );

					return true;

				break;

				case "connect":

					callback({});

					return true;

				break;

				case "_response":



				break;


				default:

					callback({});
					return true;

				break;

			}

		}

	});

}
