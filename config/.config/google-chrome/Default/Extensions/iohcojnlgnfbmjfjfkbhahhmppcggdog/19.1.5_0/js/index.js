/* init main namespace */

var fvdSynchronizer = {
	
	id: function(){

		return chrome.app.getDetails().id;				

	},
	
	eachDriver: function( callback ){
		
		for( var k in fvdSynchronizer.Driver ){
			
			callback( fvdSynchronizer.Driver[k], k );
			
		}
		
	}
	
};