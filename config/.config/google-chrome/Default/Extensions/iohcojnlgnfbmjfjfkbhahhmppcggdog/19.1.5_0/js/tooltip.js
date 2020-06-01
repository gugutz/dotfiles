(function(){
	var ToolTip = function(){
		
	};
	
	ToolTip.prototype = {
		_container: null,
		_arrowLeftOffset: 14,
		_currentElement: null,
		
		displayImage: function( elem, imageSrc, event ){
			var html = "<img src=\""+imageSrc+"\"/>";
			this.display( elem, html, event );
		},
		
		display: function( elem, html, event ){
			event.stopPropagation();
			
			if( this._currentElement == elem ){
				return;				
			}
			
			this._currentElement = elem;
			
			var that = this;
			
			var setFunction = function(){
				var toolTipContainer = chrome.extension.getBackgroundPage().document.getElementById( "tiptip_holder" ).cloneNode(true);
				that._container = toolTipContainer;	
				
				// position
				var offset = fvdSynchronizer.Utils.getOffset( elem );
				toolTipContainer.style.left = offset.left + (elem.offsetWidth/2) - that._arrowLeftOffset - 1 + "px";
				toolTipContainer.style.top = offset.top + elem.offsetHeight + 1 + "px";			
				
				document.body.appendChild( that._container );	
				var contentContainer = document.getElementById("tiptip_content");
				contentContainer.innerHTML = html;
				setTimeout( function(){
					toolTipContainer.setAttribute( "active", 1 );				
					that._assignClickListener();				
				}, 0 );	
			};
			
			if( this._container ){
				this.close( setFunction );
			}		
			else{
				setFunction();
			}	
		},
		
		close: function( callback ){
			
			fvdSynchronizer.ToolTip._container.setAttribute( "active", 0 );
			
			fvdSynchronizer.ToolTip._container.addEventListener( "webkitTransitionEnd", function(){
				try{
					fvdSynchronizer.ToolTip._container.parentNode.removeChild( fvdSynchronizer.ToolTip._container );
					fvdSynchronizer.ToolTip._container = null;
					fvdSynchronizer.ToolTip._currentElement = null;
					fvdSynchronizer.ToolTip._removeClickListener();
					
					if( callback ){
						callback();
					}
				}
				catch( ex ){
					
				}
			}, false );		
			
		},
		
		_clickListener: function( event ){
			if( fvdSynchronizer.ToolTip._container ){
				var el = event.target;
				do{
					if( el == fvdSynchronizer.ToolTip._container ){
						return;
					}
					el = el.parentNode;
				}
				while( el );
			}	
			
			fvdSynchronizer.ToolTip.close();
		},
				
		_assignClickListener: function(){
			document.addEventListener( "click", fvdSynchronizer.ToolTip._clickListener, false );
		},
		
		_removeClickListener: function(){
			document.removeEventListener( "click", fvdSynchronizer.ToolTip._clickListener );
		}
	};
	
	this.ToolTip = new ToolTip();
}).apply( fvdSynchronizer );
