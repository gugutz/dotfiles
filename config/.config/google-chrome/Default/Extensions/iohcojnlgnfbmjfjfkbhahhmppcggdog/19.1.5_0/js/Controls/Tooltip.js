(function(){
	var ToolTip = function(){
		
	}
	
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
			}
			
			if( this._container ){
				this.close( setFunction );
			}		
			else{
				setFunction();
			}	
		},
		
		close: function( callback ){
			
			fvdSynchronizer.Controls.ToolTip._container.setAttribute( "active", 0 );
			
			fvdSynchronizer.Controls.ToolTip._container.addEventListener( "webkitTransitionEnd", function(){
				try{
					fvdSynchronizer.Controls.ToolTip._container.parentNode.removeChild( fvdSynchronizer.Controls.ToolTip._container );
					fvdSynchronizer.Controls.ToolTip._container = null;
					fvdSynchronizer.Controls.ToolTip._currentElement = null;
					fvdSynchronizer.Controls.ToolTip._removeClickListener();
					
					if( callback ){
						callback();
					}
				}
				catch( ex ){
					
				}
			}, false );		
			
		},
		
		_clickListener: function( event ){
			if( fvdSynchronizer.Controls.ToolTip._container ){
				var el = event.target;
				do{
					if( el == fvdSynchronizer.Controls.ToolTip._container ){
						return;
					}
					el = el.parentNode;
				}
				while( el );
			}	
			
			fvdSynchronizer.Controls.ToolTip.close();
		},
				
		_assignClickListener: function(){
			document.addEventListener( "click", fvdSynchronizer.Controls.ToolTip._clickListener, false );
		},
		
		_removeClickListener: function(){
			document.removeEventListener( "click", fvdSynchronizer.Controls.ToolTip._clickListener );
		}
	}
	
	this.ToolTip = new ToolTip();
}).apply( fvdSynchronizer.Controls );
