



var PIXEL_STEP  = 10;
var LINE_HEIGHT = 40;
var PAGE_HEIGHT = 800;


function normalizeWheel(/*object*/ event) /*object*/ {
  var sX = 0, sY = 0,       // spinX, spinY
      pX = 0, pY = 0;       // pixelX, pixelY

  // Legacy
  if ('detail'      in event) { sY = event.detail; }
  if ('wheelDelta'  in event) { sY = -event.wheelDelta / 120; }
  if ('wheelDeltaY' in event) { sY = -event.wheelDeltaY / 120; }
  if ('wheelDeltaX' in event) { sX = -event.wheelDeltaX / 120; }

  // side scrolling on FF with DOMMouseScroll
  if ( 'axis' in event && event.axis === event.HORIZONTAL_AXIS ) {
    sX = sY;
    sY = 0;
  }

  pX = sX * PIXEL_STEP;
  pY = sY * PIXEL_STEP;

  if ('deltaY' in event) { pY = event.deltaY; }
  if ('deltaX' in event) { pX = event.deltaX; }

  if ((pX || pY) && event.deltaMode) {
    if (event.deltaMode == 1) {          // delta in LINE units
      pX *= LINE_HEIGHT;
      pY *= LINE_HEIGHT;
    } else {                             // delta in PAGE units
      pX *= PAGE_HEIGHT;
      pY *= PAGE_HEIGHT;
    }
  }

	return { deltaX : pX, deltaY : pY, deltaZ : 0 }
  // Fall-back if spin cannot be determined
  // if (pX && !sX) { sX = (pX < 1) ? -1 : 1; }
  // if (pY && !sY) { sY = (pY < 1) ? -1 : 1; }
	//
  // return { spinX  : sX,
  //          spinY  : sY,
  //          pixelX : pX,
  //          pixelY : pY };
}



function subscribe(app) {

	window.onmousedown = function(e) {
	  app.ports.windowMouseDown.send(e)
	}

	window.onmouseup = function(e) {
	  app.ports.windowMouseUp.send(e)
	}

	window.onclick = function(e) {
	  app.ports.windowClick.send(e)
	}

		window.onmousemove = function(e) {
	    app.ports.windowMouseMove.send(e)
	  }

		var onWheel = function(e) {
	    app.ports.windowMouseWheel.send(normalizeWheel(e))
			// e.preventDefault()
			// e.stopPropagation()
	  }

    window.onmousewheel = onWheel;

		window.onfocus = function() {
	    app.ports.windowFocus.send(true)
	  }

	  window.onblur = function() {
	    app.ports.windowFocus.send(false)
	  }


		app.ports.askGeometry.subscribe(function(id) {
        var elem = document.getElementById(id);
				if (elem)
        	app.ports.clientRect.send([id, elem.getBoundingClientRect()]);
    });


		app.ports.loadImage.subscribe(function(imageFile) {

				const img = document.createElement( "img" )
				img.onload = () => app.ports.imageLoaded.send( {src : img.src, size : {x : img.width, y : img.height}} )
				img.src = imageFile
    });


}
