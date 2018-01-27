



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


function getReason(event) {
  if (event.code == 1000)
    return "Normal closure, meaning that the purpose for which the connection was established has been fulfilled.";
  else if(event.code == 1001)
      return "An endpoint is \"going away\", such as a server going down or a browser having navigated away from a page.";
  else if(event.code == 1002)
      return "An endpoint is terminating the connection due to a protocol error";
  else if(event.code == 1003)
      return "An endpoint is terminating the connection because it has received a type of data it cannot accept (e.g., an endpoint that understands only text data MAY send this if it receives a binary message).";
  else if(event.code == 1004)
      return "Reserved. The specific meaning might be defined in the future.";
  else if(event.code == 1005)
      return "No status code was actually present.";
  else if(event.code == 1006)
     return "The connection was closed abnormally, e.g., without sending or receiving a Close control frame";
  else if(event.code == 1007)
      return "An endpoint is terminating the connection because it has received data within a message that was not consistent with the type of the message (e.g., non-UTF-8 [http://tools.ietf.org/html/rfc3629] data within a text message).";
  else if(event.code == 1008)
      return "An endpoint is terminating the connection because it has received a message that \"violates its policy\". This reason is given either if there is no other sutible reason, or if there is a need to hide specific details about the policy.";
  else if(event.code == 1009)
     return "An endpoint is terminating the connection because it has received a message that is too big for it to process.";
  else if(event.code == 1010) // Note that this status code is not used by the server, because it can fail the WebSocket handshake instead.
      return "An endpoint (client) is terminating the connection because it has expected the server to negotiate one or more extension, but the server didn't return them in the response message of the WebSocket handshake. <br /> Specifically, the extensions that are needed are: " + event.reason;
  else if(event.code == 1011)
      return "A server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request.";
  else if(event.code == 1015)
      return "The connection was closed due to a failure to perform a TLS handshake (e.g., the server certificate can't be verified).";
  else
      return "Unknown reason";
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


		app.ports.loadImage.subscribe(function(t) {
      const img = document.createElement( "img" )

      k = t[0]
      src = t[1]

      img.onload = () => app.ports.imageLoaded.send( [k, {src : src, size : {x : img.width, y : img.height}}] )
      img.src = src
    });


    var socket;

    app.ports.connect.subscribe(function(host) {
      socket = new WebSocket(host);

      socket.onclose = function() {
        app.ports.onClose.send ()
      }

      socket.onopen = function() {
        app.ports.onOpen.send ()
      }

      // socket.onerror = function(e) {
      //   app.ports.onError.send (getReason(e))
      // }

      socket.onmessage = function(e) {
        app.ports.onMessage.send (e.data)
      }


    });

}
